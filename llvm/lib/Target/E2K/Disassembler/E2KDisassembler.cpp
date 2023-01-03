//===- E2KDisassembler.cpp - Disassembler for E2K -----------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file is part of the E2K Disassembler.
//
//===----------------------------------------------------------------------===//

#include "E2K.h"
#include "MCTargetDesc/E2KMCTargetDesc.h"
#include "TargetInfo/E2KTargetInfo.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCDecoderOps.h"
#include "llvm/MC/MCDisassembler/MCDisassembler.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/TargetRegistry.h"

using namespace llvm;

#define DEBUG_TYPE "e2k-disassembler"

typedef MCDisassembler::DecodeStatus DecodeStatus;

namespace {

struct E2KInstruction {
  uint32_t HS = 0; // header syllable
  uint32_t SS = 0; // stub syllable
  uint32_t ALS[6] = {}; // arithmetic logic channel syllable
  uint32_t CS[2] = {}; // control syllable
  uint16_t ALES[6] = {}; // arithmetic logic extension channel semi-syllable
  uint16_t AAS[6] = {}; // array access semi-syllable
  uint32_t LTS[4] = {}; // literal syllable
  uint32_t PLS[3] = {}; // predicate logic syllable
  uint32_t CDS[3] = {}; // conditional syllable
  uint16_t SubCDS[6] = {};

  bool SetMark = false;
  bool Unused = false;
  bool LoopMode = false;

  bool HasSS = false;
  bool HasALS[6] = {false, false, false, false, false, false};
  bool HasALES[6] = {false, false, false, false, false, false};
  bool HasCS[2] = {false, false};
  bool HasAAS[6] = {false, false, false, false, false, false};

  uint8_t NumPLS = 0;
  uint8_t NumCDS = 0;
  uint8_t NumLTS = 0;

  uint8_t NOP = 0;
  uint8_t Length = 0;
  uint8_t Words = 0;

  const uint32_t BitSetMark = 13;
  const uint32_t BitSS = 12;
  const uint32_t BitUnused = 11;
  const uint32_t BitLoopMode = 10;

  uint8_t SSType = 0;

  const uint32_t BitType = 20;

  const uint32_t BitEAP = 29;
  const uint32_t BitBAP = 28;
  const uint32_t BitSRP = 27;
  const uint32_t BitVDFI = 26;
  const uint32_t BitCRP = 25;

  // memory access specifier; only applies to channels 0, 2, 3, 5
  uint8_t MAS[6] = {0, 0xFF, 0, 0, 0xFF, 0};

  uint8_t show_bit(uint32_t Value, const uint32_t Bit) const {
    uint32_t Mask = 1 << Bit;
    return (Value & Mask) == Mask ? 1 : 0;
  }

  uint32_t show_bits(uint32_t Value, const uint32_t Pos, const uint32_t Count) const {
    uint32_t Mask = (1 << Count) - 1;
    return (Value >> Pos) & Mask;
  }

  void dump() const {
    LLVM_DEBUG(dbgs() << "HS" << " : " << format("0x%04x", HS) << "\n");

    for (uint32_t i = 0; i < 6; ++i) {
      LLVM_DEBUG(dbgs() << "\tHS.HasALS" << i << " : " << HasALS[i] << "\n");
    }
    for (uint32_t i = 0; i < 6; ++i) {
      LLVM_DEBUG(dbgs() << "\tHS.HasALES" << i << " : " << HasALES[i] << "\n");
    }

    LLVM_DEBUG(dbgs() << "\tHS.NumPLS" << " : " << (uint32_t) NumPLS << "\n");
    LLVM_DEBUG(dbgs() << "\tHS.NumCDS" << " : " << (uint32_t) NumCDS << "\n");

    for (uint32_t i = 0; i < 2; ++i) {
      LLVM_DEBUG(dbgs() << "\tHS.HasCS" << i <<  " : " << HasCS[i] << "\n");
    }

    LLVM_DEBUG(dbgs() << "\tHS.SetMark" << " : " << SetMark << "\n");
    LLVM_DEBUG(dbgs() << "\tHS.HasSS" << " : " << HasSS << "\n");
    LLVM_DEBUG(dbgs() << "\tHS.Unused" << " : " << Unused << "\n");
    LLVM_DEBUG(dbgs() << "\tHS.LoopMode" << " : " << LoopMode << "\n");
    LLVM_DEBUG(dbgs() << "\tHS.NOP" << " : " << (uint32_t) NOP << "\n");
    LLVM_DEBUG(dbgs() << "\tHS.Length" << " : " << (uint32_t) Length << "\n");
    LLVM_DEBUG(dbgs() << "\tHS.Words" << " : " << (uint32_t) Words << "\n");

    LLVM_DEBUG(dbgs() << "\tNumLTS" << " : " << (uint32_t) NumLTS << "\n");

    if (HasSS) {
      LLVM_DEBUG(dbgs() << "SS" << " : " << format("0x%04x", SS) << "\n");

      for (uint32_t i = 0; i < 6; ++i) {
        LLVM_DEBUG(dbgs() << "\tSS.HasAAS" << i << " : " << HasAAS[i] << "\n");
      }
    }
    for (uint32_t i = 0; i < 6; ++i) {
      if (HasALS[i]) {
        LLVM_DEBUG(dbgs() << "ALS" << i << " : " << format("0x%04x", ALS[i]) << "\n");
      }
    }
    if (HasCS[0]) {
      LLVM_DEBUG(dbgs() << "CS0" << " : " << format("0x%04x", CS[0]) << "\n");
    }
    for (uint32_t i : {2, 5}) {
      if (HasALES[i]) {
        LLVM_DEBUG(dbgs() << "ALES" << i << " : " << format("0x%02x", ALES[i]) << "\n");
      }
    }
    if (HasCS[1]) {
      LLVM_DEBUG(dbgs() << "CS1" << " : " << format("0x%04x", CS[1]) << "\n");
    }
    for (uint32_t i : {0, 1, 3, 4}) {
      if (HasALES[i]) {
        LLVM_DEBUG(dbgs() << "ALES" << i << " : " << format("0x%02x", ALES[i]) << "\n");
      }
      }
    for (uint32_t i = 0; i < 6; ++i) {
      if (HasAAS[i]) {
        LLVM_DEBUG(dbgs() << "AAS" << i << " : " << format("0x%02x", AAS[i]) << "\n");
      }
    }
    for (uint32_t i = 0; i < NumLTS; ++i) {
      LLVM_DEBUG(dbgs() << "LTS" << i << " : " << format("0x%02x", LTS[i]) << "\n");
    }
    for (uint32_t i = 0; i < NumPLS; ++i) {
      LLVM_DEBUG(dbgs() << "PLS" << i << " : " << format("0x%02x", PLS[i]) << "\n");
    }
    for (uint32_t i = 0; i < NumCDS; ++i) {
      LLVM_DEBUG(dbgs() << "CDS" << i << " : " << format("0x%02x", CDS[i]) << "\n");
    }
  }
};

/// A disassembler class for E2K.
class E2KDisassembler : public MCDisassembler {
public:
  std::unique_ptr<MCInst *> CurrentBundle;
  std::unique_ptr<E2KInstruction*> CurrentInst;
  E2KDisassembler(const MCSubtargetInfo &STI, MCContext &Ctx)
      : MCDisassembler(STI, Ctx), CurrentBundle(new MCInst *), CurrentInst(new E2KInstruction*) {}
  virtual ~E2KDisassembler() = default;

  DecodeStatus getInstruction(MCInst &Instr, uint64_t &Size,
                              ArrayRef<uint8_t> Bytes, uint64_t Address,
                              raw_ostream &CStream) const override;

  // these may encode multiple instructions at once (or none at all)
  // almost all of them have some unique encoding anyway
  DecodeStatus DecodeSS(E2KInstruction & E2KInst, MCInst &Instr) const;
  DecodeStatus DecodeCS0(E2KInstruction & E2KInst, MCInst &Instr, uint64_t Address) const;
  DecodeStatus DecodeCS1(E2KInstruction & E2KInst, MCInst &Instr, uint64_t Address) const;
};
}

static MCDisassembler *createE2KDisassembler(const Target &T,
                                               const MCSubtargetInfo &STI,
                                               MCContext &Ctx) {
  return new E2KDisassembler(STI, Ctx);
}


extern "C" LLVM_EXTERNAL_VISIBILITY void LLVMInitializeE2KDisassembler() {
  // Register the disassembler.
  TargetRegistry::RegisterMCDisassembler(getTheE2K32Target(),
                                         createE2KDisassembler);
  TargetRegistry::RegisterMCDisassembler(getTheE2K64Target(),
                                         createE2KDisassembler);
  TargetRegistry::RegisterMCDisassembler(getTheE2K128Target(),
                                         createE2KDisassembler);
  TargetRegistry::RegisterMCDisassembler(getTheE2K12864Target(),
                                         createE2KDisassembler);
}

static const unsigned DSTRegTable[] = {
    // FIXME : check
    0, 0, 0, 0,
    0, 0, 0, 0,
    0, 0, 0, 0,
    0, E2K::TST, E2K::TC, E2K::TCD,
    0, E2K::CTPR1, E2K::CTPR2, E2K::CTPR3,
    0, 0, 0, 0,
    0, 0, 0, 0,
    0, 0, E2K::EMPTY_LO, E2K::EMPTY_HI,
};

static const unsigned StateRegTable[] = {
    E2K::PSR, E2K::WD, 0, 0, E2K::CORE_MODE, 0, E2K::CWD, E2K::PSP_HI,
    0, E2K::PSP_LO, 0, E2K::PSHTP, 0, E2K::PCSP_HI, 0, E2K::PCSP_LO,
    0, 0, 0, E2K::PCSHTP, 0, E2K::CTPR1, E2K::CTPR2, E2K::CTPR3,
    0, 0, 0, 0, 0, 0, E2K::SBR, 0,
    0, E2K::CUTD, 0, E2K::EIR,0, E2K::CUIR, E2K::OSCUD_HI, E2K::OSCUD_LO,
    E2K::OSGD_HI, E2K::OSGD_LO, E2K::OSEM, 0,E2K::USD_HI, E2K::USD_LO, 0, E2K::OSR0,
    E2K::CUD_HI, E2K::CUD_LO, E2K::GD_HI, E2K::GD_LO,E2K::CS_HI, E2K::CS_LO, E2K::DS_HI, E2K::DS_LO,
    E2K::ES_HI, E2K::ES_LO, E2K::FS_HI, E2K::FS_LO,E2K::GS_HI, E2K::GS_LO, E2K::SS_HI, E2K::SS_LO,
    E2K::DIBCR, E2K::DIMCR, E2K::DIBSR, E2K::DTCR, 0, 0, 0, 0,
    E2K::DIBAR0, E2K::DIBAR1, E2K::DIBAR2, E2K::DIBAR3, E2K::DIMAR0, E2K::DIMAR1, E2K::DTARF, E2K::DTART,
    0, E2K::CR0_HI, 0, E2K::CR0_LO, 0, E2K::CR1_HI, 0, E2K::CR1_LO,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    E2K::SCLKM1, E2K::SCLKM2, 0, 0, 0, 0, 0, 0,
    E2K::CU_HW0, E2K::CU_HW1, 0, 0, 0, 0, 0, 0,
    E2K::UPSR, E2K::IP, E2K::NIP, E2K::LSR, E2K::PFPFR, E2K::FPCR, E2K::FPSR, E2K::ILCR,
    E2K::BR, E2K::BGR, E2K::IDR, 0, 0, 0, 0, 0,
    E2K::CLKR, E2K::RNDPR, E2K::SCLKR, 0, 0, 0, 0, 0,
    0, 0, 0, 0, E2K::TIR_HI, E2K::TIR_LO, 0, 0,
    E2K::RPR_LO, E2K::SBBP, E2K::RPR_HI, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    E2K::UPSRM, 0, 0, E2K::LSR1, 0, 0, 0, E2K::ILCR1,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0
};

enum RegSize {
  s = 's',
  d = 'd',
  q = 'q',
  p = 'p',
};

enum RegType {
  r = 'r',
  b = 'b',
  g = 'g',
};

static unsigned ChooseBase(RegSize Rs, RegType Rt)
{
  switch (Rt) {
  case r: {
    switch (Rs) {
    case s: return E2K::R0;
    case d: return E2K::DR0;
    case q: return E2K::QR0;
    case p: return E2K::QPR0;
    default: llvm_unreachable("unknown register"); break;
    }
  }
  case b: {
    switch (Rs) {
    case s: return E2K::B0;
    case d: return E2K::DB0;
    case q: return E2K::QB0;
    case p: return E2K::QPB0;
    default: llvm_unreachable("unknown register"); break;
    }
  }
  case g: {
    switch (Rs) {
    case s: return E2K::G0;
    case d: return E2K::DG0;
    case q: return E2K::QG0;
    case p: return E2K::QPG0;
    default: llvm_unreachable("unknown register"); break;
    }
  }
  default: llvm_unreachable("unknown register"); break;
  }
}

static DecodeStatus DecodeRegSTATERegisterClass(MCInst &Inst, unsigned RegNo,
                        uint64_t Address,
                        const MCDisassembler *Decoder) {
  if (RegNo > 255)
    return MCDisassembler::Fail;
  unsigned reg = StateRegTable[RegNo];
  if (reg == E2K::NoRegister)
    return MCDisassembler::Fail;
  Inst.addOperand(MCOperand::createReg(reg));
  return MCDisassembler::Success;
}

static DecodeStatus DecodeDST1RegisterClass(MCInst &Inst, unsigned RegNo,
                                             uint64_t Address,
                                             const MCDisassembler *Decoder,
                                             RegSize rs) {

  if (RegNo > 255)
    return MCDisassembler::Fail;
  if ((RegNo & 0x80) == 0) {
    unsigned index = RegNo & 0x7F;
    Inst.addOperand(MCOperand::createReg(ChooseBase(rs, b) + index));
  } else if ((RegNo & 0xC0) == 0x80) {
    unsigned index = RegNo & 0x3F;
    Inst.addOperand(MCOperand::createReg(ChooseBase(rs, r) + index));
  } else if ((RegNo & 0xE0) == 0xC0) {
    unsigned index = RegNo & 0x1F;
    unsigned reg = DSTRegTable[index];
    if (reg == E2K::NoRegister)
      return MCDisassembler::Fail;
    Inst.addOperand(MCOperand::createReg(reg));
  } else {
    unsigned index = RegNo & 0x1F;
    Inst.addOperand(MCOperand::createReg(ChooseBase(rs, g) + index));
  }
  return MCDisassembler::Success;
}

static DecodeStatus DecodeSRC1RegisterClass(MCInst &Inst, unsigned RegNo,
                                             uint64_t Address,
                                             const MCDisassembler *Decoder,
                                             RegSize rs) {
  if (RegNo > 255)
    return MCDisassembler::Fail;
  if ((RegNo & 0x80) == 0) {
    unsigned index = RegNo & 0x7F;
    Inst.addOperand(MCOperand::createReg(ChooseBase(rs, b) + index));
  } else if ((RegNo & 0xC0) == 0x80) {
    unsigned index = RegNo & 0x3F;
    Inst.addOperand(MCOperand::createReg(ChooseBase(rs, r) + index));
  } else if ((RegNo & 0xE0) == 0xC0) {
    unsigned value = RegNo & 0x1F;
    Inst.addOperand(MCOperand::createImm(value));
  } else {
    unsigned index = RegNo & 0x1F;
    Inst.addOperand(MCOperand::createReg(ChooseBase(rs, g) + index));
  }
  return MCDisassembler::Success;
}

static DecodeStatus DecodeSRC2RegisterClass(MCInst &Inst, unsigned RegNo,
                                            uint64_t Address,
                                            const MCDisassembler *Decoder,
                                            RegSize rs) {
  auto DAsm = static_cast<const E2KDisassembler*>(Decoder);
  if (RegNo > 255)
    return MCDisassembler::Fail;
  if ((RegNo & 0x80) == 0) {
    unsigned index = RegNo & 0x7F;
    Inst.addOperand(MCOperand::createReg(ChooseBase(rs, b) + index));
  } else if ((RegNo & 0xC0) == 0x80) {
    unsigned index = RegNo & 0x3F;
    Inst.addOperand(MCOperand::createReg(ChooseBase(rs, r) + index));
  } else if ((RegNo & 0xF0) == 0xC0) {
    unsigned value = RegNo & 0x1F;
    Inst.addOperand(MCOperand::createImm(value));
  } else if ((RegNo & 0xF8) == 0xD0) {

    uint32_t literal = RegNo & 0b11;
    if (literal >= (*DAsm->CurrentInst)->NumLTS)
      return MCDisassembler::Fail;

    uint32_t shift = (RegNo & 0b100) ? 16 : 0;
    uint32_t lts0 = (*DAsm->CurrentInst)->LTS[literal];
    uint64_t value = (lts0 >> shift) & 0xFFFF;

    unsigned flags = shift ? E2K::F16SHI : E2K::F16SLO;
    flags |= Inst.getNumOperands() << 3;
    Inst.setFlags(Inst.getFlags() | flags);

    Inst.addOperand(MCOperand::createImm(value));

  } else if ((RegNo & 0xFC) == 0xD8) {

    uint32_t literal = RegNo & 0b11;
    if (literal >= (*DAsm->CurrentInst)->NumLTS)
      return MCDisassembler::Fail;
    uint32_t lts0 = (*DAsm->CurrentInst)->LTS[literal];
    uint32_t value = lts0;

    unsigned flags = E2K::F32S;
    flags |= Inst.getNumOperands() << 3;
    Inst.setFlags(Inst.getFlags() | flags);

    Inst.addOperand(MCOperand::createImm(value));
  } else if ((RegNo & 0xFC) == 0xDC) {

    uint32_t literal = RegNo & 0b11;
    if (literal + 1 >= (*DAsm->CurrentInst)->NumLTS)
      return MCDisassembler::Fail;
    uint32_t lts0 = (*DAsm->CurrentInst)->LTS[literal];
    uint32_t lts1 = (*DAsm->CurrentInst)->LTS[literal + 1];
    uint64_t value = lts0 | (uint64_t(lts1) << 32);

    unsigned flags = E2K::F64;
    flags |= Inst.getNumOperands() << 3;
    Inst.setFlags(Inst.getFlags() | flags);

    Inst.addOperand(MCOperand::createImm(value));

  } else {
    unsigned index = RegNo & 0x1F;
    Inst.addOperand(MCOperand::createReg(ChooseBase(rs, g) + index));
  }
  return MCDisassembler::Success;
}

static DecodeStatus DecodeSRC3RegisterClass(MCInst &Inst, unsigned RegNo,
                                            uint64_t Address,
                                            const MCDisassembler *Decoder,
                                            RegSize rs) {
  if (RegNo > 255)
    return MCDisassembler::Fail;
  if ((RegNo & 0x80) == 0) {
    unsigned index = RegNo & 0x7F;
    Inst.addOperand(MCOperand::createReg(ChooseBase(rs, b) + index));
  } else if ((RegNo & 0xC0) == 0x80) {
    unsigned index = RegNo & 0x3F;
    Inst.addOperand(MCOperand::createReg(ChooseBase(rs, r) + index));
  } else if ((RegNo & 0xE0) == 0xC0) {
    unsigned value = RegNo & 0x1F;
    Inst.addOperand(MCOperand::createImm(value));
  } else {
    unsigned index = RegNo & 0x1F;
    Inst.addOperand(MCOperand::createReg(ChooseBase(rs, g) + index));
  }
  return MCDisassembler::Success;
}

static DecodeStatus DecodeDST1SRegisterClass(MCInst &Inst, unsigned RegNo,
                                               uint64_t Address,
                                               const MCDisassembler *Decoder) {
  return DecodeDST1RegisterClass(Inst, RegNo, Address, Decoder, s);
}

static DecodeStatus DecodeDST1DRegisterClass(MCInst &Inst, unsigned RegNo,
                                             uint64_t Address,
                                             const MCDisassembler *Decoder) {
  return DecodeDST1RegisterClass(Inst, RegNo, Address, Decoder, d);
}

static DecodeStatus DecodeDST1QRegisterClass(MCInst &Inst, unsigned RegNo,
                                             uint64_t Address,
                                             const MCDisassembler *Decoder) {
  return DecodeDST1RegisterClass(Inst, RegNo, Address, Decoder, q);
}

static DecodeStatus DecodeDST1QPRegisterClass(MCInst &Inst, unsigned RegNo,
                                             uint64_t Address,
                                             const MCDisassembler *Decoder) {
  return DecodeDST1RegisterClass(Inst, RegNo, Address, Decoder, p);
}


static DecodeStatus DecodeSRC1SRegisterClass(MCInst &Inst, unsigned RegNo,
                                          uint64_t Address,
                                          const MCDisassembler *Decoder) {
  return DecodeSRC1RegisterClass(Inst, RegNo, Address, Decoder, s);
}

static DecodeStatus DecodeSRC1DRegisterClass(MCInst &Inst, unsigned RegNo,
                                             uint64_t Address,
                                             const MCDisassembler *Decoder) {
  return DecodeSRC1RegisterClass(Inst, RegNo, Address, Decoder, d);
}

static DecodeStatus DecodeSRC1QRegisterClass(MCInst &Inst, unsigned RegNo,
                                             uint64_t Address,
                                             const MCDisassembler *Decoder) {
  return DecodeSRC1RegisterClass(Inst, RegNo, Address, Decoder, q);
}

static DecodeStatus DecodeSRC1QPRegisterClass(MCInst &Inst, unsigned RegNo,
                                             uint64_t Address,
                                             const MCDisassembler *Decoder) {
  return DecodeSRC1RegisterClass(Inst, RegNo, Address, Decoder, p);
}


static DecodeStatus DecodeSRC2SRegisterClass(MCInst &Inst, unsigned RegNo,
                                             uint64_t Address,
                                             const MCDisassembler *Decoder) {
  return DecodeSRC2RegisterClass(Inst, RegNo, Address, Decoder, s);
}

static DecodeStatus DecodeSRC2DRegisterClass(MCInst &Inst, unsigned RegNo,
                                             uint64_t Address,
                                             const MCDisassembler *Decoder) {
  return DecodeSRC2RegisterClass(Inst, RegNo, Address, Decoder, d);
}

static DecodeStatus DecodeSRC2QRegisterClass(MCInst &Inst, unsigned RegNo,
                                             uint64_t Address,
                                             const MCDisassembler *Decoder) {
  return DecodeSRC2RegisterClass(Inst, RegNo, Address, Decoder, q);
}

static DecodeStatus DecodeSRC2QPRegisterClass(MCInst &Inst, unsigned RegNo,
                                             uint64_t Address,
                                             const MCDisassembler *Decoder) {
  return DecodeSRC2RegisterClass(Inst, RegNo, Address, Decoder, p);
}

static DecodeStatus DecodeSRC3SRegisterClass(MCInst &Inst, unsigned RegNo,
                                             uint64_t Address,
                                             const MCDisassembler *Decoder) {
  return DecodeSRC3RegisterClass(Inst, RegNo, Address, Decoder, s);
}

static DecodeStatus DecodeSRC3DRegisterClass(MCInst &Inst, unsigned RegNo,
                                             uint64_t Address,
                                             const MCDisassembler *Decoder) {
  return DecodeSRC3RegisterClass(Inst, RegNo, Address, Decoder, d);
}

static DecodeStatus DecodeSRC3QRegisterClass(MCInst &Inst, unsigned RegNo,
                                             uint64_t Address,
                                             const MCDisassembler *Decoder) {
  return DecodeSRC3RegisterClass(Inst, RegNo, Address, Decoder, q);
}

static DecodeStatus DecodeSRC3QPRegisterClass(MCInst &Inst, unsigned RegNo,
                                             uint64_t Address,
                                             const MCDisassembler *Decoder) {
  return DecodeSRC3RegisterClass(Inst, RegNo, Address, Decoder, p);
}

static DecodeStatus DecodePREDRegisterClass(MCInst &Inst, unsigned RegNo,
                                             uint64_t Address,
                                             const MCDisassembler *Decoder) {
  if (RegNo > 31)
    return MCDisassembler::Fail;
  Inst.addOperand(MCOperand::createReg(E2K::PRED0 + RegNo));
  return MCDisassembler::Success;
}

#include "E2KGenDisassemblerTables.inc"

/// Read four bytes from the ArrayRef and return 32 bit word.
static DecodeStatus readInstruction32(ArrayRef<uint8_t> Bytes, uint64_t & Offset,
                                      uint64_t &Size, uint32_t &Insn) {
  // We want to read exactly 4 Bytes of data.
  if (Bytes.size() < Offset + 4) {
    Size = 0;
    return MCDisassembler::Fail;
  }
  if (Size != 0 && Size < Offset + 4) {
    Size = 0;
    return MCDisassembler::Fail;
  }

  Insn = (Bytes[Offset] << 0) | (Bytes[Offset + 1] << 8) | (Bytes[Offset + 2] << 16) | (Bytes[Offset + 3] << 24);
  Offset += 4;

  return MCDisassembler::Success;
}

/// Read two bytes from the ArrayRef and return 16 bit word.
static DecodeStatus readInstruction16(ArrayRef<uint8_t> Bytes, uint64_t & Offset,
                                      uint64_t &Size, uint16_t &Insn) {
  // We want to read exactly 4 Bytes of data.
  if (Bytes.size() < Offset + 2) {
    Size = 0;
    return MCDisassembler::Fail;
  }
  if (Size != 0 && Size < Offset + 2) {
    Size = 0;
    return MCDisassembler::Fail;
  }

  Insn = (Bytes[Offset] << 0) | (Bytes[Offset + 1] << 8);
  Offset += 2;

  return MCDisassembler::Success;
}

static const uint8_t * DecoderTableE2K_ALS[6] = {
    DecoderTableE2K_ALS032,
    DecoderTableE2K_ALS132,
    DecoderTableE2K_ALS232,
    DecoderTableE2K_ALS332,
    DecoderTableE2K_ALS432,
    DecoderTableE2K_ALS532,
};

static const uint8_t * DecoderTableE2K_ALES[6] = {
    DecoderTableE2K_ALES048,
    DecoderTableE2K_ALES148,
    DecoderTableE2K_ALES248,
    DecoderTableE2K_ALES348,
    DecoderTableE2K_ALES448,
    DecoderTableE2K_ALES548,
};

DecodeStatus E2KDisassembler::DecodeSS(E2KInstruction & E2KInst, MCInst &Instr) const {

  if (!E2KInst.HasSS)
    return MCDisassembler::Success;

  uint32_t IPD = E2KInst.show_bits(E2KInst.SS, 30, 2);
  uint32_t ctop = E2KInst.show_bits(E2KInst.SS, 10, 2);
  uint32_t ctcond = E2KInst.show_bits(E2KInst.SS, 0, 9);

  uint32_t cond_type = E2KInst.show_bits(ctcond, 5, 4);
  uint32_t pred = E2KInst.show_bits(ctcond, 0, 5);

  if (ctcond != 0 && ctop != 0) {

    // there is a CALL instruction already
    if (!(E2KInst.HasCS[1] && ((E2KInst.CS[1] & 0xF0000000) >> 28) == 5)) {

      MCInst *SubInstr = getContext().createMCInst();
      SubInstr->setOpcode(E2K::CT);
      SubInstr->addOperand(MCOperand::createReg(E2K::CTPR1 - 1 + ctop));
      SubInstr->addOperand(MCOperand::createImm(cond_type));
      SubInstr->addOperand(MCOperand::createReg(E2K::PRED0 + pred));
      Instr.addOperand(MCOperand::createInst(SubInstr));
    }
  }

  if (IPD > 0) {
    MCInst *SubInstr = getContext().createMCInst();
    SubInstr->setOpcode(E2K::IPD);
    SubInstr->addOperand(MCOperand::createImm(IPD));
    Instr.addOperand(MCOperand::createInst(SubInstr));
  }

  if (E2KInst.SSType == 0) {

    if (E2KInst.show_bit(E2KInst.SS, E2KInst.BitEAP)) {
      MCInst *SubInstr = getContext().createMCInst();
      SubInstr->setOpcode(E2K::EAP);
      Instr.addOperand(MCOperand::createInst(SubInstr));
    }
    if (E2KInst.show_bit(E2KInst.SS, E2KInst.BitBAP)) {
      MCInst *SubInstr = getContext().createMCInst();
      SubInstr->setOpcode(E2K::BAP);
      Instr.addOperand(MCOperand::createInst(SubInstr));
    }

    bool HasCRP = E2KInst.show_bit(E2KInst.SS, E2KInst.BitCRP);
    bool HasSRP = E2KInst.show_bit(E2KInst.SS, E2KInst.BitSRP);

    if (HasCRP && HasSRP) {
      MCInst *SubInstr = getContext().createMCInst();
      SubInstr->setOpcode(E2K::SLRP);
      Instr.addOperand(MCOperand::createInst(SubInstr));
    } else if (HasCRP) {
      MCInst *SubInstr = getContext().createMCInst();
      SubInstr->setOpcode(E2K::CRP);
      Instr.addOperand(MCOperand::createInst(SubInstr));
    } else if (HasSRP) {
      MCInst *SubInstr = getContext().createMCInst();
      SubInstr->setOpcode(E2K::SRP);
      Instr.addOperand(MCOperand::createInst(SubInstr));
    }

    if (E2KInst.show_bit(E2KInst.SS, E2KInst.BitVDFI)) {
      MCInst *SubInstr = getContext().createMCInst();
      SubInstr->setOpcode(E2K::VDFI);
      Instr.addOperand(MCOperand::createInst(SubInstr));
    }

    uint32_t ABG = E2KInst.show_bits(E2KInst.SS, 23, 2);
    uint32_t ABN = E2KInst.show_bits(E2KInst.SS, 21, 2);
    uint32_t APB = E2KInst.show_bits(E2KInst.SS, 18, 2);
    uint32_t ALC = E2KInst.show_bits(E2KInst.SS, 16, 2);

    if (ABG) {
      MCInst *SubInstr = getContext().createMCInst();
      SubInstr->setOpcode(E2K::ABG);
      SubInstr->addOperand(MCOperand::createImm(ABG >> 1));
      SubInstr->addOperand(MCOperand::createImm(ABG & 1));
      Instr.addOperand(MCOperand::createInst(SubInstr));
    }
    if (ABN) {
      MCInst *SubInstr = getContext().createMCInst();
      SubInstr->setOpcode(E2K::ABN);
      SubInstr->addOperand(MCOperand::createImm(ABN >> 1));
      SubInstr->addOperand(MCOperand::createImm(ABN & 1));
      Instr.addOperand(MCOperand::createInst(SubInstr));
    }
    if (APB) {
      MCInst *SubInstr = getContext().createMCInst();
      SubInstr->setOpcode(E2K::ABP);
      SubInstr->addOperand(MCOperand::createImm(APB >> 1));
      SubInstr->addOperand(MCOperand::createImm(APB & 1));
      Instr.addOperand(MCOperand::createInst(SubInstr));
    }
    if (ALC) {
      MCInst *SubInstr = getContext().createMCInst();
      SubInstr->setOpcode(E2K::ALC);
      SubInstr->addOperand(MCOperand::createImm(ALC >> 1));
      SubInstr->addOperand(MCOperand::createImm(ALC & 1));
      Instr.addOperand(MCOperand::createInst(SubInstr));
    }
  }
  return MCDisassembler::Success;
}

DecodeStatus E2KDisassembler::DecodeCS0(E2KInstruction & E2KInst, MCInst &Instr, uint64_t Address) const {
  if (!E2KInst.HasCS[0])
    return MCDisassembler::Success;

  enum {
    EMPTY,
    IBRANCH,
    PREF,
    PUTTSD,
    DONE,
    IRET,
    HRET,
    GLAUNCH,
    DISP,
    SDISP,
    GETTSD,
    LDISP,
    RETURN
  };

  const uint32_t ops[] = {
      IBRANCH, PREF, PUTTSD, DONE,
      DISP, EMPTY, SDISP, GETTSD,
      DISP, LDISP, SDISP, GETTSD,
      DISP, EMPTY, SDISP, RETURN
  };

  uint32_t ctpr = E2KInst.show_bits(E2KInst.CS[0], 30, 2);
  uint32_t opcode = E2KInst.show_bits(E2KInst.CS[0], 28, 2);
  uint32_t param_type = E2KInst.show_bits(E2KInst.CS[0], 0, 3);
  uint32_t type = ops[(ctpr << 2) | opcode];
  uint32_t disp = E2KInst.show_bits(E2KInst.CS[0], 0, 28);
  int32_t sdisp = (int32_t)(disp << 4) >> 1;

  switch (type) {
  case IBRANCH:
  {
    MCInst *SubInstr = getContext().createMCInst();
    SubInstr->setOpcode(E2K::IBRANCH);
    SubInstr->addOperand(MCOperand::createImm(Address + sdisp));
    Instr.addOperand(MCOperand::createInst(SubInstr));
  }
    break;
  case DISP:
  {
    MCInst *SubInstr = getContext().createMCInst();
    SubInstr->setOpcode(E2K::DISP);
    SubInstr->addOperand(MCOperand::createReg(E2K::CTPR1 - 1 + ctpr));
    SubInstr->addOperand(MCOperand::createImm(Address + sdisp));
    Instr.addOperand(MCOperand::createInst(SubInstr));
  }
    break;
  case RETURN:
  {
    MCInst *SubInstr = getContext().createMCInst();
    SubInstr->setOpcode(E2K::RETURN);
    SubInstr->addOperand(MCOperand::createReg(E2K::CTPR1 - 1 + ctpr));
    Instr.addOperand(MCOperand::createInst(SubInstr));
  }
    break;
  }

  return MCDisassembler::Success;
}

DecodeStatus E2KDisassembler::DecodeCS1(E2KInstruction & E2KInst, MCInst &Instr, uint64_t Address) const {
  if (!E2KInst.HasCS[1])
    return MCDisassembler::Success;

  enum {
    SETR0,
    SETR1,
    SETEI,
    WAIT,
    SETBR,
    CALL,
    MAS,
    FLUSH,
    BR,
  };

  uint32_t opcode = E2KInst.show_bits(E2KInst.CS[1], 28, 4);

  switch (opcode) {
  case SETR0:
  case SETR1:
  case SETBR: {
    const uint32_t BitBP = 27;
    const uint32_t BitBN = 26;
    uint32_t setbp = E2KInst.show_bit(E2KInst.CS[1], BitBP);
    uint32_t setbn = E2KInst.show_bit(E2KInst.CS[1], BitBN);

    if (opcode == SETR0 || opcode == SETR1) {
      if (E2KInst.NumLTS < 1)
        return MCDisassembler::Fail;

      uint32_t wsz = E2KInst.show_bits(E2KInst.LTS[0], 5, 7);
      uint32_t nfx = E2KInst.show_bit(E2KInst.LTS[0], 4);
      uint32_t dbl = E2KInst.show_bit(E2KInst.LTS[0], 3);

      MCInst *SubInstr = getContext().createMCInst();
      SubInstr->setOpcode(E2K::SETWD);
      SubInstr->addOperand(MCOperand::createImm(wsz));
      SubInstr->addOperand(MCOperand::createImm(nfx));
      SubInstr->addOperand(MCOperand::createImm(dbl));
      Instr.addOperand(MCOperand::createInst(SubInstr));
    }

    if (setbn) {
      uint32_t rbs = E2KInst.show_bits(E2KInst.CS[1], 0, 6);
      uint32_t rsz = E2KInst.show_bits(E2KInst.CS[1], 6, 6);
      uint32_t rcur = E2KInst.show_bits(E2KInst.CS[1], 12, 6);

      MCInst *SubInstr = getContext().createMCInst();
      SubInstr->setOpcode(E2K::SETBN);
      SubInstr->addOperand(MCOperand::createImm(rbs));
      SubInstr->addOperand(MCOperand::createImm(rsz));
      SubInstr->addOperand(MCOperand::createImm(rcur));
      Instr.addOperand(MCOperand::createInst(SubInstr));
    }
    if (setbp) {
      uint32_t psz = E2KInst.show_bits(E2KInst.CS[1], 18, 5);
      MCInst *SubInstr = getContext().createMCInst();
      SubInstr->setOpcode(E2K::SETBP);
      SubInstr->addOperand(MCOperand::createImm(psz));
      Instr.addOperand(MCOperand::createInst(SubInstr));
    }
  }
    break;
  case CALL: {
    if (!E2KInst.HasSS)
      return E2KDisassembler::Fail;
    uint32_t ctop = E2KInst.show_bits(E2KInst.SS, 10, 2);
    uint32_t wbs = E2KInst.show_bits(E2KInst.CS[1], 0, 7);
    uint32_t ctcond = E2KInst.show_bits(E2KInst.SS, 0, 9);

    uint32_t cond_type = E2KInst.show_bits(ctcond, 5, 4);
    uint32_t pred = E2KInst.show_bits(ctcond, 0, 5);

    MCInst *SubInstr = getContext().createMCInst();
    SubInstr->setOpcode(E2K::CALL);
    SubInstr->addOperand(MCOperand::createReg(E2K::CTPR1 - 1 + ctop));
    SubInstr->addOperand(MCOperand::createImm(wbs));
    SubInstr->addOperand(MCOperand::createImm(cond_type));
    SubInstr->addOperand(MCOperand::createReg(E2K::PRED0 + pred));
    Instr.addOperand(MCOperand::createInst(SubInstr));
  }
    break;
  case MAS: {
   E2KInst.MAS[0] = E2KInst.show_bits(E2KInst.CS[1], 21, 7);
   E2KInst.MAS[2] = E2KInst.show_bits(E2KInst.CS[1], 14, 7);
   E2KInst.MAS[3] = E2KInst.show_bits(E2KInst.CS[1], 7, 7);
   E2KInst.MAS[5] = E2KInst.show_bits(E2KInst.CS[1], 0, 7);
  }
    break;
  }
  return MCDisassembler::Success;
}

DecodeStatus E2KDisassembler::getInstruction(MCInst &Instr, uint64_t &Size,
                                               ArrayRef<uint8_t> Bytes,
                                               uint64_t Address,
                                               raw_ostream &CStream) const {
  E2KInstruction E2KInst;
  *CurrentInst = &E2KInst;
  uint64_t Offset = 0;
  Size = 0;
  DecodeStatus Result =
      readInstruction32(Bytes, Offset, Size, E2KInst.HS);
  if (Result == MCDisassembler::Fail)
    return MCDisassembler::Fail;

  for (uint8_t i = 0; i < 6; ++i) {
    E2KInst.HasALS[i] = E2KInst.show_bit(E2KInst.HS, i + 26);
  }
  for (uint8_t i = 0; i < 6; ++i) {
    E2KInst.HasALES[i] = E2KInst.show_bit(E2KInst.HS, i + 20);
  }
  E2KInst.NumPLS = E2KInst.show_bits(E2KInst.HS, 18, 2);
  E2KInst.NumCDS = E2KInst.show_bits(E2KInst.HS, 16, 2);

  for (uint8_t i = 0; i < 2; ++i) {
    E2KInst.HasCS[i] = E2KInst.show_bit(E2KInst.HS, i + 14);
  }
  E2KInst.SetMark = E2KInst.show_bit(E2KInst.HS, E2KInst.BitSetMark);
  E2KInst.HasSS = E2KInst.show_bit(E2KInst.HS,E2KInst.BitSS);
  E2KInst.Unused = E2KInst.show_bit(E2KInst.HS,E2KInst.BitUnused);
  E2KInst.LoopMode = E2KInst.show_bit(E2KInst.HS,E2KInst.BitLoopMode);

  E2KInst.NOP = E2KInst.show_bits(E2KInst.HS, 7, 3);
  E2KInst.Length = E2KInst.show_bits(E2KInst.HS, 4, 3);
  E2KInst.Words = E2KInst.show_bits(E2KInst.HS, 0, 4);

  Size = (E2KInst.Length + 1) * 8;

  // max possible instruction size (Length = 0b111) is (7 + 1) * 8 = 64
  // current theoretically possible max size (according to flags) is
  // 4 /* HS */ + 4 /* SS */ + 6 * 4 /* ALES */ + 6 * 2 /* ALS */ +
  // 2 * 4 /* CS */ + 6 * 2 /* AAS */ + 4 * 4 /* LTS */ + 3 * 4 /* PLS */ +
  // 3 * 4 /* CDS */ = 4 + 4 + 24 + 12 + 8 + 12 + 16 + 12 + 12 = 104
  // so, it needs to use a fence to prevent reading outside the instruction

  if (E2KInst.HasSS) {
    Result = readInstruction32(Bytes, Offset, Size, E2KInst.SS);
    if (Result == MCDisassembler::Fail)
      return MCDisassembler::Fail;

    E2KInst.SSType = E2KInst.show_bit(E2KInst.SS, E2KInst.BitType);

    if (E2KInst.SSType == 0) {
      for (uint32_t i = 0; i < 4; ++i) {
        if (E2KInst.show_bit(E2KInst.SS, 12 + i)) {
          E2KInst.HasAAS[i >> 1] = true;
          E2KInst.HasAAS[i + 2] = true;
        }
      }
    }
  }

  for (uint32_t i = 0; i < 6; ++i) {
    if (E2KInst.HasALS[i]) {
      Result = readInstruction32(Bytes, Offset, Size, E2KInst.ALS[i]);
      if (Result == MCDisassembler::Fail)
        return MCDisassembler::Fail;
    }
  }
  if (E2KInst.HasCS[0]) {
    Result = readInstruction32(Bytes, Offset, Size, E2KInst.CS[0]);
    if (Result == MCDisassembler::Fail)
      return MCDisassembler::Fail;
  }
  if (E2KInst.HasALES[2] != E2KInst.HasALES[5]) {
    Offset += 2;
  }
  for (uint8_t i : {2, 5}) {
    if (E2KInst.HasALES[i]) {
      Result = readInstruction16(Bytes, Offset, Size, E2KInst.ALES[i]);
      if (Result == MCDisassembler::Fail)
        return MCDisassembler::Fail;
    }
  }
  if (E2KInst.HasCS[1]) {
    Result = readInstruction32(Bytes, Offset, Size, E2KInst.CS[1]);
    if (Result == MCDisassembler::Fail)
      return MCDisassembler::Fail;
  }
  Offset = (E2KInst.Words + 1) * 4; // middle pointer

  uint32_t NumALES = 0;
  for (uint8_t i : {0, 1, 3, 4})
    if (E2KInst.HasALES[i])
        ++NumALES;
  if (NumALES % 2 == 1)
    Offset += 2;

  for (uint8_t i : {0, 1, 3, 4}) {
    if (E2KInst.HasALES[i]) {
      Result = readInstruction16(Bytes, Offset, Size, E2KInst.ALES[i]);
      if (Result == MCDisassembler::Fail)
        return MCDisassembler::Fail;
    }
  }

  uint8_t NumAAS = 0;
  for (uint8_t i = 0; i < 6; ++i)
    if (E2KInst.HasAAS[i])
      ++NumAAS;
  if (NumAAS % 2 == 1)
    Offset += 2;

  for (uint8_t i = 0; i < 6; ++i) {
    if (E2KInst.HasAAS[i]) {
      if (E2KInst.HasALES[i]) {
        Result = readInstruction16(Bytes, Offset, Size, E2KInst.AAS[i]);
        if (Result == MCDisassembler::Fail)
          return MCDisassembler::Fail;
      }
    }
  }

  uint8_t SizePLSCDS = (E2KInst.NumPLS + E2KInst.NumCDS) * 4;
  if (Offset > Size + SizePLSCDS) {
    return MCDisassembler::Fail;
  }
  uint8_t Remain = Size - Offset - SizePLSCDS;
  E2KInst.NumLTS = Remain / 4;
  E2KInst.NumLTS = E2KInst.NumLTS > 4 ? 4 : E2KInst.NumLTS;

  for (uint8_t i = 0; i < E2KInst.NumPLS; ++i) {
    Offset = Size - 4 * (i + 1);
    Result = readInstruction32(Bytes, Offset, Size, E2KInst.PLS[i]);
    if (Result == MCDisassembler::Fail)
      return MCDisassembler::Fail;
  }
  for (uint8_t i = 0; i < E2KInst.NumCDS; ++i) {
    Offset = Size - 4 * (E2KInst.NumPLS + i + 1);
    Result = readInstruction32(Bytes, Offset, Size, E2KInst.CDS[i]);
    if (Result == MCDisassembler::Fail)
      return MCDisassembler::Fail;
  }
  for (uint8_t i = 0; i < E2KInst.NumLTS; ++i) {
    Offset = Size - 4 * (E2KInst.NumPLS + E2KInst.NumCDS + i + 1);
    Result = readInstruction32(Bytes, Offset, Size, E2KInst.LTS[i]);
    if (Result == MCDisassembler::Fail)
      return MCDisassembler::Fail;
  }

  E2KInst.dump();

  *CurrentBundle = &Instr;
  Instr.setOpcode(E2K::BUNDLE);

  if (E2KInst.NOP > 0) {
    MCInst *SubInstr = getContext().createMCInst();
    SubInstr->setOpcode(E2K::NOP);
    SubInstr->addOperand(MCOperand::createImm(E2KInst.NOP));
    Instr.addOperand(MCOperand::createInst(SubInstr));
  }

  Result = DecodeSS(E2KInst, Instr);
  if (Result == MCDisassembler::Fail)
    return Result;

  // MAS has to be decoded prior to ALS/ALES
  auto it = Instr.end();

  Result = DecodeCS0(E2KInst, Instr, Address);
  if (Result == MCDisassembler::Fail)
    return Result;
  Result = DecodeCS1(E2KInst, Instr, Address);
  if (Result == MCDisassembler::Fail)
    return Result;

  for (uint32_t i = 0; i < E2KInst.NumCDS; ++i) {

    E2KInst.SubCDS[2 * i + 1] = E2KInst.show_bits(E2KInst.CDS[i], 0, 16);
    E2KInst.SubCDS[2 * i] = E2KInst.show_bits(E2KInst.CDS[i], 16, 16);
  }

  for (uint32_t i = 0; i < 6; ++i) {
    if (E2KInst.HasALS[i]) {
      MCInst *SubInstr = getContext().createMCInst();
      if (E2KInst.HasALES[i]) {
        uint64_t Inst56 = E2KInst.ALS[i] | (uint64_t(E2KInst.ALES[i]) << 32) | (uint64_t(E2KInst.MAS[i]) << 48);
        Result = decodeInstruction(DecoderTableE2K_ALES[i], *SubInstr, Inst56, Address, this, STI);
      } else {
        uint64_t Inst56 = E2KInst.ALS[i] | (uint64_t(E2KInst.MAS[i]) << 48);
        Result = decodeInstruction(DecoderTableE2K_ALS[i], *SubInstr,  Inst56, Address, this, STI);
      }
      if (Result == MCDisassembler::Fail)
        return Result;

      uint32_t channel = (i <= 2) ? i : i - 3;

      for (uint32_t j = 0; j < 6; ++j) {
        uint16_t opcode = E2KInst.show_bits(E2KInst.SubCDS[j], 14, 2);
        uint16_t mask = E2KInst.show_bits(E2KInst.SubCDS[j], 10, 4);
        uint16_t neg = E2KInst.show_bits(E2KInst.SubCDS[j], 7, 3);
        uint16_t format = E2KInst.show_bits(E2KInst.SubCDS[j], 5, 2);
        uint16_t pred = E2KInst.show_bits(E2KInst.SubCDS[j], 0, 5);

        if (mask & 1 << channel) {
          SubInstr->addOperand(MCOperand::createReg(E2K::PRED0 + pred));
          SubInstr->setFlags(SubInstr->getFlags() | 0x80000000);
        }
      }

      Instr.insert(it, MCOperand::createInst(SubInstr));
      ++it;
    }
  }

  return Result;
}

typedef DecodeStatus (*DecodeFunc)(MCInst &MI, unsigned insn, uint64_t Address,
                                   const MCDisassembler *Decoder);
