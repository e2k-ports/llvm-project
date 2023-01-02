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

/// A disassembler class for E2K.
class E2KDisassembler : public MCDisassembler {
public:
  std::unique_ptr<MCInst *> CurrentBundle;
  E2KDisassembler(const MCSubtargetInfo &STI, MCContext &Ctx)
      : MCDisassembler(STI, Ctx), CurrentBundle(new MCInst *) {}
  virtual ~E2KDisassembler() = default;

  DecodeStatus getInstruction(MCInst &Instr, uint64_t &Size,
                              ArrayRef<uint8_t> Bytes, uint64_t Address,
                              raw_ostream &CStream) const override;
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
    case s: return E2K::R0; break;
    case d: return E2K::DR0; break;
    case q: return E2K::QR0; break;
    case p: return E2K::QPR0; break;
    default: llvm_unreachable("unknown register"); break;
    }
  }
  case b: {
    switch (Rs) {
    case s: return E2K::B0; break;
    case d: return E2K::DB0; break;
    case q: return E2K::QB0; break;
    case p: return E2K::QPB0; break;
    default: llvm_unreachable("unknown register"); break;
    }
  }
  case g: {
    switch (Rs) {
    case s: return E2K::G0; break;
    case d: return E2K::DG0; break;
    case q: return E2K::QG0; break;
    case p: return E2K::QPG0; break;
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

  Insn = (Bytes[Offset] << 0) | (Bytes[Offset + 1] << 8);
  Offset += 2;

  return MCDisassembler::Success;
}

typedef struct als_ales_t
{
  uint32_t ALS;
  uint16_t ALES;
}
als_ales_t;

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

static const uint8_t * DecoderTableE2K_CS[2] = {
    DecoderTableE2K_CS032,
    DecoderTableE2K_CS132,
};

DecodeStatus E2KDisassembler::getInstruction(MCInst &Instr, uint64_t &Size,
                                               ArrayRef<uint8_t> Bytes,
                                               uint64_t Address,
                                               raw_ostream &CStream) const {
  uint32_t HS; // header syllable
  uint64_t Offset = 0;
  DecodeStatus Result =
      readInstruction32(Bytes, Offset, Size, HS);
  if (Result == MCDisassembler::Fail)
    return MCDisassembler::Fail;

  bool HasALS[6];
  for (uint32_t i = 0; i < 6; ++i) {
    uint32_t MaskALS = 1 << (i + 26);
    HasALS[i] = (HS & MaskALS) == MaskALS;
    LLVM_DEBUG(dbgs() << "HasALS" << i << " : " << HasALS[i] << "\n");
  }
  bool HasALES[6];
  for (uint32_t i = 0; i < 6; ++i) {
    uint32_t MaskALES = 1 << (i + 20);
    HasALES[i] = (HS & MaskALES) == MaskALES;
    LLVM_DEBUG(dbgs() << "HasALES" << i << " : " << HasALES[i] << "\n");
  }
  uint32_t NumPLS = (HS >> 18) & 0b11;

  LLVM_DEBUG(dbgs() << "NumPLS" << " : " << NumPLS << "\n");

  uint32_t NumCDS = (HS >> 16) & 0b11;

  LLVM_DEBUG(dbgs() << "NumCDS" << " : " << NumCDS << "\n");

  bool HasCS[2];
  for (uint32_t i = 0; i < 2; ++i) {
    uint32_t MaskCS = 1 << (i + 14);
    HasCS[i] = (HS & MaskCS) == MaskCS;

    LLVM_DEBUG(dbgs() << "HasCS" << i <<  " : " << HasCS[i] << "\n");
  }
  const uint32_t MaskSetMark = 1 << 13;
  bool SetMark = (HS & MaskSetMark) == MaskSetMark;
  LLVM_DEBUG(dbgs() << "SetMark" << " : " << SetMark << "\n");

  const uint32_t MaskSS = 1 << 12;
  bool HasSS = (HS & MaskSS) == MaskSS;
  LLVM_DEBUG(dbgs() << "HasSS" << " : " << HasSS << "\n");

  const uint32_t MaskUnused = 1 << 11;
  bool HasUnused = (HS & MaskUnused) == MaskUnused;
  LLVM_DEBUG(dbgs() << "Unused" << " : " << HasUnused << "\n");

  const uint32_t MaskLoopMode = 1 << 10;
  bool LoopMode = (HS & MaskLoopMode) == MaskLoopMode;
  LLVM_DEBUG(dbgs() << "LoopMode" << " : " << LoopMode << "\n");

  uint32_t NOP = (HS >> 7) & 0b111;
  LLVM_DEBUG(dbgs() << "NOP" << " : " << NOP << "\n");
  uint32_t Length = (HS >> 4) & 0b111;

  LLVM_DEBUG(dbgs() << "Length" << " : " << Length << "\n");

  uint32_t Words = HS & 0b1111;
  LLVM_DEBUG(dbgs() << "Words" << " : " << Words << "\n");

  Size = (Length + 1) * 8;

  als_ales_t als_ales[6];
  uint32_t CS[2];
  uint32_t SS;
  bool HasAAS[6] = {false, false, false, false, false, false};
  uint16_t AAS[6];

  LLVM_DEBUG(dbgs() << "HS" << " : " << format("0x%04x", HS) << "\n");

  const uint32_t MaskType = 1 << 20;
  uint32_t SSType = 0;

  if (HasSS) {
    Result = readInstruction32(Bytes, Offset, Size, SS);
    if (Result == MCDisassembler::Fail)
      return MCDisassembler::Fail;
    LLVM_DEBUG(dbgs() << "SS" << " : " << format("0x%04x", SS) << "\n");

    SSType = ((SS & MaskType) == MaskType) ? 1 : 0;

    if (SSType == 0) {
      for (uint32_t i = 0; i < 4; ++i) {
        uint32_t MaskAAS = 1 << (12 + i);

        if ((SS & MaskAAS) == MaskAAS) {
          HasAAS[i >> 1] = true;
          HasAAS[i + 2] = true;
        }
      }
    }
    for (uint32_t i = 0; i < 6; ++i) {
      LLVM_DEBUG(dbgs() << "HasAAS" << i <<  " : " << HasAAS[i] << "\n");
    }
  }

  for (uint32_t i = 0; i < 6; ++i) {
    if (HasALS[i]) {
      Result = readInstruction32(Bytes, Offset, Size, als_ales[i].ALS);
      if (Result == MCDisassembler::Fail)
        return MCDisassembler::Fail;
      LLVM_DEBUG(dbgs() << "ALS" << i << " : " << format("0x%04x", als_ales[i].ALS) << "\n");
    }
  }
  if (HasCS[0]) {
    Result = readInstruction32(Bytes, Offset, Size, CS[0]);
    if (Result == MCDisassembler::Fail)
      return MCDisassembler::Fail;
    LLVM_DEBUG(dbgs() << "CS0" << " : " << format("0x%04x", CS[0]) << "\n");
  }
  if (HasALES[2] != HasALES[5]) {
    Offset += 2;
  }
  for (uint32_t i : {2, 5}) {
    if (HasALES[i]) {
      Result = readInstruction16(Bytes, Offset, Size, als_ales[i].ALES);
      if (Result == MCDisassembler::Fail)
        return MCDisassembler::Fail;
      LLVM_DEBUG(dbgs() << "ALES" << i << " : " << format("0x%02x", als_ales[i].ALES) << "\n");
    }
  }
  if (HasCS[1]) {
    Result = readInstruction32(Bytes, Offset, Size, CS[1]);
    if (Result == MCDisassembler::Fail)
      return MCDisassembler::Fail;
    LLVM_DEBUG(dbgs() << "CS1" << " : " << format("0x%04x", CS[1]) << "\n");
  }
  Offset = (Words + 1) * 4; // middle pointer

  uint32_t NumALES = 0;
  for (uint32_t i : {0, 1, 3, 4})
    if (HasALES[i])
        ++NumALES;
  if (NumALES % 2 == 1)
    Offset += 2;

  for (uint32_t i : {0, 1, 3, 4}) {
    if (HasALES[i]) {
      Result = readInstruction16(Bytes, Offset, Size, als_ales[i].ALES);
      if (Result == MCDisassembler::Fail)
        return MCDisassembler::Fail;
      LLVM_DEBUG(dbgs() << "ALES" << i << " : " << format("0x%02x", als_ales[i].ALES) << "\n");
    }
  }

  uint32_t NumAAS = 0;
  for (uint32_t i = 0; i < 6; ++i)
    if (HasAAS[i])
      ++NumAAS;
  if (NumAAS % 2 == 1)
    Offset += 2;

  for (uint32_t i = 0; i < 6; ++i) {
    if (HasAAS[i]) {
      if (HasALES[i]) {
        Result = readInstruction16(Bytes, Offset, Size, AAS[i]);
        if (Result == MCDisassembler::Fail)
          return MCDisassembler::Fail;
        LLVM_DEBUG(dbgs() << "AAS" << i << " : " << format("0x%02x", AAS[i]) << "\n");
      }
    }
  }

  uint32_t SizePLSCDS = (NumPLS + NumCDS) * 4;
  if (Offset > Size + SizePLSCDS) {
    return MCDisassembler::Fail;
  }
  uint32_t Remain = Size - Offset - SizePLSCDS;
  uint32_t NumLTS = Remain / 4;
  NumLTS = NumLTS > 4 ? 4 : NumLTS;
  LLVM_DEBUG(dbgs() << "NumLTS" << " : " << NumLTS << "\n");
  uint32_t PLS[3];
  uint32_t CDS[3];
  uint32_t LTS[4];

  for (uint32_t i = 0; i < NumPLS; ++i) {
    Offset = Size - 4 * (i + 1);
    Result = readInstruction32(Bytes, Offset, Size, PLS[i]);
    if (Result == MCDisassembler::Fail)
      return MCDisassembler::Fail;

    LLVM_DEBUG(dbgs() << "PLS" << i << " : " << format("0x%02x", PLS[i]) << "\n");
  }
  for (uint32_t i = 0; i < NumCDS; ++i) {
    Offset = Size - 4 * (NumPLS + i + 1);
    Result = readInstruction32(Bytes, Offset, Size, CDS[i]);
    if (Result == MCDisassembler::Fail)
      return MCDisassembler::Fail;

    LLVM_DEBUG(dbgs() << "CDS" << i << " : " << format("0x%02x", CDS[i]) << "\n");
  }
  for (uint32_t i = 0; i < NumLTS; ++i) {
    Offset = Size - 4 * (NumPLS + NumCDS + i + 1);
    Result = readInstruction32(Bytes, Offset, Size, LTS[i]);
    if (Result == MCDisassembler::Fail)
      return MCDisassembler::Fail;

    LLVM_DEBUG(dbgs() << "LTS" << i << " : " << format("0x%02x", LTS[i]) << "\n");
  }

  *CurrentBundle = &Instr;
  Instr.setOpcode(E2K::BUNDLE);

  if (NOP > 0) {
    MCInst *SubInstr = getContext().createMCInst();
    SubInstr->setOpcode(E2K::NOOP);
    SubInstr->addOperand(MCOperand::createImm(NOP));
    Instr.addOperand(MCOperand::createInst(SubInstr));
  }

  if (HasSS) {
    uint32_t IPD = (SS >> 30) & 0b11;

    if (IPD > 0) {
      MCInst *SubInstr = getContext().createMCInst();
      SubInstr->setOpcode(E2K::IPD);
      SubInstr->addOperand(MCOperand::createImm(IPD));
      Instr.addOperand(MCOperand::createInst(SubInstr));
    }

    if (SSType == 0) {
      const uint32_t MaskEAP = 1 << 29;
      const uint32_t MaskBAP = 1 << 28;
      const uint32_t MaskSRP = 1 << 27;
      const uint32_t MaskVDFI = 1 << 26;
      const uint32_t MaskCRP = 1 << 25;

      if ((SS & MaskEAP) == MaskEAP) {
        MCInst *SubInstr = getContext().createMCInst();
        SubInstr->setOpcode(E2K::EAP);
        Instr.addOperand(MCOperand::createInst(SubInstr));
      }
      if ((SS & MaskBAP) == MaskBAP) {
        MCInst *SubInstr = getContext().createMCInst();
        SubInstr->setOpcode(E2K::BAP);
        Instr.addOperand(MCOperand::createInst(SubInstr));
      }

      bool HasCRP = (SS & MaskCRP) == MaskCRP;
      bool HasSRP = (SS & MaskSRP) == MaskSRP;

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

      if ((SS & MaskVDFI) == MaskVDFI) {
        MCInst *SubInstr = getContext().createMCInst();
        SubInstr->setOpcode(E2K::VDFI);
        Instr.addOperand(MCOperand::createInst(SubInstr));
      }

      uint32_t ABG = (SS >> 23) & 0b11;
      uint32_t ABN = (SS >> 21) & 0b11;
      uint32_t APB = (SS >> 18) & 0b11;
      uint32_t ALC = (SS >> 16) & 0b11;

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
  }

  for (uint32_t i = 0; i < 2; ++i) {
    if (HasCS[i]) {
      MCInst *SubInstr = getContext().createMCInst();
      Result = decodeInstruction(DecoderTableE2K_CS[i], *SubInstr, CS[i], Address, this, STI);
      if (Result == MCDisassembler::Fail)
        return Result;
      Instr.addOperand(MCOperand::createInst(SubInstr));
    }
  }

  for (uint32_t i = 0; i < 6; ++i) {
    if (HasALS[i]) {
      MCInst *SubInstr = getContext().createMCInst();
      if (HasALES[i]) {
        uint64_t Inst48 = als_ales[i].ALS | (uint64_t(als_ales[i].ALES) << 32);
        Result = decodeInstruction(DecoderTableE2K_ALES[i], *SubInstr, Inst48, Address, this, STI);
      } else {
        Result = decodeInstruction(DecoderTableE2K_ALS[i], *SubInstr, als_ales[i].ALS, Address, this, STI);
      }
      if (Result == MCDisassembler::Fail)
        return Result;
      Instr.addOperand(MCOperand::createInst(SubInstr));
      unsigned Flags = i; // channel
      if ((als_ales[i].ALS & 0x80000000UL) == 0x80000000UL) // speculative mode
        Flags |= 0b1000;
      Instr.setFlags(Flags);
    }
  }

  return Result;
}

typedef DecodeStatus (*DecodeFunc)(MCInst &MI, unsigned insn, uint64_t Address,
                                   const MCDisassembler *Decoder);
