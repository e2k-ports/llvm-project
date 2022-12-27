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

static const unsigned IntRegDecoderTable[] = {
  E2K::G0,  E2K::G1,  E2K::G2,  E2K::G3,
  E2K::G4,  E2K::G5,  E2K::G6,  E2K::G7,
  E2K::O0,  E2K::O1,  E2K::O2,  E2K::O3,
  E2K::O4,  E2K::O5,  E2K::O6,  E2K::O7,
  E2K::L0,  E2K::L1,  E2K::L2,  E2K::L3,
  E2K::L4,  E2K::L5,  E2K::L6,  E2K::L7,
  E2K::I0,  E2K::I1,  E2K::I2,  E2K::I3,
  E2K::I4,  E2K::I5,  E2K::I6,  E2K::I7 };

static const unsigned FPRegDecoderTable[] = {
  E2K::F0,   E2K::F1,   E2K::F2,   E2K::F3,
  E2K::F4,   E2K::F5,   E2K::F6,   E2K::F7,
  E2K::F8,   E2K::F9,   E2K::F10,  E2K::F11,
  E2K::F12,  E2K::F13,  E2K::F14,  E2K::F15,
  E2K::F16,  E2K::F17,  E2K::F18,  E2K::F19,
  E2K::F20,  E2K::F21,  E2K::F22,  E2K::F23,
  E2K::F24,  E2K::F25,  E2K::F26,  E2K::F27,
  E2K::F28,  E2K::F29,  E2K::F30,  E2K::F31 };

static const unsigned DFPRegDecoderTable[] = {
  E2K::D0,   E2K::D16,  E2K::D1,   E2K::D17,
  E2K::D2,   E2K::D18,  E2K::D3,   E2K::D19,
  E2K::D4,   E2K::D20,  E2K::D5,   E2K::D21,
  E2K::D6,   E2K::D22,  E2K::D7,   E2K::D23,
  E2K::D8,   E2K::D24,  E2K::D9,   E2K::D25,
  E2K::D10,  E2K::D26,  E2K::D11,  E2K::D27,
  E2K::D12,  E2K::D28,  E2K::D13,  E2K::D29,
  E2K::D14,  E2K::D30,  E2K::D15,  E2K::D31 };

static const unsigned QFPRegDecoderTable[] = {
  E2K::Q0,  E2K::Q8,   ~0U,  ~0U,
  E2K::Q1,  E2K::Q9,   ~0U,  ~0U,
  E2K::Q2,  E2K::Q10,  ~0U,  ~0U,
  E2K::Q3,  E2K::Q11,  ~0U,  ~0U,
  E2K::Q4,  E2K::Q12,  ~0U,  ~0U,
  E2K::Q5,  E2K::Q13,  ~0U,  ~0U,
  E2K::Q6,  E2K::Q14,  ~0U,  ~0U,
  E2K::Q7,  E2K::Q15,  ~0U,  ~0U } ;

static const unsigned FCCRegDecoderTable[] = {
  E2K::FCC0, E2K::FCC1, E2K::FCC2, E2K::FCC3 };

static const unsigned ASRRegDecoderTable[] = {
  E2K::Y,     E2K::ASR1,  E2K::ASR2,  E2K::ASR3,
  E2K::ASR4,  E2K::ASR5,  E2K::ASR6,  E2K::ASR7,
  E2K::ASR8,  E2K::ASR9,  E2K::ASR10, E2K::ASR11,
  E2K::ASR12, E2K::ASR13, E2K::ASR14, E2K::ASR15,
  E2K::ASR16, E2K::ASR17, E2K::ASR18, E2K::ASR19,
  E2K::ASR20, E2K::ASR21, E2K::ASR22, E2K::ASR23,
  E2K::ASR24, E2K::ASR25, E2K::ASR26, E2K::ASR27,
  E2K::ASR28, E2K::ASR29, E2K::ASR30, E2K::ASR31};

static const unsigned PRRegDecoderTable[] = {
  E2K::TPC, E2K::TNPC, E2K::TSTATE, E2K::TT, E2K::TICK, E2K::TBA, E2K::PSTATE,
  E2K::TL, E2K::PIL, E2K::CWP, E2K::CANSAVE, E2K::CANRESTORE, E2K::CLEANWIN,
  E2K::OTHERWIN, E2K::WSTATE, E2K::PC
};

static const uint16_t IntPairDecoderTable[] = {
  E2K::G0_G1, E2K::G2_G3, E2K::G4_G5, E2K::G6_G7,
  E2K::O0_O1, E2K::O2_O3, E2K::O4_O5, E2K::O6_O7,
  E2K::L0_L1, E2K::L2_L3, E2K::L4_L5, E2K::L6_L7,
  E2K::I0_I1, E2K::I2_I3, E2K::I4_I5, E2K::I6_I7,
};

static const unsigned CPRegDecoderTable[] = {
  E2K::C0,  E2K::C1,  E2K::C2,  E2K::C3,
  E2K::C4,  E2K::C5,  E2K::C6,  E2K::C7,
  E2K::C8,  E2K::C9,  E2K::C10, E2K::C11,
  E2K::C12, E2K::C13, E2K::C14, E2K::C15,
  E2K::C16, E2K::C17, E2K::C18, E2K::C19,
  E2K::C20, E2K::C21, E2K::C22, E2K::C23,
  E2K::C24, E2K::C25, E2K::C26, E2K::C27,
  E2K::C28, E2K::C29, E2K::C30, E2K::C31
};


static const uint16_t CPPairDecoderTable[] = {
  E2K::C0_C1,   E2K::C2_C3,   E2K::C4_C5,   E2K::C6_C7,
  E2K::C8_C9,   E2K::C10_C11, E2K::C12_C13, E2K::C14_C15,
  E2K::C16_C17, E2K::C18_C19, E2K::C20_C21, E2K::C22_C23,
  E2K::C24_C25, E2K::C26_C27, E2K::C28_C29, E2K::C30_C31
};

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
  // TODO
  Inst.addOperand(MCOperand::createReg(E2K::PSR));
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

static DecodeStatus DecodeIntRegsRegisterClass(MCInst &Inst, unsigned RegNo,
                                               uint64_t Address,
                                               const MCDisassembler *Decoder) {
  if (RegNo > 31)
    return MCDisassembler::Fail;
  unsigned Reg = IntRegDecoderTable[RegNo];
  Inst.addOperand(MCOperand::createReg(Reg));
  return MCDisassembler::Success;
}

static DecodeStatus DecodeI64RegsRegisterClass(MCInst &Inst, unsigned RegNo,
                                               uint64_t Address,
                                               const MCDisassembler *Decoder) {
  if (RegNo > 31)
    return MCDisassembler::Fail;
  unsigned Reg = IntRegDecoderTable[RegNo];
  Inst.addOperand(MCOperand::createReg(Reg));
  return MCDisassembler::Success;
}

// This is used for the type "ptr_rc", which is either IntRegs or I64Regs
// depending on SparcRegisterInfo::getPointerRegClass.
static DecodeStatus DecodePointerLikeRegClass0(MCInst &Inst, unsigned RegNo,
                                               uint64_t Address,
                                               const MCDisassembler *Decoder) {
  return DecodeIntRegsRegisterClass(Inst, RegNo, Address, Decoder);
}

static DecodeStatus DecodeFPRegsRegisterClass(MCInst &Inst, unsigned RegNo,
                                              uint64_t Address,
                                              const MCDisassembler *Decoder) {
  if (RegNo > 31)
    return MCDisassembler::Fail;
  unsigned Reg = FPRegDecoderTable[RegNo];
  Inst.addOperand(MCOperand::createReg(Reg));
  return MCDisassembler::Success;
}

static DecodeStatus DecodeDFPRegsRegisterClass(MCInst &Inst, unsigned RegNo,
                                               uint64_t Address,
                                               const MCDisassembler *Decoder) {
  if (RegNo > 31)
    return MCDisassembler::Fail;
  unsigned Reg = DFPRegDecoderTable[RegNo];
  Inst.addOperand(MCOperand::createReg(Reg));
  return MCDisassembler::Success;
}

static DecodeStatus DecodeQFPRegsRegisterClass(MCInst &Inst, unsigned RegNo,
                                               uint64_t Address,
                                               const MCDisassembler *Decoder) {
  if (RegNo > 31)
    return MCDisassembler::Fail;

  unsigned Reg = QFPRegDecoderTable[RegNo];
  if (Reg == ~0U)
    return MCDisassembler::Fail;
  Inst.addOperand(MCOperand::createReg(Reg));
  return MCDisassembler::Success;
}

static DecodeStatus DecodeCPRegsRegisterClass(MCInst &Inst, unsigned RegNo,
                                              uint64_t Address,
                                              const MCDisassembler *Decoder) {
  if (RegNo > 31)
    return MCDisassembler::Fail;
  unsigned Reg = CPRegDecoderTable[RegNo];
  Inst.addOperand(MCOperand::createReg(Reg));
  return MCDisassembler::Success;
}

static DecodeStatus DecodeFCCRegsRegisterClass(MCInst &Inst, unsigned RegNo,
                                               uint64_t Address,
                                               const MCDisassembler *Decoder) {
  if (RegNo > 3)
    return MCDisassembler::Fail;
  Inst.addOperand(MCOperand::createReg(FCCRegDecoderTable[RegNo]));
  return MCDisassembler::Success;
}

static DecodeStatus DecodeASRRegsRegisterClass(MCInst &Inst, unsigned RegNo,
                                               uint64_t Address,
                                               const MCDisassembler *Decoder) {
  if (RegNo > 31)
    return MCDisassembler::Fail;
  Inst.addOperand(MCOperand::createReg(ASRRegDecoderTable[RegNo]));
  return MCDisassembler::Success;
}

static DecodeStatus DecodePRRegsRegisterClass(MCInst &Inst, unsigned RegNo,
                                              uint64_t Address,
                                              const MCDisassembler *Decoder) {
  if (RegNo >= std::size(PRRegDecoderTable))
    return MCDisassembler::Fail;
  Inst.addOperand(MCOperand::createReg(PRRegDecoderTable[RegNo]));
  return MCDisassembler::Success;
}

static DecodeStatus DecodeIntPairRegisterClass(MCInst &Inst, unsigned RegNo,
                                               uint64_t Address,
                                               const MCDisassembler *Decoder) {
  DecodeStatus S = MCDisassembler::Success;

  if (RegNo > 31)
    return MCDisassembler::Fail;

  if ((RegNo & 1))
    S = MCDisassembler::SoftFail;

  unsigned RegisterPair = IntPairDecoderTable[RegNo/2];
  Inst.addOperand(MCOperand::createReg(RegisterPair));
  return S;
}

static DecodeStatus DecodeCPPairRegisterClass(MCInst &Inst, unsigned RegNo,
                                              uint64_t Address,
                                              const MCDisassembler *Decoder) {
  if (RegNo > 31)
    return MCDisassembler::Fail;

  unsigned RegisterPair = CPPairDecoderTable[RegNo/2];
  Inst.addOperand(MCOperand::createReg(RegisterPair));
  return MCDisassembler::Success;
}

static DecodeStatus DecodeLoadInt(MCInst &Inst, unsigned insn, uint64_t Address,
                                  const MCDisassembler *Decoder);
static DecodeStatus DecodeLoadIntPair(MCInst &Inst, unsigned insn,
                                      uint64_t Address,
                                      const MCDisassembler *Decoder);
static DecodeStatus DecodeLoadFP(MCInst &Inst, unsigned insn, uint64_t Address,
                                 const MCDisassembler *Decoder);
static DecodeStatus DecodeLoadDFP(MCInst &Inst, unsigned insn, uint64_t Address,
                                  const MCDisassembler *Decoder);
static DecodeStatus DecodeLoadQFP(MCInst &Inst, unsigned insn, uint64_t Address,
                                  const MCDisassembler *Decoder);
static DecodeStatus DecodeLoadCP(MCInst &Inst, unsigned insn, uint64_t Address,
                                 const MCDisassembler *Decoder);
static DecodeStatus DecodeLoadCPPair(MCInst &Inst, unsigned insn,
                                     uint64_t Address,
                                     const MCDisassembler *Decoder);
static DecodeStatus DecodeStoreInt(MCInst &Inst, unsigned insn,
                                   uint64_t Address,
                                   const MCDisassembler *Decoder);
static DecodeStatus DecodeStoreIntPair(MCInst &Inst, unsigned insn,
                                       uint64_t Address,
                                       const MCDisassembler *Decoder);
static DecodeStatus DecodeStoreFP(MCInst &Inst, unsigned insn, uint64_t Address,
                                  const MCDisassembler *Decoder);
static DecodeStatus DecodeStoreDFP(MCInst &Inst, unsigned insn,
                                   uint64_t Address,
                                   const MCDisassembler *Decoder);
static DecodeStatus DecodeStoreQFP(MCInst &Inst, unsigned insn,
                                   uint64_t Address,
                                   const MCDisassembler *Decoder);
static DecodeStatus DecodeStoreCP(MCInst &Inst, unsigned insn, uint64_t Address,
                                  const MCDisassembler *Decoder);
static DecodeStatus DecodeStoreCPPair(MCInst &Inst, unsigned insn,
                                      uint64_t Address,
                                      const MCDisassembler *Decoder);
static DecodeStatus DecodeCall(MCInst &Inst, unsigned insn, uint64_t Address,
                               const MCDisassembler *Decoder);
static DecodeStatus DecodeSIMM13(MCInst &Inst, unsigned insn, uint64_t Address,
                                 const MCDisassembler *Decoder);
static DecodeStatus DecodeJMPL(MCInst &Inst, unsigned insn, uint64_t Address,
                               const MCDisassembler *Decoder);
static DecodeStatus DecodeReturn(MCInst &MI, unsigned insn, uint64_t Address,
                                 const MCDisassembler *Decoder);
static DecodeStatus DecodeSWAP(MCInst &Inst, unsigned insn, uint64_t Address,
                               const MCDisassembler *Decoder);
static DecodeStatus DecodeTRAP(MCInst &Inst, unsigned insn, uint64_t Address,
                               const MCDisassembler *Decoder);

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

  LLVM_DEBUG(dbgs() << "HS" << " : " << format("0x%04x", HS) << "\n");

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

  *CurrentBundle = &Instr;
  Instr.setOpcode(E2K::BUNDLE);

  if (NOP > 0) {
    MCInst *SubInstr = getContext().createMCInst();
    SubInstr->setOpcode(E2K::NOOP);
    SubInstr->addOperand(MCOperand::createImm(NOP));
    Instr.addOperand(MCOperand::createInst(SubInstr));
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

static DecodeStatus DecodeMem(MCInst &MI, unsigned insn, uint64_t Address,
                              const MCDisassembler *Decoder, bool isLoad,
                              DecodeFunc DecodeRD) {
  unsigned rd = fieldFromInstruction(insn, 25, 5);
  unsigned rs1 = fieldFromInstruction(insn, 14, 5);
  bool isImm = fieldFromInstruction(insn, 13, 1);
  bool hasAsi = fieldFromInstruction(insn, 23, 1); // (in op3 field)
  unsigned asi = fieldFromInstruction(insn, 5, 8);
  unsigned rs2 = 0;
  unsigned simm13 = 0;
  if (isImm)
    simm13 = SignExtend32<13>(fieldFromInstruction(insn, 0, 13));
  else
    rs2 = fieldFromInstruction(insn, 0, 5);

  DecodeStatus status;
  if (isLoad) {
    status = DecodeRD(MI, rd, Address, Decoder);
    if (status != MCDisassembler::Success)
      return status;
  }

  // Decode rs1.
  status = DecodeIntRegsRegisterClass(MI, rs1, Address, Decoder);
  if (status != MCDisassembler::Success)
    return status;

  // Decode imm|rs2.
  if (isImm)
    MI.addOperand(MCOperand::createImm(simm13));
  else {
    status = DecodeIntRegsRegisterClass(MI, rs2, Address, Decoder);
    if (status != MCDisassembler::Success)
      return status;
  }

  if (hasAsi)
    MI.addOperand(MCOperand::createImm(asi));

  if (!isLoad) {
    status = DecodeRD(MI, rd, Address, Decoder);
    if (status != MCDisassembler::Success)
      return status;
  }
  return MCDisassembler::Success;
}

static DecodeStatus DecodeLoadInt(MCInst &Inst, unsigned insn, uint64_t Address,
                                  const MCDisassembler *Decoder) {
  return DecodeMem(Inst, insn, Address, Decoder, true,
                   DecodeIntRegsRegisterClass);
}

static DecodeStatus DecodeLoadIntPair(MCInst &Inst, unsigned insn,
                                      uint64_t Address,
                                      const MCDisassembler *Decoder) {
  return DecodeMem(Inst, insn, Address, Decoder, true,
                   DecodeIntPairRegisterClass);
}

static DecodeStatus DecodeLoadFP(MCInst &Inst, unsigned insn, uint64_t Address,
                                 const MCDisassembler *Decoder) {
  return DecodeMem(Inst, insn, Address, Decoder, true,
                   DecodeFPRegsRegisterClass);
}

static DecodeStatus DecodeLoadDFP(MCInst &Inst, unsigned insn, uint64_t Address,
                                  const MCDisassembler *Decoder) {
  return DecodeMem(Inst, insn, Address, Decoder, true,
                   DecodeDFPRegsRegisterClass);
}

static DecodeStatus DecodeLoadQFP(MCInst &Inst, unsigned insn, uint64_t Address,
                                  const MCDisassembler *Decoder) {
  return DecodeMem(Inst, insn, Address, Decoder, true,
                   DecodeQFPRegsRegisterClass);
}

static DecodeStatus DecodeLoadCP(MCInst &Inst, unsigned insn, uint64_t Address,
                                 const MCDisassembler *Decoder) {
  return DecodeMem(Inst, insn, Address, Decoder, true,
                   DecodeCPRegsRegisterClass);
}

static DecodeStatus DecodeLoadCPPair(MCInst &Inst, unsigned insn,
                                     uint64_t Address,
                                     const MCDisassembler *Decoder) {
  return DecodeMem(Inst, insn, Address, Decoder, true,
                   DecodeCPPairRegisterClass);
}

static DecodeStatus DecodeStoreInt(MCInst &Inst, unsigned insn,
                                   uint64_t Address,
                                   const MCDisassembler *Decoder) {
  return DecodeMem(Inst, insn, Address, Decoder, false,
                   DecodeIntRegsRegisterClass);
}

static DecodeStatus DecodeStoreIntPair(MCInst &Inst, unsigned insn,
                                       uint64_t Address,
                                       const MCDisassembler *Decoder) {
  return DecodeMem(Inst, insn, Address, Decoder, false,
                   DecodeIntPairRegisterClass);
}

static DecodeStatus DecodeStoreFP(MCInst &Inst, unsigned insn, uint64_t Address,
                                  const MCDisassembler *Decoder) {
  return DecodeMem(Inst, insn, Address, Decoder, false,
                   DecodeFPRegsRegisterClass);
}

static DecodeStatus DecodeStoreDFP(MCInst &Inst, unsigned insn,
                                   uint64_t Address,
                                   const MCDisassembler *Decoder) {
  return DecodeMem(Inst, insn, Address, Decoder, false,
                   DecodeDFPRegsRegisterClass);
}

static DecodeStatus DecodeStoreQFP(MCInst &Inst, unsigned insn,
                                   uint64_t Address,
                                   const MCDisassembler *Decoder) {
  return DecodeMem(Inst, insn, Address, Decoder, false,
                   DecodeQFPRegsRegisterClass);
}

static DecodeStatus DecodeStoreCP(MCInst &Inst, unsigned insn, uint64_t Address,
                                  const MCDisassembler *Decoder) {
  return DecodeMem(Inst, insn, Address, Decoder, false,
                   DecodeCPRegsRegisterClass);
}

static DecodeStatus DecodeStoreCPPair(MCInst &Inst, unsigned insn,
                                      uint64_t Address,
                                      const MCDisassembler *Decoder) {
  return DecodeMem(Inst, insn, Address, Decoder, false,
                   DecodeCPPairRegisterClass);
}

static bool tryAddingSymbolicOperand(int64_t Value, bool isBranch,
                                     uint64_t Address, uint64_t Offset,
                                     uint64_t Width, MCInst &MI,
                                     const MCDisassembler *Decoder) {
  return Decoder->tryAddingSymbolicOperand(MI, Value, Address, isBranch, Offset,
                                           Width, /*InstSize=*/4);
}

static DecodeStatus DecodeCall(MCInst &MI, unsigned insn, uint64_t Address,
                               const MCDisassembler *Decoder) {
  unsigned tgt = fieldFromInstruction(insn, 0, 30);
  tgt <<= 2;
  if (!tryAddingSymbolicOperand(tgt+Address, false, Address,
                                0, 30, MI, Decoder))
    MI.addOperand(MCOperand::createImm(tgt));
  return MCDisassembler::Success;
}

static DecodeStatus DecodeSIMM13(MCInst &MI, unsigned insn, uint64_t Address,
                                 const MCDisassembler *Decoder) {
  unsigned tgt = SignExtend32<13>(fieldFromInstruction(insn, 0, 13));
  MI.addOperand(MCOperand::createImm(tgt));
  return MCDisassembler::Success;
}

static DecodeStatus DecodeJMPL(MCInst &MI, unsigned insn, uint64_t Address,
                               const MCDisassembler *Decoder) {

  unsigned rd = fieldFromInstruction(insn, 25, 5);
  unsigned rs1 = fieldFromInstruction(insn, 14, 5);
  unsigned isImm = fieldFromInstruction(insn, 13, 1);
  unsigned rs2 = 0;
  unsigned simm13 = 0;
  if (isImm)
    simm13 = SignExtend32<13>(fieldFromInstruction(insn, 0, 13));
  else
    rs2 = fieldFromInstruction(insn, 0, 5);

  // Decode RD.
  DecodeStatus status = DecodeIntRegsRegisterClass(MI, rd, Address, Decoder);
  if (status != MCDisassembler::Success)
    return status;

  // Decode RS1.
  status = DecodeIntRegsRegisterClass(MI, rs1, Address, Decoder);
  if (status != MCDisassembler::Success)
    return status;

  // Decode RS1 | SIMM13.
  if (isImm)
    MI.addOperand(MCOperand::createImm(simm13));
  else {
    status = DecodeIntRegsRegisterClass(MI, rs2, Address, Decoder);
    if (status != MCDisassembler::Success)
      return status;
  }
  return MCDisassembler::Success;
}

static DecodeStatus DecodeReturn(MCInst &MI, unsigned insn, uint64_t Address,
                                 const MCDisassembler *Decoder) {

  unsigned rs1 = fieldFromInstruction(insn, 14, 5);
  unsigned isImm = fieldFromInstruction(insn, 13, 1);
  unsigned rs2 = 0;
  unsigned simm13 = 0;
  if (isImm)
    simm13 = SignExtend32<13>(fieldFromInstruction(insn, 0, 13));
  else
    rs2 = fieldFromInstruction(insn, 0, 5);

  // Decode RS1.
  DecodeStatus status = DecodeIntRegsRegisterClass(MI, rs1, Address, Decoder);
  if (status != MCDisassembler::Success)
    return status;

  // Decode RS2 | SIMM13.
  if (isImm)
    MI.addOperand(MCOperand::createImm(simm13));
  else {
    status = DecodeIntRegsRegisterClass(MI, rs2, Address, Decoder);
    if (status != MCDisassembler::Success)
      return status;
  }
  return MCDisassembler::Success;
}

static DecodeStatus DecodeSWAP(MCInst &MI, unsigned insn, uint64_t Address,
                               const MCDisassembler *Decoder) {

  unsigned rd = fieldFromInstruction(insn, 25, 5);
  unsigned rs1 = fieldFromInstruction(insn, 14, 5);
  unsigned isImm = fieldFromInstruction(insn, 13, 1);
  bool hasAsi = fieldFromInstruction(insn, 23, 1); // (in op3 field)
  unsigned asi = fieldFromInstruction(insn, 5, 8);
  unsigned rs2 = 0;
  unsigned simm13 = 0;
  if (isImm)
    simm13 = SignExtend32<13>(fieldFromInstruction(insn, 0, 13));
  else
    rs2 = fieldFromInstruction(insn, 0, 5);

  // Decode RD.
  DecodeStatus status = DecodeIntRegsRegisterClass(MI, rd, Address, Decoder);
  if (status != MCDisassembler::Success)
    return status;

  // Decode RS1.
  status = DecodeIntRegsRegisterClass(MI, rs1, Address, Decoder);
  if (status != MCDisassembler::Success)
    return status;

  // Decode RS1 | SIMM13.
  if (isImm)
    MI.addOperand(MCOperand::createImm(simm13));
  else {
    status = DecodeIntRegsRegisterClass(MI, rs2, Address, Decoder);
    if (status != MCDisassembler::Success)
      return status;
  }

  if (hasAsi)
    MI.addOperand(MCOperand::createImm(asi));

  return MCDisassembler::Success;
}

static DecodeStatus DecodeTRAP(MCInst &MI, unsigned insn, uint64_t Address,
                               const MCDisassembler *Decoder) {

  unsigned rs1 = fieldFromInstruction(insn, 14, 5);
  unsigned isImm = fieldFromInstruction(insn, 13, 1);
  unsigned cc =fieldFromInstruction(insn, 25, 4);
  unsigned rs2 = 0;
  unsigned imm7 = 0;
  if (isImm)
    imm7 = fieldFromInstruction(insn, 0, 7);
  else
    rs2 = fieldFromInstruction(insn, 0, 5);

  // Decode RS1.
  DecodeStatus status = DecodeIntRegsRegisterClass(MI, rs1, Address, Decoder);
  if (status != MCDisassembler::Success)
    return status;

  // Decode RS1 | IMM7.
  if (isImm)
    MI.addOperand(MCOperand::createImm(imm7));
  else {
    status = DecodeIntRegsRegisterClass(MI, rs2, Address, Decoder);
    if (status != MCDisassembler::Success)
      return status;
  }

  // Decode CC
  MI.addOperand(MCOperand::createImm(cc));

  return MCDisassembler::Success;
}
