//===-- E2K.h - Top-level interface for E2K representation --*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the entry points for global functions defined in the LLVM
// E2K back-end.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_E2K_E2K_H
#define LLVM_LIB_TARGET_E2K_E2K_H

#include "MCTargetDesc/E2KMCTargetDesc.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Target/TargetMachine.h"

namespace llvm {
  class FunctionPass;
  class E2KTargetMachine;
  class AsmPrinter;
  class MCInst;
  class MachineInstr;
  class PassRegistry;

  FunctionPass *createE2KISelDag(E2KTargetMachine &TM);
  FunctionPass *createE2KDelaySlotFillerPass();
  FunctionPass *createE2KExpandPseudoPass();

  void LowerE2KMachineInstrToMCInst(const MachineInstr *MI,
                                      MCInst &OutMI,
                                      AsmPrinter &AP);

  void initializeE2KExpandPseudoPass(PassRegistry &);
} // end namespace llvm;

namespace llvm {
  // Enums corresponding to E2K condition codes, both icc's and fcc's.  These
  // values must be kept in sync with the ones in the .td file.
  namespace E2KCC {
    enum CondCodes {
      ICC_A   =  8   ,  // Always
      ICC_N   =  0   ,  // Never
      ICC_NE  =  9   ,  // Not Equal
      ICC_E   =  1   ,  // Equal
      ICC_G   = 10   ,  // Greater
      ICC_LE  =  2   ,  // Less or Equal
      ICC_GE  = 11   ,  // Greater or Equal
      ICC_L   =  3   ,  // Less
      ICC_GU  = 12   ,  // Greater Unsigned
      ICC_LEU =  4   ,  // Less or Equal Unsigned
      ICC_CC  = 13   ,  // Carry Clear/Great or Equal Unsigned
      ICC_CS  =  5   ,  // Carry Set/Less Unsigned
      ICC_POS = 14   ,  // Positive
      ICC_NEG =  6   ,  // Negative
      ICC_VC  = 15   ,  // Overflow Clear
      ICC_VS  =  7   ,  // Overflow Set

      FCC_A   =  8+16,  // Always
      FCC_N   =  0+16,  // Never
      FCC_U   =  7+16,  // Unordered
      FCC_G   =  6+16,  // Greater
      FCC_UG  =  5+16,  // Unordered or Greater
      FCC_L   =  4+16,  // Less
      FCC_UL  =  3+16,  // Unordered or Less
      FCC_LG  =  2+16,  // Less or Greater
      FCC_NE  =  1+16,  // Not Equal
      FCC_E   =  9+16,  // Equal
      FCC_UE  = 10+16,  // Unordered or Equal
      FCC_GE  = 11+16,  // Greater or Equal
      FCC_UGE = 12+16,  // Unordered or Greater or Equal
      FCC_LE  = 13+16,  // Less or Equal
      FCC_ULE = 14+16,  // Unordered or Less or Equal
      FCC_O   = 15+16,  // Ordered

      CPCC_A   =  8+32,  // Always
      CPCC_N   =  0+32,  // Never
      CPCC_3   =  7+32,
      CPCC_2   =  6+32,
      CPCC_23  =  5+32,
      CPCC_1   =  4+32,
      CPCC_13  =  3+32,
      CPCC_12  =  2+32,
      CPCC_123 =  1+32,
      CPCC_0   =  9+32,
      CPCC_03  = 10+32,
      CPCC_02  = 11+32,
      CPCC_023 = 12+32,
      CPCC_01  = 13+32,
      CPCC_013 = 14+32,
      CPCC_012 = 15+32
    };
  }

  inline static const char *E2KCondCodeToString(E2KCC::CondCodes CC) {
    switch (CC) {
    case E2KCC::ICC_A:   return "a";
    case E2KCC::ICC_N:   return "n";
    case E2KCC::ICC_NE:  return "ne";
    case E2KCC::ICC_E:   return "e";
    case E2KCC::ICC_G:   return "g";
    case E2KCC::ICC_LE:  return "le";
    case E2KCC::ICC_GE:  return "ge";
    case E2KCC::ICC_L:   return "l";
    case E2KCC::ICC_GU:  return "gu";
    case E2KCC::ICC_LEU: return "leu";
    case E2KCC::ICC_CC:  return "cc";
    case E2KCC::ICC_CS:  return "cs";
    case E2KCC::ICC_POS: return "pos";
    case E2KCC::ICC_NEG: return "neg";
    case E2KCC::ICC_VC:  return "vc";
    case E2KCC::ICC_VS:  return "vs";
    case E2KCC::FCC_A:   return "a";
    case E2KCC::FCC_N:   return "n";
    case E2KCC::FCC_U:   return "u";
    case E2KCC::FCC_G:   return "g";
    case E2KCC::FCC_UG:  return "ug";
    case E2KCC::FCC_L:   return "l";
    case E2KCC::FCC_UL:  return "ul";
    case E2KCC::FCC_LG:  return "lg";
    case E2KCC::FCC_NE:  return "ne";
    case E2KCC::FCC_E:   return "e";
    case E2KCC::FCC_UE:  return "ue";
    case E2KCC::FCC_GE:  return "ge";
    case E2KCC::FCC_UGE: return "uge";
    case E2KCC::FCC_LE:  return "le";
    case E2KCC::FCC_ULE: return "ule";
    case E2KCC::FCC_O:   return "o";
    case E2KCC::CPCC_A:   return "a";
    case E2KCC::CPCC_N:   return "n";
    case E2KCC::CPCC_3:   return "3";
    case E2KCC::CPCC_2:   return "2";
    case E2KCC::CPCC_23:  return "23";
    case E2KCC::CPCC_1:   return "1";
    case E2KCC::CPCC_13:  return "13";
    case E2KCC::CPCC_12:  return "12";
    case E2KCC::CPCC_123: return "123";
    case E2KCC::CPCC_0:   return "0";
    case E2KCC::CPCC_03:  return "03";
    case E2KCC::CPCC_02:  return "02";
    case E2KCC::CPCC_023: return "023";
    case E2KCC::CPCC_01:  return "01";
    case E2KCC::CPCC_013: return "013";
    case E2KCC::CPCC_012: return "012";
    }
    llvm_unreachable("Invalid cond code");
  }

  inline static unsigned HI22(int64_t imm) {
    return (unsigned)((imm >> 10) & ((1 << 22)-1));
  }

  inline static unsigned LO10(int64_t imm) {
    return (unsigned)(imm & 0x3FF);
  }

  inline static unsigned HIX22(int64_t imm) {
    return HI22(~imm);
  }

  inline static unsigned LOX10(int64_t imm) {
    return ~LO10(~imm);
  }

}  // end namespace llvm
#endif
