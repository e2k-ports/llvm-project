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

  namespace E2K {
      enum InstFlags {
      F64 = 1,
      F32S = 2,
      F16SHI = 3,
      F16SLO = 4,
    };
  }

  namespace E2KCC {
    enum CondCodes {
      CC_NEVER = 0,
      CC_ALWAYS = 1,
      CC_PRED = 2,
      CC_NOT_PRED = 3,
      CC_LOOP_END = 4,
      CC_NOT_LOOP_END = 5,
      CC_PRED_OR_LOOP_END = 6,
      CC_NOT_PRED_AND_NOT_LOOP_END = 7,
      CC_NOT_PRED_OR_LOOP_END = 14,
      CC_PRED_AND_NOT_LOOP_END = 15,
    };
  }

  inline static std::string E2KCondCodeToString(E2KCC::CondCodes CC, const std::string & Reg) {
    switch (CC) {
    case E2KCC::CC_ALWAYS: return "";
    case E2KCC::CC_NEVER: return "";
    case E2KCC::CC_PRED: return " ? " + Reg;
    case E2KCC::CC_NOT_PRED: return " ? ~" + Reg;
    case E2KCC::CC_LOOP_END: return " ? #LOOP_END";
    case E2KCC::CC_NOT_LOOP_END: return " ? #NOT_LOOP_END";
    case E2KCC::CC_PRED_OR_LOOP_END: return " ? " + Reg + " || #LOOP_END";
    case E2KCC::CC_NOT_PRED_AND_NOT_LOOP_END: return " ? ~" + Reg + " && #NOT_LOOP_END";
    case E2KCC::CC_NOT_PRED_OR_LOOP_END: return " ? ~" + Reg + " || #LOOP_END";
    case E2KCC::CC_PRED_AND_NOT_LOOP_END: return " ? " + Reg + " && #NOT_LOOP_END";
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
