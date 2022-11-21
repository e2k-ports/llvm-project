//===- E2KMachineFunctionInfo.h - E2K Machine Function Info -*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file declares  E2K specific per-machine-function information.
//
//===----------------------------------------------------------------------===//
#ifndef LLVM_LIB_TARGET_E2K_E2KMACHINEFUNCTIONINFO_H
#define LLVM_LIB_TARGET_E2K_E2KMACHINEFUNCTIONINFO_H

#include "llvm/CodeGen/MachineFunction.h"

namespace llvm {

  class E2KMachineFunctionInfo : public MachineFunctionInfo {
    virtual void anchor();
  private:
    Register GlobalBaseReg;

    /// VarArgsFrameOffset - Frame offset to start of varargs area.
    int VarArgsFrameOffset;

    /// SRetReturnReg - Holds the virtual register into which the sret
    /// argument is passed.
    Register SRetReturnReg;

    /// IsLeafProc - True if the function is a leaf procedure.
    bool IsLeafProc;
  public:
    E2KMachineFunctionInfo()
      : GlobalBaseReg(0), VarArgsFrameOffset(0), SRetReturnReg(0),
        IsLeafProc(false) {}
    explicit E2KMachineFunctionInfo(MachineFunction &MF)
      : GlobalBaseReg(0), VarArgsFrameOffset(0), SRetReturnReg(0),
        IsLeafProc(false) {}

    MachineFunctionInfo *
    clone(BumpPtrAllocator &Allocator, MachineFunction &DestMF,
          const DenseMap<MachineBasicBlock *, MachineBasicBlock *> &Src2DstMBB)
        const override;

    Register getGlobalBaseReg() const { return GlobalBaseReg; }
    void setGlobalBaseReg(Register Reg) { GlobalBaseReg = Reg; }

    int getVarArgsFrameOffset() const { return VarArgsFrameOffset; }
    void setVarArgsFrameOffset(int Offset) { VarArgsFrameOffset = Offset; }

    Register getSRetReturnReg() const { return SRetReturnReg; }
    void setSRetReturnReg(Register Reg) { SRetReturnReg = Reg; }

    void setLeafProc(bool rhs) { IsLeafProc = rhs; }
    bool isLeafProc() const { return IsLeafProc; }
  };
}

#endif
