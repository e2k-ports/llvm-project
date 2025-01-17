//===-- E2KMachineFunctionInfo.cpp - E2K Machine Function Info --------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "E2KMachineFunctionInfo.h"

using namespace llvm;

void E2KMachineFunctionInfo::anchor() { }

MachineFunctionInfo *E2KMachineFunctionInfo::clone(
    BumpPtrAllocator &Allocator, MachineFunction &DestMF,
    const DenseMap<MachineBasicBlock *, MachineBasicBlock *> &Src2DstMBB)
    const {
  return DestMF.cloneInfo<E2KMachineFunctionInfo>(*this);
}
