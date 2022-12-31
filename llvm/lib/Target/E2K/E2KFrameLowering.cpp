//===-- E2KFrameLowering.cpp - E2K Frame Information ------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the E2K implementation of TargetFrameLowering class.
//
//===----------------------------------------------------------------------===//

#include "E2KFrameLowering.h"
#include "E2KInstrInfo.h"
#include "E2KMachineFunctionInfo.h"
#include "E2KSubtarget.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/Function.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Target/TargetOptions.h"

using namespace llvm;

static cl::opt<bool>
DisableLeafProc("disable-e2k-leaf-proc",
                cl::init(false),
                cl::desc("Disable E2K leaf procedure optimization."),
                cl::Hidden);

E2KFrameLowering::E2KFrameLowering(const E2KSubtarget &ST)
    : TargetFrameLowering(TargetFrameLowering::StackGrowsDown,
                          ST.is64Bit() ? Align(16) : Align(8), 0,
                          ST.is64Bit() ? Align(16) : Align(8)) {}

void E2KFrameLowering::emitSPAdjustment(MachineFunction &MF,
                                          MachineBasicBlock &MBB,
                                          MachineBasicBlock::iterator MBBI,
                                          int NumBytes,
                                          unsigned ADDrr,
                                          unsigned ADDri) const {

  DebugLoc dl;
  const E2KInstrInfo &TII =
      *static_cast<const E2KInstrInfo *>(MF.getSubtarget().getInstrInfo());

  if (NumBytes >= -4096 && NumBytes < 4096) {
    BuildMI(MBB, MBBI, dl, TII.get(ADDri), E2K::R6)
      .addReg(E2K::R6).addImm(NumBytes);
    return;
  }
}

void E2KFrameLowering::emitPrologue(MachineFunction &MF,
                                      MachineBasicBlock &MBB) const {
}

MachineBasicBlock::iterator E2KFrameLowering::
eliminateCallFramePseudoInstr(MachineFunction &MF, MachineBasicBlock &MBB,
                              MachineBasicBlock::iterator I) const {
  return MBB.erase(I);
}


void E2KFrameLowering::emitEpilogue(MachineFunction &MF,
                                  MachineBasicBlock &MBB) const {
}

bool E2KFrameLowering::hasReservedCallFrame(const MachineFunction &MF) const {
  // Reserve call frame if there are no variable sized objects on the stack.
  return !MF.getFrameInfo().hasVarSizedObjects();
}

// hasFP - Return true if the specified function should have a dedicated frame
// pointer register.  This is true if the function has variable sized allocas or
// if frame pointer elimination is disabled.
bool E2KFrameLowering::hasFP(const MachineFunction &MF) const {
  const TargetRegisterInfo *RegInfo = MF.getSubtarget().getRegisterInfo();

  const MachineFrameInfo &MFI = MF.getFrameInfo();
  return MF.getTarget().Options.DisableFramePointerElim(MF) ||
         RegInfo->hasStackRealignment(MF) || MFI.hasVarSizedObjects() ||
         MFI.isFrameAddressTaken();
}

StackOffset
E2KFrameLowering::getFrameIndexReference(const MachineFunction &MF, int FI,
                                           Register &FrameReg) const {
  const E2KSubtarget &Subtarget = MF.getSubtarget<E2KSubtarget>();
  const MachineFrameInfo &MFI = MF.getFrameInfo();
  const E2KRegisterInfo *RegInfo = Subtarget.getRegisterInfo();
  const E2KMachineFunctionInfo *FuncInfo = MF.getInfo<E2KMachineFunctionInfo>();
  bool isFixed = MFI.isFixedObjectIndex(FI);

  // Addressable stack objects are accessed using neg. offsets from
  // %fp, or positive offsets from %sp.
  bool UseFP;

  // E2K uses FP-based references in general, even when "hasFP" is
  // false. That function is rather a misnomer, because %fp is
  // actually always available, unless isLeafProc.
  if (FuncInfo->isLeafProc()) {
    // If there's a leaf proc, all offsets need to be %sp-based,
    // because we haven't caused %fp to actually point to our frame.
    UseFP = false;
  } else if (isFixed) {
    // Otherwise, argument access should always use %fp.
    UseFP = true;
  } else if (RegInfo->hasStackRealignment(MF)) {
    // If there is dynamic stack realignment, all local object
    // references need to be via %sp, to take account of the
    // re-alignment.
    UseFP = false;
  } else {
    // Finally, default to using %fp.
    UseFP = true;
  }

  int64_t FrameOffset = MF.getFrameInfo().getObjectOffset(FI) +
      Subtarget.getStackPointerBias();

  if (UseFP) {
    FrameReg = RegInfo->getFrameRegister(MF);
    return StackOffset::getFixed(FrameOffset);
  } else {
    FrameReg = E2K::R6; // %sp
    return StackOffset::getFixed(FrameOffset + MF.getFrameInfo().getStackSize());
  }
}

static bool LLVM_ATTRIBUTE_UNUSED verifyLeafProcRegUse(MachineRegisterInfo *MRI)
{

  for (unsigned reg = E2K::R0; reg <= E2K::R7; ++reg)
    if (MRI->isPhysRegUsed(reg))
      return false;

  for (unsigned reg = E2K::B1; reg <= E2K::B7; ++reg)
    if (MRI->isPhysRegUsed(reg))
      return false;

  return true;
}

bool E2KFrameLowering::isLeafProc(MachineFunction &MF) const
{

  MachineRegisterInfo &MRI = MF.getRegInfo();
  MachineFrameInfo    &MFI = MF.getFrameInfo();

  return !(MFI.hasCalls()               // has calls
           || MRI.isPhysRegUsed(E2K::R0) // Too many registers needed
           || MRI.isPhysRegUsed(E2K::R6) // %sp is used
           || hasFP(MF)                 // need %fp
           || MF.hasInlineAsm());       // has inline assembly
}

void E2KFrameLowering::remapRegsForLeafProc(MachineFunction &MF) const {
  MachineRegisterInfo &MRI = MF.getRegInfo();
  // Remap %i[0-7] to %o[0-7].
  for (unsigned reg = E2K::R0; reg <= E2K::R6; ++reg) {
    if (!MRI.isPhysRegUsed(reg))
      continue;

    unsigned mapped_reg = reg - E2K::R0 + E2K::R0;

    // Replace I register with O register.
    MRI.replaceRegWith(reg, mapped_reg);

    // Also replace register pair super-registers.
    if ((reg - E2K::R0) % 2 == 0) {
      unsigned preg = (reg - E2K::R0) / 2 + E2K::R0;
      unsigned mapped_preg = preg - E2K::R0 + E2K::R0;
      MRI.replaceRegWith(preg, mapped_preg);
    }
  }

  // Rewrite MBB's Live-ins.
  for (MachineBasicBlock &MBB : MF) {
    for (unsigned reg = E2K::R0; reg <= E2K::R7; ++reg) {
      if (!MBB.isLiveIn(reg))
        continue;
      MBB.removeLiveIn(reg);
      MBB.addLiveIn(reg - E2K::R0 + E2K::R0);
    }
    for (unsigned reg = E2K::R0; reg <= E2K::R7; ++reg) {
      if (!MBB.isLiveIn(reg))
        continue;
      MBB.removeLiveIn(reg);
      MBB.addLiveIn(reg - E2K::R0 + E2K::R0);
    }
  }

  assert(verifyLeafProcRegUse(&MRI));
#ifdef EXPENSIVE_CHECKS
  MF.verify(0, "After LeafProc Remapping");
#endif
}

void E2KFrameLowering::determineCalleeSaves(MachineFunction &MF,
                                              BitVector &SavedRegs,
                                              RegScavenger *RS) const {
  TargetFrameLowering::determineCalleeSaves(MF, SavedRegs, RS);
  if (!DisableLeafProc && isLeafProc(MF)) {
    E2KMachineFunctionInfo *MFI = MF.getInfo<E2KMachineFunctionInfo>();
    MFI->setLeafProc(true);

    remapRegsForLeafProc(MF);
  }

}
