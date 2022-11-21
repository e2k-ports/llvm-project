//===-- E2KRegisterInfo.cpp - E2K Register Information ----------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the E2K implementation of the TargetRegisterInfo class.
//
//===----------------------------------------------------------------------===//

#include "E2KRegisterInfo.h"
#include "E2K.h"
#include "E2KMachineFunctionInfo.h"
#include "E2KSubtarget.h"
#include "llvm/ADT/BitVector.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/TargetInstrInfo.h"
#include "llvm/IR/Type.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/ErrorHandling.h"

using namespace llvm;

#define GET_REGINFO_TARGET_DESC
#include "E2KGenRegisterInfo.inc"

static cl::opt<bool>
ReserveAppRegisters("e2k-reserve-app-registers", cl::Hidden, cl::init(false),
                    cl::desc("Reserve application registers (%g2-%g4)"));

E2KRegisterInfo::E2KRegisterInfo() : E2KGenRegisterInfo(E2K::O7) {}

const MCPhysReg*
E2KRegisterInfo::getCalleeSavedRegs(const MachineFunction *MF) const {
  return CSR_SaveList;
}

const uint32_t *
E2KRegisterInfo::getCallPreservedMask(const MachineFunction &MF,
                                        CallingConv::ID CC) const {
  return CSR_RegMask;
}

const uint32_t*
E2KRegisterInfo::getRTCallPreservedMask(CallingConv::ID CC) const {
  return RTCSR_RegMask;
}

BitVector E2KRegisterInfo::getReservedRegs(const MachineFunction &MF) const {
  BitVector Reserved(getNumRegs());
  const E2KSubtarget &Subtarget = MF.getSubtarget<E2KSubtarget>();
  // FIXME: G1 reserved for now for large imm generation by frame code.
  Reserved.set(E2K::G1);

  // G1-G4 can be used in applications.
  if (ReserveAppRegisters) {
    Reserved.set(E2K::G2);
    Reserved.set(E2K::G3);
    Reserved.set(E2K::G4);
  }
  // G5 is not reserved in 64 bit mode.
  if (!Subtarget.is64Bit())
    Reserved.set(E2K::G5);

  Reserved.set(E2K::O6);
  Reserved.set(E2K::I6);
  Reserved.set(E2K::I7);
  Reserved.set(E2K::G0);
  Reserved.set(E2K::G6);
  Reserved.set(E2K::G7);

  // Also reserve the register pair aliases covering the above
  // registers, with the same conditions.
  Reserved.set(E2K::G0_G1);
  if (ReserveAppRegisters)
    Reserved.set(E2K::G2_G3);
  if (ReserveAppRegisters || !Subtarget.is64Bit())
    Reserved.set(E2K::G4_G5);

  Reserved.set(E2K::O6_O7);
  Reserved.set(E2K::I6_I7);
  Reserved.set(E2K::G6_G7);

  // Unaliased double registers are not available in non-V9 targets.
  if (!Subtarget.isV9()) {
    for (unsigned n = 0; n != 16; ++n) {
      for (MCRegAliasIterator AI(E2K::D16 + n, this, true); AI.isValid(); ++AI)
        Reserved.set(*AI);
    }
  }

  // Reserve ASR1-ASR31
  for (unsigned n = 0; n < 31; n++)
    Reserved.set(E2K::ASR1 + n);

  return Reserved;
}

const TargetRegisterClass*
E2KRegisterInfo::getPointerRegClass(const MachineFunction &MF,
                                      unsigned Kind) const {
  const E2KSubtarget &Subtarget = MF.getSubtarget<E2KSubtarget>();
  return Subtarget.is64Bit() ? &E2K::I64RegsRegClass : &E2K::IntRegsRegClass;
}

static void replaceFI(MachineFunction &MF, MachineBasicBlock::iterator II,
                      MachineInstr &MI, const DebugLoc &dl,
                      unsigned FIOperandNum, int Offset, unsigned FramePtr) {
  // Replace frame index with a frame pointer reference.
  if (Offset >= -4096 && Offset <= 4095) {
    // If the offset is small enough to fit in the immediate field, directly
    // encode it.
    MI.getOperand(FIOperandNum).ChangeToRegister(FramePtr, false);
    MI.getOperand(FIOperandNum + 1).ChangeToImmediate(Offset);
    return;
  }

  const TargetInstrInfo &TII = *MF.getSubtarget().getInstrInfo();

  // FIXME: it would be better to scavenge a register here instead of
  // reserving G1 all of the time.
  if (Offset >= 0) {
    // Emit nonnegaive immediates with sethi + or.
    // sethi %hi(Offset), %g1
    // add %g1, %fp, %g1
    // Insert G1+%lo(offset) into the user.
    BuildMI(*MI.getParent(), II, dl, TII.get(E2K::SETHIi), E2K::G1)
      .addImm(HI22(Offset));


    // Emit G1 = G1 + I6
    BuildMI(*MI.getParent(), II, dl, TII.get(E2K::ADDrr), E2K::G1).addReg(E2K::G1)
      .addReg(FramePtr);
    // Insert: G1+%lo(offset) into the user.
    MI.getOperand(FIOperandNum).ChangeToRegister(E2K::G1, false);
    MI.getOperand(FIOperandNum + 1).ChangeToImmediate(LO10(Offset));
    return;
  }

  // Emit Negative numbers with sethi + xor
  // sethi %hix(Offset), %g1
  // xor  %g1, %lox(offset), %g1
  // add %g1, %fp, %g1
  // Insert: G1 + 0 into the user.
  BuildMI(*MI.getParent(), II, dl, TII.get(E2K::SETHIi), E2K::G1)
    .addImm(HIX22(Offset));
  BuildMI(*MI.getParent(), II, dl, TII.get(E2K::XORri), E2K::G1)
    .addReg(E2K::G1).addImm(LOX10(Offset));

  BuildMI(*MI.getParent(), II, dl, TII.get(E2K::ADDrr), E2K::G1).addReg(E2K::G1)
    .addReg(FramePtr);
  // Insert: G1+%lo(offset) into the user.
  MI.getOperand(FIOperandNum).ChangeToRegister(E2K::G1, false);
  MI.getOperand(FIOperandNum + 1).ChangeToImmediate(0);
}


bool
E2KRegisterInfo::eliminateFrameIndex(MachineBasicBlock::iterator II,
                                       int SPAdj, unsigned FIOperandNum,
                                       RegScavenger *RS) const {
  assert(SPAdj == 0 && "Unexpected");

  MachineInstr &MI = *II;
  DebugLoc dl = MI.getDebugLoc();
  int FrameIndex = MI.getOperand(FIOperandNum).getIndex();
  MachineFunction &MF = *MI.getParent()->getParent();
  const E2KSubtarget &Subtarget = MF.getSubtarget<E2KSubtarget>();
  const E2KFrameLowering *TFI = getFrameLowering(MF);

  Register FrameReg;
  int Offset;
  Offset = TFI->getFrameIndexReference(MF, FrameIndex, FrameReg).getFixed();

  Offset += MI.getOperand(FIOperandNum + 1).getImm();

  if (!Subtarget.isV9() || !Subtarget.hasHardQuad()) {
    if (MI.getOpcode() == E2K::STQFri) {
      const TargetInstrInfo &TII = *Subtarget.getInstrInfo();
      Register SrcReg = MI.getOperand(2).getReg();
      Register SrcEvenReg = getSubReg(SrcReg, E2K::sub_even64);
      Register SrcOddReg = getSubReg(SrcReg, E2K::sub_odd64);
      MachineInstr *StMI =
        BuildMI(*MI.getParent(), II, dl, TII.get(E2K::STDFri))
        .addReg(FrameReg).addImm(0).addReg(SrcEvenReg);
      replaceFI(MF, *StMI, *StMI, dl, 0, Offset, FrameReg);
      MI.setDesc(TII.get(E2K::STDFri));
      MI.getOperand(2).setReg(SrcOddReg);
      Offset += 8;
    } else if (MI.getOpcode() == E2K::LDQFri) {
      const TargetInstrInfo &TII = *Subtarget.getInstrInfo();
      Register DestReg = MI.getOperand(0).getReg();
      Register DestEvenReg = getSubReg(DestReg, E2K::sub_even64);
      Register DestOddReg = getSubReg(DestReg, E2K::sub_odd64);
      MachineInstr *LdMI =
        BuildMI(*MI.getParent(), II, dl, TII.get(E2K::LDDFri), DestEvenReg)
        .addReg(FrameReg).addImm(0);
      replaceFI(MF, *LdMI, *LdMI, dl, 1, Offset, FrameReg);

      MI.setDesc(TII.get(E2K::LDDFri));
      MI.getOperand(0).setReg(DestOddReg);
      Offset += 8;
    }
  }

  replaceFI(MF, II, MI, dl, FIOperandNum, Offset, FrameReg);
  // replaceFI never removes II
  return false;
}

Register E2KRegisterInfo::getFrameRegister(const MachineFunction &MF) const {
  return E2K::I6;
}

// E2K has no architectural need for stack realignment support,
// except that LLVM unfortunately currently implements overaligned
// stack objects by depending upon stack realignment support.
// If that ever changes, this can probably be deleted.
bool E2KRegisterInfo::canRealignStack(const MachineFunction &MF) const {
  if (!TargetRegisterInfo::canRealignStack(MF))
    return false;

  // E2K always has a fixed frame pointer register, so don't need to
  // worry about needing to reserve it. [even if we don't have a frame
  // pointer for our frame, it still cannot be used for other things,
  // or register window traps will be SADNESS.]

  // If there's a reserved call frame, we can use SP to access locals.
  if (getFrameLowering(MF)->hasReservedCallFrame(MF))
    return true;

  // Otherwise, we'd need a base pointer, but those aren't implemented
  // for E2K at the moment.

  return false;
}
