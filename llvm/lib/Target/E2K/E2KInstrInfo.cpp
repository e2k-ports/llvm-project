//===-- E2KInstrInfo.cpp - E2K Instruction Information ----------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the E2K implementation of the TargetInstrInfo class.
//
//===----------------------------------------------------------------------===//

#include "E2KInstrInfo.h"
#include "E2K.h"
#include "E2KMachineFunctionInfo.h"
#include "E2KSubtarget.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineMemOperand.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/ErrorHandling.h"

using namespace llvm;

#define GET_INSTRINFO_CTOR_DTOR
#include "E2KGenInstrInfo.inc"

// Pin the vtable to this file.
void E2KInstrInfo::anchor() {}

E2KInstrInfo::E2KInstrInfo(E2KSubtarget &ST)
    : E2KGenInstrInfo(E2K::ADJCALLSTACKDOWN, E2K::ADJCALLSTACKUP), RI(),
      Subtarget(ST) {}

/// isLoadFromStackSlot - If the specified machine instruction is a direct
/// load from a stack slot, return the virtual or physical register number of
/// the destination along with the FrameIndex of the loaded stack slot.  If
/// not, return 0.  This predicate must return 0 if the instruction has
/// any side effects other than loading from the stack slot.
unsigned E2KInstrInfo::isLoadFromStackSlot(const MachineInstr &MI,
                                             int &FrameIndex) const {
  return 0;
}

/// isStoreToStackSlot - If the specified machine instruction is a direct
/// store to a stack slot, return the virtual or physical register number of
/// the source reg along with the FrameIndex of the loaded stack slot.  If
/// not, return 0.  This predicate must return 0 if the instruction has
/// any side effects other than storing to the stack slot.
unsigned E2KInstrInfo::isStoreToStackSlot(const MachineInstr &MI,
                                            int &FrameIndex) const {
  return 0;
}

static bool IsIntegerCC(unsigned CC)
{
  return  (CC <= E2KCC::ICC_VC);
}

static E2KCC::CondCodes GetOppositeBranchCondition(E2KCC::CondCodes CC)
{
  llvm_unreachable("Invalid cond code");
}

static bool isUncondBranchOpcode(int Opc) {
  return false;
}

static bool isI32CondBranchOpcode(int Opc) {
  return false;
}

static bool isI64CondBranchOpcode(int Opc) {
  return false;
}

static bool isFCondBranchOpcode(int Opc) { return false; }

static bool isCondBranchOpcode(int Opc) {
  return isI32CondBranchOpcode(Opc) || isI64CondBranchOpcode(Opc) ||
         isFCondBranchOpcode(Opc);
}

static bool isIndirectBranchOpcode(int Opc) {
  return false;
}

static void parseCondBranch(MachineInstr *LastInst, MachineBasicBlock *&Target,
                            SmallVectorImpl<MachineOperand> &Cond) {
  unsigned Opc = LastInst->getOpcode();
  int64_t CC = LastInst->getOperand(1).getImm();

  // Push the branch opcode into Cond too so later in insertBranch
  // it can use the information to emit the correct E2K branch opcode.
  Cond.push_back(MachineOperand::CreateImm(Opc));
  Cond.push_back(MachineOperand::CreateImm(CC));

  Target = LastInst->getOperand(0).getMBB();
}

bool E2KInstrInfo::analyzeBranch(MachineBasicBlock &MBB,
                                   MachineBasicBlock *&TBB,
                                   MachineBasicBlock *&FBB,
                                   SmallVectorImpl<MachineOperand> &Cond,
                                   bool AllowModify) const {
  MachineBasicBlock::iterator I = MBB.getLastNonDebugInstr();
  if (I == MBB.end())
    return false;

  if (!isUnpredicatedTerminator(*I))
    return false;

  // Get the last instruction in the block.
  MachineInstr *LastInst = &*I;
  unsigned LastOpc = LastInst->getOpcode();

  // If there is only one terminator instruction, process it.
  if (I == MBB.begin() || !isUnpredicatedTerminator(*--I)) {
    if (isUncondBranchOpcode(LastOpc)) {
      TBB = LastInst->getOperand(0).getMBB();
      return false;
    }
    if (isCondBranchOpcode(LastOpc)) {
      // Block ends with fall-through condbranch.
      parseCondBranch(LastInst, TBB, Cond);
      return false;
    }
    return true; // Can't handle indirect branch.
  }

  // Get the instruction before it if it is a terminator.
  MachineInstr *SecondLastInst = &*I;
  unsigned SecondLastOpc = SecondLastInst->getOpcode();

  // If AllowModify is true and the block ends with two or more unconditional
  // branches, delete all but the first unconditional branch.
  if (AllowModify && isUncondBranchOpcode(LastOpc)) {
    while (isUncondBranchOpcode(SecondLastOpc)) {
      LastInst->eraseFromParent();
      LastInst = SecondLastInst;
      LastOpc = LastInst->getOpcode();
      if (I == MBB.begin() || !isUnpredicatedTerminator(*--I)) {
        // Return now the only terminator is an unconditional branch.
        TBB = LastInst->getOperand(0).getMBB();
        return false;
      } else {
        SecondLastInst = &*I;
        SecondLastOpc = SecondLastInst->getOpcode();
      }
    }
  }

  // If there are three terminators, we don't know what sort of block this is.
  if (SecondLastInst && I != MBB.begin() && isUnpredicatedTerminator(*--I))
    return true;

  // If the block ends with a B and a Bcc, handle it.
  if (isCondBranchOpcode(SecondLastOpc) && isUncondBranchOpcode(LastOpc)) {
    parseCondBranch(SecondLastInst, TBB, Cond);
    FBB = LastInst->getOperand(0).getMBB();
    return false;
  }

  // If the block ends with two unconditional branches, handle it.  The second
  // one is not executed.
  if (isUncondBranchOpcode(SecondLastOpc) && isUncondBranchOpcode(LastOpc)) {
    TBB = SecondLastInst->getOperand(0).getMBB();
    return false;
  }

  // ...likewise if it ends with an indirect branch followed by an unconditional
  // branch.
  if (isIndirectBranchOpcode(SecondLastOpc) && isUncondBranchOpcode(LastOpc)) {
    I = LastInst;
    if (AllowModify)
      I->eraseFromParent();
    return true;
  }

  // Otherwise, can't handle this.
  return true;
}

unsigned E2KInstrInfo::insertBranch(MachineBasicBlock &MBB,
                                      MachineBasicBlock *TBB,
                                      MachineBasicBlock *FBB,
                                      ArrayRef<MachineOperand> Cond,
                                      const DebugLoc &DL,
                                      int *BytesAdded) const {
  assert(TBB && "insertBranch must not be told to insert a fallthrough");
  assert((Cond.size() <= 2) &&
         "E2K branch conditions should have at most two components!");
  assert(!BytesAdded && "code size not handled");

  llvm_unreachable("cannot insert branch");
}

unsigned E2KInstrInfo::removeBranch(MachineBasicBlock &MBB,
                                      int *BytesRemoved) const {
  assert(!BytesRemoved && "code size not handled");

  MachineBasicBlock::iterator I = MBB.end();
  unsigned Count = 0;
  while (I != MBB.begin()) {
    --I;

    if (I->isDebugInstr())
      continue;

    if (!isCondBranchOpcode(I->getOpcode()) &&
        !isUncondBranchOpcode(I->getOpcode()))
      break; // Not a branch

    I->eraseFromParent();
    I = MBB.end();
    ++Count;
  }
  return Count;
}

bool E2KInstrInfo::reverseBranchCondition(
    SmallVectorImpl<MachineOperand> &Cond) const {
  assert(Cond.size() <= 2);
  E2KCC::CondCodes CC = static_cast<E2KCC::CondCodes>(Cond[1].getImm());
  Cond[1].setImm(GetOppositeBranchCondition(CC));
  return false;
}

void E2KInstrInfo::copyPhysReg(MachineBasicBlock &MBB,
                                 MachineBasicBlock::iterator I,
                                 const DebugLoc &DL, MCRegister DestReg,
                                 MCRegister SrcReg, bool KillSrc) const {

  llvm_unreachable("Impossible reg-to-reg copy");
}

void E2KInstrInfo::
storeRegToStackSlot(MachineBasicBlock &MBB,
                    MachineBasicBlock::iterator I,
                    Register SrcReg, bool isKill, int FI,
                    const TargetRegisterClass *RC,
                    const TargetRegisterInfo *TRI,
                    Register VReg) const {
  llvm_unreachable("Can't store this register to stack slot");
}

void E2KInstrInfo::
loadRegFromStackSlot(MachineBasicBlock &MBB,
                     MachineBasicBlock::iterator I,
                     Register DestReg, int FI,
                     const TargetRegisterClass *RC,
                     const TargetRegisterInfo *TRI,
                     Register VReg) const {
  llvm_unreachable("Can't load this register from stack slot");
}

Register E2KInstrInfo::getGlobalBaseReg(MachineFunction *MF) const {
  E2KMachineFunctionInfo *E2KFI = MF->getInfo<E2KMachineFunctionInfo>();
  Register GlobalBaseReg = E2KFI->getGlobalBaseReg();
  if (GlobalBaseReg)
    return GlobalBaseReg;

  // Insert the set of GlobalBaseReg into the first MBB of the function
  MachineBasicBlock &FirstMBB = MF->front();
  MachineBasicBlock::iterator MBBI = FirstMBB.begin();
  MachineRegisterInfo &RegInfo = MF->getRegInfo();

  const TargetRegisterClass *PtrRC =
    Subtarget.is64Bit() ? &E2K::RegDRRegClass : &E2K::RegSRRegClass;
  GlobalBaseReg = RegInfo.createVirtualRegister(PtrRC);

  return GlobalBaseReg;
}

bool E2KInstrInfo::expandPostRAPseudo(MachineInstr &MI) const {
  return false;
}
