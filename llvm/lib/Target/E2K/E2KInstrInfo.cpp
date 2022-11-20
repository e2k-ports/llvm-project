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
  if (MI.getOpcode() == E2K::LDri || MI.getOpcode() == E2K::LDXri ||
      MI.getOpcode() == E2K::LDFri || MI.getOpcode() == E2K::LDDFri ||
      MI.getOpcode() == E2K::LDQFri) {
    if (MI.getOperand(1).isFI() && MI.getOperand(2).isImm() &&
        MI.getOperand(2).getImm() == 0) {
      FrameIndex = MI.getOperand(1).getIndex();
      return MI.getOperand(0).getReg();
    }
  }
  return 0;
}

/// isStoreToStackSlot - If the specified machine instruction is a direct
/// store to a stack slot, return the virtual or physical register number of
/// the source reg along with the FrameIndex of the loaded stack slot.  If
/// not, return 0.  This predicate must return 0 if the instruction has
/// any side effects other than storing to the stack slot.
unsigned E2KInstrInfo::isStoreToStackSlot(const MachineInstr &MI,
                                            int &FrameIndex) const {
  if (MI.getOpcode() == E2K::STri || MI.getOpcode() == E2K::STXri ||
      MI.getOpcode() == E2K::STFri || MI.getOpcode() == E2K::STDFri ||
      MI.getOpcode() == E2K::STQFri) {
    if (MI.getOperand(0).isFI() && MI.getOperand(1).isImm() &&
        MI.getOperand(1).getImm() == 0) {
      FrameIndex = MI.getOperand(0).getIndex();
      return MI.getOperand(2).getReg();
    }
  }
  return 0;
}

static bool IsIntegerCC(unsigned CC)
{
  return  (CC <= E2KCC::ICC_VC);
}

static E2KCC::CondCodes GetOppositeBranchCondition(E2KCC::CondCodes CC)
{
  switch(CC) {
  case E2KCC::ICC_A:    return E2KCC::ICC_N;
  case E2KCC::ICC_N:    return E2KCC::ICC_A;
  case E2KCC::ICC_NE:   return E2KCC::ICC_E;
  case E2KCC::ICC_E:    return E2KCC::ICC_NE;
  case E2KCC::ICC_G:    return E2KCC::ICC_LE;
  case E2KCC::ICC_LE:   return E2KCC::ICC_G;
  case E2KCC::ICC_GE:   return E2KCC::ICC_L;
  case E2KCC::ICC_L:    return E2KCC::ICC_GE;
  case E2KCC::ICC_GU:   return E2KCC::ICC_LEU;
  case E2KCC::ICC_LEU:  return E2KCC::ICC_GU;
  case E2KCC::ICC_CC:   return E2KCC::ICC_CS;
  case E2KCC::ICC_CS:   return E2KCC::ICC_CC;
  case E2KCC::ICC_POS:  return E2KCC::ICC_NEG;
  case E2KCC::ICC_NEG:  return E2KCC::ICC_POS;
  case E2KCC::ICC_VC:   return E2KCC::ICC_VS;
  case E2KCC::ICC_VS:   return E2KCC::ICC_VC;

  case E2KCC::FCC_A:    return E2KCC::FCC_N;
  case E2KCC::FCC_N:    return E2KCC::FCC_A;
  case E2KCC::FCC_U:    return E2KCC::FCC_O;
  case E2KCC::FCC_O:    return E2KCC::FCC_U;
  case E2KCC::FCC_G:    return E2KCC::FCC_ULE;
  case E2KCC::FCC_LE:   return E2KCC::FCC_UG;
  case E2KCC::FCC_UG:   return E2KCC::FCC_LE;
  case E2KCC::FCC_ULE:  return E2KCC::FCC_G;
  case E2KCC::FCC_L:    return E2KCC::FCC_UGE;
  case E2KCC::FCC_GE:   return E2KCC::FCC_UL;
  case E2KCC::FCC_UL:   return E2KCC::FCC_GE;
  case E2KCC::FCC_UGE:  return E2KCC::FCC_L;
  case E2KCC::FCC_LG:   return E2KCC::FCC_UE;
  case E2KCC::FCC_UE:   return E2KCC::FCC_LG;
  case E2KCC::FCC_NE:   return E2KCC::FCC_E;
  case E2KCC::FCC_E:    return E2KCC::FCC_NE;

  case E2KCC::CPCC_A:   return E2KCC::CPCC_N;
  case E2KCC::CPCC_N:   return E2KCC::CPCC_A;
  case E2KCC::CPCC_3:   [[fallthrough]];
  case E2KCC::CPCC_2:   [[fallthrough]];
  case E2KCC::CPCC_23:  [[fallthrough]];
  case E2KCC::CPCC_1:   [[fallthrough]];
  case E2KCC::CPCC_13:  [[fallthrough]];
  case E2KCC::CPCC_12:  [[fallthrough]];
  case E2KCC::CPCC_123: [[fallthrough]];
  case E2KCC::CPCC_0:   [[fallthrough]];
  case E2KCC::CPCC_03:  [[fallthrough]];
  case E2KCC::CPCC_02:  [[fallthrough]];
  case E2KCC::CPCC_023: [[fallthrough]];
  case E2KCC::CPCC_01:  [[fallthrough]];
  case E2KCC::CPCC_013: [[fallthrough]];
  case E2KCC::CPCC_012:
      // "Opposite" code is not meaningful, as we don't know
      // what the CoProc condition means here. The cond-code will
      // only be used in inline assembler, so this code should
      // not be reached in a normal compilation pass.
      llvm_unreachable("Meaningless inversion of co-processor cond code");
  }
  llvm_unreachable("Invalid cond code");
}

static bool isUncondBranchOpcode(int Opc) {
  return Opc == E2K::BA || Opc == E2K::BPA;
}

static bool isI32CondBranchOpcode(int Opc) {
  return Opc == E2K::BCOND || Opc == E2K::BPICC || Opc == E2K::BPICCA ||
         Opc == E2K::BPICCNT || Opc == E2K::BPICCANT;
}

static bool isI64CondBranchOpcode(int Opc) {
  return Opc == E2K::BPXCC || Opc == E2K::BPXCCA || Opc == E2K::BPXCCNT ||
         Opc == E2K::BPXCCANT;
}

static bool isFCondBranchOpcode(int Opc) { return Opc == E2K::FBCOND; }

static bool isCondBranchOpcode(int Opc) {
  return isI32CondBranchOpcode(Opc) || isI64CondBranchOpcode(Opc) ||
         isFCondBranchOpcode(Opc);
}

static bool isIndirectBranchOpcode(int Opc) {
  return Opc == E2K::BINDrr || Opc == E2K::BINDri;
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

  if (Cond.empty()) {
    assert(!FBB && "Unconditional branch with multiple successors!");
    BuildMI(&MBB, DL, get(Subtarget.isV9() ? E2K::BPA : E2K::BA)).addMBB(TBB);
    return 1;
  }

  // Conditional branch
  unsigned Opc = Cond[0].getImm();
  unsigned CC = Cond[1].getImm();

  if (IsIntegerCC(CC)) {
    BuildMI(&MBB, DL, get(Opc)).addMBB(TBB).addImm(CC);
  } else {
    BuildMI(&MBB, DL, get(E2K::FBCOND)).addMBB(TBB).addImm(CC);
  }
  if (!FBB)
    return 1;

  BuildMI(&MBB, DL, get(Subtarget.isV9() ? E2K::BPA : E2K::BA)).addMBB(FBB);
  return 2;
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
  unsigned numSubRegs = 0;
  unsigned movOpc     = 0;
  const unsigned *subRegIdx = nullptr;
  bool ExtraG0 = false;

  const unsigned DW_SubRegsIdx[]  = { E2K::sub_even, E2K::sub_odd };
  const unsigned DFP_FP_SubRegsIdx[]  = { E2K::sub_even, E2K::sub_odd };
  const unsigned QFP_DFP_SubRegsIdx[] = { E2K::sub_even64, E2K::sub_odd64 };
  const unsigned QFP_FP_SubRegsIdx[]  = { E2K::sub_even, E2K::sub_odd,
                                          E2K::sub_odd64_then_sub_even,
                                          E2K::sub_odd64_then_sub_odd };

  if (E2K::IntRegsRegClass.contains(DestReg, SrcReg))
    BuildMI(MBB, I, DL, get(E2K::ORrr), DestReg).addReg(E2K::G0)
      .addReg(SrcReg, getKillRegState(KillSrc));
  else if (E2K::IntPairRegClass.contains(DestReg, SrcReg)) {
    subRegIdx  = DW_SubRegsIdx;
    numSubRegs = 2;
    movOpc     = E2K::ORrr;
    ExtraG0 = true;
  } else if (E2K::FPRegsRegClass.contains(DestReg, SrcReg))
    BuildMI(MBB, I, DL, get(E2K::FMOVS), DestReg)
      .addReg(SrcReg, getKillRegState(KillSrc));
  else if (E2K::DFPRegsRegClass.contains(DestReg, SrcReg)) {
    if (Subtarget.isV9()) {
      BuildMI(MBB, I, DL, get(E2K::FMOVD), DestReg)
        .addReg(SrcReg, getKillRegState(KillSrc));
    } else {
      // Use two FMOVS instructions.
      subRegIdx  = DFP_FP_SubRegsIdx;
      numSubRegs = 2;
      movOpc     = E2K::FMOVS;
    }
  } else if (E2K::QFPRegsRegClass.contains(DestReg, SrcReg)) {
    if (Subtarget.isV9()) {
      if (Subtarget.hasHardQuad()) {
        BuildMI(MBB, I, DL, get(E2K::FMOVQ), DestReg)
          .addReg(SrcReg, getKillRegState(KillSrc));
      } else {
        // Use two FMOVD instructions.
        subRegIdx  = QFP_DFP_SubRegsIdx;
        numSubRegs = 2;
        movOpc     = E2K::FMOVD;
      }
    } else {
      // Use four FMOVS instructions.
      subRegIdx  = QFP_FP_SubRegsIdx;
      numSubRegs = 4;
      movOpc     = E2K::FMOVS;
    }
  } else if (E2K::ASRRegsRegClass.contains(DestReg) &&
             E2K::IntRegsRegClass.contains(SrcReg)) {
    BuildMI(MBB, I, DL, get(E2K::WRASRrr), DestReg)
        .addReg(E2K::G0)
        .addReg(SrcReg, getKillRegState(KillSrc));
  } else if (E2K::IntRegsRegClass.contains(DestReg) &&
             E2K::ASRRegsRegClass.contains(SrcReg)) {
    BuildMI(MBB, I, DL, get(E2K::RDASR), DestReg)
        .addReg(SrcReg, getKillRegState(KillSrc));
  } else
    llvm_unreachable("Impossible reg-to-reg copy");

  if (numSubRegs == 0 || subRegIdx == nullptr || movOpc == 0)
    return;

  const TargetRegisterInfo *TRI = &getRegisterInfo();
  MachineInstr *MovMI = nullptr;

  for (unsigned i = 0; i != numSubRegs; ++i) {
    Register Dst = TRI->getSubReg(DestReg, subRegIdx[i]);
    Register Src = TRI->getSubReg(SrcReg, subRegIdx[i]);
    assert(Dst && Src && "Bad sub-register");

    MachineInstrBuilder MIB = BuildMI(MBB, I, DL, get(movOpc), Dst);
    if (ExtraG0)
      MIB.addReg(E2K::G0);
    MIB.addReg(Src);
    MovMI = MIB.getInstr();
  }
  // Add implicit super-register defs and kills to the last MovMI.
  MovMI->addRegisterDefined(DestReg, TRI);
  if (KillSrc)
    MovMI->addRegisterKilled(SrcReg, TRI);
}

void E2KInstrInfo::
storeRegToStackSlot(MachineBasicBlock &MBB,
                    MachineBasicBlock::iterator I,
                    Register SrcReg, bool isKill, int FI,
                    const TargetRegisterClass *RC,
                    const TargetRegisterInfo *TRI,
                    Register VReg) const {
  DebugLoc DL;
  if (I != MBB.end()) DL = I->getDebugLoc();

  MachineFunction *MF = MBB.getParent();
  const MachineFrameInfo &MFI = MF->getFrameInfo();
  MachineMemOperand *MMO = MF->getMachineMemOperand(
      MachinePointerInfo::getFixedStack(*MF, FI), MachineMemOperand::MOStore,
      MFI.getObjectSize(FI), MFI.getObjectAlign(FI));

  // On the order of operands here: think "[FrameIdx + 0] = SrcReg".
  if (RC == &E2K::I64RegsRegClass)
    BuildMI(MBB, I, DL, get(E2K::STXri)).addFrameIndex(FI).addImm(0)
      .addReg(SrcReg, getKillRegState(isKill)).addMemOperand(MMO);
  else if (RC == &E2K::IntRegsRegClass)
    BuildMI(MBB, I, DL, get(E2K::STri)).addFrameIndex(FI).addImm(0)
      .addReg(SrcReg, getKillRegState(isKill)).addMemOperand(MMO);
  else if (RC == &E2K::IntPairRegClass)
    BuildMI(MBB, I, DL, get(E2K::STDri)).addFrameIndex(FI).addImm(0)
      .addReg(SrcReg, getKillRegState(isKill)).addMemOperand(MMO);
  else if (RC == &E2K::FPRegsRegClass)
    BuildMI(MBB, I, DL, get(E2K::STFri)).addFrameIndex(FI).addImm(0)
      .addReg(SrcReg,  getKillRegState(isKill)).addMemOperand(MMO);
  else if (E2K::DFPRegsRegClass.hasSubClassEq(RC))
    BuildMI(MBB, I, DL, get(E2K::STDFri)).addFrameIndex(FI).addImm(0)
      .addReg(SrcReg,  getKillRegState(isKill)).addMemOperand(MMO);
  else if (E2K::QFPRegsRegClass.hasSubClassEq(RC))
    // Use STQFri irrespective of its legality. If STQ is not legal, it will be
    // lowered into two STDs in eliminateFrameIndex.
    BuildMI(MBB, I, DL, get(E2K::STQFri)).addFrameIndex(FI).addImm(0)
      .addReg(SrcReg,  getKillRegState(isKill)).addMemOperand(MMO);
  else
    llvm_unreachable("Can't store this register to stack slot");
}

void E2KInstrInfo::
loadRegFromStackSlot(MachineBasicBlock &MBB,
                     MachineBasicBlock::iterator I,
                     Register DestReg, int FI,
                     const TargetRegisterClass *RC,
                     const TargetRegisterInfo *TRI,
                     Register VReg) const {
  DebugLoc DL;
  if (I != MBB.end()) DL = I->getDebugLoc();

  MachineFunction *MF = MBB.getParent();
  const MachineFrameInfo &MFI = MF->getFrameInfo();
  MachineMemOperand *MMO = MF->getMachineMemOperand(
      MachinePointerInfo::getFixedStack(*MF, FI), MachineMemOperand::MOLoad,
      MFI.getObjectSize(FI), MFI.getObjectAlign(FI));

  if (RC == &E2K::I64RegsRegClass)
    BuildMI(MBB, I, DL, get(E2K::LDXri), DestReg).addFrameIndex(FI).addImm(0)
      .addMemOperand(MMO);
  else if (RC == &E2K::IntRegsRegClass)
    BuildMI(MBB, I, DL, get(E2K::LDri), DestReg).addFrameIndex(FI).addImm(0)
      .addMemOperand(MMO);
  else if (RC == &E2K::IntPairRegClass)
    BuildMI(MBB, I, DL, get(E2K::LDDri), DestReg).addFrameIndex(FI).addImm(0)
      .addMemOperand(MMO);
  else if (RC == &E2K::FPRegsRegClass)
    BuildMI(MBB, I, DL, get(E2K::LDFri), DestReg).addFrameIndex(FI).addImm(0)
      .addMemOperand(MMO);
  else if (E2K::DFPRegsRegClass.hasSubClassEq(RC))
    BuildMI(MBB, I, DL, get(E2K::LDDFri), DestReg).addFrameIndex(FI).addImm(0)
      .addMemOperand(MMO);
  else if (E2K::QFPRegsRegClass.hasSubClassEq(RC))
    // Use LDQFri irrespective of its legality. If LDQ is not legal, it will be
    // lowered into two LDDs in eliminateFrameIndex.
    BuildMI(MBB, I, DL, get(E2K::LDQFri), DestReg).addFrameIndex(FI).addImm(0)
      .addMemOperand(MMO);
  else
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
    Subtarget.is64Bit() ? &E2K::I64RegsRegClass : &E2K::IntRegsRegClass;
  GlobalBaseReg = RegInfo.createVirtualRegister(PtrRC);

  DebugLoc dl;

  BuildMI(FirstMBB, MBBI, dl, get(E2K::GETPCX), GlobalBaseReg);
  E2KFI->setGlobalBaseReg(GlobalBaseReg);
  return GlobalBaseReg;
}

bool E2KInstrInfo::expandPostRAPseudo(MachineInstr &MI) const {
  switch (MI.getOpcode()) {
  case TargetOpcode::LOAD_STACK_GUARD: {
    assert(Subtarget.isTargetLinux() &&
           "Only Linux target is expected to contain LOAD_STACK_GUARD");
    // offsetof(tcbhead_t, stack_guard) from sysdeps/e2k/nptl/tls.h in glibc.
    const int64_t Offset = Subtarget.is64Bit() ? 0x28 : 0x14;
    MI.setDesc(get(Subtarget.is64Bit() ? E2K::LDXri : E2K::LDri));
    MachineInstrBuilder(*MI.getParent()->getParent(), MI)
        .addReg(E2K::G7)
        .addImm(Offset);
    return true;
  }
  }
  return false;
}
