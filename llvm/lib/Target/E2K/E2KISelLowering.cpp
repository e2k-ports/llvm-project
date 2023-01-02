//===-- E2KISelLowering.cpp - E2K DAG Lowering Implementation ---------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file implements the interfaces that E2K uses to lower LLVM code into a
// selection DAG.
//
//===----------------------------------------------------------------------===//

#include "E2KISelLowering.h"
#include "MCTargetDesc/E2KMCExpr.h"
#include "E2KMachineFunctionInfo.h"
#include "E2KRegisterInfo.h"
#include "E2KTargetMachine.h"
#include "E2KTargetObjectFile.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/CodeGen/CallingConvLower.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/SelectionDAG.h"
#include "llvm/CodeGen/TargetLoweringObjectFileImpl.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/KnownBits.h"
using namespace llvm;


//===----------------------------------------------------------------------===//
// Calling Convention Implementation
//===----------------------------------------------------------------------===//

static bool CC_E2K_Assign_SRet(unsigned &ValNo, MVT &ValVT,
                                 MVT &LocVT, CCValAssign::LocInfo &LocInfo,
                                 ISD::ArgFlagsTy &ArgFlags, CCState &State)
{
  assert (ArgFlags.isSRet());

  // Assign SRet argument.
  State.addLoc(CCValAssign::getCustomMem(ValNo, ValVT,
                                         0,
                                         LocVT, LocInfo));
  return true;
}

static bool CC_E2K_Assign_Split_64(unsigned &ValNo, MVT &ValVT,
                                     MVT &LocVT, CCValAssign::LocInfo &LocInfo,
                                     ISD::ArgFlagsTy &ArgFlags, CCState &State)
{
  static const MCPhysReg RegList[] = {
    E2K::R0, E2K::R1, E2K::R2, E2K::R3, E2K::R4, E2K::R5, E2K::R6, E2K::R7
  };
  // Try to get first reg.
  if (Register Reg = State.AllocateReg(RegList)) {
    State.addLoc(CCValAssign::getCustomReg(ValNo, ValVT, Reg, LocVT, LocInfo));
  } else {
    // Assign whole thing in stack.
    State.addLoc(CCValAssign::getCustomMem(
        ValNo, ValVT, State.AllocateStack(8, Align(4)), LocVT, LocInfo));
    return true;
  }

  // Try to get second reg.
  if (Register Reg = State.AllocateReg(RegList))
    State.addLoc(CCValAssign::getCustomReg(ValNo, ValVT, Reg, LocVT, LocInfo));
  else
    State.addLoc(CCValAssign::getCustomMem(
        ValNo, ValVT, State.AllocateStack(4, Align(4)), LocVT, LocInfo));
  return true;
}

static bool CC_E2K_Assign_Ret_Split_64(unsigned &ValNo, MVT &ValVT,
                                         MVT &LocVT, CCValAssign::LocInfo &LocInfo,
                                         ISD::ArgFlagsTy &ArgFlags, CCState &State)
{
  static const MCPhysReg RegList[] = {
      E2K::DR0, E2K::DR1, E2K::DR2, E2K::DR3, E2K::DR4, E2K::DR5, E2K::DR6, E2K::DR7
  };

  // Try to get first reg.
  if (Register Reg = State.AllocateReg(RegList))
    State.addLoc(CCValAssign::getCustomReg(ValNo, ValVT, Reg, LocVT, LocInfo));
  else
    return false;

  // Try to get second reg.
  if (Register Reg = State.AllocateReg(RegList))
    State.addLoc(CCValAssign::getCustomReg(ValNo, ValVT, Reg, LocVT, LocInfo));
  else
    return false;

  return true;
}

// Allocate a full-sized argument for the 64-bit ABI.
static bool Analyze_CC_E2K64_Full(bool IsReturn, unsigned &ValNo, MVT &ValVT,
                                    MVT &LocVT, CCValAssign::LocInfo &LocInfo,
                                    ISD::ArgFlagsTy &ArgFlags, CCState &State) {
  assert((LocVT == MVT::f32 || LocVT == MVT::f128
          || LocVT.getSizeInBits() == 64) &&
         "Can't handle non-64 bits locations");

  // Stack space is allocated for all arguments starting from [%fp+BIAS+128].
  unsigned size      = (LocVT == MVT::f128) ? 16 : 8;
  Align alignment = (LocVT == MVT::f128) ? Align(16) : Align(8);
  unsigned Offset = State.AllocateStack(size, alignment);
  unsigned Reg = 0;

  if (LocVT == MVT::i64 && Offset < 6*8)
    // Promote integers to %i0-%i5.
    Reg = E2K::DR0 + Offset/8;
  else if (LocVT == MVT::f64 && Offset < 16*8)
    // Promote doubles to %d0-%d30. (Which LLVM calls D0-D15).
    Reg = E2K::DR0 + Offset/8;
  else if (LocVT == MVT::f32 && Offset < 16*8)
    // Promote floats to %f1, %f3, ...
    Reg = E2K::R0 + Offset/4;
  else if (LocVT == MVT::f128 && Offset < 16*8)
    // Promote long doubles to %q0-%q28. (Which LLVM calls Q0-Q7).
    Reg = E2K::QR0 + Offset/16;

  // Promote to register when possible, otherwise use the stack slot.
  if (Reg) {
    State.addLoc(CCValAssign::getReg(ValNo, ValVT, Reg, LocVT, LocInfo));
    return true;
  }

  // Bail out if this is a return CC and we run out of registers to place
  // values into.
  if (IsReturn)
    return false;

  // This argument goes on the stack in an 8-byte slot.
  // When passing floats, LocVT is smaller than 8 bytes. Adjust the offset to
  // the right-aligned float. The first 4 bytes of the stack slot are undefined.
  if (LocVT == MVT::f32)
    Offset += 4;

  State.addLoc(CCValAssign::getMem(ValNo, ValVT, Offset, LocVT, LocInfo));
  return true;
}

// Allocate a half-sized argument for the 64-bit ABI.
//
// This is used when passing { float, int } structs by value in registers.
static bool Analyze_CC_E2K64_Half(bool IsReturn, unsigned &ValNo, MVT &ValVT,
                                    MVT &LocVT, CCValAssign::LocInfo &LocInfo,
                                    ISD::ArgFlagsTy &ArgFlags, CCState &State) {
  assert(LocVT.getSizeInBits() == 32 && "Can't handle non-32 bits locations");
  unsigned Offset = State.AllocateStack(4, Align(4));

  if (LocVT == MVT::f32 && Offset < 16*8) {
    // Promote floats to %f0-%f31.
    State.addLoc(CCValAssign::getReg(ValNo, ValVT, E2K::R0 + Offset/4,
                                     LocVT, LocInfo));
    return true;
  }

  if (LocVT == MVT::i32 && Offset < 6*8) {
    // Promote integers to %i0-%i5, using half the register.
    unsigned Reg = E2K::R0 + Offset/8;
    LocVT = MVT::i64;
    LocInfo = CCValAssign::AExt;

    // Set the Custom bit if this i32 goes in the high bits of a register.
    if (Offset % 8 == 0)
      State.addLoc(CCValAssign::getCustomReg(ValNo, ValVT, Reg,
                                             LocVT, LocInfo));
    else
      State.addLoc(CCValAssign::getReg(ValNo, ValVT, Reg, LocVT, LocInfo));
    return true;
  }

  // Bail out if this is a return CC and we run out of registers to place
  // values into.
  if (IsReturn)
    return false;

  State.addLoc(CCValAssign::getMem(ValNo, ValVT, Offset, LocVT, LocInfo));
  return true;
}

static bool CC_E2K64_Full(unsigned &ValNo, MVT &ValVT, MVT &LocVT,
                            CCValAssign::LocInfo &LocInfo,
                            ISD::ArgFlagsTy &ArgFlags, CCState &State) {
  return Analyze_CC_E2K64_Full(false, ValNo, ValVT, LocVT, LocInfo, ArgFlags,
                                 State);
}

static bool CC_E2K64_Half(unsigned &ValNo, MVT &ValVT, MVT &LocVT,
                            CCValAssign::LocInfo &LocInfo,
                            ISD::ArgFlagsTy &ArgFlags, CCState &State) {
  return Analyze_CC_E2K64_Half(false, ValNo, ValVT, LocVT, LocInfo, ArgFlags,
                                 State);
}

static bool RetCC_E2K64_Full(unsigned &ValNo, MVT &ValVT, MVT &LocVT,
                               CCValAssign::LocInfo &LocInfo,
                               ISD::ArgFlagsTy &ArgFlags, CCState &State) {
  return Analyze_CC_E2K64_Full(true, ValNo, ValVT, LocVT, LocInfo, ArgFlags,
                                 State);
}

static bool RetCC_E2K64_Half(unsigned &ValNo, MVT &ValVT, MVT &LocVT,
                               CCValAssign::LocInfo &LocInfo,
                               ISD::ArgFlagsTy &ArgFlags, CCState &State) {
  return Analyze_CC_E2K64_Half(true, ValNo, ValVT, LocVT, LocInfo, ArgFlags,
                                 State);
}

#include "E2KGenCallingConv.inc"


bool E2KTargetLowering::CanLowerReturn(
    CallingConv::ID CallConv, MachineFunction &MF, bool isVarArg,
    const SmallVectorImpl<ISD::OutputArg> &Outs, LLVMContext &Context) const {
  return false;
}

SDValue
E2KTargetLowering::LowerReturn(SDValue Chain, CallingConv::ID CallConv,
                                 bool IsVarArg,
                                 const SmallVectorImpl<ISD::OutputArg> &Outs,
                                 const SmallVectorImpl<SDValue> &OutVals,
                                 const SDLoc &DL, SelectionDAG &DAG) const {

  // CCValAssign represent the assignment of the return value to a location
  SmallVector<CCValAssign, 16> RVLocs;
  MachineFunction &MF = DAG.getMachineFunction();

  // CCState represent the info about the registers and stack slot.
  CCState CCInfo(CallConv, IsVarArg, MF, RVLocs, *DAG.getContext());

  SDValue Flag;
  SmallVector<SDValue, 4> RetOps(1, Chain);

  // Copy the result values into the output registers.
  for (unsigned i = 0; i != RVLocs.size(); ++i) {
    SDValue Val = OutVals[i];
    CCValAssign &VA = RVLocs[i];
    assert(VA.isRegLoc() && "Can only return in registers!");

    if (RVLocs[i].getValVT() != RVLocs[i].getLocVT())
      Val = DAG.getNode(ISD::BITCAST, DL, RVLocs[i].getLocVT(), Val);

    Chain = DAG.getCopyToReg(Chain, DL, VA.getLocReg(), Val, Flag);

    // Guarantee that all emitted copies are stuck together with flags.
    Flag = Chain.getValue(1);
    RetOps.push_back(DAG.getRegister(VA.getLocReg(), VA.getLocVT()));
  }

  if (MF.getFunction().hasStructRetAttr()) {
    E2KMachineFunctionInfo *Cpu0FI = MF.getInfo<E2KMachineFunctionInfo>();
    unsigned Reg = Cpu0FI->getSRetReturnReg();

    if (!Reg)
      llvm_unreachable("sret virtual register not created in the entry block");
    SDValue Val = DAG.getCopyFromReg(Chain, DL, Reg,
                                     getPointerTy(DAG.getDataLayout()));
    Chain = DAG.getCopyToReg(Chain, DL, E2K::R0, Val, Flag);
    Flag = Chain.getValue(1);
    RetOps.push_back(DAG.getRegister(E2K::R0, getPointerTy(DAG.getDataLayout())));
  }

  RetOps[0] = Chain;    // Update chain.

  // Add the flag if we have it.
  if (Flag.getNode())
    RetOps.push_back(Flag);

  return DAG.getNode(E2KISD::RET_FLAG, DL, MVT::Other, RetOps);
}

SDValue E2KTargetLowering::LowerFormalArguments(
    SDValue Chain, CallingConv::ID CallConv, bool IsVarArg,
    const SmallVectorImpl<ISD::InputArg> &Ins, const SDLoc &DL,
    SelectionDAG &DAG, SmallVectorImpl<SDValue> &InVals) const {
  if (Subtarget->is64Bit())
    return LowerFormalArguments_64(Chain, CallConv, IsVarArg, Ins,
                                   DL, DAG, InVals);
  return LowerFormalArguments_32(Chain, CallConv, IsVarArg, Ins,
                                 DL, DAG, InVals);
}

/// LowerFormalArguments32 - V8 uses a very simple ABI, where all values are
/// passed in either one or two GPRs, including FP values.  TODO: we should
/// pass FP values in FP registers for fastcc functions.
SDValue E2KTargetLowering::LowerFormalArguments_32(
    SDValue Chain, CallingConv::ID CallConv, bool isVarArg,
    const SmallVectorImpl<ISD::InputArg> &Ins, const SDLoc &dl,
    SelectionDAG &DAG, SmallVectorImpl<SDValue> &InVals) const {
  MachineFunction &MF = DAG.getMachineFunction();
  MachineRegisterInfo &RegInfo = MF.getRegInfo();
  E2KMachineFunctionInfo *FuncInfo = MF.getInfo<E2KMachineFunctionInfo>();

  // Assign locations to all of the incoming arguments.
  SmallVector<CCValAssign, 16> ArgLocs;
  CCState CCInfo(CallConv, isVarArg, DAG.getMachineFunction(), ArgLocs,
                 *DAG.getContext());
  CCInfo.AnalyzeFormalArguments(Ins, CC_E2K32);

  const unsigned StackOffset = 92;
  bool IsLittleEndian = DAG.getDataLayout().isLittleEndian();

  unsigned InIdx = 0;
  for (unsigned i = 0, e = ArgLocs.size(); i != e; ++i, ++InIdx) {
    CCValAssign &VA = ArgLocs[i];

    if (Ins[InIdx].Flags.isSRet()) {
      if (InIdx != 0)
        report_fatal_error("E2K only supports sret on the first parameter");
      // Get SRet from [%fp+64].
      int FrameIdx = MF.getFrameInfo().CreateFixedObject(4, 64, true);
      SDValue FIPtr = DAG.getFrameIndex(FrameIdx, MVT::i32);
      SDValue Arg =
          DAG.getLoad(MVT::i32, dl, Chain, FIPtr, MachinePointerInfo());
      InVals.push_back(Arg);
      continue;
    }

    if (VA.isRegLoc()) {
      if (VA.needsCustom()) {
        assert(VA.getLocVT() == MVT::f64 || VA.getLocVT() == MVT::v2i32);

        Register VRegHi = RegInfo.createVirtualRegister(&E2K::RegSRRegClass);
        MF.getRegInfo().addLiveIn(VA.getLocReg(), VRegHi);
        SDValue HiVal = DAG.getCopyFromReg(Chain, dl, VRegHi, MVT::i32);

        assert(i+1 < e);
        CCValAssign &NextVA = ArgLocs[++i];

        SDValue LoVal;
        if (NextVA.isMemLoc()) {
          int FrameIdx = MF.getFrameInfo().
            CreateFixedObject(4, StackOffset+NextVA.getLocMemOffset(),true);
          SDValue FIPtr = DAG.getFrameIndex(FrameIdx, MVT::i32);
          LoVal = DAG.getLoad(MVT::i32, dl, Chain, FIPtr, MachinePointerInfo());
        } else {
          Register loReg = MF.addLiveIn(NextVA.getLocReg(),
                                        &E2K::RegSRRegClass);
          LoVal = DAG.getCopyFromReg(Chain, dl, loReg, MVT::i32);
        }

        if (IsLittleEndian)
          std::swap(LoVal, HiVal);

        SDValue WholeValue =
          DAG.getNode(ISD::BUILD_PAIR, dl, MVT::i64, LoVal, HiVal);
        WholeValue = DAG.getNode(ISD::BITCAST, dl, VA.getLocVT(), WholeValue);
        InVals.push_back(WholeValue);
        continue;
      }
      Register VReg = RegInfo.createVirtualRegister(&E2K::RegSRRegClass);
      MF.getRegInfo().addLiveIn(VA.getLocReg(), VReg);
      SDValue Arg = DAG.getCopyFromReg(Chain, dl, VReg, MVT::i32);
      if (VA.getLocVT() == MVT::f32)
        Arg = DAG.getNode(ISD::BITCAST, dl, MVT::f32, Arg);
      else if (VA.getLocVT() != MVT::i32) {
        Arg = DAG.getNode(ISD::AssertSext, dl, MVT::i32, Arg,
                          DAG.getValueType(VA.getLocVT()));
        Arg = DAG.getNode(ISD::TRUNCATE, dl, VA.getLocVT(), Arg);
      }
      InVals.push_back(Arg);
      continue;
    }

    assert(VA.isMemLoc());

    unsigned Offset = VA.getLocMemOffset()+StackOffset;
    auto PtrVT = getPointerTy(DAG.getDataLayout());

    if (VA.needsCustom()) {
      assert(VA.getValVT() == MVT::f64 || VA.getValVT() == MVT::v2i32);
      // If it is double-word aligned, just load.
      if (Offset % 8 == 0) {
        int FI = MF.getFrameInfo().CreateFixedObject(8,
                                                     Offset,
                                                     true);
        SDValue FIPtr = DAG.getFrameIndex(FI, PtrVT);
        SDValue Load =
            DAG.getLoad(VA.getValVT(), dl, Chain, FIPtr, MachinePointerInfo());
        InVals.push_back(Load);
        continue;
      }

      int FI = MF.getFrameInfo().CreateFixedObject(4,
                                                   Offset,
                                                   true);
      SDValue FIPtr = DAG.getFrameIndex(FI, PtrVT);
      SDValue HiVal =
          DAG.getLoad(MVT::i32, dl, Chain, FIPtr, MachinePointerInfo());
      int FI2 = MF.getFrameInfo().CreateFixedObject(4,
                                                    Offset+4,
                                                    true);
      SDValue FIPtr2 = DAG.getFrameIndex(FI2, PtrVT);

      SDValue LoVal =
          DAG.getLoad(MVT::i32, dl, Chain, FIPtr2, MachinePointerInfo());

      if (IsLittleEndian)
        std::swap(LoVal, HiVal);

      SDValue WholeValue =
        DAG.getNode(ISD::BUILD_PAIR, dl, MVT::i64, LoVal, HiVal);
      WholeValue = DAG.getNode(ISD::BITCAST, dl, VA.getValVT(), WholeValue);
      InVals.push_back(WholeValue);
      continue;
    }

    int FI = MF.getFrameInfo().CreateFixedObject(4,
                                                 Offset,
                                                 true);
    SDValue FIPtr = DAG.getFrameIndex(FI, PtrVT);
    SDValue Load ;
    if (VA.getValVT() == MVT::i32 || VA.getValVT() == MVT::f32) {
      Load = DAG.getLoad(VA.getValVT(), dl, Chain, FIPtr, MachinePointerInfo());
    } else if (VA.getValVT() == MVT::f128) {
      report_fatal_error("E2Kv8 does not handle f128 in calls; "
                         "pass indirectly");
    } else {
      // We shouldn't see any other value types here.
      llvm_unreachable("Unexpected ValVT encountered in frame lowering.");
    }
    InVals.push_back(Load);
  }

  if (MF.getFunction().hasStructRetAttr()) {
    // Copy the SRet Argument to SRetReturnReg.
    E2KMachineFunctionInfo *SFI = MF.getInfo<E2KMachineFunctionInfo>();
    Register Reg = SFI->getSRetReturnReg();
    if (!Reg) {
      Reg = MF.getRegInfo().createVirtualRegister(&E2K::RegSRRegClass);
      SFI->setSRetReturnReg(Reg);
    }
    SDValue Copy = DAG.getCopyToReg(DAG.getEntryNode(), dl, Reg, InVals[0]);
    Chain = DAG.getNode(ISD::TokenFactor, dl, MVT::Other, Copy, Chain);
  }

  // Store remaining ArgRegs to the stack if this is a varargs function.
  if (isVarArg) {
    static const MCPhysReg ArgRegs[] = {
      E2K::R0, E2K::R1, E2K::R2, E2K::R3, E2K::R4, E2K::R5, E2K::R6, E2K::R7
    };
    unsigned NumAllocated = CCInfo.getFirstUnallocated(ArgRegs);
    const MCPhysReg *CurArgReg = ArgRegs+NumAllocated, *ArgRegEnd = ArgRegs+6;
    unsigned ArgOffset = CCInfo.getNextStackOffset();
    if (NumAllocated == 6)
      ArgOffset += StackOffset;
    else {
      assert(!ArgOffset);
      ArgOffset = 68+4*NumAllocated;
    }

    // Remember the vararg offset for the va_start implementation.
    FuncInfo->setVarArgsFrameOffset(ArgOffset);

    std::vector<SDValue> OutChains;

    for (; CurArgReg != ArgRegEnd; ++CurArgReg) {
      Register VReg = RegInfo.createVirtualRegister(&E2K::RegSRRegClass);
      MF.getRegInfo().addLiveIn(*CurArgReg, VReg);
      SDValue Arg = DAG.getCopyFromReg(DAG.getRoot(), dl, VReg, MVT::i32);

      int FrameIdx = MF.getFrameInfo().CreateFixedObject(4, ArgOffset,
                                                         true);
      SDValue FIPtr = DAG.getFrameIndex(FrameIdx, MVT::i32);

      OutChains.push_back(
          DAG.getStore(DAG.getRoot(), dl, Arg, FIPtr, MachinePointerInfo()));
      ArgOffset += 4;
    }

    if (!OutChains.empty()) {
      OutChains.push_back(Chain);
      Chain = DAG.getNode(ISD::TokenFactor, dl, MVT::Other, OutChains);
    }
  }

  return Chain;
}

// Lower formal arguments for the 64 bit ABI.
SDValue E2KTargetLowering::LowerFormalArguments_64(
    SDValue Chain, CallingConv::ID CallConv, bool IsVarArg,
    const SmallVectorImpl<ISD::InputArg> &Ins, const SDLoc &DL,
    SelectionDAG &DAG, SmallVectorImpl<SDValue> &InVals) const {
  MachineFunction &MF = DAG.getMachineFunction();

  // Analyze arguments according to CC_E2K64.
  SmallVector<CCValAssign, 16> ArgLocs;
  CCState CCInfo(CallConv, IsVarArg, DAG.getMachineFunction(), ArgLocs,
                 *DAG.getContext());
  CCInfo.AnalyzeFormalArguments(Ins, CC_E2K64);

  // The argument array begins at %fp+BIAS+128, after the register save area.
  const unsigned ArgArea = 128;

  for (unsigned i = 0, e = ArgLocs.size(); i != e; ++i) {
    CCValAssign &VA = ArgLocs[i];
    if (VA.isRegLoc()) {
      // This argument is passed in a register.
      // All integer register arguments are promoted by the caller to i64.

      // Create a virtual register for the promoted live-in value.
      Register VReg = MF.addLiveIn(VA.getLocReg(),
                                   getRegClassFor(VA.getLocVT()));
      SDValue Arg = DAG.getCopyFromReg(Chain, DL, VReg, VA.getLocVT());

      // Get the high bits for i32 struct elements.
      if (VA.getValVT() == MVT::i32 && VA.needsCustom())
        Arg = DAG.getNode(ISD::SRL, DL, VA.getLocVT(), Arg,
                          DAG.getConstant(32, DL, MVT::i32));

      // The caller promoted the argument, so insert an Assert?ext SDNode so we
      // won't promote the value again in this function.
      switch (VA.getLocInfo()) {
      case CCValAssign::SExt:
        Arg = DAG.getNode(ISD::AssertSext, DL, VA.getLocVT(), Arg,
                          DAG.getValueType(VA.getValVT()));
        break;
      case CCValAssign::ZExt:
        Arg = DAG.getNode(ISD::AssertZext, DL, VA.getLocVT(), Arg,
                          DAG.getValueType(VA.getValVT()));
        break;
      default:
        break;
      }

      // Truncate the register down to the argument type.
      if (VA.isExtInLoc())
        Arg = DAG.getNode(ISD::TRUNCATE, DL, VA.getValVT(), Arg);

      InVals.push_back(Arg);
      continue;
    }

    // The registers are exhausted. This argument was passed on the stack.
    assert(VA.isMemLoc());
    // The CC_E2K64_Full/Half functions compute stack offsets relative to the
    // beginning of the arguments area at %fp+BIAS+128.
    unsigned Offset = VA.getLocMemOffset() + ArgArea;
    unsigned ValSize = VA.getValVT().getSizeInBits() / 8;
    // Adjust offset for extended arguments, E2K is big-endian.
    // The caller will have written the full slot with extended bytes, but we
    // prefer our own extending loads.
    if (VA.isExtInLoc())
      Offset += 8 - ValSize;
    int FI = MF.getFrameInfo().CreateFixedObject(ValSize, Offset, true);
    InVals.push_back(
        DAG.getLoad(VA.getValVT(), DL, Chain,
                    DAG.getFrameIndex(FI, getPointerTy(MF.getDataLayout())),
                    MachinePointerInfo::getFixedStack(MF, FI)));
  }

  if (!IsVarArg)
    return Chain;

  // This function takes variable arguments, some of which may have been passed
  // in registers %i0-%i5. Variable floating point arguments are never passed
  // in floating point registers. They go on %i0-%i5 or on the stack like
  // integer arguments.
  //
  // The va_start intrinsic needs to know the offset to the first variable
  // argument.
  unsigned ArgOffset = CCInfo.getNextStackOffset();
  E2KMachineFunctionInfo *FuncInfo = MF.getInfo<E2KMachineFunctionInfo>();
  // Skip the 128 bytes of register save area.
  FuncInfo->setVarArgsFrameOffset(ArgOffset + ArgArea +
                                  Subtarget->getStackPointerBias());

  // Save the variable arguments that were passed in registers.
  // The caller is required to reserve stack space for 6 arguments regardless
  // of how many arguments were actually passed.
  SmallVector<SDValue, 8> OutChains;
  for (; ArgOffset < 6*8; ArgOffset += 8) {
    Register VReg = MF.addLiveIn(E2K::R0 + ArgOffset/8, &E2K::RegDRRegClass);
    SDValue VArg = DAG.getCopyFromReg(Chain, DL, VReg, MVT::i64);
    int FI = MF.getFrameInfo().CreateFixedObject(8, ArgOffset + ArgArea, true);
    auto PtrVT = getPointerTy(MF.getDataLayout());
    OutChains.push_back(
        DAG.getStore(Chain, DL, VArg, DAG.getFrameIndex(FI, PtrVT),
                     MachinePointerInfo::getFixedStack(MF, FI)));
  }

  if (!OutChains.empty())
    Chain = DAG.getNode(ISD::TokenFactor, DL, MVT::Other, OutChains);

  return Chain;
}

SDValue
E2KTargetLowering::LowerCall(TargetLowering::CallLoweringInfo &CLI,
                               SmallVectorImpl<SDValue> &InVals) const {
  SDValue Chain                         = CLI.Chain;

  return Chain;
}

/// IsEligibleForTailCallOptimization - Check whether the call is eligible
/// for tail call optimization.
bool E2KTargetLowering::IsEligibleForTailCallOptimization(
    CCState &CCInfo, CallLoweringInfo &CLI, MachineFunction &MF) const {

  auto &Outs = CLI.Outs;
  auto &Caller = MF.getFunction();

  // Do not tail call opt functions with "disable-tail-calls" attribute.
  if (Caller.getFnAttribute("disable-tail-calls").getValueAsString() == "true")
    return false;

  // Do not tail call opt if the stack is used to pass parameters.
  if (CCInfo.getNextStackOffset() != 0)
    return false;

  // Do not tail call opt if either the callee or caller returns
  // a struct and the other does not.
  if (!Outs.empty() && Caller.hasStructRetAttr() != Outs[0].Flags.isSRet())
    return false;

  // Byval parameters hand the function a pointer directly into the stack area
  // we want to reuse during a tail call.
  for (auto &Arg : Outs)
    if (Arg.Flags.isByVal())
      return false;

  return true;
}

// FIXME? Maybe this could be a TableGen attribute on some registers and
// this table could be generated automatically from RegInfo.
Register E2KTargetLowering::getRegisterByName(const char* RegName, LLT VT,
                                                const MachineFunction &MF) const {
  Register Reg = StringSwitch<Register>(RegName)
    .Default(0);

  if (Reg)
    return Reg;

  report_fatal_error("Invalid register name global variable");
}

//===----------------------------------------------------------------------===//
// TargetLowering Implementation
//===----------------------------------------------------------------------===//

TargetLowering::AtomicExpansionKind E2KTargetLowering::shouldExpandAtomicRMWInIR(AtomicRMWInst *AI) const {
  if (AI->getOperation() == AtomicRMWInst::Xchg &&
      AI->getType()->getPrimitiveSizeInBits() == 32)
    return AtomicExpansionKind::None; // Uses xchg instruction

  return AtomicExpansionKind::CmpXChg;
}

E2KTargetLowering::E2KTargetLowering(const TargetMachine &TM,
                                         const E2KSubtarget &STI)
    : TargetLowering(TM), Subtarget(&STI) {
  MVT PtrVT = MVT::getIntegerVT(TM.getPointerSizeInBits(0));

  // Instructions which use registers as conditionals examine all the
  // bits (as does the pseudo SELECT_CC expansion). I don't think it
  // matters much whether it's ZeroOrOneBooleanContent, or
  // ZeroOrNegativeOneBooleanContent, so, arbitrarily choose the
  // former.
  setBooleanContents(ZeroOrOneBooleanContent);
  setBooleanVectorContents(ZeroOrOneBooleanContent);

  // Set up the register classes.
  addRegisterClass(MVT::i32, &E2K::RegSRRegClass);

  addRegisterClass(MVT::f32, &E2K::RegSRRegClass);
  addRegisterClass(MVT::f64, &E2K::RegDRRegClass);
  addRegisterClass(MVT::f128, &E2K::RegQRRegClass);

  addRegisterClass(MVT::i64, &E2K::RegDRRegClass);

  {

    // ...but almost all operations must be expanded, so set that as
    // the default.
    for (unsigned Op = 0; Op < ISD::BUILTIN_OP_END; ++Op) {
      setOperationAction(Op, MVT::v2i32, Expand);
    }
    // Truncating/extending stores/loads are also not supported.
    for (MVT VT : MVT::integer_fixedlen_vector_valuetypes()) {
      setLoadExtAction(ISD::SEXTLOAD, VT, MVT::v2i32, Expand);
      setLoadExtAction(ISD::ZEXTLOAD, VT, MVT::v2i32, Expand);
      setLoadExtAction(ISD::EXTLOAD, VT, MVT::v2i32, Expand);

      setLoadExtAction(ISD::SEXTLOAD, MVT::v2i32, VT, Expand);
      setLoadExtAction(ISD::ZEXTLOAD, MVT::v2i32, VT, Expand);
      setLoadExtAction(ISD::EXTLOAD, MVT::v2i32, VT, Expand);

      setTruncStoreAction(VT, MVT::v2i32, Expand);
      setTruncStoreAction(MVT::v2i32, VT, Expand);
    }
    // However, load and store *are* legal.
    setOperationAction(ISD::LOAD, MVT::v2i32, Legal);
    setOperationAction(ISD::STORE, MVT::v2i32, Legal);
    setOperationAction(ISD::EXTRACT_VECTOR_ELT, MVT::v2i32, Legal);
    setOperationAction(ISD::BUILD_VECTOR, MVT::v2i32, Legal);

    // And we need to promote i64 loads/stores into vector load/store
    setOperationAction(ISD::LOAD, MVT::i64, Custom);
    setOperationAction(ISD::STORE, MVT::i64, Custom);

    // Sadly, this doesn't work:
    //    AddPromotedToType(ISD::LOAD, MVT::i64, MVT::v2i32);
    //    AddPromotedToType(ISD::STORE, MVT::i64, MVT::v2i32);
  }

  // Turn FP extload into load/fpextend
  for (MVT VT : MVT::fp_valuetypes()) {
    setLoadExtAction(ISD::EXTLOAD, VT, MVT::f16, Expand);
    setLoadExtAction(ISD::EXTLOAD, VT, MVT::f32, Expand);
    setLoadExtAction(ISD::EXTLOAD, VT, MVT::f64, Expand);
  }

  // E2K doesn't have i1 sign extending load
  for (MVT VT : MVT::integer_valuetypes())
    setLoadExtAction(ISD::SEXTLOAD, VT, MVT::i1, Promote);

  // Turn FP truncstore into trunc + store.
  setTruncStoreAction(MVT::f32, MVT::f16, Expand);
  setTruncStoreAction(MVT::f64, MVT::f16, Expand);
  setTruncStoreAction(MVT::f64, MVT::f32, Expand);
  setTruncStoreAction(MVT::f128, MVT::f16, Expand);
  setTruncStoreAction(MVT::f128, MVT::f32, Expand);
  setTruncStoreAction(MVT::f128, MVT::f64, Expand);

  // Custom legalize GlobalAddress nodes into LO/HI parts.
  setOperationAction(ISD::GlobalAddress, PtrVT, Custom);
  setOperationAction(ISD::GlobalTLSAddress, PtrVT, Custom);
  setOperationAction(ISD::ConstantPool, PtrVT, Custom);
  setOperationAction(ISD::BlockAddress, PtrVT, Custom);

  // E2K doesn't have sext_inreg, replace them with shl/sra
  setOperationAction(ISD::SIGN_EXTEND_INREG, MVT::i16, Expand);
  setOperationAction(ISD::SIGN_EXTEND_INREG, MVT::i8 , Expand);
  setOperationAction(ISD::SIGN_EXTEND_INREG, MVT::i1 , Expand);

  // E2K has no REM or DIVREM operations.
  setOperationAction(ISD::UREM, MVT::i32, Expand);
  setOperationAction(ISD::SREM, MVT::i32, Expand);
  setOperationAction(ISD::SDIVREM, MVT::i32, Expand);
  setOperationAction(ISD::UDIVREM, MVT::i32, Expand);

  // ... nor does E2KV9.
  if (Subtarget->is64Bit()) {
    setOperationAction(ISD::UREM, MVT::i64, Expand);
    setOperationAction(ISD::SREM, MVT::i64, Expand);
    setOperationAction(ISD::SDIVREM, MVT::i64, Expand);
    setOperationAction(ISD::UDIVREM, MVT::i64, Expand);
  }

  // Custom expand fp<->sint
  setOperationAction(ISD::FP_TO_SINT, MVT::i32, Custom);
  setOperationAction(ISD::SINT_TO_FP, MVT::i32, Custom);
  setOperationAction(ISD::FP_TO_SINT, MVT::i64, Custom);
  setOperationAction(ISD::SINT_TO_FP, MVT::i64, Custom);

  // Custom Expand fp<->uint
  setOperationAction(ISD::FP_TO_UINT, MVT::i32, Custom);
  setOperationAction(ISD::UINT_TO_FP, MVT::i32, Custom);
  setOperationAction(ISD::FP_TO_UINT, MVT::i64, Custom);
  setOperationAction(ISD::UINT_TO_FP, MVT::i64, Custom);

  // Lower f16 conversion operations into library calls
  setOperationAction(ISD::FP16_TO_FP, MVT::f32, Expand);
  setOperationAction(ISD::FP_TO_FP16, MVT::f32, Expand);
  setOperationAction(ISD::FP16_TO_FP, MVT::f64, Expand);
  setOperationAction(ISD::FP_TO_FP16, MVT::f64, Expand);
  setOperationAction(ISD::FP16_TO_FP, MVT::f128, Expand);
  setOperationAction(ISD::FP_TO_FP16, MVT::f128, Expand);

  setOperationAction(ISD::BITCAST, MVT::f32, Expand);
  setOperationAction(ISD::BITCAST, MVT::i32, Expand);

  // E2K has no select or setcc: expand to SELECT_CC.
  setOperationAction(ISD::SELECT, MVT::i32, Expand);
  setOperationAction(ISD::SELECT, MVT::f32, Expand);
  setOperationAction(ISD::SELECT, MVT::f64, Expand);
  setOperationAction(ISD::SELECT, MVT::f128, Expand);

  setOperationAction(ISD::SETCC, MVT::i32, Expand);
  setOperationAction(ISD::SETCC, MVT::f32, Expand);
  setOperationAction(ISD::SETCC, MVT::f64, Expand);
  setOperationAction(ISD::SETCC, MVT::f128, Expand);

  // E2K doesn't have BRCOND either, it has BR_CC.
  setOperationAction(ISD::BRCOND, MVT::Other, Expand);
  setOperationAction(ISD::BRIND, MVT::Other, Expand);
  setOperationAction(ISD::BR_JT, MVT::Other, Expand);
  setOperationAction(ISD::BR_CC, MVT::i32, Custom);
  setOperationAction(ISD::BR_CC, MVT::f32, Custom);
  setOperationAction(ISD::BR_CC, MVT::f64, Custom);
  setOperationAction(ISD::BR_CC, MVT::f128, Custom);

  setOperationAction(ISD::SELECT_CC, MVT::i32, Custom);
  setOperationAction(ISD::SELECT_CC, MVT::f32, Custom);
  setOperationAction(ISD::SELECT_CC, MVT::f64, Custom);
  setOperationAction(ISD::SELECT_CC, MVT::f128, Custom);

  setOperationAction(ISD::ADDC, MVT::i32, Custom);
  setOperationAction(ISD::ADDE, MVT::i32, Custom);
  setOperationAction(ISD::SUBC, MVT::i32, Custom);
  setOperationAction(ISD::SUBE, MVT::i32, Custom);

  if (Subtarget->is64Bit()) {
    setOperationAction(ISD::ADDC, MVT::i64, Custom);
    setOperationAction(ISD::ADDE, MVT::i64, Custom);
    setOperationAction(ISD::SUBC, MVT::i64, Custom);
    setOperationAction(ISD::SUBE, MVT::i64, Custom);
    setOperationAction(ISD::BITCAST, MVT::f64, Expand);
    setOperationAction(ISD::BITCAST, MVT::i64, Expand);
    setOperationAction(ISD::SELECT, MVT::i64, Expand);
    setOperationAction(ISD::SETCC, MVT::i64, Expand);
    setOperationAction(ISD::BR_CC, MVT::i64, Custom);
    setOperationAction(ISD::SELECT_CC, MVT::i64, Custom);

    setOperationAction(ISD::CTPOP, MVT::i64, Expand);
    setOperationAction(ISD::CTTZ , MVT::i64, Expand);
    setOperationAction(ISD::CTLZ , MVT::i64, Expand);
    setOperationAction(ISD::BSWAP, MVT::i64, Expand);
    setOperationAction(ISD::ROTL , MVT::i64, Expand);
    setOperationAction(ISD::ROTR , MVT::i64, Expand);
    setOperationAction(ISD::DYNAMIC_STACKALLOC, MVT::i64, Custom);
  }

  // ATOMICs.
  // Atomics are supported on E2KV9. 32-bit atomics are also
  // supported by some E2KV8 variants. Otherwise, atomics
  // are unsupported.
  if (Subtarget->is64Bit())
    setMaxAtomicSizeInBitsSupported(64);
  else
    setMaxAtomicSizeInBitsSupported(0);

  setMinCmpXchgSizeInBits(32);

  setOperationAction(ISD::ATOMIC_SWAP, MVT::i32, Legal);

  setOperationAction(ISD::ATOMIC_FENCE, MVT::Other, Legal);

  // Custom Lower Atomic LOAD/STORE
  setOperationAction(ISD::ATOMIC_LOAD, MVT::i32, Custom);
  setOperationAction(ISD::ATOMIC_STORE, MVT::i32, Custom);

  if (Subtarget->is64Bit()) {
    setOperationAction(ISD::ATOMIC_CMP_SWAP, MVT::i64, Legal);
    setOperationAction(ISD::ATOMIC_SWAP, MVT::i64, Legal);
    setOperationAction(ISD::ATOMIC_LOAD, MVT::i64, Custom);
    setOperationAction(ISD::ATOMIC_STORE, MVT::i64, Custom);
  }

  if (!Subtarget->is64Bit()) {
    // These libcalls are not available in 32-bit.
    setLibcallName(RTLIB::MULO_I64, nullptr);
    setLibcallName(RTLIB::SHL_I128, nullptr);
    setLibcallName(RTLIB::SRL_I128, nullptr);
    setLibcallName(RTLIB::SRA_I128, nullptr);
  }

  setLibcallName(RTLIB::MULO_I128, nullptr);

  if (Subtarget->is64Bit()) {
    // E2KV8 does not have FNEGD and FABSD.
    setOperationAction(ISD::FNEG, MVT::f64, Custom);
    setOperationAction(ISD::FABS, MVT::f64, Custom);
  }

  setOperationAction(ISD::FSIN , MVT::f128, Expand);
  setOperationAction(ISD::FCOS , MVT::f128, Expand);
  setOperationAction(ISD::FSINCOS, MVT::f128, Expand);
  setOperationAction(ISD::FREM , MVT::f128, Expand);
  setOperationAction(ISD::FMA  , MVT::f128, Expand);
  setOperationAction(ISD::FSIN , MVT::f64, Expand);
  setOperationAction(ISD::FCOS , MVT::f64, Expand);
  setOperationAction(ISD::FSINCOS, MVT::f64, Expand);
  setOperationAction(ISD::FREM , MVT::f64, Expand);
  setOperationAction(ISD::FMA  , MVT::f64, Expand);
  setOperationAction(ISD::FSIN , MVT::f32, Expand);
  setOperationAction(ISD::FCOS , MVT::f32, Expand);
  setOperationAction(ISD::FSINCOS, MVT::f32, Expand);
  setOperationAction(ISD::FREM , MVT::f32, Expand);
  setOperationAction(ISD::FMA  , MVT::f32, Expand);
  setOperationAction(ISD::CTTZ , MVT::i32, Expand);
  setOperationAction(ISD::CTLZ , MVT::i32, Expand);
  setOperationAction(ISD::ROTL , MVT::i32, Expand);
  setOperationAction(ISD::ROTR , MVT::i32, Expand);
  setOperationAction(ISD::BSWAP, MVT::i32, Expand);
  setOperationAction(ISD::FCOPYSIGN, MVT::f128, Expand);
  setOperationAction(ISD::FCOPYSIGN, MVT::f64, Expand);
  setOperationAction(ISD::FCOPYSIGN, MVT::f32, Expand);
  setOperationAction(ISD::FPOW , MVT::f128, Expand);
  setOperationAction(ISD::FPOW , MVT::f64, Expand);
  setOperationAction(ISD::FPOW , MVT::f32, Expand);

  setOperationAction(ISD::SHL_PARTS, MVT::i32, Expand);
  setOperationAction(ISD::SRA_PARTS, MVT::i32, Expand);
  setOperationAction(ISD::SRL_PARTS, MVT::i32, Expand);

  // Expands to [SU]MUL_LOHI.
  setOperationAction(ISD::MULHU,     MVT::i32, Expand);
  setOperationAction(ISD::MULHS,     MVT::i32, Expand);
  setOperationAction(ISD::MUL,       MVT::i32, Expand);

  if (Subtarget->is64Bit()) {
    setOperationAction(ISD::UMUL_LOHI, MVT::i64, Expand);
    setOperationAction(ISD::SMUL_LOHI, MVT::i64, Expand);
    setOperationAction(ISD::MULHU,     MVT::i64, Expand);
    setOperationAction(ISD::MULHS,     MVT::i64, Expand);

    setOperationAction(ISD::UMULO,     MVT::i64, Custom);
    setOperationAction(ISD::SMULO,     MVT::i64, Custom);

    setOperationAction(ISD::SHL_PARTS, MVT::i64, Expand);
    setOperationAction(ISD::SRA_PARTS, MVT::i64, Expand);
    setOperationAction(ISD::SRL_PARTS, MVT::i64, Expand);
  }

  // VASTART needs to be custom lowered to use the VarArgsFrameIndex.
  setOperationAction(ISD::VASTART           , MVT::Other, Custom);
  // VAARG needs to be lowered to not do unaligned accesses for doubles.
  setOperationAction(ISD::VAARG             , MVT::Other, Custom);

  setOperationAction(ISD::TRAP              , MVT::Other, Legal);
  setOperationAction(ISD::DEBUGTRAP         , MVT::Other, Legal);

  // Use the default implementation.
  setOperationAction(ISD::VACOPY            , MVT::Other, Expand);
  setOperationAction(ISD::VAEND             , MVT::Other, Expand);
  setOperationAction(ISD::STACKSAVE         , MVT::Other, Expand);
  setOperationAction(ISD::STACKRESTORE      , MVT::Other, Expand);
  setOperationAction(ISD::DYNAMIC_STACKALLOC, MVT::i32  , Custom);

  setStackPointerRegisterToSaveRestore(E2K::R6);

  setOperationAction(ISD::CTPOP, MVT::i32, Expand);

  setOperationAction(ISD::LOAD, MVT::f128, Custom);
  setOperationAction(ISD::STORE, MVT::f128, Custom);


    // Custom legalize f128 operations.

  setOperationAction(ISD::FADD,  MVT::f128, Custom);
  setOperationAction(ISD::FSUB,  MVT::f128, Custom);
  setOperationAction(ISD::FMUL,  MVT::f128, Custom);
  setOperationAction(ISD::FDIV,  MVT::f128, Custom);
  setOperationAction(ISD::FSQRT, MVT::f128, Custom);
  setOperationAction(ISD::FNEG,  MVT::f128, Custom);
  setOperationAction(ISD::FABS,  MVT::f128, Custom);

  setOperationAction(ISD::FP_EXTEND, MVT::f128, Custom);
  setOperationAction(ISD::FP_ROUND,  MVT::f64, Custom);
  setOperationAction(ISD::FP_ROUND,  MVT::f32, Custom);

    // Setup Runtime library names.
    setLibcallName(RTLIB::ADD_F128,  "_Q_add");
    setLibcallName(RTLIB::SUB_F128,  "_Q_sub");
    setLibcallName(RTLIB::MUL_F128,  "_Q_mul");
    setLibcallName(RTLIB::DIV_F128,  "_Q_div");
    setLibcallName(RTLIB::SQRT_F128, "_Q_sqrt");
    setLibcallName(RTLIB::FPTOSINT_F128_I32, "_Q_qtoi");
    setLibcallName(RTLIB::FPTOUINT_F128_I32, "_Q_qtou");
    setLibcallName(RTLIB::SINTTOFP_I32_F128, "_Q_itoq");
    setLibcallName(RTLIB::UINTTOFP_I32_F128, "_Q_utoq");
    setLibcallName(RTLIB::FPTOSINT_F128_I64, "_Q_qtoll");
    setLibcallName(RTLIB::FPTOUINT_F128_I64, "_Q_qtoull");
    setLibcallName(RTLIB::SINTTOFP_I64_F128, "_Q_lltoq");
    setLibcallName(RTLIB::UINTTOFP_I64_F128, "_Q_ulltoq");
    setLibcallName(RTLIB::FPEXT_F32_F128, "_Q_stoq");
    setLibcallName(RTLIB::FPEXT_F64_F128, "_Q_dtoq");
    setLibcallName(RTLIB::FPROUND_F128_F32, "_Q_qtos");
    setLibcallName(RTLIB::FPROUND_F128_F64, "_Q_qtod");

  // Custom combine bitcast between f64 and v2i32
  if (!Subtarget->is64Bit())
    setTargetDAGCombine(ISD::BITCAST);

  setOperationAction(ISD::INTRINSIC_WO_CHAIN, MVT::Other, Custom);

  setMinFunctionAlignment(Align(4));

  computeRegisterProperties(Subtarget->getRegisterInfo());
}

#define NODE_NAME_CASE(node) case E2KISD::node: return #node;

const char *E2KTargetLowering::getTargetNodeName(unsigned Opcode) const {
  switch ((E2KISD::NodeType)Opcode) {
  case E2KISD::FIRST_NUMBER:    break;
  NODE_NAME_CASE(RET_FLAG);
  }
  return nullptr;
}

EVT E2KTargetLowering::getSetCCResultType(const DataLayout &, LLVMContext &,
                                            EVT VT) const {
  if (!VT.isVector())
    return MVT::i32;
  return VT.changeVectorElementTypeToInteger();
}

/// isMaskedValueZeroForTargetNode - Return true if 'Op & Mask' is known to
/// be zero. Op is expected to be a target specific node. Used by DAG
/// combiner.
void E2KTargetLowering::computeKnownBitsForTargetNode
                                (const SDValue Op,
                                 KnownBits &Known,
                                 const APInt &DemandedElts,
                                 const SelectionDAG &DAG,
                                 unsigned Depth) const {
  KnownBits Known2;
  Known.resetAll();

  switch (Op.getOpcode()) {
  default: break;
  }
}

// Build SDNodes for producing an address from a GlobalAddress, ConstantPool,
// or ExternalSymbol SDNode.
SDValue E2KTargetLowering::makeAddress(SDValue Op, SelectionDAG &DAG) const {
  SDLoc DL(Op);
  EVT VT = getPointerTy(DAG.getDataLayout());

  // This is one of the absolute code models.
  switch(getTargetMachine().getCodeModel()) {
  default:
    llvm_unreachable("Unsupported absolute code model");
  }
}

SDValue E2KTargetLowering::LowerGlobalAddress(SDValue Op,
                                                SelectionDAG &DAG) const {
  return makeAddress(Op, DAG);
}

SDValue E2KTargetLowering::LowerConstantPool(SDValue Op,
                                               SelectionDAG &DAG) const {
  return makeAddress(Op, DAG);
}

SDValue E2KTargetLowering::LowerBlockAddress(SDValue Op,
                                               SelectionDAG &DAG) const {
  return makeAddress(Op, DAG);
}

SDValue E2KTargetLowering::LowerGlobalTLSAddress(SDValue Op,
                                                   SelectionDAG &DAG) const {

  llvm_unreachable("unimplemented");
  return SDValue();
}

SDValue
E2KTargetLowering::LowerF128Op(SDValue Op, SelectionDAG &DAG,
                                 const char *LibFuncName,
                                 unsigned numArgs) const {

  llvm_unreachable("unimplemented");
  return SDValue();
}

static SDValue
LowerF128_FPEXTEND(SDValue Op, SelectionDAG &DAG,
                   const E2KTargetLowering &TLI) {


  llvm_unreachable("unimplemented");
  return SDValue();
}

static SDValue
LowerF128_FPROUND(SDValue Op, SelectionDAG &DAG,
                  const E2KTargetLowering &TLI) {
  llvm_unreachable("unimplemented");
  return SDValue();
}

static SDValue LowerFP_TO_SINT(SDValue Op, SelectionDAG &DAG,
                               const E2KTargetLowering &TLI) {
  llvm_unreachable("unimplemented");
  return SDValue();
}

static SDValue LowerSINT_TO_FP(SDValue Op, SelectionDAG &DAG,
                               const E2KTargetLowering &TLI) {
  llvm_unreachable("unimplemented");
  return SDValue();
}

static SDValue LowerFP_TO_UINT(SDValue Op, SelectionDAG &DAG,
                               const E2KTargetLowering &TLI) {
  llvm_unreachable("unimplemented");
  return SDValue();
}

static SDValue LowerUINT_TO_FP(SDValue Op, SelectionDAG &DAG,
                               const E2KTargetLowering &TLI) {
  llvm_unreachable("unimplemented");
  return SDValue();
}

static SDValue LowerBR_CC(SDValue Op, SelectionDAG &DAG,
                          const E2KTargetLowering &TLI) {
  llvm_unreachable("unimplemented");
  return SDValue();
}

static SDValue LowerSELECT_CC(SDValue Op, SelectionDAG &DAG,
                              const E2KTargetLowering &TLI) {
  llvm_unreachable("unimplemented");
  return SDValue();
}

static SDValue LowerVASTART(SDValue Op, SelectionDAG &DAG,
                            const E2KTargetLowering &TLI) {
  llvm_unreachable("unimplemented");
  return SDValue();
}

static SDValue LowerVAARG(SDValue Op, SelectionDAG &DAG) {
  llvm_unreachable("unimplemented");
  return SDValue();
}

static SDValue LowerDYNAMIC_STACKALLOC(SDValue Op, SelectionDAG &DAG,
                                       const E2KSubtarget *Subtarget) {
  llvm_unreachable("unimplemented");
  return SDValue();
}


static SDValue getFLUSHW(SDValue Op, SelectionDAG &DAG) {
  llvm_unreachable("unimplemented");
  return SDValue();
}

static SDValue getFRAMEADDR(uint64_t depth, SDValue Op, SelectionDAG &DAG,
                            const E2KSubtarget *Subtarget,
                            bool AlwaysFlush = false) {
  llvm_unreachable("unimplemented");
  return SDValue();
}


static SDValue LowerFRAMEADDR(SDValue Op, SelectionDAG &DAG,
                              const E2KSubtarget *Subtarget) {

  llvm_unreachable("unimplemented");
  return SDValue();
}

static SDValue LowerRETURNADDR(SDValue Op, SelectionDAG &DAG,
                               const E2KTargetLowering &TLI,
                               const E2KSubtarget *Subtarget) {
  llvm_unreachable("unimplemented");
  return SDValue();
}

// Lower a f128 load into two f64 loads.
static SDValue LowerF128Load(SDValue Op, SelectionDAG &DAG)
{
  llvm_unreachable("unimplemented");
  return SDValue();
}

static SDValue LowerLOAD(SDValue Op, SelectionDAG &DAG)
{
  llvm_unreachable("unimplemented");
  return SDValue();
}

// Lower a f128 store into two f64 stores.
static SDValue LowerF128Store(SDValue Op, SelectionDAG &DAG) {
  llvm_unreachable("unimplemented");
  return SDValue();
}

static SDValue LowerSTORE(SDValue Op, SelectionDAG &DAG)
{
  llvm_unreachable("unimplemented");
  return SDValue();
}

static SDValue LowerFNEGorFABS(SDValue Op, SelectionDAG &DAG) {
  llvm_unreachable("unimplemented");
  return SDValue();
}

static SDValue LowerADDC_ADDE_SUBC_SUBE(SDValue Op, SelectionDAG &DAG) {

  llvm_unreachable("unimplemented");
  return SDValue();
}

// Custom lower UMULO/SMULO for E2K. This code is similar to ExpandNode()
// in LegalizeDAG.cpp except the order of arguments to the library function.
static SDValue LowerUMULO_SMULO(SDValue Op, SelectionDAG &DAG,
                                const E2KTargetLowering &TLI)
{
  llvm_unreachable("unimplemented");
  return SDValue();
}

static SDValue LowerATOMIC_LOAD_STORE(SDValue Op, SelectionDAG &DAG) {

  llvm_unreachable("unimplemented");
  return SDValue();
}

SDValue E2KTargetLowering::LowerINTRINSIC_WO_CHAIN(SDValue Op,
                                                     SelectionDAG &DAG) const {
  llvm_unreachable("unimplemented");
  return SDValue();
}

SDValue E2KTargetLowering::
LowerOperation(SDValue Op, SelectionDAG &DAG) const {


  switch (Op.getOpcode()) {
  default: llvm_unreachable("Should not custom lower this!");

  case ISD::RETURNADDR:         return LowerRETURNADDR(Op, DAG, *this,
                                                       Subtarget);
  case ISD::FRAMEADDR:          return LowerFRAMEADDR(Op, DAG,
                                                      Subtarget);
  case ISD::GlobalTLSAddress:   return LowerGlobalTLSAddress(Op, DAG);
  case ISD::GlobalAddress:      return LowerGlobalAddress(Op, DAG);
  case ISD::BlockAddress:       return LowerBlockAddress(Op, DAG);
  case ISD::ConstantPool:       return LowerConstantPool(Op, DAG);
  case ISD::FP_TO_SINT:         return LowerFP_TO_SINT(Op, DAG, *this);
  case ISD::SINT_TO_FP:         return LowerSINT_TO_FP(Op, DAG, *this);
  case ISD::FP_TO_UINT:         return LowerFP_TO_UINT(Op, DAG, *this);
  case ISD::UINT_TO_FP:         return LowerUINT_TO_FP(Op, DAG, *this);
  case ISD::BR_CC:
    return LowerBR_CC(Op, DAG, *this);
  case ISD::SELECT_CC:
    return LowerSELECT_CC(Op, DAG, *this);
  case ISD::VASTART:            return LowerVASTART(Op, DAG, *this);
  case ISD::VAARG:              return LowerVAARG(Op, DAG);
  case ISD::DYNAMIC_STACKALLOC: return LowerDYNAMIC_STACKALLOC(Op, DAG,
                                                               Subtarget);

  case ISD::LOAD:               return LowerLOAD(Op, DAG);
  case ISD::STORE:              return LowerSTORE(Op, DAG);
  case ISD::FADD:               return LowerF128Op(Op, DAG,
                                       getLibcallName(RTLIB::ADD_F128), 2);
  case ISD::FSUB:               return LowerF128Op(Op, DAG,
                                       getLibcallName(RTLIB::SUB_F128), 2);
  case ISD::FMUL:               return LowerF128Op(Op, DAG,
                                       getLibcallName(RTLIB::MUL_F128), 2);
  case ISD::FDIV:               return LowerF128Op(Op, DAG,
                                       getLibcallName(RTLIB::DIV_F128), 2);
  case ISD::FSQRT:              return LowerF128Op(Op, DAG,
                                       getLibcallName(RTLIB::SQRT_F128),1);
  case ISD::FABS:
  case ISD::FNEG:               return LowerFNEGorFABS(Op, DAG);
  case ISD::FP_EXTEND:          return LowerF128_FPEXTEND(Op, DAG, *this);
  case ISD::FP_ROUND:           return LowerF128_FPROUND(Op, DAG, *this);
  case ISD::ADDC:
  case ISD::ADDE:
  case ISD::SUBC:
  case ISD::SUBE:               return LowerADDC_ADDE_SUBC_SUBE(Op, DAG);
  case ISD::UMULO:
  case ISD::SMULO:              return LowerUMULO_SMULO(Op, DAG, *this);
  case ISD::ATOMIC_LOAD:
  case ISD::ATOMIC_STORE:       return LowerATOMIC_LOAD_STORE(Op, DAG);
  case ISD::INTRINSIC_WO_CHAIN: return LowerINTRINSIC_WO_CHAIN(Op, DAG);
  }
}

SDValue E2KTargetLowering::bitcastConstantFPToInt(ConstantFPSDNode *C,
                                                    const SDLoc &DL,
                                                    SelectionDAG &DAG) const {
  APInt V = C->getValueAPF().bitcastToAPInt();
  SDValue Lo = DAG.getConstant(V.zextOrTrunc(32), DL, MVT::i32);
  SDValue Hi = DAG.getConstant(V.lshr(32).zextOrTrunc(32), DL, MVT::i32);
  if (DAG.getDataLayout().isLittleEndian())
    std::swap(Lo, Hi);
  return DAG.getBuildVector(MVT::v2i32, DL, {Hi, Lo});
}

SDValue E2KTargetLowering::PerformBITCASTCombine(SDNode *N,
                                                   DAGCombinerInfo &DCI) const {
  SDLoc dl(N);
  SDValue Src = N->getOperand(0);

  if (isa<ConstantFPSDNode>(Src) && N->getSimpleValueType(0) == MVT::v2i32 &&
      Src.getSimpleValueType() == MVT::f64)
    return bitcastConstantFPToInt(cast<ConstantFPSDNode>(Src), dl, DCI.DAG);

  return SDValue();
}

SDValue E2KTargetLowering::PerformDAGCombine(SDNode *N,
                                               DAGCombinerInfo &DCI) const {
  switch (N->getOpcode()) {
  default:
    break;
  case ISD::BITCAST:
    return PerformBITCASTCombine(N, DCI);
  }
  return SDValue();
}

MachineBasicBlock *
E2KTargetLowering::EmitInstrWithCustomInserter(MachineInstr &MI,
                                                 MachineBasicBlock *BB) const {
  switch (MI.getOpcode()) {
  default: llvm_unreachable("Unknown SELECT_CC!");
  }
}

//===----------------------------------------------------------------------===//
//                         E2K Inline Assembly Support
//===----------------------------------------------------------------------===//

/// getConstraintType - Given a constraint letter, return the type of
/// constraint it is for this target.
E2KTargetLowering::ConstraintType
E2KTargetLowering::getConstraintType(StringRef Constraint) const {
  if (Constraint.size() == 1) {
    switch (Constraint[0]) {
    default:  break;
    case 'r':
    case 'f':
    case 'e':
      return C_RegisterClass;
    case 'I': // SIMM13
      return C_Immediate;
    }
  }

  return TargetLowering::getConstraintType(Constraint);
}

TargetLowering::ConstraintWeight E2KTargetLowering::
getSingleConstraintMatchWeight(AsmOperandInfo &info,
                               const char *constraint) const {
  ConstraintWeight weight = CW_Invalid;
  Value *CallOperandVal = info.CallOperandVal;
  // If we don't have a value, we can't do a match,
  // but allow it at the lowest weight.
  if (!CallOperandVal)
    return CW_Default;

  // Look at the constraint type.
  switch (*constraint) {
  default:
    weight = TargetLowering::getSingleConstraintMatchWeight(info, constraint);
    break;
  case 'I': // SIMM13
    if (ConstantInt *C = dyn_cast<ConstantInt>(info.CallOperandVal)) {
      if (isInt<13>(C->getSExtValue()))
        weight = CW_Constant;
    }
    break;
  }
  return weight;
}

/// LowerAsmOperandForConstraint - Lower the specified operand into the Ops
/// vector.  If it is invalid, don't add anything to Ops.
void E2KTargetLowering::
LowerAsmOperandForConstraint(SDValue Op,
                             std::string &Constraint,
                             std::vector<SDValue> &Ops,
                             SelectionDAG &DAG) const {
  SDValue Result;

  // Only support length 1 constraints for now.
  if (Constraint.length() > 1)
    return;

  char ConstraintLetter = Constraint[0];
  switch (ConstraintLetter) {
  default: break;
  case 'I':
    if (ConstantSDNode *C = dyn_cast<ConstantSDNode>(Op)) {
      if (isInt<13>(C->getSExtValue())) {
        Result = DAG.getTargetConstant(C->getSExtValue(), SDLoc(Op),
                                       Op.getValueType());
        break;
      }
      return;
    }
  }

  if (Result.getNode()) {
    Ops.push_back(Result);
    return;
  }
  TargetLowering::LowerAsmOperandForConstraint(Op, Constraint, Ops, DAG);
}

std::pair<unsigned, const TargetRegisterClass *>
E2KTargetLowering::getRegForInlineAsmConstraint(const TargetRegisterInfo *TRI,
                                                  StringRef Constraint,
                                                  MVT VT) const {
  if (Constraint.empty())
    return std::make_pair(0U, nullptr);

  if (Constraint.size() == 1) {
    switch (Constraint[0]) {
    case 'r':
      if (Subtarget->is64Bit())
        return std::make_pair(0U, &E2K::RegDRRegClass);
      else
        return std::make_pair(0U, &E2K::RegSRRegClass);
    case 'f':
      if (VT == MVT::f32 || VT == MVT::i32)
        return std::make_pair(0U, &E2K::RegSRRegClass);
      else if (VT == MVT::f64 || VT == MVT::i64)
        return std::make_pair(0U, &E2K::RegDRRegClass);
      else if (VT == MVT::f128)
        return std::make_pair(0U, &E2K::RegQRRegClass);
      // This will generate an error message
      return std::make_pair(0U, nullptr);
    case 'e':
      if (VT == MVT::f32 || VT == MVT::i32)
        return std::make_pair(0U, &E2K::RegSRRegClass);
      else if (VT == MVT::f64 || VT == MVT::i64 )
        return std::make_pair(0U, &E2K::RegDRRegClass);
      else if (VT == MVT::f128)
        return std::make_pair(0U, &E2K::RegQRRegClass);
      // This will generate an error message
      return std::make_pair(0U, nullptr);
    }
  }

  if (Constraint.front() != '{')
    return std::make_pair(0U, nullptr);

  assert(Constraint.back() == '}' && "Not a brace enclosed constraint?");
  StringRef RegName(Constraint.data() + 1, Constraint.size() - 2);
  if (RegName.empty())
    return std::make_pair(0U, nullptr);

  unsigned long long RegNo;
  // Handle numbered register aliases.
  if (RegName[0] == 'r' &&
      getAsUnsignedInteger(RegName.begin() + 1, 10, RegNo)) {
    // r0-r7   -> g0-g7
    // r8-r15  -> o0-o7
    // r16-r23 -> l0-l7
    // r24-r31 -> i0-i7
    if (RegNo > 31)
      return std::make_pair(0U, nullptr);
    const char RegTypes[] = {'g', 'o', 'l', 'i'};
    char RegType = RegTypes[RegNo / 8];
    char RegIndex = '0' + (RegNo % 8);
    char Tmp[] = {'{', RegType, RegIndex, '}', 0};
    return getRegForInlineAsmConstraint(TRI, Tmp, VT);
  }

  // Rewrite the fN constraint according to the value type if needed.
  if (VT != MVT::f32 && VT != MVT::Other && RegName[0] == 'f' &&
      getAsUnsignedInteger(RegName.begin() + 1, 10, RegNo)) {
    if (VT == MVT::f64 && (RegNo % 2 == 0)) {
      return getRegForInlineAsmConstraint(
          TRI, StringRef("{d" + utostr(RegNo / 2) + "}"), VT);
    } else if (VT == MVT::f128 && (RegNo % 4 == 0)) {
      return getRegForInlineAsmConstraint(
          TRI, StringRef("{q" + utostr(RegNo / 4) + "}"), VT);
    } else {
      return std::make_pair(0U, nullptr);
    }
  }

  auto ResultPair =
      TargetLowering::getRegForInlineAsmConstraint(TRI, Constraint, VT);
  if (!ResultPair.second)
    return std::make_pair(0U, nullptr);

  // Force the use of I64Regs over IntRegs for 64-bit values.
  if (Subtarget->is64Bit() && VT == MVT::i64) {
    assert(ResultPair.second == &E2K::RegSRRegClass &&
           "Unexpected register class");
    return std::make_pair(ResultPair.first, &E2K::RegDRRegClass);
  }

  return ResultPair;
}

bool
E2KTargetLowering::isOffsetFoldingLegal(const GlobalAddressSDNode *GA) const {
  // The E2K target isn't yet aware of offsets.
  return false;
}

void E2KTargetLowering::ReplaceNodeResults(SDNode *N,
                                             SmallVectorImpl<SDValue>& Results,
                                             SelectionDAG &DAG) const {

  SDLoc dl(N);

  RTLIB::Libcall libCall = RTLIB::UNKNOWN_LIBCALL;

  switch (N->getOpcode()) {
  default:
    llvm_unreachable("Do not know how to custom type legalize this operation!");

  case ISD::FP_TO_SINT:
  case ISD::FP_TO_UINT:
    // Custom lower only if it involves f128 or i64.
    if (N->getOperand(0).getValueType() != MVT::f128
        || N->getValueType(0) != MVT::i64)
      return;
    libCall = ((N->getOpcode() == ISD::FP_TO_SINT)
               ? RTLIB::FPTOSINT_F128_I64
               : RTLIB::FPTOUINT_F128_I64);

    Results.push_back(LowerF128Op(SDValue(N, 0),
                                  DAG,
                                  getLibcallName(libCall),
                                  1));
    return;
  case ISD::SINT_TO_FP:
  case ISD::UINT_TO_FP:
    // Custom lower only if it involves f128 or i64.
    if (N->getValueType(0) != MVT::f128
        || N->getOperand(0).getValueType() != MVT::i64)
      return;

    libCall = ((N->getOpcode() == ISD::SINT_TO_FP)
               ? RTLIB::SINTTOFP_I64_F128
               : RTLIB::UINTTOFP_I64_F128);

    Results.push_back(LowerF128Op(SDValue(N, 0),
                                  DAG,
                                  getLibcallName(libCall),
                                  1));
    return;
  case ISD::LOAD: {
    LoadSDNode *Ld = cast<LoadSDNode>(N);
    // Custom handling only for i64: turn i64 load into a v2i32 load,
    // and a bitcast.
    if (Ld->getValueType(0) != MVT::i64 || Ld->getMemoryVT() != MVT::i64)
      return;

    SDLoc dl(N);
    SDValue LoadRes = DAG.getExtLoad(
        Ld->getExtensionType(), dl, MVT::v2i32, Ld->getChain(),
        Ld->getBasePtr(), Ld->getPointerInfo(), MVT::v2i32,
        Ld->getOriginalAlign(), Ld->getMemOperand()->getFlags(),
        Ld->getAAInfo());

    SDValue Res = DAG.getNode(ISD::BITCAST, dl, MVT::i64, LoadRes);
    Results.push_back(Res);
    Results.push_back(LoadRes.getValue(1));
    return;
  }
  }
}

// Override to enable LOAD_STACK_GUARD lowering on Linux.
bool E2KTargetLowering::useLoadStackGuardNode() const {
  if (!Subtarget->isTargetLinux())
    return TargetLowering::useLoadStackGuardNode();
  return true;
}

// Override to disable global variable loading on Linux.
void E2KTargetLowering::insertSSPDeclarations(Module &M) const {
  if (!Subtarget->isTargetLinux())
    return TargetLowering::insertSSPDeclarations(M);
}
