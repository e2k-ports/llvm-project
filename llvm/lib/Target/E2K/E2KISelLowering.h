//===-- E2KISelLowering.h - E2K DAG Lowering Interface ------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file defines the interfaces that E2K uses to lower LLVM code into a
// selection DAG.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_E2K_E2KISELLOWERING_H
#define LLVM_LIB_TARGET_E2K_E2KISELLOWERING_H

#include "E2K.h"
#include "llvm/CodeGen/TargetLowering.h"

namespace llvm {
  class E2KSubtarget;

  namespace E2KISD {
  enum NodeType : unsigned {
    FIRST_NUMBER = ISD::BUILTIN_OP_END,
    RET_FLAG,
  };
  }

  class E2KTargetLowering : public TargetLowering {
    const E2KSubtarget *Subtarget;
  public:
    E2KTargetLowering(const TargetMachine &TM, const E2KSubtarget &STI);
    SDValue LowerOperation(SDValue Op, SelectionDAG &DAG) const override;

    /// computeKnownBitsForTargetNode - Determine which of the bits specified
    /// in Mask are known to be either zero or one and return them in the
    /// KnownZero/KnownOne bitsets.
    void computeKnownBitsForTargetNode(const SDValue Op,
                                       KnownBits &Known,
                                       const APInt &DemandedElts,
                                       const SelectionDAG &DAG,
                                       unsigned Depth = 0) const override;

    MachineBasicBlock *
    EmitInstrWithCustomInserter(MachineInstr &MI,
                                MachineBasicBlock *MBB) const override;

    const char *getTargetNodeName(unsigned Opcode) const override;

    ConstraintType getConstraintType(StringRef Constraint) const override;
    ConstraintWeight
    getSingleConstraintMatchWeight(AsmOperandInfo &info,
                                   const char *constraint) const override;
    void LowerAsmOperandForConstraint(SDValue Op,
                                      std::string &Constraint,
                                      std::vector<SDValue> &Ops,
                                      SelectionDAG &DAG) const override;

    std::pair<unsigned, const TargetRegisterClass *>
    getRegForInlineAsmConstraint(const TargetRegisterInfo *TRI,
                                 StringRef Constraint, MVT VT) const override;

    bool isOffsetFoldingLegal(const GlobalAddressSDNode *GA) const override;
    MVT getScalarShiftAmountTy(const DataLayout &, EVT) const override {
      return MVT::i32;
    }

    Register getRegisterByName(const char* RegName, LLT VT,
                               const MachineFunction &MF) const override;

    /// If a physical register, this returns the register that receives the
    /// exception address on entry to an EH pad.
    Register
    getExceptionPointerRegister(const Constant *PersonalityFn) const override {
      return E2K::R0;
    }

    /// If a physical register, this returns the register that receives the
    /// exception typeid on entry to a landing pad.
    Register
    getExceptionSelectorRegister(const Constant *PersonalityFn) const override {
      return E2K::R0;
    }

    /// Override to support customized stack guard loading.
    bool useLoadStackGuardNode() const override;
    void insertSSPDeclarations(Module &M) const override;

    /// getSetCCResultType - Return the ISD::SETCC ValueType
    EVT getSetCCResultType(const DataLayout &DL, LLVMContext &Context,
                           EVT VT) const override;

    SDValue
    LowerFormalArguments(SDValue Chain, CallingConv::ID CallConv, bool isVarArg,
                         const SmallVectorImpl<ISD::InputArg> &Ins,
                         const SDLoc &dl, SelectionDAG &DAG,
                         SmallVectorImpl<SDValue> &InVals) const override;
    SDValue LowerFormalArguments_32(SDValue Chain, CallingConv::ID CallConv,
                                    bool isVarArg,
                                    const SmallVectorImpl<ISD::InputArg> &Ins,
                                    const SDLoc &dl, SelectionDAG &DAG,
                                    SmallVectorImpl<SDValue> &InVals) const;
    SDValue LowerFormalArguments_64(SDValue Chain, CallingConv::ID CallConv,
                                    bool isVarArg,
                                    const SmallVectorImpl<ISD::InputArg> &Ins,
                                    const SDLoc &dl, SelectionDAG &DAG,
                                    SmallVectorImpl<SDValue> &InVals) const;

    SDValue
      LowerCall(TargetLowering::CallLoweringInfo &CLI,
                SmallVectorImpl<SDValue> &InVals) const override;
    bool CanLowerReturn(CallingConv::ID CallConv, MachineFunction &MF,
                        bool isVarArg,
                        const SmallVectorImpl<ISD::OutputArg> &Outs,
                        LLVMContext &Context) const override;

    SDValue LowerReturn(SDValue Chain, CallingConv::ID CallConv, bool isVarArg,
                        const SmallVectorImpl<ISD::OutputArg> &Outs,
                        const SmallVectorImpl<SDValue> &OutVals,
                        const SDLoc &dl, SelectionDAG &DAG) const override;


    SDValue LowerGlobalAddress(SDValue Op, SelectionDAG &DAG) const;
    SDValue LowerGlobalTLSAddress(SDValue Op, SelectionDAG &DAG) const;
    SDValue LowerConstantPool(SDValue Op, SelectionDAG &DAG) const;
    SDValue LowerBlockAddress(SDValue Op, SelectionDAG &DAG) const;


    SDValue makeAddress(SDValue Op, SelectionDAG &DAG) const;

    SDValue LowerF128Op(SDValue Op, SelectionDAG &DAG,
                        const char *LibFuncName,
                        unsigned numArgs) const;

    SDValue LowerINTRINSIC_WO_CHAIN(SDValue Op, SelectionDAG &DAG) const;

    SDValue PerformBITCASTCombine(SDNode *N, DAGCombinerInfo &DCI) const;

    SDValue bitcastConstantFPToInt(ConstantFPSDNode *C, const SDLoc &DL,
                                   SelectionDAG &DAG) const;

    SDValue PerformDAGCombine(SDNode *N, DAGCombinerInfo &DCI) const override;

    bool IsEligibleForTailCallOptimization(CCState &CCInfo,
                                           CallLoweringInfo &CLI,
                                           MachineFunction &MF) const;

    bool ShouldShrinkFPConstant(EVT VT) const override {
      // Do not shrink FP constpool if VT == MVT::f128.
      // (ldd, call _Q_fdtoq) is more expensive than two ldds.
      return VT != MVT::f128;
    }

    bool shouldInsertFencesForAtomic(const Instruction *I) const override {
      // FIXME: We insert fences for each atomics and generate
      // sub-optimal code for PSO/TSO. (Approximately nobody uses any
      // mode but TSO, which makes this even more silly)
      return true;
    }

    AtomicExpansionKind shouldExpandAtomicRMWInIR(AtomicRMWInst *AI) const override;

    void ReplaceNodeResults(SDNode *N,
                            SmallVectorImpl<SDValue>& Results,
                            SelectionDAG &DAG) const override;

  };
} // end namespace llvm

#endif    // LLVM_LIB_TARGET_E2K_E2KISELLOWERING_H
