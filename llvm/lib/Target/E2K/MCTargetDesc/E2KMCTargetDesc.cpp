//===-- E2KMCTargetDesc.cpp - E2K Target Descriptions -----------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file provides E2K specific target descriptions.
//
//===----------------------------------------------------------------------===//

#include "E2KMCTargetDesc.h"
#include "E2KInstPrinter.h"
#include "E2KMCAsmInfo.h"
#include "E2KTargetStreamer.h"
#include "TargetInfo/E2KTargetInfo.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/ErrorHandling.h"

using namespace llvm;

#define GET_INSTRINFO_MC_DESC
#define ENABLE_INSTR_PREDICATE_VERIFIER
#include "E2KGenInstrInfo.inc"

#define GET_SUBTARGETINFO_MC_DESC
#include "E2KGenSubtargetInfo.inc"

#define GET_REGINFO_MC_DESC
#include "E2KGenRegisterInfo.inc"

static MCAsmInfo *createE2KMCAsmInfo(const MCRegisterInfo &MRI,
                                       const Triple &TT,
                                       const MCTargetOptions &Options) {
  MCAsmInfo *MAI = new E2KELFMCAsmInfo(TT);
  unsigned Reg = MRI.getDwarfRegNum(E2K::O6, true);
  MCCFIInstruction Inst = MCCFIInstruction::cfiDefCfa(nullptr, Reg, 0);
  MAI->addInitialFrameState(Inst);
  return MAI;
}

static MCAsmInfo *createE2KV9MCAsmInfo(const MCRegisterInfo &MRI,
                                         const Triple &TT,
                                         const MCTargetOptions &Options) {
  MCAsmInfo *MAI = new E2KELFMCAsmInfo(TT);
  unsigned Reg = MRI.getDwarfRegNum(E2K::O6, true);
  MCCFIInstruction Inst = MCCFIInstruction::cfiDefCfa(nullptr, Reg, 2047);
  MAI->addInitialFrameState(Inst);
  return MAI;
}

static MCInstrInfo *createE2KMCInstrInfo() {
  MCInstrInfo *X = new MCInstrInfo();
  InitE2KMCInstrInfo(X);
  return X;
}

static MCRegisterInfo *createE2KMCRegisterInfo(const Triple &TT) {
  MCRegisterInfo *X = new MCRegisterInfo();
  InitE2KMCRegisterInfo(X, E2K::O7);
  return X;
}

static MCSubtargetInfo *
createE2KMCSubtargetInfo(const Triple &TT, StringRef CPU, StringRef FS) {
  if (CPU.empty())
    CPU = (TT.getArch() == Triple::e2kv9) ? "v9" : "v8";
  return createE2KMCSubtargetInfoImpl(TT, CPU, /*TuneCPU*/ CPU, FS);
}

static MCTargetStreamer *
createObjectTargetStreamer(MCStreamer &S, const MCSubtargetInfo &STI) {
  return new E2KTargetELFStreamer(S);
}

static MCTargetStreamer *createTargetAsmStreamer(MCStreamer &S,
                                                 formatted_raw_ostream &OS,
                                                 MCInstPrinter *InstPrint,
                                                 bool isVerboseAsm) {
  return new E2KTargetAsmStreamer(S, OS);
}

static MCTargetStreamer *createNullTargetStreamer(MCStreamer &S) {
  return new E2KTargetStreamer(S);
}

static MCInstPrinter *createE2KMCInstPrinter(const Triple &T,
                                               unsigned SyntaxVariant,
                                               const MCAsmInfo &MAI,
                                               const MCInstrInfo &MII,
                                               const MCRegisterInfo &MRI) {
  return new E2KInstPrinter(MAI, MII, MRI);
}

extern "C" LLVM_EXTERNAL_VISIBILITY void LLVMInitializeE2KTargetMC() {
  // Register the MC asm info.
  RegisterMCAsmInfoFn X(getTheE2KTarget(), createE2KMCAsmInfo);
  RegisterMCAsmInfoFn Y(getTheE2KV9Target(), createE2KV9MCAsmInfo);
  RegisterMCAsmInfoFn Z(getTheE2KelTarget(), createE2KMCAsmInfo);

  for (Target *T :
       {&getTheE2KTarget(), &getTheE2KV9Target(), &getTheE2KelTarget()}) {
    // Register the MC instruction info.
    TargetRegistry::RegisterMCInstrInfo(*T, createE2KMCInstrInfo);

    // Register the MC register info.
    TargetRegistry::RegisterMCRegInfo(*T, createE2KMCRegisterInfo);

    // Register the MC subtarget info.
    TargetRegistry::RegisterMCSubtargetInfo(*T, createE2KMCSubtargetInfo);

    // Register the MC Code Emitter.
    TargetRegistry::RegisterMCCodeEmitter(*T, createE2KMCCodeEmitter);

    // Register the asm backend.
    TargetRegistry::RegisterMCAsmBackend(*T, createE2KAsmBackend);

    // Register the object target streamer.
    TargetRegistry::RegisterObjectTargetStreamer(*T,
                                                 createObjectTargetStreamer);

    // Register the asm streamer.
    TargetRegistry::RegisterAsmTargetStreamer(*T, createTargetAsmStreamer);

    // Register the null streamer.
    TargetRegistry::RegisterNullTargetStreamer(*T, createNullTargetStreamer);

    // Register the MCInstPrinter
    TargetRegistry::RegisterMCInstPrinter(*T, createE2KMCInstPrinter);
  }
}
