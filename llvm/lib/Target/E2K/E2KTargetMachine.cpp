//===-- E2KTargetMachine.cpp - Define TargetMachine for E2K -----------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
//
//===----------------------------------------------------------------------===//

#include "E2KTargetMachine.h"
#include "E2K.h"
#include "E2KTargetObjectFile.h"
#include "TargetInfo/E2KTargetInfo.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/CodeGen/TargetPassConfig.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/MC/TargetRegistry.h"
using namespace llvm;

extern "C" LLVM_EXTERNAL_VISIBILITY void LLVMInitializeE2KTarget() {
  // Register the target.
  RegisterTargetMachine<E2K32TargetMachine> X(getTheE2K32Target());
  RegisterTargetMachine<E2K64TargetMachine> Y(getTheE2K64Target());
}

static std::string computeDataLayout(const Triple &T, bool is64Bit) {
  // E2K is always little endian
  std::string Ret = "e";
  Ret += "-m:e";

  // Some ABIs have 32bit pointers.
  if (!is64Bit)
    Ret += "-p:32:32";

  // Alignments for 64 bit integers.
  Ret += "-i64:64";

  // On E2K64 128 floats are aligned to 128 bits, on others only to 64.
  // On E2K64 registers can hold 64 or 32 bits, on others only 32.
  if (is64Bit)
    Ret += "-n32:64";
  else
    Ret += "-f128:64-n32";

  if (is64Bit)
    Ret += "-S128";
  else
    Ret += "-S64";

  return Ret;
}

static Reloc::Model getEffectiveRelocModel(std::optional<Reloc::Model> RM) {
  return RM.value_or(Reloc::Static);
}

// Code models. Some only make sense for 64-bit code.
//
// SunCC  Reloc   CodeModel  Constraints
// abs32  Static  Small      text+data+bss linked below 2^32 bytes
// abs44  Static  Medium     text+data+bss linked below 2^44 bytes
// abs64  Static  Large      text smaller than 2^31 bytes
// pic13  PIC_    Small      GOT < 2^13 bytes
// pic32  PIC_    Medium     GOT < 2^32 bytes
//
// All code models require that the text segment is smaller than 2GB.
static CodeModel::Model
getEffectiveE2KCodeModel(std::optional<CodeModel::Model> CM, Reloc::Model RM,
                           bool Is64Bit, bool JIT) {
  if (CM) {
    if (*CM == CodeModel::Tiny)
      report_fatal_error("Target does not support the tiny CodeModel", false);
    if (*CM == CodeModel::Kernel)
      report_fatal_error("Target does not support the kernel CodeModel", false);
    return *CM;
  }
  if (Is64Bit) {
    if (JIT)
      return CodeModel::Large;
    return RM == Reloc::PIC_ ? CodeModel::Small : CodeModel::Medium;
  }
  return CodeModel::Small;
}

/// Create an ILP32 architecture model
E2KTargetMachine::E2KTargetMachine(
    const Target &T, const Triple &TT, StringRef CPU, StringRef FS,
    const TargetOptions &Options, std::optional<Reloc::Model> RM,
    std::optional<CodeModel::Model> CM, CodeGenOpt::Level OL, bool JIT, bool is64bit)
    : LLVMTargetMachine(T, computeDataLayout(TT, is64bit), TT, CPU, FS, Options,
                        getEffectiveRelocModel(RM),
                        getEffectiveE2KCodeModel(
                            CM, getEffectiveRelocModel(RM), is64bit, JIT),
                        OL),
      TLOF(std::make_unique<E2KELFTargetObjectFile>()),
      Subtarget(TT, std::string(CPU), std::string(FS), *this, is64bit),
      is64Bit(is64bit) {
  initAsmInfo();
}

E2KTargetMachine::~E2KTargetMachine() = default;

const E2KSubtarget *
E2KTargetMachine::getSubtargetImpl(const Function &F) const {
  Attribute CPUAttr = F.getFnAttribute("target-cpu");
  Attribute FSAttr = F.getFnAttribute("target-features");

  std::string CPU =
      CPUAttr.isValid() ? CPUAttr.getValueAsString().str() : TargetCPU;
  std::string FS =
      FSAttr.isValid() ? FSAttr.getValueAsString().str() : TargetFS;

  auto &I = SubtargetMap[CPU + FS];
  if (!I) {
    // This needs to be done before we create a new subtarget since any
    // creation will depend on the TM and the code generation flags on the
    // function that reside in TargetOptions.
    resetTargetOptions(F);
    I = std::make_unique<E2KSubtarget>(TargetTriple, CPU, FS, *this,
                                          this->is64Bit);
  }
  return I.get();
}

namespace {
/// E2K Code Generator Pass Configuration Options.
class E2KPassConfig : public TargetPassConfig {
public:
  E2KPassConfig(E2KTargetMachine &TM, PassManagerBase &PM)
    : TargetPassConfig(TM, PM) {}

  E2KTargetMachine &getE2KTargetMachine() const {
    return getTM<E2KTargetMachine>();
  }

  void addIRPasses() override;
  bool addInstSelector() override;
  void addPreEmitPass() override;
};
} // namespace

TargetPassConfig *E2KTargetMachine::createPassConfig(PassManagerBase &PM) {
  return new E2KPassConfig(*this, PM);
}

void E2KPassConfig::addIRPasses() {
  addPass(createAtomicExpandPass());

  TargetPassConfig::addIRPasses();
}

bool E2KPassConfig::addInstSelector() {
  addPass(createE2KISelDag(getE2KTargetMachine()));
  return false;
}

void E2KPassConfig::addPreEmitPass(){
  addPass(createE2KDelaySlotFillerPass());
}

void E2K32TargetMachine::anchor() { }

E2K32TargetMachine::E2K32TargetMachine(const Target &T, const Triple &TT,
                                           StringRef CPU, StringRef FS,
                                           const TargetOptions &Options,
                                           std::optional<Reloc::Model> RM,
                                           std::optional<CodeModel::Model> CM,
                                           CodeGenOpt::Level OL, bool JIT)
    : E2KTargetMachine(T, TT, CPU, FS, Options, RM, CM, OL, JIT, false) {}

void E2K64TargetMachine::anchor() { }

E2K64TargetMachine::E2K64TargetMachine(const Target &T, const Triple &TT,
                                           StringRef CPU, StringRef FS,
                                           const TargetOptions &Options,
                                           std::optional<Reloc::Model> RM,
                                           std::optional<CodeModel::Model> CM,
                                           CodeGenOpt::Level OL, bool JIT)
    : E2KTargetMachine(T, TT, CPU, FS, Options, RM, CM, OL, JIT, true) {}
