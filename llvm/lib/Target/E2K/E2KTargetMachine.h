//===-- E2KTargetMachine.h - Define TargetMachine for E2K ---*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file declares the E2K specific subclass of TargetMachine.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_E2K_E2KTARGETMACHINE_H
#define LLVM_LIB_TARGET_E2K_E2KTARGETMACHINE_H

#include "E2KInstrInfo.h"
#include "E2KSubtarget.h"
#include "llvm/Target/TargetMachine.h"

namespace llvm {

class E2KTargetMachine : public LLVMTargetMachine {
  std::unique_ptr<TargetLoweringObjectFile> TLOF;
  E2KSubtarget Subtarget;
  E2KBitness Bitness;
  mutable StringMap<std::unique_ptr<E2KSubtarget>> SubtargetMap;
public:
  E2KTargetMachine(const Target &T, const Triple &TT, StringRef CPU,
                     StringRef FS, const TargetOptions &Options,
                     std::optional<Reloc::Model> RM, std::optional<CodeModel::Model> CM,
                     CodeGenOpt::Level OL, bool JIT, E2KBitness bitness);
  ~E2KTargetMachine() override;

  const E2KSubtarget *getSubtargetImpl() const { return &Subtarget; }
  const E2KSubtarget *getSubtargetImpl(const Function &) const override;

  // Pass Pipeline Configuration
  TargetPassConfig *createPassConfig(PassManagerBase &PM) override;
  TargetLoweringObjectFile *getObjFileLowering() const override {
    return TLOF.get();
  }
};

/// E2K 32-bit target machine
///
class E2K32TargetMachine : public E2KTargetMachine {
  virtual void anchor();
public:
  E2K32TargetMachine(const Target &T, const Triple &TT, StringRef CPU,
                       StringRef FS, const TargetOptions &Options,
                       std::optional<Reloc::Model> RM, std::optional<CodeModel::Model> CM,
                       CodeGenOpt::Level OL, bool JIT);
};

/// E2K 64-bit target machine
///
class E2K64TargetMachine : public E2KTargetMachine {
  virtual void anchor();
public:
  E2K64TargetMachine(const Target &T, const Triple &TT, StringRef CPU,
                       StringRef FS, const TargetOptions &Options,
                       std::optional<Reloc::Model> RM, std::optional<CodeModel::Model> CM,
                       CodeGenOpt::Level OL, bool JIT);
};

/// E2K 128-bit target machine
///
class E2K128TargetMachine : public E2KTargetMachine {
  virtual void anchor();
public:
  E2K128TargetMachine(const Target &T, const Triple &TT, StringRef CPU,
                     StringRef FS, const TargetOptions &Options,
                     std::optional<Reloc::Model> RM, std::optional<CodeModel::Model> CM,
                     CodeGenOpt::Level OL, bool JIT);
};

/// E2K 128/64-bit target machine
///
class E2K12864TargetMachine : public E2KTargetMachine {
  virtual void anchor();
public:
  E2K12864TargetMachine(const Target &T, const Triple &TT, StringRef CPU,
                      StringRef FS, const TargetOptions &Options,
                      std::optional<Reloc::Model> RM, std::optional<CodeModel::Model> CM,
                      CodeGenOpt::Level OL, bool JIT);
};

} // end namespace llvm

#endif
