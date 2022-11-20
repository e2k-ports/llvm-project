//===-- E2KTargetInfo.cpp - E2K Target Implementation -----------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "TargetInfo/E2KTargetInfo.h"
#include "llvm/MC/TargetRegistry.h"
using namespace llvm;

Target &llvm::getTheE2KTarget() {
  static Target TheE2KTarget;
  return TheE2KTarget;
}
Target &llvm::getTheE2KV9Target() {
  static Target TheE2KV9Target;
  return TheE2KV9Target;
}
Target &llvm::getTheE2KelTarget() {
  static Target TheE2KelTarget;
  return TheE2KelTarget;
}

extern "C" LLVM_EXTERNAL_VISIBILITY void LLVMInitializeE2KTargetInfo() {
  RegisterTarget<Triple::e2k, /*HasJIT=*/false> X(getTheE2KTarget(),
                                                    "e2k", "E2K", "E2K");
  RegisterTarget<Triple::e2kv9, /*HasJIT=*/false> Y(
      getTheE2KV9Target(), "e2kv9", "E2K V9", "E2K");
  RegisterTarget<Triple::e2kel, /*HasJIT=*/false> Z(
      getTheE2KelTarget(), "e2kel", "E2K LE", "E2K");
}
