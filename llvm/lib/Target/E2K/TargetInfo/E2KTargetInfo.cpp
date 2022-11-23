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

Target &llvm::getTheE2K32Target() {
  static Target TheE2K32Target;
  return TheE2K32Target;
}
Target &llvm::getTheE2K64Target() {
  static Target TheE2K64Target;
  return TheE2K64Target;
}

extern "C" LLVM_EXTERNAL_VISIBILITY void LLVMInitializeE2KTargetInfo() {
  RegisterTarget<Triple::e2k32, /*HasJIT=*/false> X(getTheE2K32Target(),
                                                    "e2k32", "E2K32", "E2K");
  RegisterTarget<Triple::e2k64, /*HasJIT=*/false> Y(getTheE2K64Target(),
                                                    "e2k64", "E2K64", "E2K");
}
