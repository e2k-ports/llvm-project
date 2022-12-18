//===-- E2KTargetInfo.h - E2K Target Implementation ---------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_E2K_TARGETINFO_E2KTARGETINFO_H
#define LLVM_LIB_TARGET_E2K_TARGETINFO_E2KTARGETINFO_H

namespace llvm {

class Target;

Target &getTheE2K32Target();
Target &getTheE2K64Target();
Target &getTheE2K128Target();
Target &getTheE2K12864Target();

} // namespace llvm

#endif // LLVM_LIB_TARGET_E2K_TARGETINFO_E2KTARGETINFO_H
