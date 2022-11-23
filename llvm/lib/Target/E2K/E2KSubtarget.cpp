//===-- E2KSubtarget.cpp - E2K Subtarget Information ------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file implements the E2K specific subclass of TargetSubtargetInfo.
//
//===----------------------------------------------------------------------===//

#include "E2KSubtarget.h"
#include "E2K.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/MathExtras.h"

using namespace llvm;

#define DEBUG_TYPE "e2k-subtarget"

#define GET_SUBTARGETINFO_TARGET_DESC
#define GET_SUBTARGETINFO_CTOR
#include "E2KGenSubtargetInfo.inc"

void E2KSubtarget::anchor() { }

E2KSubtarget &E2KSubtarget::initializeSubtargetDependencies(StringRef CPU,
                                                                StringRef FS) {

  // Determine default and user specified characteristics
  std::string CPUName = std::string(CPU);
  if (CPUName.empty())
    CPUName = (Is64Bit) ? "v9" : "v8";

  // Parse features string.
  ParseSubtargetFeatures(CPUName, /*TuneCPU*/ CPUName, FS);

  return *this;
}

E2KSubtarget::E2KSubtarget(const Triple &TT, const std::string &CPU,
                               const std::string &FS, const TargetMachine &TM,
                               bool is64Bit)
    : E2KGenSubtargetInfo(TT, CPU, /*TuneCPU*/ CPU, FS), TargetTriple(TT),
      Is64Bit(is64Bit), InstrInfo(initializeSubtargetDependencies(CPU, FS)),
      TLInfo(TM, *this), FrameLowering(*this) {}

int E2KSubtarget::getAdjustedFrameSize(int frameSize) const {

  if (is64Bit()) {
    // All 64-bit stack frames must be 16-byte aligned, and must reserve space
    // for spilling the 16 window registers at %sp+BIAS..%sp+BIAS+128.
    frameSize += 128;
    // Frames with calls must also reserve space for 6 outgoing arguments
    // whether they are used or not. LowerCall_64 takes care of that.
    frameSize = alignTo(frameSize, 16);
  } else {
    // Emit the correct save instruction based on the number of bytes in
    // the frame. Minimum stack frame size according to V8 ABI is:
    //   16 words for register window spill
    //    1 word for address of returned aggregate-value
    // +  6 words for passing parameters on the stack
    // ----------
    //   23 words * 4 bytes per word = 92 bytes
    frameSize += 92;

    // Round up to next doubleword boundary -- a double-word boundary
    // is required by the ABI.
    frameSize = alignTo(frameSize, 8);
  }
  return frameSize;
}

bool E2KSubtarget::enableMachineScheduler() const {
  return true;
}
