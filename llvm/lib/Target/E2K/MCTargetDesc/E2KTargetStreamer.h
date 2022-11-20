//===-- E2KTargetStreamer.h - E2K Target Streamer ----------*- C++ -*--===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_E2K_MCTARGETDESC_E2KTARGETSTREAMER_H
#define LLVM_LIB_TARGET_E2K_MCTARGETDESC_E2KTARGETSTREAMER_H

#include "llvm/MC/MCELFStreamer.h"
#include "llvm/MC/MCStreamer.h"

namespace llvm {

class formatted_raw_ostream;

class E2KTargetStreamer : public MCTargetStreamer {
  virtual void anchor();

public:
  E2KTargetStreamer(MCStreamer &S);
  /// Emit ".register <reg>, #ignore".
  virtual void emitE2KRegisterIgnore(unsigned reg){};
  /// Emit ".register <reg>, #scratch".
  virtual void emitE2KRegisterScratch(unsigned reg){};
};

// This part is for ascii assembly output
class E2KTargetAsmStreamer : public E2KTargetStreamer {
  formatted_raw_ostream &OS;

public:
  E2KTargetAsmStreamer(MCStreamer &S, formatted_raw_ostream &OS);
  void emitE2KRegisterIgnore(unsigned reg) override;
  void emitE2KRegisterScratch(unsigned reg) override;
};

// This part is for ELF object output
class E2KTargetELFStreamer : public E2KTargetStreamer {
public:
  E2KTargetELFStreamer(MCStreamer &S);
  MCELFStreamer &getStreamer();
  void emitE2KRegisterIgnore(unsigned reg) override {}
  void emitE2KRegisterScratch(unsigned reg) override {}
};
} // end namespace llvm

#endif
