//===-- E2KTargetStreamer.cpp - E2K Target Streamer Methods -----------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file provides E2K specific target streamer methods.
//
//===----------------------------------------------------------------------===//

#include "E2KTargetStreamer.h"
#include "E2KInstPrinter.h"
#include "llvm/Support/FormattedStream.h"

using namespace llvm;

// pin vtable to this file
E2KTargetStreamer::E2KTargetStreamer(MCStreamer &S) : MCTargetStreamer(S) {}

void E2KTargetStreamer::anchor() {}

E2KTargetAsmStreamer::E2KTargetAsmStreamer(MCStreamer &S,
                                               formatted_raw_ostream &OS)
    : E2KTargetStreamer(S), OS(OS) {}

void E2KTargetAsmStreamer::emitE2KRegisterIgnore(unsigned reg) {
  OS << "\t.register "
     << "%" << StringRef(E2KInstPrinter::getRegisterName(reg)).lower()
     << ", #ignore\n";
}

void E2KTargetAsmStreamer::emitE2KRegisterScratch(unsigned reg) {
  OS << "\t.register "
     << "%" << StringRef(E2KInstPrinter::getRegisterName(reg)).lower()
     << ", #scratch\n";
}

E2KTargetELFStreamer::E2KTargetELFStreamer(MCStreamer &S)
    : E2KTargetStreamer(S) {}

MCELFStreamer &E2KTargetELFStreamer::getStreamer() {
  return static_cast<MCELFStreamer &>(Streamer);
}
