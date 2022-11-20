//===-- E2KMCTargetDesc.h - E2K Target Descriptions ---------*- C++ -*-===//
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

#ifndef LLVM_LIB_TARGET_E2K_MCTARGETDESC_E2KMCTARGETDESC_H
#define LLVM_LIB_TARGET_E2K_MCTARGETDESC_E2KMCTARGETDESC_H

#include "llvm/Support/DataTypes.h"

#include <memory>

namespace llvm {
class MCAsmBackend;
class MCCodeEmitter;
class MCContext;
class MCInstrInfo;
class MCObjectTargetWriter;
class MCRegisterInfo;
class MCSubtargetInfo;
class MCTargetOptions;
class Target;

MCCodeEmitter *createE2KMCCodeEmitter(const MCInstrInfo &MCII,
                                        MCContext &Ctx);
MCAsmBackend *createE2KAsmBackend(const Target &T, const MCSubtargetInfo &STI,
                                    const MCRegisterInfo &MRI,
                                    const MCTargetOptions &Options);
std::unique_ptr<MCObjectTargetWriter> createE2KELFObjectWriter(bool Is64Bit,
                                                                 uint8_t OSABI);
} // End llvm namespace

// Defines symbolic names for E2K registers.  This defines a mapping from
// register name to register number.
//
#define GET_REGINFO_ENUM
#include "E2KGenRegisterInfo.inc"

// Defines symbolic names for the E2K instructions.
//
#define GET_INSTRINFO_ENUM
#define GET_INSTRINFO_MC_HELPER_DECLS
#include "E2KGenInstrInfo.inc"

#define GET_SUBTARGETINFO_ENUM
#include "E2KGenSubtargetInfo.inc"

#endif
