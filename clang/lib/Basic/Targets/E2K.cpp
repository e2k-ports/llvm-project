//===--- E2K.cpp - Implement E2K target feature support ---------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file implements E2K TargetInfo objects.
//
//===----------------------------------------------------------------------===//

#include "E2K.h"
#include "Targets.h"
#include "clang/Basic/MacroBuilder.h"
#include "llvm/ADT/StringSwitch.h"

using namespace clang;
using namespace clang::targets;

const char *const E2KTargetInfo::GCCRegNames[] = {
    // Integer registers
    "r0",  "r1",  "r2",  "r3",  "r4",  "r5",  "r6",  "r7",  "r8",  "r9",  "r10",
    "r11", "r12", "r13", "r14", "r15", "r16", "r17", "r18", "r19", "r20", "r21",
    "r22", "r23", "r24", "r25", "r26", "r27", "r28", "r29", "r30", "r31",

    // Floating-point registers
    "f0",  "f1",  "f2",  "f3",  "f4",  "f5",  "f6",  "f7",  "f8",  "f9",  "f10",
    "f11", "f12", "f13", "f14", "f15", "f16", "f17", "f18", "f19", "f20", "f21",
    "f22", "f23", "f24", "f25", "f26", "f27", "f28", "f29", "f30", "f31", "f32",
    "f34", "f36", "f38", "f40", "f42", "f44", "f46", "f48", "f50", "f52", "f54",
    "f56", "f58", "f60", "f62",
};

ArrayRef<const char *> E2KTargetInfo::getGCCRegNames() const {
  return llvm::makeArrayRef(GCCRegNames);
}

const TargetInfo::GCCRegAlias E2KTargetInfo::GCCRegAliases[] = {
    {{"g0"}, "r0"},  {{"g1"}, "r1"},  {{"g2"}, "r2"},        {{"g3"}, "r3"},
    {{"g4"}, "r4"},  {{"g5"}, "r5"},  {{"g6"}, "r6"},        {{"g7"}, "r7"},
    {{"o0"}, "r8"},  {{"o1"}, "r9"},  {{"o2"}, "r10"},       {{"o3"}, "r11"},
    {{"o4"}, "r12"}, {{"o5"}, "r13"}, {{"o6", "sp"}, "r14"}, {{"o7"}, "r15"},
    {{"l0"}, "r16"}, {{"l1"}, "r17"}, {{"l2"}, "r18"},       {{"l3"}, "r19"},
    {{"l4"}, "r20"}, {{"l5"}, "r21"}, {{"l6"}, "r22"},       {{"l7"}, "r23"},
    {{"i0"}, "r24"}, {{"i1"}, "r25"}, {{"i2"}, "r26"},       {{"i3"}, "r27"},
    {{"i4"}, "r28"}, {{"i5"}, "r29"}, {{"i6", "fp"}, "r30"}, {{"i7"}, "r31"},
};

ArrayRef<TargetInfo::GCCRegAlias> E2KTargetInfo::getGCCRegAliases() const {
  return llvm::makeArrayRef(GCCRegAliases);
}

bool E2KTargetInfo::hasFeature(StringRef Feature) const {
  return llvm::StringSwitch<bool>(Feature)
      .Case("softfloat", SoftFloat)
      .Case("e2k", true)
      .Default(false);
}

struct E2KCPUInfo {
  llvm::StringLiteral Name;
  E2KTargetInfo::CPUKind Kind;
  E2KTargetInfo::CPUGeneration Generation;
};

static constexpr E2KCPUInfo CPUInfo[] = {
    {{"v1"}, E2KTargetInfo::CK_V1, E2KTargetInfo::CG_V1},
    {{"v2"}, E2KTargetInfo::CK_V2, E2KTargetInfo::CG_V2},
    {{"v3"}, E2KTargetInfo::CK_V3, E2KTargetInfo::CG_V3},
    {{"v4"}, E2KTargetInfo::CK_V4, E2KTargetInfo::CG_V4},
    {{"v5"}, E2KTargetInfo::CK_V5, E2KTargetInfo::CG_V5},
    {{"v6"}, E2KTargetInfo::CK_V6, E2KTargetInfo::CG_V6},
    {{"v7"}, E2KTargetInfo::CK_V7, E2KTargetInfo::CG_V7},

    {{"s"}, E2KTargetInfo::CK_ElbrusS, E2KTargetInfo::CG_V2},
    {{"3s"}, E2KTargetInfo::CK_Elbrus3S, E2KTargetInfo::CG_V2},
    {{"2c"}, E2KTargetInfo::CK_Elbrus2C, E2KTargetInfo::CG_V2},
    {{"2c1"}, E2KTargetInfo::CK_Elbrus2C1, E2KTargetInfo::CG_V2},

    {{"1c+"}, E2KTargetInfo::CK_Elbrus1CPlus, E2KTargetInfo::CG_V4},
    {{"4c1c+"}, E2KTargetInfo::CK_Elbrus4C1Plus, E2KTargetInfo::CG_V4},

    {{"1ck"}, E2KTargetInfo::CK_Elbrus1CK, E2KTargetInfo::CG_V4},

    {{"2c+"}, E2KTargetInfo::CK_Elbrus2CPlus, E2KTargetInfo::CG_V2},
    {{"2c2"}, E2KTargetInfo::CK_Elbrus2C2, E2KTargetInfo::CG_V2},
    {{"s2"}, E2KTargetInfo::CK_ElbrusS2, E2KTargetInfo::CG_V2},
    {{"sx2"}, E2KTargetInfo::CK_ElbrusSX2, E2KTargetInfo::CG_V2},
    {{"3s2"}, E2KTargetInfo::CK_Elbrus3S2, E2KTargetInfo::CG_V2},

    {{"2cm"}, E2KTargetInfo::CK_Elbrus2CM, E2KTargetInfo::CG_V2},
    {{"1c"}, E2KTargetInfo::CK_Elbrus1C, E2KTargetInfo::CG_V2},

    {{"2s3"}, E2KTargetInfo::CK_Elbrus2S3, E2KTargetInfo::CG_V6},
    {{"2c3"}, E2KTargetInfo::CK_Elbrus2C3, E2KTargetInfo::CG_V6},

    {{"4c"}, E2KTargetInfo::CK_Elbrus4C, E2KTargetInfo::CG_V3},
    {{"2s"}, E2KTargetInfo::CK_Elbrus2S, E2KTargetInfo::CG_V3},
    {{"2s4m"}, E2KTargetInfo::CK_Elbrus2S4M, E2KTargetInfo::CG_V3},
    {{"3c4"}, E2KTargetInfo::CK_Elbrus3C4, E2KTargetInfo::CG_V3},

    {{"8c"}, E2KTargetInfo::CK_Elbrus8C, E2KTargetInfo::CG_V4},
    {{"4c+"}, E2KTargetInfo::CK_Elbrus4CPlus, E2KTargetInfo::CG_V4},
    {{"4c8"}, E2KTargetInfo::CK_Elbrus4C8, E2KTargetInfo::CG_V4},
    {{"4s"}, E2KTargetInfo::CK_Elbrus4S, E2KTargetInfo::CG_V4},

    {{"8c1"}, E2KTargetInfo::CK_Elbrus8C1, E2KTargetInfo::CG_V4},

    {{"8sv"}, E2KTargetInfo::CK_Elbrus8SV, E2KTargetInfo::CG_V5},
    {{"8c2"}, E2KTargetInfo::CK_Elbrus8C2, E2KTargetInfo::CG_V5},
    {{"8cb"}, E2KTargetInfo::CK_Elbrus8CB, E2KTargetInfo::CG_V5},

    {{"12s"}, E2KTargetInfo::CK_Elbrus12S, E2KTargetInfo::CG_V6},
    {{"12c"}, E2KTargetInfo::CK_Elbrus12C, E2KTargetInfo::CG_V6},

    {{"16s"}, E2KTargetInfo::CK_Elbrus16S, E2KTargetInfo::CG_V6},
    {{"16c"}, E2KTargetInfo::CK_Elbrus16C, E2KTargetInfo::CG_V6},

    {{"32s"}, E2KTargetInfo::CK_Elbrus32S, E2KTargetInfo::CG_V7},
    {{"32c"}, E2KTargetInfo::CK_Elbrus32C, E2KTargetInfo::CG_V7},
};

E2KTargetInfo::CPUGeneration
E2KTargetInfo::getCPUGeneration(CPUKind Kind) const {
  if (Kind == CK_GENERIC)
    return CG_V1;
  const E2KCPUInfo *Item = llvm::find_if(
      CPUInfo, [Kind](const E2KCPUInfo &Info) { return Info.Kind == Kind; });
  if (Item == std::end(CPUInfo))
    llvm_unreachable("Unexpected CPU kind");
  return Item->Generation;
}

E2KTargetInfo::CPUKind E2KTargetInfo::getCPUKind(StringRef Name) const {
  const E2KCPUInfo *Item = llvm::find_if(
      CPUInfo, [Name](const E2KCPUInfo &Info) { return Info.Name == Name; });

  if (Item == std::end(CPUInfo))
    return CK_GENERIC;
  return Item->Kind;
}

void E2KTargetInfo::fillValidCPUList(
    SmallVectorImpl<StringRef> &Values) const {
  for (const E2KCPUInfo &Info : CPUInfo)
    Values.push_back(Info.Name);
}

void E2KTargetInfo::getTargetDefines(const LangOptions &Opts,
                                       MacroBuilder &Builder) const {
  DefineStd(Builder, "e2k", Opts);
  Builder.defineMacro("__REGISTER_PREFIX__", "");

  if (SoftFloat)
    Builder.defineMacro("SOFT_FLOAT", "1");
}

void E2KV8TargetInfo::getTargetDefines(const LangOptions &Opts,
                                         MacroBuilder &Builder) const {
  E2KTargetInfo::getTargetDefines(Opts, Builder);
}

void E2KV9TargetInfo::getTargetDefines(const LangOptions &Opts,
                                         MacroBuilder &Builder) const {
  E2KTargetInfo::getTargetDefines(Opts, Builder);
  Builder.defineMacro("__e2kv9");
  Builder.defineMacro("__arch64__");

  Builder.defineMacro("__e2k64__");
  Builder.defineMacro("__e2k_v9__");
  Builder.defineMacro("__e2kv9__");

  Builder.defineMacro("__GCC_HAVE_SYNC_COMPARE_AND_SWAP_1");
  Builder.defineMacro("__GCC_HAVE_SYNC_COMPARE_AND_SWAP_2");
  Builder.defineMacro("__GCC_HAVE_SYNC_COMPARE_AND_SWAP_4");
  Builder.defineMacro("__GCC_HAVE_SYNC_COMPARE_AND_SWAP_8");
}

void E2KV9TargetInfo::fillValidCPUList(
    SmallVectorImpl<StringRef> &Values) const {
}
