//===--- E2K.h - declare E2K target feature support ---------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file declares E2K TargetInfo objects.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_LIB_BASIC_TARGETS_E2K_H
#define LLVM_CLANG_LIB_BASIC_TARGETS_E2K_H
#include "clang/Basic/TargetInfo.h"
#include "clang/Basic/TargetOptions.h"
#include "llvm/ADT/Triple.h"
#include "llvm/Support/Compiler.h"
namespace clang {
namespace targets {
// Shared base class for E2K v8 (32-bit) and E2K v9 (64-bit).
class LLVM_LIBRARY_VISIBILITY E2KTargetInfo : public TargetInfo {
  static const TargetInfo::GCCRegAlias GCCRegAliases[];
  static const char *const GCCRegNames[];

public:
  E2KTargetInfo(const llvm::Triple &Triple, const TargetOptions &)
      : TargetInfo(Triple) {

    // long double is always 8 byte length with 8 byte alignment (IEEE 754-1985)
    LongDoubleWidth = 128;
    LongDoubleAlign = 128;
    LongDoubleFormat = &llvm::APFloat::IEEEdouble();

    SizeType = UnsignedInt;
    IntPtrType = SignedInt;
    PtrDiffType = SignedInt;
    IntMaxType = SignedLongLong;
    Int64Type = IntMaxType;
    WCharType = SignedInt;

    SuitableAlign = 128;

    MaxAtomicPromoteWidth = MaxAtomicInlineWidth = 64;
  }

  int getEHDataRegisterNumber(unsigned RegNo) const override {
    if (RegNo == 0)
      return 24;
    if (RegNo == 1)
      return 25;
    return -1;
  }

  bool handleTargetFeatures(std::vector<std::string> &Features,
                            DiagnosticsEngine &Diags) override {
    return true;
  }
  void getTargetDefines(const LangOptions &Opts,
                        MacroBuilder &Builder) const override;

  bool hasFeature(StringRef Feature) const override;

  ArrayRef<Builtin::Info> getTargetBuiltins() const override {
    // FIXME: Implement!
    return std::nullopt;
  }
  BuiltinVaListKind getBuiltinVaListKind() const override {
    return TargetInfo::VoidPtrBuiltinVaList;
  }
  ArrayRef<const char *> getGCCRegNames() const override;
  ArrayRef<TargetInfo::GCCRegAlias> getGCCRegAliases() const override;
  bool validateAsmConstraint(const char *&Name,
                             TargetInfo::ConstraintInfo &info) const override {
    // FIXME: Implement!
    switch (*Name) {
    case 'I': // Signed 13-bit constant
    case 'J': // Zero
    case 'K': // 32-bit constant with the low 12 bits clear
    case 'L': // A constant in the range supported by movcc (11-bit signed imm)
    case 'M': // A constant in the range supported by movrcc (19-bit signed imm)
    case 'N': // Same as 'K' but zext (required for SIMode)
    case 'O': // The constant 4096
      return true;

    case 'f':
    case 'e':
      info.setAllowsRegister();
      return true;
    }
    return false;
  }
  const char *getClobbers() const override {
    // FIXME: Implement!
    return "";
  }

  enum CPUKind {
    CK_GENERIC,
    CK_V1,
    CK_V2,
    CK_V3,
    CK_V4,
    CK_V5,
    CK_V6,
    CK_V7,

    CK_Elbrus,

    CK_ElbrusS,
    CK_Elbrus3S,      // same as CK_ElbrusS
    CK_Elbrus2C,      // same as CK_ElbrusS
    CK_Elbrus2C1,     // same as CK_ElbrusS

    CK_Elbrus1CPlus,
    CK_Elbrus4C1Plus, // same as CK_Elbrus1CPlus

    CK_Elbrus1CK,
    CK_Elbrus1CHK,    // same as CK_Elbrus1CK

    CK_Elbrus2CPlus,
    CK_Elbrus2C2,     // same as CK_Elbrus2CPlus
    CK_ElbrusS2,      // same as CK_Elbrus2CPlus
    CK_ElbrusSX2,     // same as CK_Elbrus2CPlus
    CK_Elbrus3S2,     // same as CK_Elbrus2CPlus

    CK_Elbrus2CM,
    CK_Elbrus1C,      // same as CK_Elbrus2CM

    CK_Elbrus2S3,
    CK_Elbrus2C3,     // same as CK_Elbrus2S3

    CK_Elbrus4C,
    CK_Elbrus2S,      // same as CK_Elbrus4C
    CK_Elbrus2S4M,    // same as CK_Elbrus4C
    CK_Elbrus3C4,     // same as CK_Elbrus4C

    CK_Elbrus8C,
    CK_Elbrus4CPlus,  // same as CK_Elbrus8C
    CK_Elbrus4C8,     // same as CK_Elbrus8C
    CK_Elbrus4S,      // same as CK_Elbrus8C

    CK_Elbrus8C1,

    CK_Elbrus8SV,
    CK_Elbrus8C2,     // same as CK_Elbrus8SV
    CK_Elbrus8CB,     // same as CK_Elbrus8SV

    CK_Elbrus12S,
    CK_Elbrus12C,     // same as CK_Elbrus12S

    CK_Elbrus16S,
    CK_Elbrus16C,     // same as CK_Elbrus16S

    CK_Elbrus32S,
    CK_Elbrus32C,     // same as CK_Elbrus32S

  } CPU = CK_GENERIC;

  enum CPUGeneration {
    CG_V1 = 1,
    CG_V2,
    CG_V3,
    CG_V4,
    CG_V5,
    CG_V6,
    CG_V7
  };

  CPUGeneration getCPUGeneration(CPUKind Kind) const;
  StringRef getCPUName(CPUKind Kind) const;

  CPUKind getCPUKind(StringRef Name) const;

  bool isValidCPUName(StringRef Name) const override {
    return getCPUKind(Name) != CK_GENERIC;
  }

  void fillValidCPUList(SmallVectorImpl<StringRef> &Values) const override;

  bool setCPU(const std::string &Name) override {
    CPU = getCPUKind(Name);
    return CPU != CK_GENERIC;
  }
};

// E2K32 is the 32-bit mode selected by Triple::e2k32.
class LLVM_LIBRARY_VISIBILITY E2K32TargetInfo : public E2KTargetInfo {
public:
  E2K32TargetInfo(const llvm::Triple &Triple, const TargetOptions &Opts)
      : E2KTargetInfo(Triple, Opts) {
    resetDataLayout("e-m:e-p:32:32-i1:8:8-i8:8-i16:16-i32:32-i64:64-i128:128-f32:32-f64:64-f128:128-n8:16:32:64:128-S64");

    LongWidth = LongAlign = 32;
    PointerWidth = PointerAlign = 32;
  }

  void getTargetDefines(const LangOptions &Opts,
                        MacroBuilder &Builder) const override;

  bool hasBitIntType() const override { return true; }
};

// E2K64 is the 64-bit mode selected by Triple::e2k64.
class LLVM_LIBRARY_VISIBILITY E2K64TargetInfo : public E2KTargetInfo {
public:
  E2K64TargetInfo(const llvm::Triple &Triple, const TargetOptions &Opts)
      : E2KTargetInfo(Triple, Opts) {

    resetDataLayout("e-m:e-p:64:64-i1:8:8-i8:8-i16:16-i32:32-i64:64-i128:128-f32:32-f64:64-f128:128-n8:16:32:64:128-S64");

    LongWidth = LongAlign = 64;
    PointerWidth = PointerAlign = 64;
  }

  void getTargetDefines(const LangOptions &Opts,
                        MacroBuilder &Builder) const override;

  bool hasBitIntType() const override { return true; }
};

// E2K128 is the 128-bit mode selected by Triple::e2k128.
class LLVM_LIBRARY_VISIBILITY E2K128TargetInfo : public E2KTargetInfo {
public:
  E2K128TargetInfo(const llvm::Triple &Triple, const TargetOptions &Opts)
      : E2KTargetInfo(Triple, Opts) {
    resetDataLayout("e-m:e-p:128:128-i1:8:8-i8:8-i16:16-i32:32-i64:64-i128:128-f32:32-f64:64-f128:128-n8:16:32:64:128-S64");

    LongWidth = LongAlign = 64;
    PointerWidth = PointerAlign = 128;
  }

  void getTargetDefines(const LangOptions &Opts,
                        MacroBuilder &Builder) const override;

  bool hasBitIntType() const override { return true; }
};

// E2K128_64 is the 64-bit/128-bit mixed mode selected by Triple::e2k128_64.
class LLVM_LIBRARY_VISIBILITY E2K12864TargetInfo : public E2K128TargetInfo {
public:
  E2K12864TargetInfo(const llvm::Triple &Triple, const TargetOptions &Opts)
      : E2K128TargetInfo(Triple, Opts) {
  }

  void getTargetDefines(const LangOptions &Opts,
                        MacroBuilder &Builder) const override;

  bool hasBitIntType() const override { return true; }
};

} // namespace targets
} // namespace clang
#endif // LLVM_CLANG_LIB_BASIC_TARGETS_E2K_H
