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
  bool SoftFloat;

public:
  E2KTargetInfo(const llvm::Triple &Triple, const TargetOptions &)
      : TargetInfo(Triple), SoftFloat(false) {}

  int getEHDataRegisterNumber(unsigned RegNo) const override {
    if (RegNo == 0)
      return 24;
    if (RegNo == 1)
      return 25;
    return -1;
  }

  bool handleTargetFeatures(std::vector<std::string> &Features,
                            DiagnosticsEngine &Diags) override {
    // Check if software floating point is enabled
    if (llvm::is_contained(Features, "+soft-float"))
      SoftFloat = true;
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

  // No E2K V7 for now, the backend doesn't support it anyway.
  enum CPUKind {
    CK_GENERIC,
    CK_V8,
    CK_SUPERE2K,
    CK_E2KLITE,
    CK_F934,
    CK_HYPERE2K,
    CK_E2KLITE86X,
    CK_E2KLET,
    CK_TSC701,
    CK_V9,
    CK_ULTRAE2K,
    CK_ULTRAE2K3,
    CK_NIAGARA,
    CK_NIAGARA2,
    CK_NIAGARA3,
    CK_NIAGARA4,
    CK_MYRIAD2100,
    CK_MYRIAD2150,
    CK_MYRIAD2155,
    CK_MYRIAD2450,
    CK_MYRIAD2455,
    CK_MYRIAD2x5x,
    CK_MYRIAD2080,
    CK_MYRIAD2085,
    CK_MYRIAD2480,
    CK_MYRIAD2485,
    CK_MYRIAD2x8x,
  } CPU = CK_GENERIC;

  enum CPUGeneration {
    CG_V8,
    CG_V9,
  };

  CPUGeneration getCPUGeneration(CPUKind Kind) const;

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

// E2K v8 is the 32-bit mode selected by Triple::e2k.
class LLVM_LIBRARY_VISIBILITY E2KV8TargetInfo : public E2KTargetInfo {
public:
  E2KV8TargetInfo(const llvm::Triple &Triple, const TargetOptions &Opts)
      : E2KTargetInfo(Triple, Opts) {
    resetDataLayout("E-m:e-p:32:32-i64:64-f128:64-n32-S64");
    // NetBSD / OpenBSD use long (same as llvm default); everyone else uses int.
    switch (getTriple().getOS()) {
    default:
      SizeType = UnsignedInt;
      IntPtrType = SignedInt;
      PtrDiffType = SignedInt;
      break;
    case llvm::Triple::NetBSD:
    case llvm::Triple::OpenBSD:
      SizeType = UnsignedLong;
      IntPtrType = SignedLong;
      PtrDiffType = SignedLong;
      break;
    }
    // Up to 32 bits (V8) or 64 bits (V9) are lock-free atomic, but we're
    // willing to do atomic ops on up to 64 bits.
    MaxAtomicPromoteWidth = 64;
    if (getCPUGeneration(CPU) == CG_V9)
      MaxAtomicInlineWidth = 64;
    else
      // FIXME: This isn't correct for plain V8 which lacks CAS,
      // only for Myriad.
      MaxAtomicInlineWidth = 32;
  }

  void getTargetDefines(const LangOptions &Opts,
                        MacroBuilder &Builder) const override;

  bool hasBitIntType() const override { return true; }
};

// E2KV8el is the 32-bit little-endian mode selected by Triple::e2kel.
class LLVM_LIBRARY_VISIBILITY E2KV8elTargetInfo : public E2KV8TargetInfo {
public:
  E2KV8elTargetInfo(const llvm::Triple &Triple, const TargetOptions &Opts)
      : E2KV8TargetInfo(Triple, Opts) {
    resetDataLayout("e-m:e-p:32:32-i64:64-f128:64-n32-S64");
  }
};

// E2K v9 is the 64-bit mode selected by Triple::e2kv9.
class LLVM_LIBRARY_VISIBILITY E2KV9TargetInfo : public E2KTargetInfo {
public:
  E2KV9TargetInfo(const llvm::Triple &Triple, const TargetOptions &Opts)
      : E2KTargetInfo(Triple, Opts) {
    // FIXME: Support E2K quad-precision long double?
    resetDataLayout("E-m:e-i64:64-n32:64-S128");
    // This is an LP64 platform.
    LongWidth = LongAlign = PointerWidth = PointerAlign = 64;

    // OpenBSD uses long long for int64_t and intmax_t.
    if (getTriple().isOSOpenBSD())
      IntMaxType = SignedLongLong;
    else
      IntMaxType = SignedLong;
    Int64Type = IntMaxType;

    // The E2Kv8 System V ABI has long double 128-bits in size, but 64-bit
    // aligned. The E2Kv9 SCD 2.4.1 says 16-byte aligned.
    LongDoubleWidth = 128;
    LongDoubleAlign = 128;
    SuitableAlign = 128;
    LongDoubleFormat = &llvm::APFloat::IEEEquad();
    MaxAtomicPromoteWidth = MaxAtomicInlineWidth = 64;
  }

  void getTargetDefines(const LangOptions &Opts,
                        MacroBuilder &Builder) const override;

  bool isValidCPUName(StringRef Name) const override {
    return getCPUGeneration(E2KTargetInfo::getCPUKind(Name)) == CG_V9;
  }

  void fillValidCPUList(SmallVectorImpl<StringRef> &Values) const override;

  bool setCPU(const std::string &Name) override {
    if (!E2KTargetInfo::setCPU(Name))
      return false;
    return getCPUGeneration(CPU) == CG_V9;
  }

  bool hasBitIntType() const override { return true; }
};
} // namespace targets
} // namespace clang
#endif // LLVM_CLANG_LIB_BASIC_TARGETS_E2K_H
