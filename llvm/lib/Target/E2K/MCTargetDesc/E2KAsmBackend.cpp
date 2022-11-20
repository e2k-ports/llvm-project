//===-- E2KAsmBackend.cpp - E2K Assembler Backend ---------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/E2KFixupKinds.h"
#include "MCTargetDesc/E2KMCTargetDesc.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/MC/MCAsmBackend.h"
#include "llvm/MC/MCELFObjectWriter.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCFixupKindInfo.h"
#include "llvm/MC/MCObjectWriter.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCValue.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/EndianStream.h"

using namespace llvm;

static unsigned adjustFixupValue(unsigned Kind, uint64_t Value) {
  switch (Kind) {
  default:
    llvm_unreachable("Unknown fixup kind!");
  case FK_Data_1:
  case FK_Data_2:
  case FK_Data_4:
  case FK_Data_8:
    return Value;

  case E2K::fixup_e2k_wplt30:
  case E2K::fixup_e2k_call30:
    return (Value >> 2) & 0x3fffffff;

  case E2K::fixup_e2k_br22:
    return (Value >> 2) & 0x3fffff;

  case E2K::fixup_e2k_br19:
    return (Value >> 2) & 0x7ffff;

  case E2K::fixup_e2k_br16_2:
    return (Value >> 2) & 0xc000;

  case E2K::fixup_e2k_br16_14:
    return (Value >> 2) & 0x3fff;

  case E2K::fixup_e2k_hix22:
    return (~Value >> 10) & 0x3fffff;

  case E2K::fixup_e2k_pc22:
  case E2K::fixup_e2k_got22:
  case E2K::fixup_e2k_tls_gd_hi22:
  case E2K::fixup_e2k_tls_ldm_hi22:
  case E2K::fixup_e2k_tls_ie_hi22:
  case E2K::fixup_e2k_hi22:
  case E2K::fixup_e2k_lm:
    return (Value >> 10) & 0x3fffff;

  case E2K::fixup_e2k_got13:
  case E2K::fixup_e2k_13:
    return Value & 0x1fff;

  case E2K::fixup_e2k_lox10:
    return (Value & 0x3ff) | 0x1c00;

  case E2K::fixup_e2k_pc10:
  case E2K::fixup_e2k_got10:
  case E2K::fixup_e2k_tls_gd_lo10:
  case E2K::fixup_e2k_tls_ldm_lo10:
  case E2K::fixup_e2k_tls_ie_lo10:
  case E2K::fixup_e2k_lo10:
    return Value & 0x3ff;

  case E2K::fixup_e2k_h44:
    return (Value >> 22) & 0x3fffff;

  case E2K::fixup_e2k_m44:
    return (Value >> 12) & 0x3ff;

  case E2K::fixup_e2k_l44:
    return Value & 0xfff;

  case E2K::fixup_e2k_hh:
    return (Value >> 42) & 0x3fffff;

  case E2K::fixup_e2k_hm:
    return (Value >> 32) & 0x3ff;

  case E2K::fixup_e2k_tls_ldo_hix22:
  case E2K::fixup_e2k_tls_le_hix22:
  case E2K::fixup_e2k_tls_ldo_lox10:
  case E2K::fixup_e2k_tls_le_lox10:
    assert(Value == 0 && "E2K TLS relocs expect zero Value");
    return 0;

  case E2K::fixup_e2k_tls_gd_add:
  case E2K::fixup_e2k_tls_gd_call:
  case E2K::fixup_e2k_tls_ldm_add:
  case E2K::fixup_e2k_tls_ldm_call:
  case E2K::fixup_e2k_tls_ldo_add:
  case E2K::fixup_e2k_tls_ie_ld:
  case E2K::fixup_e2k_tls_ie_ldx:
  case E2K::fixup_e2k_tls_ie_add:
  case E2K::fixup_e2k_gotdata_lox10:
  case E2K::fixup_e2k_gotdata_hix22:
  case E2K::fixup_e2k_gotdata_op:
    return 0;
  }
}

/// getFixupKindNumBytes - The number of bytes the fixup may change.
static unsigned getFixupKindNumBytes(unsigned Kind) {
    switch (Kind) {
  default:
    return 4;
  case FK_Data_1:
    return 1;
  case FK_Data_2:
    return 2;
  case FK_Data_8:
    return 8;
  }
}

namespace {
  class E2KAsmBackend : public MCAsmBackend {
  protected:
    const Target &TheTarget;
    bool Is64Bit;

  public:
    E2KAsmBackend(const Target &T)
        : MCAsmBackend(StringRef(T.getName()) == "e2kel" ? support::little
                                                           : support::big),
          TheTarget(T), Is64Bit(StringRef(TheTarget.getName()) == "e2kv9") {}

    unsigned getNumFixupKinds() const override {
      return E2K::NumTargetFixupKinds;
    }

    std::optional<MCFixupKind> getFixupKind(StringRef Name) const override {
      unsigned Type;
      Type = llvm::StringSwitch<unsigned>(Name)
#define ELF_RELOC(X, Y) .Case(#X, Y)
#include "llvm/BinaryFormat/ELFRelocs/E2K.def"
#undef ELF_RELOC
                 .Case("BFD_RELOC_NONE", ELF::R_E2K_NONE)
                 .Case("BFD_RELOC_8", ELF::R_E2K_8)
                 .Case("BFD_RELOC_16", ELF::R_E2K_16)
                 .Case("BFD_RELOC_32", ELF::R_E2K_32)
                 .Case("BFD_RELOC_64", ELF::R_E2K_64)
                 .Default(-1u);
      if (Type == -1u)
        return std::nullopt;
      return static_cast<MCFixupKind>(FirstLiteralRelocationKind + Type);
    }

    const MCFixupKindInfo &getFixupKindInfo(MCFixupKind Kind) const override {
      const static MCFixupKindInfo InfosBE[E2K::NumTargetFixupKinds] = {
        // name                    offset bits  flags
        { "fixup_e2k_call30",     2,     30,  MCFixupKindInfo::FKF_IsPCRel },
        { "fixup_e2k_br22",      10,     22,  MCFixupKindInfo::FKF_IsPCRel },
        { "fixup_e2k_br19",      13,     19,  MCFixupKindInfo::FKF_IsPCRel },
        { "fixup_e2k_br16_2",    10,      2,  MCFixupKindInfo::FKF_IsPCRel },
        { "fixup_e2k_br16_14",   18,     14,  MCFixupKindInfo::FKF_IsPCRel },
        { "fixup_e2k_13",        19,     13,  0 },
        { "fixup_e2k_hi22",      10,     22,  0 },
        { "fixup_e2k_lo10",      22,     10,  0 },
        { "fixup_e2k_h44",       10,     22,  0 },
        { "fixup_e2k_m44",       22,     10,  0 },
        { "fixup_e2k_l44",       20,     12,  0 },
        { "fixup_e2k_hh",        10,     22,  0 },
        { "fixup_e2k_hm",        22,     10,  0 },
        { "fixup_e2k_lm",        10,     22,  0 },
        { "fixup_e2k_pc22",      10,     22,  MCFixupKindInfo::FKF_IsPCRel },
        { "fixup_e2k_pc10",      22,     10,  MCFixupKindInfo::FKF_IsPCRel },
        { "fixup_e2k_got22",     10,     22,  0 },
        { "fixup_e2k_got10",     22,     10,  0 },
        { "fixup_e2k_got13",     19,     13,  0 },
        { "fixup_e2k_wplt30",     2,     30,  MCFixupKindInfo::FKF_IsPCRel },
        { "fixup_e2k_tls_gd_hi22",   10, 22,  0 },
        { "fixup_e2k_tls_gd_lo10",   22, 10,  0 },
        { "fixup_e2k_tls_gd_add",     0,  0,  0 },
        { "fixup_e2k_tls_gd_call",    0,  0,  0 },
        { "fixup_e2k_tls_ldm_hi22",  10, 22,  0 },
        { "fixup_e2k_tls_ldm_lo10",  22, 10,  0 },
        { "fixup_e2k_tls_ldm_add",    0,  0,  0 },
        { "fixup_e2k_tls_ldm_call",   0,  0,  0 },
        { "fixup_e2k_tls_ldo_hix22", 10, 22,  0 },
        { "fixup_e2k_tls_ldo_lox10", 22, 10,  0 },
        { "fixup_e2k_tls_ldo_add",    0,  0,  0 },
        { "fixup_e2k_tls_ie_hi22",   10, 22,  0 },
        { "fixup_e2k_tls_ie_lo10",   22, 10,  0 },
        { "fixup_e2k_tls_ie_ld",      0,  0,  0 },
        { "fixup_e2k_tls_ie_ldx",     0,  0,  0 },
        { "fixup_e2k_tls_ie_add",     0,  0,  0 },
        { "fixup_e2k_tls_le_hix22",   0,  0,  0 },
        { "fixup_e2k_tls_le_lox10",   0,  0,  0 },
        { "fixup_e2k_hix22",         10, 22,  0 },
        { "fixup_e2k_lox10",         19, 13,  0 },
        { "fixup_e2k_gotdata_hix22",  0,  0,  0 },
        { "fixup_e2k_gotdata_lox10",  0,  0,  0 },
        { "fixup_e2k_gotdata_op",     0,  0,  0 },
      };

      const static MCFixupKindInfo InfosLE[E2K::NumTargetFixupKinds] = {
        // name                    offset bits  flags
        { "fixup_e2k_call30",     0,     30,  MCFixupKindInfo::FKF_IsPCRel },
        { "fixup_e2k_br22",       0,     22,  MCFixupKindInfo::FKF_IsPCRel },
        { "fixup_e2k_br19",       0,     19,  MCFixupKindInfo::FKF_IsPCRel },
        { "fixup_e2k_br16_2",    20,      2,  MCFixupKindInfo::FKF_IsPCRel },
        { "fixup_e2k_br16_14",    0,     14,  MCFixupKindInfo::FKF_IsPCRel },
        { "fixup_e2k_13",         0,     13,  0 },
        { "fixup_e2k_hi22",       0,     22,  0 },
        { "fixup_e2k_lo10",       0,     10,  0 },
        { "fixup_e2k_h44",        0,     22,  0 },
        { "fixup_e2k_m44",        0,     10,  0 },
        { "fixup_e2k_l44",        0,     12,  0 },
        { "fixup_e2k_hh",         0,     22,  0 },
        { "fixup_e2k_hm",         0,     10,  0 },
        { "fixup_e2k_lm",         0,     22,  0 },
        { "fixup_e2k_pc22",       0,     22,  MCFixupKindInfo::FKF_IsPCRel },
        { "fixup_e2k_pc10",       0,     10,  MCFixupKindInfo::FKF_IsPCRel },
        { "fixup_e2k_got22",      0,     22,  0 },
        { "fixup_e2k_got10",      0,     10,  0 },
        { "fixup_e2k_got13",      0,     13,  0 },
        { "fixup_e2k_wplt30",      0,     30,  MCFixupKindInfo::FKF_IsPCRel },
        { "fixup_e2k_tls_gd_hi22",    0, 22,  0 },
        { "fixup_e2k_tls_gd_lo10",    0, 10,  0 },
        { "fixup_e2k_tls_gd_add",     0,  0,  0 },
        { "fixup_e2k_tls_gd_call",    0,  0,  0 },
        { "fixup_e2k_tls_ldm_hi22",   0, 22,  0 },
        { "fixup_e2k_tls_ldm_lo10",   0, 10,  0 },
        { "fixup_e2k_tls_ldm_add",    0,  0,  0 },
        { "fixup_e2k_tls_ldm_call",   0,  0,  0 },
        { "fixup_e2k_tls_ldo_hix22",  0, 22,  0 },
        { "fixup_e2k_tls_ldo_lox10",  0, 10,  0 },
        { "fixup_e2k_tls_ldo_add",    0,  0,  0 },
        { "fixup_e2k_tls_ie_hi22",    0, 22,  0 },
        { "fixup_e2k_tls_ie_lo10",    0, 10,  0 },
        { "fixup_e2k_tls_ie_ld",      0,  0,  0 },
        { "fixup_e2k_tls_ie_ldx",     0,  0,  0 },
        { "fixup_e2k_tls_ie_add",     0,  0,  0 },
        { "fixup_e2k_tls_le_hix22",   0,  0,  0 },
        { "fixup_e2k_tls_le_lox10",   0,  0,  0 },
        { "fixup_e2k_hix22",          0, 22,  0 },
        { "fixup_e2k_lox10",          0, 13,  0 },
        { "fixup_e2k_gotdata_hix22",  0,  0,  0 },
        { "fixup_e2k_gotdata_lox10",  0,  0,  0 },
        { "fixup_e2k_gotdata_op",     0,  0,  0 },
      };

      // Fixup kinds from .reloc directive are like R_E2K_NONE. They do
      // not require any extra processing.
      if (Kind >= FirstLiteralRelocationKind)
        return MCAsmBackend::getFixupKindInfo(FK_NONE);

      if (Kind < FirstTargetFixupKind)
        return MCAsmBackend::getFixupKindInfo(Kind);

      assert(unsigned(Kind - FirstTargetFixupKind) < getNumFixupKinds() &&
             "Invalid kind!");
      if (Endian == support::little)
        return InfosLE[Kind - FirstTargetFixupKind];

      return InfosBE[Kind - FirstTargetFixupKind];
    }

    bool shouldForceRelocation(const MCAssembler &Asm, const MCFixup &Fixup,
                               const MCValue &Target) override {
      if (Fixup.getKind() >= FirstLiteralRelocationKind)
        return true;
      switch ((E2K::Fixups)Fixup.getKind()) {
      default:
        return false;
      case E2K::fixup_e2k_wplt30:
        if (Target.getSymA()->getSymbol().isTemporary())
          return false;
        [[fallthrough]];
      case E2K::fixup_e2k_tls_gd_hi22:
      case E2K::fixup_e2k_tls_gd_lo10:
      case E2K::fixup_e2k_tls_gd_add:
      case E2K::fixup_e2k_tls_gd_call:
      case E2K::fixup_e2k_tls_ldm_hi22:
      case E2K::fixup_e2k_tls_ldm_lo10:
      case E2K::fixup_e2k_tls_ldm_add:
      case E2K::fixup_e2k_tls_ldm_call:
      case E2K::fixup_e2k_tls_ldo_hix22:
      case E2K::fixup_e2k_tls_ldo_lox10:
      case E2K::fixup_e2k_tls_ldo_add:
      case E2K::fixup_e2k_tls_ie_hi22:
      case E2K::fixup_e2k_tls_ie_lo10:
      case E2K::fixup_e2k_tls_ie_ld:
      case E2K::fixup_e2k_tls_ie_ldx:
      case E2K::fixup_e2k_tls_ie_add:
      case E2K::fixup_e2k_tls_le_hix22:
      case E2K::fixup_e2k_tls_le_lox10:
        return true;
      }
    }

    /// fixupNeedsRelaxation - Target specific predicate for whether a given
    /// fixup requires the associated instruction to be relaxed.
    bool fixupNeedsRelaxation(const MCFixup &Fixup,
                              uint64_t Value,
                              const MCRelaxableFragment *DF,
                              const MCAsmLayout &Layout) const override {
      // FIXME.
      llvm_unreachable("fixupNeedsRelaxation() unimplemented");
      return false;
    }
    void relaxInstruction(MCInst &Inst,
                          const MCSubtargetInfo &STI) const override {
      // FIXME.
      llvm_unreachable("relaxInstruction() unimplemented");
    }

    bool writeNopData(raw_ostream &OS, uint64_t Count,
                      const MCSubtargetInfo *STI) const override {
      // Cannot emit NOP with size not multiple of 32 bits.
      if (Count % 4 != 0)
        return false;

      uint64_t NumNops = Count / 4;
      for (uint64_t i = 0; i != NumNops; ++i)
        support::endian::write<uint32_t>(OS, 0x01000000, Endian);

      return true;
    }
  };

  class ELFE2KAsmBackend : public E2KAsmBackend {
    Triple::OSType OSType;
  public:
    ELFE2KAsmBackend(const Target &T, Triple::OSType OSType) :
      E2KAsmBackend(T), OSType(OSType) { }

    void applyFixup(const MCAssembler &Asm, const MCFixup &Fixup,
                    const MCValue &Target, MutableArrayRef<char> Data,
                    uint64_t Value, bool IsResolved,
                    const MCSubtargetInfo *STI) const override {

      if (Fixup.getKind() >= FirstLiteralRelocationKind)
        return;
      Value = adjustFixupValue(Fixup.getKind(), Value);
      if (!Value) return;           // Doesn't change encoding.

      unsigned NumBytes = getFixupKindNumBytes(Fixup.getKind());
      unsigned Offset = Fixup.getOffset();
      // For each byte of the fragment that the fixup touches, mask in the bits
      // from the fixup value. The Value has been "split up" into the
      // appropriate bitfields above.
      for (unsigned i = 0; i != NumBytes; ++i) {
        unsigned Idx = Endian == support::little ? i : (NumBytes - 1) - i;
        Data[Offset + Idx] |= uint8_t((Value >> (i * 8)) & 0xff);
      }
    }

    std::unique_ptr<MCObjectTargetWriter>
    createObjectTargetWriter() const override {
      uint8_t OSABI = MCELFObjectTargetWriter::getOSABI(OSType);
      return createE2KELFObjectWriter(Is64Bit, OSABI);
    }
  };

} // end anonymous namespace

MCAsmBackend *llvm::createE2KAsmBackend(const Target &T,
                                          const MCSubtargetInfo &STI,
                                          const MCRegisterInfo &MRI,
                                          const MCTargetOptions &Options) {
  return new ELFE2KAsmBackend(T, STI.getTargetTriple().getOS());
}
