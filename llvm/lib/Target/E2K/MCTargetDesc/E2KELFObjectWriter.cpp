//===-- E2KELFObjectWriter.cpp - E2K ELF Writer -----------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/E2KFixupKinds.h"
#include "MCTargetDesc/E2KMCExpr.h"
#include "MCTargetDesc/E2KMCTargetDesc.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/MC/MCELFObjectWriter.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCObjectWriter.h"
#include "llvm/MC/MCValue.h"
#include "llvm/Support/ErrorHandling.h"

using namespace llvm;

namespace {
  class E2KELFObjectWriter : public MCELFObjectTargetWriter {
  public:
    E2KELFObjectWriter(bool Is64Bit, uint8_t OSABI)
      : MCELFObjectTargetWriter(Is64Bit, OSABI,
                                ELF::EM_MCST_ELBRUS,
                                /*HasRelocationAddend*/ true) {}

    ~E2KELFObjectWriter() override = default;

  protected:
    unsigned getRelocType(MCContext &Ctx, const MCValue &Target,
                          const MCFixup &Fixup, bool IsPCRel) const override;

    bool needsRelocateWithSymbol(const MCSymbol &Sym,
                                 unsigned Type) const override;

  };
}

unsigned E2KELFObjectWriter::getRelocType(MCContext &Ctx,
                                            const MCValue &Target,
                                            const MCFixup &Fixup,
                                            bool IsPCRel) const {
  MCFixupKind Kind = Fixup.getKind();
  if (Kind >= FirstLiteralRelocationKind)
    return Kind - FirstLiteralRelocationKind;

  if (const E2KMCExpr *SExpr = dyn_cast<E2KMCExpr>(Fixup.getValue())) {
    if (SExpr->getKind() == E2KMCExpr::VK_E2K_R_DISP32)
      return ELF::R_E2K_DISP32;
  }

  if (IsPCRel) {
    switch(Fixup.getTargetKind()) {
    default:
      llvm_unreachable("Unimplemented fixup -> relocation");
    case FK_Data_1:                  return ELF::R_E2K_DISP8;
    case FK_Data_2:                  return ELF::R_E2K_DISP16;
    case FK_Data_4:                  return ELF::R_E2K_DISP32;
    case FK_Data_8:                  return ELF::R_E2K_DISP64;
    case E2K::fixup_e2k_call30:  return ELF::R_E2K_WDISP30;
    case E2K::fixup_e2k_br22:    return ELF::R_E2K_WDISP22;
    case E2K::fixup_e2k_br19:    return ELF::R_E2K_WDISP19;
    case E2K::fixup_e2k_pc22:    return ELF::R_E2K_PC22;
    case E2K::fixup_e2k_pc10:    return ELF::R_E2K_PC10;
    case E2K::fixup_e2k_wplt30:  return ELF::R_E2K_WPLT30;
    }
  }

  switch(Fixup.getTargetKind()) {
  default:
    llvm_unreachable("Unimplemented fixup -> relocation");
  case FK_NONE:                  return ELF::R_E2K_NONE;
  case FK_Data_1:                return ELF::R_E2K_8;
  case FK_Data_2:                return ((Fixup.getOffset() % 2)
                                         ? ELF::R_E2K_UA16
                                         : ELF::R_E2K_16);
  case FK_Data_4:                return ((Fixup.getOffset() % 4)
                                         ? ELF::R_E2K_UA32
                                         : ELF::R_E2K_32);
  case FK_Data_8:                return ((Fixup.getOffset() % 8)
                                         ? ELF::R_E2K_UA64
                                         : ELF::R_E2K_64);
  case E2K::fixup_e2k_13:    return ELF::R_E2K_13;
  case E2K::fixup_e2k_hi22:  return ELF::R_E2K_HI22;
  case E2K::fixup_e2k_lo10:  return ELF::R_E2K_LO10;
  case E2K::fixup_e2k_h44:   return ELF::R_E2K_H44;
  case E2K::fixup_e2k_m44:   return ELF::R_E2K_M44;
  case E2K::fixup_e2k_l44:   return ELF::R_E2K_L44;
  case E2K::fixup_e2k_hh:    return ELF::R_E2K_HH22;
  case E2K::fixup_e2k_hm:    return ELF::R_E2K_HM10;
  case E2K::fixup_e2k_lm:    return ELF::R_E2K_LM22;
  case E2K::fixup_e2k_got22: return ELF::R_E2K_GOT22;
  case E2K::fixup_e2k_got10: return ELF::R_E2K_GOT10;
  case E2K::fixup_e2k_got13: return ELF::R_E2K_GOT13;
  case E2K::fixup_e2k_tls_gd_hi22:   return ELF::R_E2K_TLS_GD_HI22;
  case E2K::fixup_e2k_tls_gd_lo10:   return ELF::R_E2K_TLS_GD_LO10;
  case E2K::fixup_e2k_tls_gd_add:    return ELF::R_E2K_TLS_GD_ADD;
  case E2K::fixup_e2k_tls_gd_call:   return ELF::R_E2K_TLS_GD_CALL;
  case E2K::fixup_e2k_tls_ldm_hi22:  return ELF::R_E2K_TLS_LDM_HI22;
  case E2K::fixup_e2k_tls_ldm_lo10:  return ELF::R_E2K_TLS_LDM_LO10;
  case E2K::fixup_e2k_tls_ldm_add:   return ELF::R_E2K_TLS_LDM_ADD;
  case E2K::fixup_e2k_tls_ldm_call:  return ELF::R_E2K_TLS_LDM_CALL;
  case E2K::fixup_e2k_tls_ldo_hix22: return ELF::R_E2K_TLS_LDO_HIX22;
  case E2K::fixup_e2k_tls_ldo_lox10: return ELF::R_E2K_TLS_LDO_LOX10;
  case E2K::fixup_e2k_tls_ldo_add:   return ELF::R_E2K_TLS_LDO_ADD;
  case E2K::fixup_e2k_tls_ie_hi22:   return ELF::R_E2K_TLS_IE_HI22;
  case E2K::fixup_e2k_tls_ie_lo10:   return ELF::R_E2K_TLS_IE_LO10;
  case E2K::fixup_e2k_tls_ie_ld:     return ELF::R_E2K_TLS_IE_LD;
  case E2K::fixup_e2k_tls_ie_ldx:    return ELF::R_E2K_TLS_IE_LDX;
  case E2K::fixup_e2k_tls_ie_add:    return ELF::R_E2K_TLS_IE_ADD;
  case E2K::fixup_e2k_tls_le_hix22:  return ELF::R_E2K_TLS_LE_HIX22;
  case E2K::fixup_e2k_tls_le_lox10:  return ELF::R_E2K_TLS_LE_LOX10;
  case E2K::fixup_e2k_hix22:         return ELF::R_E2K_HIX22;
  case E2K::fixup_e2k_lox10:         return ELF::R_E2K_LOX10;
  case E2K::fixup_e2k_gotdata_hix22: return ELF::R_E2K_GOTDATA_HIX22;
  case E2K::fixup_e2k_gotdata_lox10: return ELF::R_E2K_GOTDATA_LOX10;
  case E2K::fixup_e2k_gotdata_op:    return ELF::R_E2K_GOTDATA_OP;
  }

  return ELF::R_E2K_NONE;
}

bool E2KELFObjectWriter::needsRelocateWithSymbol(const MCSymbol &Sym,
                                                 unsigned Type) const {
  switch (Type) {
    default:
      return false;

    // All relocations that use a GOT need a symbol, not an offset, as
    // the offset of the symbol within the section is irrelevant to
    // where the GOT entry is. Don't need to list all the TLS entries,
    // as they're all marked as requiring a symbol anyways.
    case ELF::R_E2K_GOT10:
    case ELF::R_E2K_GOT13:
    case ELF::R_E2K_GOT22:
    case ELF::R_E2K_GOTDATA_HIX22:
    case ELF::R_E2K_GOTDATA_LOX10:
    case ELF::R_E2K_GOTDATA_OP_HIX22:
    case ELF::R_E2K_GOTDATA_OP_LOX10:
      return true;
  }
}

std::unique_ptr<MCObjectTargetWriter>
llvm::createE2KELFObjectWriter(bool Is64Bit, uint8_t OSABI) {
  return std::make_unique<E2KELFObjectWriter>(Is64Bit, OSABI);
}
