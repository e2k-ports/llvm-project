//===-- E2KMCExpr.cpp - E2K specific MC expression classes --------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the implementation of the assembly expression modifiers
// accepted by the E2K architecture (e.g. "%hi", "%lo", ...).
//
//===----------------------------------------------------------------------===//

#include "E2KMCExpr.h"
#include "llvm/BinaryFormat/ELF.h"
#include "llvm/MC/MCAssembler.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCObjectStreamer.h"
#include "llvm/MC/MCSymbolELF.h"
#include "llvm/Support/Casting.h"

using namespace llvm;

#define DEBUG_TYPE "e2kmcexpr"

const E2KMCExpr*
E2KMCExpr::create(VariantKind Kind, const MCExpr *Expr,
                      MCContext &Ctx) {
    return new (Ctx) E2KMCExpr(Kind, Expr);
}

void E2KMCExpr::printImpl(raw_ostream &OS, const MCAsmInfo *MAI) const {

  bool closeParen = printVariantKind(OS, Kind);

  const MCExpr *Expr = getSubExpr();
  Expr->print(OS, MAI);

  if (closeParen)
    OS << ')';
}

bool E2KMCExpr::printVariantKind(raw_ostream &OS, VariantKind Kind)
{
  switch (Kind) {
  case VK_E2K_None:     return false;
  case VK_E2K_LO:       OS << "%lo(";  return true;
  case VK_E2K_HI:       OS << "%hi(";  return true;
  case VK_E2K_H44:      OS << "%h44("; return true;
  case VK_E2K_M44:      OS << "%m44("; return true;
  case VK_E2K_L44:      OS << "%l44("; return true;
  case VK_E2K_HH:       OS << "%hh(";  return true;
  case VK_E2K_HM:       OS << "%hm(";  return true;
  case VK_E2K_LM:       OS << "%lm(";  return true;
    // FIXME: use %pc22/%pc10, if system assembler supports them.
  case VK_E2K_PC22:     OS << "%hi("; return true;
  case VK_E2K_PC10:     OS << "%lo("; return true;
    // FIXME: use %got22/%got10, if system assembler supports them.
  case VK_E2K_GOT22:    OS << "%hi("; return true;
  case VK_E2K_GOT10:    OS << "%lo("; return true;
  case VK_E2K_GOT13:    return false;
  case VK_E2K_13:       return false;
  case VK_E2K_WDISP30:  return false;
  case VK_E2K_WPLT30:   return false;
  case VK_E2K_R_DISP32: OS << "%r_disp32("; return true;
  case VK_E2K_TLS_GD_HI22:   OS << "%tgd_hi22(";   return true;
  case VK_E2K_TLS_GD_LO10:   OS << "%tgd_lo10(";   return true;
  case VK_E2K_TLS_GD_ADD:    OS << "%tgd_add(";    return true;
  case VK_E2K_TLS_GD_CALL:   OS << "%tgd_call(";   return true;
  case VK_E2K_TLS_LDM_HI22:  OS << "%tldm_hi22(";  return true;
  case VK_E2K_TLS_LDM_LO10:  OS << "%tldm_lo10(";  return true;
  case VK_E2K_TLS_LDM_ADD:   OS << "%tldm_add(";   return true;
  case VK_E2K_TLS_LDM_CALL:  OS << "%tldm_call(";  return true;
  case VK_E2K_TLS_LDO_HIX22: OS << "%tldo_hix22("; return true;
  case VK_E2K_TLS_LDO_LOX10: OS << "%tldo_lox10("; return true;
  case VK_E2K_TLS_LDO_ADD:   OS << "%tldo_add(";   return true;
  case VK_E2K_TLS_IE_HI22:   OS << "%tie_hi22(";   return true;
  case VK_E2K_TLS_IE_LO10:   OS << "%tie_lo10(";   return true;
  case VK_E2K_TLS_IE_LD:     OS << "%tie_ld(";     return true;
  case VK_E2K_TLS_IE_LDX:    OS << "%tie_ldx(";    return true;
  case VK_E2K_TLS_IE_ADD:    OS << "%tie_add(";    return true;
  case VK_E2K_TLS_LE_HIX22:  OS << "%tle_hix22(";  return true;
  case VK_E2K_TLS_LE_LOX10:  OS << "%tle_lox10(";  return true;
  case VK_E2K_HIX22:         OS << "%hix(";        return true;
  case VK_E2K_LOX10:         OS << "%lox(";        return true;
  case VK_E2K_GOTDATA_HIX22: OS << "%gdop_hix22("; return true;
  case VK_E2K_GOTDATA_LOX10: OS << "%gdop_lox10("; return true;
  case VK_E2K_GOTDATA_OP:    OS << "%gdop(";       return true;
  }
  llvm_unreachable("Unhandled E2KMCExpr::VariantKind");
}

E2KMCExpr::VariantKind E2KMCExpr::parseVariantKind(StringRef name)
{
  return StringSwitch<E2KMCExpr::VariantKind>(name)
    .Case("lo",  VK_E2K_LO)
    .Case("hi",  VK_E2K_HI)
    .Case("h44", VK_E2K_H44)
    .Case("m44", VK_E2K_M44)
    .Case("l44", VK_E2K_L44)
    .Case("hh",  VK_E2K_HH)
    .Case("hm",  VK_E2K_HM)
    .Case("lm",  VK_E2K_LM)
    .Case("pc22",  VK_E2K_PC22)
    .Case("pc10",  VK_E2K_PC10)
    .Case("got22", VK_E2K_GOT22)
    .Case("got10", VK_E2K_GOT10)
    .Case("got13", VK_E2K_GOT13)
    .Case("r_disp32",   VK_E2K_R_DISP32)
    .Case("tgd_hi22",   VK_E2K_TLS_GD_HI22)
    .Case("tgd_lo10",   VK_E2K_TLS_GD_LO10)
    .Case("tgd_add",    VK_E2K_TLS_GD_ADD)
    .Case("tgd_call",   VK_E2K_TLS_GD_CALL)
    .Case("tldm_hi22",  VK_E2K_TLS_LDM_HI22)
    .Case("tldm_lo10",  VK_E2K_TLS_LDM_LO10)
    .Case("tldm_add",   VK_E2K_TLS_LDM_ADD)
    .Case("tldm_call",  VK_E2K_TLS_LDM_CALL)
    .Case("tldo_hix22", VK_E2K_TLS_LDO_HIX22)
    .Case("tldo_lox10", VK_E2K_TLS_LDO_LOX10)
    .Case("tldo_add",   VK_E2K_TLS_LDO_ADD)
    .Case("tie_hi22",   VK_E2K_TLS_IE_HI22)
    .Case("tie_lo10",   VK_E2K_TLS_IE_LO10)
    .Case("tie_ld",     VK_E2K_TLS_IE_LD)
    .Case("tie_ldx",    VK_E2K_TLS_IE_LDX)
    .Case("tie_add",    VK_E2K_TLS_IE_ADD)
    .Case("tle_hix22",  VK_E2K_TLS_LE_HIX22)
    .Case("tle_lox10",  VK_E2K_TLS_LE_LOX10)
    .Case("hix",        VK_E2K_HIX22)
    .Case("lox",        VK_E2K_LOX10)
    .Case("gdop_hix22", VK_E2K_GOTDATA_HIX22)
    .Case("gdop_lox10", VK_E2K_GOTDATA_LOX10)
    .Case("gdop",       VK_E2K_GOTDATA_OP)
    .Default(VK_E2K_None);
}

E2K::Fixups E2KMCExpr::getFixupKind(E2KMCExpr::VariantKind Kind) {
  switch (Kind) {
  default: llvm_unreachable("Unhandled E2KMCExpr::VariantKind");
  case VK_E2K_LO:       return E2K::fixup_e2k_lo10;
  case VK_E2K_HI:       return E2K::fixup_e2k_hi22;
  case VK_E2K_H44:      return E2K::fixup_e2k_h44;
  case VK_E2K_M44:      return E2K::fixup_e2k_m44;
  case VK_E2K_L44:      return E2K::fixup_e2k_l44;
  case VK_E2K_HH:       return E2K::fixup_e2k_hh;
  case VK_E2K_HM:       return E2K::fixup_e2k_hm;
  case VK_E2K_LM:       return E2K::fixup_e2k_lm;
  case VK_E2K_PC22:     return E2K::fixup_e2k_pc22;
  case VK_E2K_PC10:     return E2K::fixup_e2k_pc10;
  case VK_E2K_GOT22:    return E2K::fixup_e2k_got22;
  case VK_E2K_GOT10:    return E2K::fixup_e2k_got10;
  case VK_E2K_GOT13:    return E2K::fixup_e2k_got13;
  case VK_E2K_13:       return E2K::fixup_e2k_13;
  case VK_E2K_WPLT30:   return E2K::fixup_e2k_wplt30;
  case VK_E2K_WDISP30:  return E2K::fixup_e2k_call30;
  case VK_E2K_TLS_GD_HI22:   return E2K::fixup_e2k_tls_gd_hi22;
  case VK_E2K_TLS_GD_LO10:   return E2K::fixup_e2k_tls_gd_lo10;
  case VK_E2K_TLS_GD_ADD:    return E2K::fixup_e2k_tls_gd_add;
  case VK_E2K_TLS_GD_CALL:   return E2K::fixup_e2k_tls_gd_call;
  case VK_E2K_TLS_LDM_HI22:  return E2K::fixup_e2k_tls_ldm_hi22;
  case VK_E2K_TLS_LDM_LO10:  return E2K::fixup_e2k_tls_ldm_lo10;
  case VK_E2K_TLS_LDM_ADD:   return E2K::fixup_e2k_tls_ldm_add;
  case VK_E2K_TLS_LDM_CALL:  return E2K::fixup_e2k_tls_ldm_call;
  case VK_E2K_TLS_LDO_HIX22: return E2K::fixup_e2k_tls_ldo_hix22;
  case VK_E2K_TLS_LDO_LOX10: return E2K::fixup_e2k_tls_ldo_lox10;
  case VK_E2K_TLS_LDO_ADD:   return E2K::fixup_e2k_tls_ldo_add;
  case VK_E2K_TLS_IE_HI22:   return E2K::fixup_e2k_tls_ie_hi22;
  case VK_E2K_TLS_IE_LO10:   return E2K::fixup_e2k_tls_ie_lo10;
  case VK_E2K_TLS_IE_LD:     return E2K::fixup_e2k_tls_ie_ld;
  case VK_E2K_TLS_IE_LDX:    return E2K::fixup_e2k_tls_ie_ldx;
  case VK_E2K_TLS_IE_ADD:    return E2K::fixup_e2k_tls_ie_add;
  case VK_E2K_TLS_LE_HIX22:  return E2K::fixup_e2k_tls_le_hix22;
  case VK_E2K_TLS_LE_LOX10:  return E2K::fixup_e2k_tls_le_lox10;
  case VK_E2K_HIX22:         return E2K::fixup_e2k_hix22;
  case VK_E2K_LOX10:         return E2K::fixup_e2k_lox10;
  case VK_E2K_GOTDATA_HIX22: return E2K::fixup_e2k_gotdata_hix22;
  case VK_E2K_GOTDATA_LOX10: return E2K::fixup_e2k_gotdata_lox10;
  case VK_E2K_GOTDATA_OP:    return E2K::fixup_e2k_gotdata_op;
  }
}

bool
E2KMCExpr::evaluateAsRelocatableImpl(MCValue &Res,
                                       const MCAsmLayout *Layout,
                                       const MCFixup *Fixup) const {
  return getSubExpr()->evaluateAsRelocatable(Res, Layout, Fixup);
}

static void fixELFSymbolsInTLSFixupsImpl(const MCExpr *Expr, MCAssembler &Asm) {
  switch (Expr->getKind()) {
  case MCExpr::Target:
    llvm_unreachable("Can't handle nested target expr!");
    break;

  case MCExpr::Constant:
    break;

  case MCExpr::Binary: {
    const MCBinaryExpr *BE = cast<MCBinaryExpr>(Expr);
    fixELFSymbolsInTLSFixupsImpl(BE->getLHS(), Asm);
    fixELFSymbolsInTLSFixupsImpl(BE->getRHS(), Asm);
    break;
  }

  case MCExpr::SymbolRef: {
    const MCSymbolRefExpr &SymRef = *cast<MCSymbolRefExpr>(Expr);
    cast<MCSymbolELF>(SymRef.getSymbol()).setType(ELF::STT_TLS);
    break;
  }

  case MCExpr::Unary:
    fixELFSymbolsInTLSFixupsImpl(cast<MCUnaryExpr>(Expr)->getSubExpr(), Asm);
    break;
  }

}

void E2KMCExpr::fixELFSymbolsInTLSFixups(MCAssembler &Asm) const {
  switch(getKind()) {
  default: return;
  case VK_E2K_TLS_GD_CALL:
  case VK_E2K_TLS_LDM_CALL: {
    // The corresponding relocations reference __tls_get_addr, as they call it,
    // but this is only implicit; we must explicitly add it to our symbol table
    // to bind it for these uses.
    MCSymbol *Symbol = Asm.getContext().getOrCreateSymbol("__tls_get_addr");
    Asm.registerSymbol(*Symbol);
    auto ELFSymbol = cast<MCSymbolELF>(Symbol);
    if (!ELFSymbol->isBindingSet())
      ELFSymbol->setBinding(ELF::STB_GLOBAL);
    [[fallthrough]];
  }
  case VK_E2K_TLS_GD_HI22:
  case VK_E2K_TLS_GD_LO10:
  case VK_E2K_TLS_GD_ADD:
  case VK_E2K_TLS_LDM_HI22:
  case VK_E2K_TLS_LDM_LO10:
  case VK_E2K_TLS_LDM_ADD:
  case VK_E2K_TLS_LDO_HIX22:
  case VK_E2K_TLS_LDO_LOX10:
  case VK_E2K_TLS_LDO_ADD:
  case VK_E2K_TLS_IE_HI22:
  case VK_E2K_TLS_IE_LO10:
  case VK_E2K_TLS_IE_LD:
  case VK_E2K_TLS_IE_LDX:
  case VK_E2K_TLS_IE_ADD:
  case VK_E2K_TLS_LE_HIX22:
  case VK_E2K_TLS_LE_LOX10: break;
  }
  fixELFSymbolsInTLSFixupsImpl(getSubExpr(), Asm);
}

void E2KMCExpr::visitUsedExpr(MCStreamer &Streamer) const {
  Streamer.visitUsedExpr(*getSubExpr());
}
