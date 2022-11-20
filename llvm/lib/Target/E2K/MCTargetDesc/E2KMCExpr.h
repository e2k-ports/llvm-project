//====- E2KMCExpr.h - E2K specific MC expression classes --*- C++ -*-=====//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file describes E2K-specific MCExprs, used for modifiers like
// "%hi" or "%lo" etc.,
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_E2K_MCTARGETDESC_E2KMCEXPR_H
#define LLVM_LIB_TARGET_E2K_MCTARGETDESC_E2KMCEXPR_H

#include "E2KFixupKinds.h"
#include "llvm/MC/MCExpr.h"

namespace llvm {

class StringRef;
class E2KMCExpr : public MCTargetExpr {
public:
  enum VariantKind {
    VK_E2K_None,
    VK_E2K_LO,
    VK_E2K_HI,
    VK_E2K_H44,
    VK_E2K_M44,
    VK_E2K_L44,
    VK_E2K_HH,
    VK_E2K_HM,
    VK_E2K_LM,
    VK_E2K_PC22,
    VK_E2K_PC10,
    VK_E2K_GOT22,
    VK_E2K_GOT10,
    VK_E2K_GOT13,
    VK_E2K_13,
    VK_E2K_WPLT30,
    VK_E2K_WDISP30,
    VK_E2K_R_DISP32,
    VK_E2K_TLS_GD_HI22,
    VK_E2K_TLS_GD_LO10,
    VK_E2K_TLS_GD_ADD,
    VK_E2K_TLS_GD_CALL,
    VK_E2K_TLS_LDM_HI22,
    VK_E2K_TLS_LDM_LO10,
    VK_E2K_TLS_LDM_ADD,
    VK_E2K_TLS_LDM_CALL,
    VK_E2K_TLS_LDO_HIX22,
    VK_E2K_TLS_LDO_LOX10,
    VK_E2K_TLS_LDO_ADD,
    VK_E2K_TLS_IE_HI22,
    VK_E2K_TLS_IE_LO10,
    VK_E2K_TLS_IE_LD,
    VK_E2K_TLS_IE_LDX,
    VK_E2K_TLS_IE_ADD,
    VK_E2K_TLS_LE_HIX22,
    VK_E2K_TLS_LE_LOX10,
    VK_E2K_HIX22,
    VK_E2K_LOX10,
    VK_E2K_GOTDATA_HIX22,
    VK_E2K_GOTDATA_LOX10,
    VK_E2K_GOTDATA_OP,
  };

private:
  const VariantKind Kind;
  const MCExpr *Expr;

  explicit E2KMCExpr(VariantKind Kind, const MCExpr *Expr)
      : Kind(Kind), Expr(Expr) {}

public:
  /// @name Construction
  /// @{

  static const E2KMCExpr *create(VariantKind Kind, const MCExpr *Expr,
                                 MCContext &Ctx);
  /// @}
  /// @name Accessors
  /// @{

  /// getOpcode - Get the kind of this expression.
  VariantKind getKind() const { return Kind; }

  /// getSubExpr - Get the child of this expression.
  const MCExpr *getSubExpr() const { return Expr; }

  /// getFixupKind - Get the fixup kind of this expression.
  E2K::Fixups getFixupKind() const { return getFixupKind(Kind); }

  /// @}
  void printImpl(raw_ostream &OS, const MCAsmInfo *MAI) const override;
  bool evaluateAsRelocatableImpl(MCValue &Res,
                                 const MCAsmLayout *Layout,
                                 const MCFixup *Fixup) const override;
  void visitUsedExpr(MCStreamer &Streamer) const override;
  MCFragment *findAssociatedFragment() const override {
    return getSubExpr()->findAssociatedFragment();
  }

  void fixELFSymbolsInTLSFixups(MCAssembler &Asm) const override;

  static bool classof(const MCExpr *E) {
    return E->getKind() == MCExpr::Target;
  }

  static bool classof(const E2KMCExpr *) { return true; }

  static VariantKind parseVariantKind(StringRef name);
  static bool printVariantKind(raw_ostream &OS, VariantKind Kind);
  static E2K::Fixups getFixupKind(VariantKind Kind);
};

} // end namespace llvm.

#endif
