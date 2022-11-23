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
      return ELF::R_E2K_DISP;
  }

  if (IsPCRel) {
    switch(Fixup.getTargetKind()) {
    default:
      llvm_unreachable("Unimplemented fixup -> relocation");
    case FK_Data_1:                  return ELF::R_E2K_DISP;
    case FK_Data_2:                  return ELF::R_E2K_DISP;
    case FK_Data_4:                  return ELF::R_E2K_DISP;
    case FK_Data_8:                  return ELF::R_E2K_DISP;
    }
  }

  switch(Fixup.getTargetKind()) {
  default:
    llvm_unreachable("Unimplemented fixup -> relocation");
  case FK_NONE:                  return ELF::R_E2K_NONE;
  case FK_Data_1:                return ELF::R_E2K_DISP;
  case FK_Data_2:                return ELF::R_E2K_DISP;
  case FK_Data_4:                return ELF::R_E2K_DISP;
  case FK_Data_8:                return ELF::R_E2K_DISP;
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
    case ELF::R_E2K_GOT:
    case ELF::R_E2K_GOTOFF:
    case ELF::R_E2K_64_GOTOFF:
      return true;
  }
}

std::unique_ptr<MCObjectTargetWriter>
llvm::createE2KELFObjectWriter(bool Is64Bit, uint8_t OSABI) {
  return std::make_unique<E2KELFObjectWriter>(Is64Bit, OSABI);
}
