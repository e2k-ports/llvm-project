//===-- E2KFixupKinds.h - E2K Specific Fixup Entries --------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_E2K_MCTARGETDESC_E2KFIXUPKINDS_H
#define LLVM_LIB_TARGET_E2K_MCTARGETDESC_E2KFIXUPKINDS_H

#include "llvm/MC/MCFixup.h"

namespace llvm {
  namespace E2K {
    enum Fixups {
      // fixup_e2k_call30 - 30-bit PC relative relocation for call
      fixup_e2k_call30 = FirstTargetFixupKind,

      /// fixup_e2k_br22 - 22-bit PC relative relocation for
      /// branches
      fixup_e2k_br22,

      /// fixup_e2k_br19 - 19-bit PC relative relocation for
      /// branches on icc/xcc
      fixup_e2k_br19,

      /// fixup_e2k_bpr  - 16-bit fixup for bpr
      fixup_e2k_br16_2,
      fixup_e2k_br16_14,

      /// fixup_e2k_13 - 13-bit fixup
      fixup_e2k_13,

      /// fixup_e2k_hi22  - 22-bit fixup corresponding to %hi(foo)
      /// for sethi
      fixup_e2k_hi22,

      /// fixup_e2k_lo10  - 10-bit fixup corresponding to %lo(foo)
      fixup_e2k_lo10,

      /// fixup_e2k_h44  - 22-bit fixup corresponding to %h44(foo)
      fixup_e2k_h44,

      /// fixup_e2k_m44  - 10-bit fixup corresponding to %m44(foo)
      fixup_e2k_m44,

      /// fixup_e2k_l44  - 12-bit fixup corresponding to %l44(foo)
      fixup_e2k_l44,

      /// fixup_e2k_hh  -  22-bit fixup corresponding to %hh(foo)
      fixup_e2k_hh,

      /// fixup_e2k_hm  -  10-bit fixup corresponding to %hm(foo)
      fixup_e2k_hm,

      /// fixup_e2k_lm  -  22-bit fixup corresponding to %lm(foo)
      fixup_e2k_lm,

      /// fixup_e2k_pc22 - 22-bit fixup corresponding to %pc22(foo)
      fixup_e2k_pc22,

      /// fixup_e2k_pc10 - 10-bit fixup corresponding to %pc10(foo)
      fixup_e2k_pc10,

      /// fixup_e2k_got22 - 22-bit fixup corresponding to %got22(foo)
      fixup_e2k_got22,

      /// fixup_e2k_got10 - 10-bit fixup corresponding to %got10(foo)
      fixup_e2k_got10,

      /// fixup_e2k_got13 - 13-bit fixup corresponding to %got13(foo)
      fixup_e2k_got13,

      /// fixup_e2k_wplt30
      fixup_e2k_wplt30,

      /// fixups for Thread Local Storage
      fixup_e2k_tls_gd_hi22,
      fixup_e2k_tls_gd_lo10,
      fixup_e2k_tls_gd_add,
      fixup_e2k_tls_gd_call,
      fixup_e2k_tls_ldm_hi22,
      fixup_e2k_tls_ldm_lo10,
      fixup_e2k_tls_ldm_add,
      fixup_e2k_tls_ldm_call,
      fixup_e2k_tls_ldo_hix22,
      fixup_e2k_tls_ldo_lox10,
      fixup_e2k_tls_ldo_add,
      fixup_e2k_tls_ie_hi22,
      fixup_e2k_tls_ie_lo10,
      fixup_e2k_tls_ie_ld,
      fixup_e2k_tls_ie_ldx,
      fixup_e2k_tls_ie_add,
      fixup_e2k_tls_le_hix22,
      fixup_e2k_tls_le_lox10,

      /// 22-bit fixup corresponding to %hix(foo)
      fixup_e2k_hix22,
      /// 13-bit fixup corresponding to %lox(foo)
      fixup_e2k_lox10,

      /// 22-bit fixup corresponding to %gdop_hix22(foo)
      fixup_e2k_gotdata_hix22,
      /// 13-bit fixup corresponding to %gdop_lox10(foo)
      fixup_e2k_gotdata_lox10,
      /// 32-bit fixup corresponding to %gdop(foo)
      fixup_e2k_gotdata_op,

      // Marker
      LastTargetFixupKind,
      NumTargetFixupKinds = LastTargetFixupKind - FirstTargetFixupKind
    };
  }
}

#endif
