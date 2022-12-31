//===-- E2KInstrAliases.td - Instruction Aliases for E2K Target -------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains instruction aliases for E2K.
//===----------------------------------------------------------------------===//

def : MnemonicAlias<"apurrd", "aaurrd">;
def : MnemonicAlias<"apurrq", "aaurrq">;

def : MnemonicAlias<"apurwd", "aaurwd">;
def : MnemonicAlias<"apurwq", "aaurwq">;
