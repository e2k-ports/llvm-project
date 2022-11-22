//===-- E2K.td - Describe the E2K Target Machine -------*- tablegen -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Target-independent interfaces which we are implementing
//===----------------------------------------------------------------------===//

include "llvm/Target/Target.td"

//===----------------------------------------------------------------------===//
// E2K Subtarget features.
//

//===----------------------------------------------------------------------===//
// Register File, Calling Conv, Instruction Descriptions
//===----------------------------------------------------------------------===//

include "E2KRegisterInfo.td"
include "E2KCallingConv.td"
include "E2KSchedule.td"
include "E2KInstrInfo.td"

def E2KInstrInfo : InstrInfo;

def E2KAsmParser : AsmParser {
  bit ShouldEmitMatchRegisterName = 0;
}

//===----------------------------------------------------------------------===//
// E2K processors supported.
//===----------------------------------------------------------------------===//

class Proc<string Name, list<SubtargetFeature> Features>
 : Processor<Name, NoItineraries, Features>;

def : Proc<"generic",         []>;
def : Proc<"v7",              []>;
def : Proc<"v8",              []>;
def : Proc<"supere2k",        []>;
def : Proc<"e2klite",         []>;
def : Proc<"f934",            []>;
def : Proc<"hypere2k",        []>;
def : Proc<"e2klite86x",      []>;
def : Proc<"e2klet",          []>;
def : Proc<"tsc701",          []>;
def : Proc<"v9",              []>;
def : Proc<"ultre2k",         []>;
def : Proc<"ultrae2k3",       []>;
def : Proc<"niagara",         []>;
def : Proc<"niagara2",        []>;
def : Proc<"niagara3",        []>;
def : Proc<"niagara4",        []>;

//===----------------------------------------------------------------------===//
// Declare the target which we are implementing
//===----------------------------------------------------------------------===//

def E2KAsmWriter : AsmWriter {
  string AsmWriterClassName  = "InstPrinter";
  int PassSubtarget = 1;
  int Variant = 0;
}

def E2K : Target {
  // Pull in Instruction Info:
  let InstructionSet = E2KInstrInfo;
  let AssemblyParsers  = [E2KAsmParser];
  let AssemblyWriters = [E2KAsmWriter];
  let AllowRegisterRenaming = 1;
}
