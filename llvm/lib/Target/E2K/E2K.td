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
//===----------------------------------------------------------------------===//

def FeatureV1 : SubtargetFeature<"v1", "IsV1", "true", "Enable E2K-V1 instructions">;
def FeatureV2 : SubtargetFeature<"v2", "IsV2", "true", "Enable E2K-V2 instructions">;
def FeatureV3 : SubtargetFeature<"v3", "IsV3", "true", "Enable E2K-V3 instructions">;
def FeatureV4 : SubtargetFeature<"v4", "IsV4", "true", "Enable E2K-V4 instructions">;
def FeatureV5 : SubtargetFeature<"v5", "IsV5", "true", "Enable E2K-V5 instructions">;
def FeatureV6 : SubtargetFeature<"v6", "IsV6", "true", "Enable E2K-V6 instructions">;
def FeatureV7 : SubtargetFeature<"v7", "IsV7", "true", "Enable E2K-V7 instructions">;

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

// https://wiki.elbrus.ru/Характеристики_процессоров_Эльбрус
// https://wiki.elbrus.ru/Идентификаторы_процессоров

class Proc<string Name, list<SubtargetFeature> Features>
 : Processor<Name, NoItineraries, Features>;

def : Proc<"elbrus",          [FeatureV1]>;

def : Proc<"elbrus-s",        [FeatureV2]>;
def : Proc<"elbrus-3s",       [FeatureV2]>; // same as "elbrus-s"
def : Proc<"elbrus-2c",       [FeatureV2]>; // same as "elbrus-s"
def : Proc<"elbrus-2c1",      [FeatureV2]>; // same as "elbrus-s"

def : Proc<"elbrus-1c+",      [FeatureV4]>;
def : Proc<"elbrus-4c1+",     [FeatureV4]>; // same as "elbrus-1c+"

def : Proc<"elbrus-1ck",      [FeatureV4]>;

def : Proc<"elbrus-2c+",      [FeatureV2]>;
def : Proc<"elbrus-2c2",      [FeatureV2]>; // same as "elbrus-2c+"
def : Proc<"elbrus-s2",       [FeatureV2]>; // same as "elbrus-2c+"
def : Proc<"elbrus-sx2",      [FeatureV2]>; // same as "elbrus-2c+"
def : Proc<"elbrus-3s2",      [FeatureV2]>; // same as "elbrus-2c+"

def : Proc<"elbrus-2cm",      [FeatureV2]>;
def : Proc<"elbrus-1c",       [FeatureV2]>; // same as "elbrus-2cm"

def : Proc<"elbrus-2s3",      [FeatureV6]>;
def : Proc<"elbrus-2c3",      [FeatureV6]>; // same as "elbrus-2s3"

def : Proc<"elbrus-4c",       [FeatureV3]>;
def : Proc<"elbrus-2s",       [FeatureV3]>; // same as "elbrus-4c"
def : Proc<"elbrus-2s4m",     [FeatureV3]>; // same as "elbrus-4c"
def : Proc<"elbrus-3c4",      [FeatureV3]>; // same as "elbrus-4c"

def : Proc<"elbrus-8c",       [FeatureV4]>;
def : Proc<"elbrus-4c+",      [FeatureV4]>; // same as "elbrus-8c"
def : Proc<"elbrus-4c8",      [FeatureV4]>; // same as "elbrus-8c"
def : Proc<"elbrus-4s",       [FeatureV4]>; // same as "elbrus-8c"

def : Proc<"elbrus-8c1",      [FeatureV4]>;

def : Proc<"elbrus-8sv",      [FeatureV5]>;
def : Proc<"elbrus-8c2",      [FeatureV5]>; // same as "elbrus-8sv"
def : Proc<"elbrus-8cb",      [FeatureV5]>; // same as "elbrus-8sv"

def : Proc<"elbrus-12s",      [FeatureV6]>;
def : Proc<"elbrus-12c",      [FeatureV6]>; // same as "elbrus-12s"

def : Proc<"elbrus-16s",      [FeatureV6]>;
def : Proc<"elbrus-16c",      [FeatureV6]>; // same as "elbrus-16s"

def : Proc<"elbrus-32s",      [FeatureV7]>;
def : Proc<"elbrus-32c",      [FeatureV7]>; // same as "elbrus-32s"

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
