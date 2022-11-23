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

class Architecture<string fname, string aname, string desc, list<SubtargetFeature> features>
  : SubtargetFeature<fname, "E2KArch", aname, desc, features>;

def E2Kv1: Architecture<"e2kv1", "E2Kv1", "E2K-V1 architecture", []>;
def E2Kv2: Architecture<"e2kv2", "E2Kv2", "E2K-V2 architecture", []>;
def E2Kv3: Architecture<"e2kv3", "E2Kv3", "E2K-V3 architecture", []>;
def E2Kv4: Architecture<"e2kv4", "E2Kv4", "E2K-V4 architecture", []>;
def E2Kv5: Architecture<"e2kv5", "E2Kv5", "E2K-V5 architecture", []>;
def E2Kv6: Architecture<"e2kv6", "E2Kv6", "E2K-V6 architecture", []>;
def E2Kv7: Architecture<"e2kv7", "E2Kv7", "E2K-V7 architecture", []>;

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

def E2KModel : SchedMachineModel {
  let CompleteModel = 0;
}

def : ProcessorModel<"elbrus",          E2KModel, [E2Kv1]>;

def : ProcessorModel<"elbrus-s",        E2KModel, [E2Kv2]>;
def : ProcessorModel<"elbrus-3s",       E2KModel, [E2Kv2]>; // same as "elbrus-s"
def : ProcessorModel<"elbrus-2c",       E2KModel, [E2Kv2]>; // same as "elbrus-s"
def : ProcessorModel<"elbrus-2c1",      E2KModel, [E2Kv2]>; // same as "elbrus-s"

def : ProcessorModel<"elbrus-1c+",      E2KModel, [E2Kv4]>;
def : ProcessorModel<"elbrus-4c1+",     E2KModel, [E2Kv4]>; // same as "elbrus-1c+"

def : ProcessorModel<"elbrus-1ck",      E2KModel, [E2Kv4]>;
def : ProcessorModel<"elbrus-1chk",     E2KModel, [E2Kv4]>; // same as "elbrus-1ck"

def : ProcessorModel<"elbrus-2c+",      E2KModel, [E2Kv2]>;
def : ProcessorModel<"elbrus-2c2",      E2KModel, [E2Kv2]>; // same as "elbrus-2c+"
def : ProcessorModel<"elbrus-s2",       E2KModel, [E2Kv2]>; // same as "elbrus-2c+"
def : ProcessorModel<"elbrus-sx2",      E2KModel, [E2Kv2]>; // same as "elbrus-2c+"
def : ProcessorModel<"elbrus-3s2",      E2KModel, [E2Kv2]>; // same as "elbrus-2c+"

def : ProcessorModel<"elbrus-2cm",      E2KModel, [E2Kv2]>;
def : ProcessorModel<"elbrus-1c",       E2KModel, [E2Kv2]>; // same as "elbrus-2cm"

def : ProcessorModel<"elbrus-2s3",      E2KModel, [E2Kv6]>;
def : ProcessorModel<"elbrus-2c3",      E2KModel, [E2Kv6]>; // same as "elbrus-2s3"

def : ProcessorModel<"elbrus-4c",       E2KModel, [E2Kv3]>;
def : ProcessorModel<"elbrus-2s",       E2KModel, [E2Kv3]>; // same as "elbrus-4c"
def : ProcessorModel<"elbrus-2s4m",     E2KModel, [E2Kv3]>; // same as "elbrus-4c"
def : ProcessorModel<"elbrus-3c4",      E2KModel, [E2Kv3]>; // same as "elbrus-4c"

def : ProcessorModel<"elbrus-8c",       E2KModel, [E2Kv4]>;
def : ProcessorModel<"elbrus-4c+",      E2KModel, [E2Kv4]>; // same as "elbrus-8c"
def : ProcessorModel<"elbrus-4c8",      E2KModel, [E2Kv4]>; // same as "elbrus-8c"
def : ProcessorModel<"elbrus-4s",       E2KModel, [E2Kv4]>; // same as "elbrus-8c"

def : ProcessorModel<"elbrus-8c1",      E2KModel, [E2Kv4]>;

def : ProcessorModel<"elbrus-8sv",      E2KModel, [E2Kv5]>;
def : ProcessorModel<"elbrus-8c2",      E2KModel, [E2Kv5]>; // same as "elbrus-8sv"
def : ProcessorModel<"elbrus-8cb",      E2KModel, [E2Kv5]>; // same as "elbrus-8sv"

def : ProcessorModel<"elbrus-12s",      E2KModel, [E2Kv6]>;
def : ProcessorModel<"elbrus-12c",      E2KModel, [E2Kv6]>; // same as "elbrus-12s"

def : ProcessorModel<"elbrus-16s",      E2KModel, [E2Kv6]>;
def : ProcessorModel<"elbrus-16c",      E2KModel, [E2Kv6]>; // same as "elbrus-16s"

def : ProcessorModel<"elbrus-32s",      E2KModel, [E2Kv7]>;
def : ProcessorModel<"elbrus-32c",      E2KModel, [E2Kv7]>; // same as "elbrus-32s"

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
