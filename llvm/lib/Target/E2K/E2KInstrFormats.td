//===-- E2KInstrFormats.td - E2K Instruction Formats ---*- tablegen -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

def c {
  list<int> all = [0, 1, 2, 3, 4, 5];
  list<int> c01345 = [0, 1, 3, 4, 5];
  list<int> c0134 = [0, 1, 3, 4];
  list<int> c0235 = [0, 2, 3, 5];
  list<int> c035 = [0, 3, 5];
  list<int> c03 = [0, 3];
  list<int> c14 = [1, 4];
  list<int> c25 = [2, 5];
  list<int> c0 = [0];
  list<int> c2 = [2];
  list<int> c3 = [3];
  list<int> c5 = [5];
}

def format {
  // simple ALS, used by the majority of arithmetic operations
  string alopf1 =  " $src1, $src2, $dst";
  // ALS with EXT, src1 is occupied by EXT
  string alopf2 =  " $src2, $dst";
  // simple ALS, used by st* operations
  string alopf3 =  " $src1, $src2, src3";

  // used by ALS/ALES with CMP extension, comparison operations
  string alopf7 =  " $src1, $src2, $dst";
  // used by ALS with CMP extensions, ctop* operations
  string alopf8 =  " $src2, $dst";
  // staa*
  string alopf10 = " $src1, $src2, $src3";

  string alopf11 = " $src1, $src2, $dst";
  // getsp
  string alopf12 = " $src2, $dst";

  string alopf13 = " $src1, $src2, $src3";
  // rws/rwd
  string alopf15 = " $src2, $dst";
  // rrs/rrd
  string alopf16 = " $src1, $dst";
  // used by ALES EXT=2 comparison operations
  string alopf17 = " $src1, $src2, $dst";
  // aaurr/aaurrd
  string alopf19 = " $src1, $src2, $dst";

  string alopf21 = " $src1, $src2, $src3, $dst";

  string todo = " $src1, $src2, $dst";
}

class E2KOperands {
  dag Ins;
  dag Outs;
}

// S - single-word operand
// D - double-word operand
// Q - quad-word operand
// B - boolean (predicate)
// R - state register
// P - predicate
// p - packed quad-word operand?
// underscore divides src and dst operands, if any

// used by majority of ALOPF1 like ADDS
def SS_S : E2KOperands {
  let Outs = (outs DST1S:$dst);
  let Ins = (ins SRC1S:$src1, SRC2S:$src2);
}

// used by majority of ALOPF1 like ADDD
def DD_D : E2KOperands {
  let Outs = (outs DST1D:$dst);
  let Ins = (ins SRC1D:$src1, SRC2D:$src2);
}

def SS_D : E2KOperands {
  let Outs = (outs DST1D:$dst);
  let Ins = (ins SRC1S:$src1, SRC2S:$src2);
}

// udivx, umodx
def DS_S : E2KOperands {
  let Outs = (outs DST1S:$dst);
  let Ins = (ins SRC1D:$src1, SRC2S:$src2);
}

def DS_D : E2KOperands {
  let Outs = (outs DST1D:$dst);
  let Ins = (ins SRC1D:$src1, SRC2S:$src2);
}

def DD_S : E2KOperands {
  let Outs = (outs DST1S:$dst);
  let Ins = (ins SRC1D:$src1, SRC2D:$src2);
}

def SD_D : E2KOperands {
  let Outs = (outs DST1D:$dst);
  let Ins = (ins SRC1S:$src1, SRC2D:$src2);
}

// used by ALOPF3 commands st*
def DDS : E2KOperands {
  let Outs = (outs);
  let Ins = (ins SRC1D:$src1, SRC2D:$src2, SRC3S:$src3);
}

def SSS : E2KOperands {
  let Outs = (outs);
  let Ins = (ins SRC1S:$src1, SRC2S:$src2, SRC3S:$src3);
}

def DDD : E2KOperands {
  let Outs = (outs);
  let Ins = (ins SRC1D:$src1, SRC2D:$src2, SRC3D:$src3);
}

def SSD : E2KOperands {
  let Outs = (outs);
  let Ins = (ins SRC1S:$src1, SRC2S:$src2, SRC3D:$src3);
}

def DDQ : E2KOperands {
  let Outs = (outs);
  let Ins = (ins SRC1D:$src1, SRC2D:$src2, SRC3Q:$src3);
}

def SSQ : E2KOperands {
  let Outs = (outs);
  let Ins = (ins SRC1S:$src1, SRC2S:$src2, SRC3Q:$src3);
}

// used by ALOPF7 commands cmp*
def SS_B : E2KOperands {
  let Outs = (outs PRED:$dst);
  let Ins = (ins SRC1S:$src1, SRC2S:$src2);
}

def DD_B : E2KOperands {
  let Outs = (outs PRED:$dst);
  let Ins = (ins SRC1D:$src1, SRC2D:$src2);
}

def S_B : E2KOperands {
  let Outs = (outs PRED:$dst);
  // FIXME : src1 is unused, how to indicate that?
  let Ins = (ins SRC2S:$src1, SRC2S:$src2);
}

def Q_Q : E2KOperands {
  let Outs = (outs DST1Q:$dst);
  let Ins = (ins SRC2Q:$src2);
}

def S_S : E2KOperands {
  let Outs = (outs DST1S:$dst);
  let Ins = (ins SRC2S:$src2);
}

def D_D : E2KOperands {
  let Outs = (outs DST1D:$dst);
  let Ins = (ins SRC2D:$src2);
}

def S_D : E2KOperands {
  let Outs = (outs DST1D:$dst);
  let Ins = (ins SRC2S:$src2);
}

def p_D : E2KOperands {
  let Outs = (outs DST1D:$dst);
  let Ins = (ins SRC2QP:$src2);
}

def D_S : E2KOperands {
  let Outs = (outs DST1S:$dst);
  let Ins = (ins SRC2D:$src2);
}

def S_Q : E2KOperands {
  let Outs = (outs DST1Q:$dst);
  let Ins = (ins SRC2S:$src2);
}

// ALOPF10

def PPS : E2KOperands {
  let Outs = (outs);
  let Ins = (ins PRED:$src1, PRED:$src2, SRC3S:$src3);
}

def PPD : E2KOperands {
  let Outs = (outs);
  let Ins = (ins PRED:$src1, PRED:$src2, SRC3D:$src3);
}

def PPQ : E2KOperands {
  let Outs = (outs);
  let Ins = (ins PRED:$src1, PRED:$src2, SRC3Q:$src3);
}

// ALOPF11

def DD_Q : E2KOperands {
  let Outs = (outs DST1Q:$dst);
  let Ins = (ins SRC1D:$src1, SRC2D:$src2);
}

def SS_Q : E2KOperands {
  let Outs = (outs DST1Q:$dst);
  let Ins = (ins SRC1S:$src1, SRC2S:$src2);
}

def QQ_Q : E2KOperands {
  let Outs = (outs DST1Q:$dst);
  let Ins = (ins SRC1Q:$src1, SRC2Q:$src2);
}

def QS_Q : E2KOperands {
  let Outs = (outs DST1Q:$dst);
  let Ins = (ins SRC1Q:$src1, SRC2S:$src2);
}

def QS_D : E2KOperands {
  let Outs = (outs DST1D:$dst);
  let Ins = (ins SRC1Q:$src1, SRC2S:$src2);
}

// ALOPF21

def SSS_S : E2KOperands {
  let Outs = (outs DST1S:$dst);
  let Ins = (ins SRC1S:$src1, SRC2S:$src2, SRC3S:$src3);
}

def DDD_D : E2KOperands {
  let Outs = (outs DST1D:$dst);
  let Ins = (ins SRC1D:$src1, SRC2D:$src2, SRC3D:$src3);
}

def DDS_S : E2KOperands {
  let Outs = (outs DST1S:$dst);
  let Ins = (ins SRC1D:$src1, SRC2D:$src2, SRC3S:$src3);
}

def DDS_D : E2KOperands {
  let Outs = (outs DST1D:$dst);
  let Ins = (ins SRC1D:$src1, SRC2D:$src2, SRC3S:$src3);
}

// ALOPF13

def QSS : E2KOperands {
  let Outs = (outs);
  let Ins = (ins SRC1Q:$src1, SRC2S:$src2, SRC3S:$src3);
}

def QSD : E2KOperands {
  let Outs = (outs);
  let Ins = (ins SRC1Q:$src1, SRC2S:$src2, SRC3D:$src3);
}

def QSQ : E2KOperands {
  let Outs = (outs);
  let Ins = (ins SRC1Q:$src1, SRC2S:$src2, SRC3Q:$src3);
}

// ALOPF15

def S_R : E2KOperands {
  let Outs = (outs RegSTATE:$dst);
  let Ins = (ins SRC1S:$src1, SRC2S:$src2);
}

def D_R : E2KOperands {
  let Outs = (outs RegSTATE:$dst);
  let Ins = (ins SRC1S:$src1, SRC2D:$src2);
}

// ALOPF16

def R_S : E2KOperands {
  let Outs = (outs DST1S:$dst);
  let Ins = (ins SRC1S:$src2, RegSTATE:$src1);
}

def R_D : E2KOperands {
  let Outs = (outs DST1D:$dst);
  let Ins = (ins SRC1S:$src2, RegSTATE:$src1);
}

// ALOPF17

def pp_B : E2KOperands {
  let Outs = (outs PRED:$dst);
  let Ins = (ins SRC1QP:$src1, SRC2QP:$src2);
}

// ALOPF19

def PP_S : E2KOperands {
  let Outs = (outs DST1S:$dst);
  let Ins = (ins PRED:$src1, PRED:$src2);
}

def PP_D : E2KOperands {
  let Outs = (outs DST1D:$dst);
  let Ins = (ins PRED:$src1, PRED:$src2);
}

def PP_Q : E2KOperands {
  let Outs = (outs DST1Q:$dst);
  let Ins = (ins PRED:$src1, PRED:$src2);
}

// ALES EXT2 = 2, packed operations?

def pp_p : E2KOperands {
  let Outs = (outs DST1QP:$dst);
  let Ins = (ins SRC1QP:$src1, SRC2QP:$src2);
}

def pS_p : E2KOperands {
  let Outs = (outs DST1QP:$dst);
  let Ins = (ins SRC1QP:$src1, SRC2S:$src2);
}

def pD_p : E2KOperands {
  let Outs = (outs DST1QP:$dst);
  let Ins = (ins SRC1QP:$src1, SRC2D:$src2);
}

def DD_p : E2KOperands {
  let Outs = (outs DST1QP:$dst);
  let Ins = (ins SRC1D:$src1, SRC2D:$src2);
}

def SS_p : E2KOperands {
  let Outs = (outs DST1QP:$dst);
  let Ins = (ins SRC1S:$src1, SRC2S:$src2);
}

def QS_p : E2KOperands {
  let Outs = (outs DST1QP:$dst);
  let Ins = (ins SRC1Q:$src1, SRC2S:$src2);
}

def pp_D : E2KOperands {
  let Outs = (outs DST1D:$dst);
  let Ins = (ins SRC1QP:$src1, SRC2QP:$src2);
}

def Dp_p : E2KOperands {
  let Outs = (outs DST1QP:$dst);
  let Ins = (ins SRC1D:$src1, SRC2QP:$src2);
}

def DDp : E2KOperands {
  let Outs = (outs);
  let Ins = (ins SRC1D:$src1, SRC2D:$src2, SRC3QP:$src3);
}

def SSp : E2KOperands {
  let Outs = (outs);
  let Ins = (ins SRC1S:$src1, SRC2S:$src2, SRC3QP:$src3);
}

def QSp : E2KOperands {
  let Outs = (outs);
  let Ins = (ins SRC1Q:$src1, SRC2S:$src2, SRC3QP:$src3);
}

def pDD : E2KOperands {
  let Outs = (outs);
  let Ins = (ins SRC1QP:$src1, SRC2D:$src2, SRC3D:$src3);
}

// virtual NOP

def ImmUnsignedi8AsmOperand : AsmOperandClass {
  let Name = "ImmUnsignedi8";
  let RenderMethod = "addImmOperands";
}

def u8imm : Operand<i8> {
  let PrintMethod = "printU8Imm";
  let ParserMatchClass = ImmUnsignedi8AsmOperand;
  let OperandType = "OPERAND_IMMEDIATE";
}

// Pseudo instructions.
class Pseudo<dag outs, dag ins, string asmstr, list<dag> pattern = []> : Instruction {
  let isCodeGenOnly = 1;
  let isPseudo = 1;
  let Namespace = "E2K";
  field bits<32> Inst;
  dag OutOperandList = outs;
  dag InOperandList = ins;
  let AsmString = asmstr;
  let Pattern = pattern;
}

class NOPInst<dag outs, dag ins, string asmstr, list<dag> pattern = []>: Instruction {

  let Namespace = "E2K";
  field bits<32> Inst;
  dag OutOperandList = outs;
  dag InOperandList = ins;
  let AsmString = asmstr;
  let Pattern = pattern;
}

class SSInst<dag outs, dag ins, string asmstr, list<dag> pattern = []>: Instruction {

  let Namespace = "E2K";
  field bits<32> Inst;
  dag OutOperandList = outs;
  dag InOperandList = ins;
  let AsmString = asmstr;
  let Pattern = pattern;
}

// regular ALS command
class ALSInstOneChannel<bits<7> opcode, dag outs, dag ins, string asmstr,
              int channel,
              list<dag> pattern = []>: Instruction {

  let Namespace = "E2K";
  let Size = 4;
  field bits<32> Inst;
  dag OutOperandList = outs;
  dag InOperandList = ins;
  let AsmString = asmstr;
  let Pattern = pattern;
  bits<7> OpCode = opcode;
  bits<8> src1;
  bits<8> src2;
  bits<8> dst;
  int Channel = channel;
  let Inst{30-24} = OpCode;
  let Inst{23-16} = src1;
  let Inst{15-8} = src2;
  let Inst{7-0} = dst;
  let DecoderNamespace = "E2K_ALS" # channel;
}

class ALSInstSRC3OneChannel<bits<7> opcode, dag outs, dag ins, string asmstr,
              int channel,
              list<dag> pattern = []>: ALSInstOneChannel<opcode, outs, ins, asmstr, channel, pattern> {

  bits<8> src3;
  let Inst{7-0} = src3;
}

multiclass ALSInst<bits<7> opcode, string mnemonic, string params, E2KOperands operands,
                                list<int> channels = c.all,
                                list<dag> pattern = []> {
  foreach channel = channels in {
    def c#channel : ALSInstOneChannel<opcode, operands.Outs, operands.Ins, !strconcat(mnemonic, "," # channel, params), channel, pattern>;
  }
}

multiclass ALSInstSRC3<bits<7> opcode, string mnemonic, string params, E2KOperands operands,
                                list<int> channels = c.all,
                                list<dag> pattern = []> {
  foreach channel = channels in {
    def : ALSInstSRC3OneChannel<opcode, operands.Outs, operands.Ins,
    !strconcat(mnemonic, "," # channel, params), channel, pattern>;
  }
}


// ALS with CMP opcode extension
class ALSCMPInstOneChannel<bits<7> opcode, bits<3> cc, dag outs, dag ins, string asmstr,
              int channel,
              list<dag> pattern = []>: ALSInstOneChannel<opcode, outs, ins,
              asmstr, channel, pattern> {

  bits<3> CC = cc;
  let Inst{7-5} = CC;
}


multiclass ALSCMPInst<bits<7> opcode, bits<3> cc, string mnemonic, string params, E2KOperands operands,
                                list<int> channels = c.c0134,
                                list<dag> pattern = []> {
  foreach channel = channels in {
    def c#channel : ALSCMPInstOneChannel<opcode, cc, operands.Outs, operands.Ins,
    !strconcat(mnemonic, "," # channel, params), channel, pattern>;
  }
}

// ALS with opcode extension
class ALSEXTInstOneChannel<bits<7> opcode, bits<8> ext, dag outs, dag ins, string asmstr,
              int channel,
              list<dag> pattern = []>: ALSInstOneChannel<opcode, outs, ins, asmstr, channel, pattern> {

  bits<8> Ext = ext;
  let Inst{23-16} = Ext;
}

multiclass ALSEXTInst<bits<7> opcode, bits<8> ext, string mnemonic, string params, E2KOperands operands,
                                list<int> channels = c.all,
                                list<dag> pattern = []> {
  foreach channel = channels in {
    def c#channel : ALSEXTInstOneChannel<opcode, ext, operands.Outs, operands.Ins,
    !strconcat(mnemonic, "," # channel, params), channel, pattern>;
  }
}

// ALES command
class ALESInstOneChannel<bits<7> opcode, bits<8> opcode2, dag outs, dag ins, string asmstr,
              int channel,
              list<dag> pattern = []>: Instruction {

  let Namespace = "E2K";
  let Size = 6;
  field bits<48> Inst;
  dag OutOperandList = outs;
  dag InOperandList = ins;
  let AsmString = asmstr;
  let Pattern = pattern;
  int Channel = channel;
  bits<7> OpCode = opcode;
  bits<8> OpCode2 = opcode2;
  bits<8> src1;
  bits<8> src2;
  bits<8> src3;
  bits<8> dst;
  let Inst{47-40} = OpCode2;
  let Inst{39-32} = src3;
  let Inst{30-24} = OpCode;
  let Inst{23-16} = src1;
  let Inst{15-8} = src2;
  let Inst{7-0} = dst;
  let DecoderNamespace = "E2K_ALES" # channel;
}

multiclass ALESInst<bits<7> opcode, bits<8> opcode2, string mnemonic, string params, E2KOperands operands,
                                list<int> channels = c.all,
                                list<dag> pattern = []> {
  foreach channel = channels in {
    def c#channel : ALESInstOneChannel<opcode, opcode2, operands.Outs, operands.Ins,
    !strconcat(mnemonic, "," # channel, params), channel, pattern>;
  }
}

// ALES with opcode extension 2
class ALESEXT2InstOneChannel<bits<7> opcode, bits<8> opcode2, bits<8> ext2, dag outs, dag ins, string asmstr,
              int channel,
              list<dag> pattern = []>: ALESInstOneChannel<opcode, opcode2, outs, ins, asmstr, channel, pattern> {

  bits<8> Ext2 = ext2;
  let Inst{39-32} = Ext2;
}

multiclass ALESEXT2Inst<bits<7> opcode, bits<8> opcode2, bits<8> ext2, string mnemonic, string params, E2KOperands operands,
                                list<int> channels = c.all,
                                list<dag> pattern = []> {
  foreach channel = channels in {
    def c#channel : ALESEXT2InstOneChannel<opcode, opcode2, ext2, operands.Outs, operands.Ins,
    !strconcat(mnemonic, "," # channel, params), channel, pattern>;
  }
}

// ALES with both EXT1 and EXT2, used by getsp
class ALESEXT1EXT2InstOneChannel<bits<7> opcode, bits<8> ext1, bits<8> opcode2, bits<8> ext2, dag outs, dag ins, string asmstr,
              int channel,
              list<dag> pattern = []>: ALESInstOneChannel<opcode, opcode2, outs, ins, asmstr, channel, pattern> {

  bits<8> Ext2 = ext2;
  let Inst{39-32} = Ext2;
  bits<8> Ext1 = ext1;
  let Inst{23-16} = Ext1;
}

multiclass ALESEXT1EXT2Inst<bits<7> opcode, bits<8> ext1, bits<8> opcode2, bits<8> ext2, string mnemonic, string params, E2KOperands operands,
                                list<int> channels = c.all,
                                list<dag> pattern = []> {
  foreach channel = channels in {
    def c#channel : ALESEXT1EXT2InstOneChannel<opcode, ext1, opcode2, ext2, operands.Outs, operands.Ins,
    !strconcat(mnemonic, "," # channel, params), channel, pattern>;
  }
}

class ALESEXT2SRC3InstOneChannel<bits<7> opcode, bits<8> opcode2, bits<8> ext2, dag outs, dag ins, string asmstr,
              int channel,
              list<dag> pattern = []>: ALESEXT2InstOneChannel<opcode, opcode2, ext2, outs, ins, asmstr, channel, pattern> {

  bits<8> Ext2 = src3;
  let Inst{7-0} = src3;
}

multiclass ALESEXT2SRC3Inst<bits<7> opcode, bits<8> opcode2, bits<8> ext2, string mnemonic, string params, E2KOperands operands,
                                list<int> channels = c.all,
                                list<dag> pattern = []> {
  foreach channel = channels in {
    def c#channel : ALESEXT2SRC3InstOneChannel<opcode, opcode2, ext2, operands.Outs, operands.Ins,
    !strconcat(mnemonic, "," # channel, params), channel, pattern>;
  }
}

// CS0 command
class CS0Inst<bits<4> opcode, dag outs, dag ins, string asmstr,
              list<dag> pattern = []>: Instruction {

  let Namespace = "E2K";
  let Size = 4;
  field bits<32> Inst;
  dag OutOperandList = outs;
  dag InOperandList = ins;
  let AsmString = asmstr;
  let Pattern = pattern;
  bits<4> OpCode = opcode;
  let Inst{31-28} = OpCode;
  let DecoderNamespace = "E2K_CS0";
}

// CS1 command
class CS1Inst<bits<4> opcode, dag outs, dag ins, string asmstr,
              list<dag> pattern = []>: Instruction {

  let Namespace = "E2K";
  let Size = 4;
  field bits<32> Inst;
  dag OutOperandList = outs;
  dag InOperandList = ins;
  let AsmString = asmstr;
  let Pattern = pattern;
  bits<4> OpCode = opcode;
  let Inst{31-28} = OpCode;
  let DecoderNamespace = "E2K_CS1";
}

// CS1 with extra flag (setei/setfst)
class CS1InstF1<bits<4> opcode, bits<1> flag, dag outs, dag ins, string asmstr,
              list<dag> pattern = []>: CS1Inst<opcode, outs, ins, asmstr, pattern> {

  bits<1> Flag = flag;
  let Inst{27} = Flag;
}

// CS1 with two extra flags
class CS1InstF2<bits<4> opcode, bits<1> flag1, bits<1> flag2, dag outs, dag ins, string asmstr,
              list<dag> pattern = []>: CS1Inst<opcode, outs, ins, asmstr, pattern> {

  bits<1> Flag1 = flag1;
  bits<1> Flag2 = flag2;
  let Inst{26} = Flag1;
  let Inst{27} = Flag2;
}

// FLUSH
class FLUSHInst<bits<4> opcode, bits<1> flushr, bits<1> flushc, bits<1> fillr, bits<1> fillc, dag outs, dag ins, string asmstr,
              list<dag> pattern = []>: CS1Inst<opcode, outs, ins, asmstr, pattern> {

  bits<1> FlushR = flushr;
  bits<1> FlushC = flushc;
  bits<1> FillR = fillr;
  bits<1> FillC = fillc;
  let Inst{0} = FlushR;
  let Inst{1} = FlushC;
  let Inst{2} = FillR;
  let Inst{3} = FillC;
}

