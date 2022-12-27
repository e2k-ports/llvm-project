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
    def : ALSInstOneChannel<opcode, operands.Outs, operands.Ins, !strconcat(mnemonic, "," # channel, params), channel, pattern>;
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
    def : ALSCMPInstOneChannel<opcode, cc, operands.Outs, operands.Ins,
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
    def : ALSEXTInstOneChannel<opcode, ext, operands.Outs, operands.Ins,
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
    def : ALESInstOneChannel<opcode, opcode2, operands.Outs, operands.Ins,
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
    def : ALESEXT2InstOneChannel<opcode, opcode2, ext2, operands.Outs, operands.Ins,
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
    def : ALESEXT1EXT2InstOneChannel<opcode, ext1, opcode2, ext2, operands.Outs, operands.Ins,
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
    def : ALESEXT2SRC3InstOneChannel<opcode, opcode2, ext2, operands.Outs, operands.Ins,
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

class InstSP<dag outs, dag ins, string asmstr, list<dag> pattern,
             InstrItinClass itin = NoItinerary>
   : Instruction {
  field bits<32> Inst;

  let Namespace = "E2K";
  let Size = 4;

  bits<2> op;
  let Inst{31-30} = op;               // Top two bits are the 'op' field

  dag OutOperandList = outs;
  dag InOperandList = ins;
  let AsmString   = asmstr;
  let Pattern = pattern;

  let DecoderNamespace = "E2K";
  field bits<32> SoftFail = 0;

  let Itinerary = itin;
}

//===----------------------------------------------------------------------===//
// Format #2 instruction classes in the E2K
//===----------------------------------------------------------------------===//

// Format 2 instructions
class F2<dag outs, dag ins, string asmstr, list<dag> pattern,
         InstrItinClass itin = NoItinerary>
   : InstSP<outs, ins, asmstr, pattern, itin> {
  bits<3>  op2;
  bits<22> imm22;
  let op          = 0;    // op = 0
  let Inst{24-22} = op2;
  let Inst{21-0}  = imm22;
}

// Specific F2 classes: E2KV8 manual, page 44
//
class F2_1<bits<3> op2Val, dag outs, dag ins, string asmstr, list<dag> pattern,
           InstrItinClass itin = NoItinerary>
   : F2<outs, ins, asmstr, pattern, itin> {
  bits<5>  rd;

  let op2         = op2Val;

  let Inst{29-25} = rd;
}

class F2_2<bits<3> op2Val, bit annul, dag outs, dag ins, string asmstr,
           list<dag> pattern, InstrItinClass itin = NoItinerary>
   : F2<outs, ins, asmstr, pattern, itin> {
  bits<4>   cond;
  let op2         = op2Val;

  let Inst{29}    = annul;
  let Inst{28-25} = cond;
}

class F2_3<bits<3> op2Val, bit annul, bit pred,
           dag outs, dag ins, string asmstr, list<dag> pattern,
           InstrItinClass itin = NoItinerary>
   : InstSP<outs, ins, asmstr, pattern, itin> {
  bits<2>  cc;
  bits<4>  cond;
  bits<19> imm19;

  let op          = 0;    // op = 0

  let Inst{29}    = annul;
  let Inst{28-25} = cond;
  let Inst{24-22} = op2Val;
  let Inst{21-20} = cc;
  let Inst{19}    = pred;
  let Inst{18-0}  = imm19;
}

class F2_4<bits<3> cond, bit annul, bit pred, dag outs, dag ins,
           string asmstr, list<dag> pattern, InstrItinClass itin = NoItinerary>
   : InstSP<outs, ins, asmstr, pattern, itin> {
  bits<16> imm16;
  bits<5>  rs1;

  let op          = 0;    // op = 0

  let Inst{29}    = annul;
  let Inst{28}    = 0;
  let Inst{27-25} = cond;
  let Inst{24-22} = 0b011;
  let Inst{21-20} = imm16{15-14};
  let Inst{19}    = pred;
  let Inst{18-14} = rs1;
  let Inst{13-0}  = imm16{13-0};
}


//===----------------------------------------------------------------------===//
// Format #3 instruction classes in the E2K
//===----------------------------------------------------------------------===//

class F3<dag outs, dag ins, string asmstr, list<dag> pattern,
         InstrItinClass itin = NoItinerary>
   : InstSP<outs, ins, asmstr, pattern, itin> {
  bits<5> rd;
  bits<6> op3;
  bits<5> rs1;
  let op{1} = 1;   // Op = 2 or 3
  let Inst{29-25} = rd;
  let Inst{24-19} = op3;
  let Inst{18-14} = rs1;
}

// Specific F3 classes: E2KV8 manual, page 44
//
class F3_1_asi<bits<2> opVal, bits<6> op3val, dag outs, dag ins,
           string asmstr, list<dag> pattern, InstrItinClass itin = NoItinerary>
   : F3<outs, ins, asmstr, pattern, itin> {
  bits<8> asi;
  bits<5> rs2;

  let op         = opVal;
  let op3        = op3val;

  let Inst{13}   = 0;     // i field = 0
  let Inst{12-5} = asi;   // address space identifier
  let Inst{4-0}  = rs2;
}

class F3_1<bits<2> opVal, bits<6> op3val, dag outs, dag ins, string asmstr,
       list<dag> pattern, InstrItinClass itin = IIC_iu_instr>
  : F3_1_asi<opVal, op3val, outs, ins, asmstr, pattern, itin> {
  let asi = 0;
}

class F3_2<bits<2> opVal, bits<6> op3val, dag outs, dag ins,
           string asmstr, list<dag> pattern, InstrItinClass itin = IIC_iu_instr>
   : F3<outs, ins, asmstr, pattern, itin> {
  bits<13> simm13;

  let op         = opVal;
  let op3        = op3val;

  let Inst{13}   = 1;     // i field = 1
  let Inst{12-0} = simm13;
}

// floating-point
class F3_3<bits<2> opVal, bits<6> op3val, bits<9> opfval, dag outs, dag ins,
           string asmstr, list<dag> pattern, InstrItinClass itin = NoItinerary>
   : F3<outs, ins, asmstr, pattern, itin> {
  bits<5> rs2;

  let op         = opVal;
  let op3        = op3val;

  let Inst{13-5} = opfval;   // fp opcode
  let Inst{4-0}  = rs2;
}

// floating-point unary operations.
class F3_3u<bits<2> opVal, bits<6> op3val, bits<9> opfval, dag outs, dag ins,
           string asmstr, list<dag> pattern, InstrItinClass itin = NoItinerary>
   : F3<outs, ins, asmstr, pattern, itin> {
  bits<5> rs2;

  let op         = opVal;
  let op3        = op3val;
  let rs1        = 0;

  let Inst{13-5} = opfval;   // fp opcode
  let Inst{4-0}  = rs2;
}

// floating-point compares.
class F3_3c<bits<2> opVal, bits<6> op3val, bits<9> opfval, dag outs, dag ins,
           string asmstr, list<dag> pattern, InstrItinClass itin = NoItinerary>
   : F3<outs, ins, asmstr, pattern, itin> {
  bits<5> rs2;

  let op         = opVal;
  let op3        = op3val;

  let Inst{13-5} = opfval;   // fp opcode
  let Inst{4-0}  = rs2;
}

// Shift by register rs2.
class F3_Sr<bits<2> opVal, bits<6> op3val, bit xVal, dag outs, dag ins,
            string asmstr, list<dag> pattern, InstrItinClass itin = IIC_iu_instr>
   : F3<outs, ins, asmstr, pattern, itin> {
  bit x = xVal;           // 1 for 64-bit shifts.
  bits<5> rs2;

  let op         = opVal;
  let op3        = op3val;

  let Inst{13}   = 0;     // i field = 0
  let Inst{12}   = x;     // extended registers.
  let Inst{4-0}  = rs2;
}

// Shift by immediate.
class F3_Si<bits<2> opVal, bits<6> op3val, bit xVal, dag outs, dag ins,
            string asmstr, list<dag> pattern, InstrItinClass itin = IIC_iu_instr>
   : F3<outs, ins, asmstr, pattern, itin> {
  bit x = xVal;           // 1 for 64-bit shifts.
  bits<6> shcnt;          // shcnt32 / shcnt64.

  let op         = opVal;
  let op3        = op3val;

  let Inst{13}   = 1;     // i field = 1
  let Inst{12}   = x;     // extended registers.
  let Inst{5-0}  = shcnt;
}

// Define rr and ri shift instructions with patterns.
multiclass F3_S<string OpcStr, bits<6> Op3Val, bit XVal, SDNode OpNode,
                ValueType VT, Operand SIT, RegisterClass RC,
                InstrItinClass itin = IIC_iu_instr> {
  def rr : F3_Sr<2, Op3Val, XVal, (outs RC:$rd), (ins RC:$rs1, IntRegs:$rs2),
                 !strconcat(OpcStr, " $rs1, $rs2, $rd"),
                 [(set VT:$rd, (OpNode VT:$rs1, i32:$rs2))],
                 itin>;
  def ri : F3_Si<2, Op3Val, XVal, (outs RC:$rd), (ins RC:$rs1, SIT:$shcnt),
                 !strconcat(OpcStr, " $rs1, $shcnt, $rd"),
                 [(set VT:$rd, (OpNode VT:$rs1, (i32 imm:$shcnt)))],
                 itin>;
}

class F4<bits<6> op3, dag outs, dag ins, string asmstr, list<dag> pattern,
         InstrItinClass itin = NoItinerary>
   : InstSP<outs, ins, asmstr, pattern, itin> {
  bits<5> rd;

  let op          = 2;
  let Inst{29-25} = rd;
  let Inst{24-19} = op3;
}


class F4_1<bits<6> op3, dag outs, dag ins,
           string asmstr, list<dag> pattern,
           InstrItinClass itin = NoItinerary>
   : F4<op3, outs, ins, asmstr, pattern, itin> {
  bit    intcc;
  bits<2> cc;
  bits<4> cond;
  bits<5> rs2;

  let Inst{4-0}   = rs2;
  let Inst{12-11} = cc;
  let Inst{13}    = 0;
  let Inst{17-14} = cond;
  let Inst{18}    = intcc;
}

class F4_2<bits<6> op3, dag outs, dag ins,
            string asmstr, list<dag> pattern,
            InstrItinClass itin = NoItinerary>
   : F4<op3, outs, ins, asmstr, pattern, itin> {
  bit      intcc;
  bits<2>  cc;
  bits<4>  cond;
  bits<11> simm11;

  let Inst{10-0}  = simm11;
  let Inst{12-11} = cc;
  let Inst{13}    = 1;
  let Inst{17-14} = cond;
  let Inst{18}    = intcc;
}

class F4_3<bits<6> op3, bits<6> opf_low, dag outs, dag ins,
           string asmstr, list<dag> pattern,
           InstrItinClass itin = NoItinerary>
   : F4<op3, outs, ins, asmstr, pattern, itin> {
  bits<4> cond;
  bit     intcc;
  bits<2> opf_cc;
  bits<5> rs2;

  let Inst{18}     = 0;
  let Inst{17-14}  = cond;
  let Inst{13}     = intcc;
  let Inst{12-11}  = opf_cc;
  let Inst{10-5}   = opf_low;
  let Inst{4-0}    = rs2;
}

class F4_4r<bits<6> op3, bits<5> opf_low, bits<3> rcond, dag outs, dag ins,
            string asmstr, list<dag> pattern,
            InstrItinClass itin = NoItinerary>
   : F4<op3, outs, ins, asmstr, pattern, itin> {
  bits <5> rs1;
  bits <5> rs2;
  let Inst{18-14} = rs1;
  let Inst{13}    = 0;  // IsImm
  let Inst{12-10} = rcond;
  let Inst{9-5}   = opf_low;
  let Inst{4-0}   = rs2;
}


class F4_4i<bits<6> op3, bits<3> rcond, dag outs, dag ins,
            string asmstr, list<dag> pattern,
           InstrItinClass itin = NoItinerary>
   : F4<op3, outs, ins, asmstr, pattern, itin> {
  bits<5> rs1;
  bits<10> simm10;
  let Inst{18-14} = rs1;
  let Inst{13}    = 1;  // IsImm
  let Inst{12-10} = rcond;
  let Inst{9-0}   = simm10;
}


class TRAPSP<bits<6> op3Val, bit isimm, dag outs, dag ins,
             string asmstr, list<dag> pattern,
             InstrItinClass itin = NoItinerary>
   : F3<outs, ins, asmstr, pattern, itin> {
   bits<4> cond;
   bits<2> cc;

   let op = 0b10;
   let rd{4} = 0;
   let rd{3-0} = cond;
   let op3 = op3Val;
   let Inst{13} = isimm;
   let Inst{12-11} = cc;

}

class TRAPSPrr<bits<6> op3Val, dag outs, dag ins,
               string asmstr, list<dag> pattern,
               InstrItinClass itin = NoItinerary>
   : TRAPSP<op3Val, 0, outs, ins, asmstr, pattern, itin> {
   bits<5> rs2;

   let Inst{10-5} = 0;
   let Inst{4-0}  = rs2;
}

class TRAPSPri<bits<6> op3Val, dag outs, dag ins,
               string asmstr, list<dag> pattern,
               InstrItinClass itin = NoItinerary>
   : TRAPSP<op3Val, 1, outs, ins, asmstr, pattern, itin> {
   bits<8> imm;

   let Inst{10-8} = 0;
   let Inst{7-0}  = imm;
}

// Pseudo-instructions for alternate assembly syntax (never used by codegen).
// These are aliases that require C++ handling to convert to the target
// instruction, while InstAliases can be handled directly by tblgen.
class AsmPseudoInst<dag outs, dag ins, string asm>
  : InstSP<outs, ins, asm, []> {
  let isPseudo = 1;
}
