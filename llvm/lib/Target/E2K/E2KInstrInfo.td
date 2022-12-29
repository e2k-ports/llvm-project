//===-- E2KInstrInfo.td - Target Description for E2K Target -----------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file describes the E2K instructions in TableGen format.
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Instruction format superclass
//===----------------------------------------------------------------------===//

include "E2KInstrFormats.td"

//===----------------------------------------------------------------------===//
// NOP
//===----------------------------------------------------------------------===//

// no operation, encoded in HS itself
def NOOP : NOPInst<(outs), (ins u8imm:$count), "nop $count">;

//===----------------------------------------------------------------------===//
// SS
//===----------------------------------------------------------------------===//

// instruction prefetch depth
def IPD : SSInst<(outs), (ins u8imm:$count), "ipd $count">;

def VDFI : SSInst<(outs), (ins), "vdfi">;

// end array prefetch
def EAP : SSInst<(outs), (ins), "eap">;

// begin array prefetch
def BAP : SSInst<(outs), (ins), "bap">;

def CRP : SSInst<(outs), (ins), "crp">;

def SRP : SSInst<(outs), (ins), "srp">;

def SLRP : SSInst<(outs), (ins), "slrp">;

// adjust base of global registers
def ABG : SSInst<(outs), (ins u8imm:$abgi, u8imm:$abgd), "abg abgi=$abgi, abgd=$abgd">;

// adjust base of numeric registers
def ABN : SSInst<(outs), (ins u8imm:$abnf, u8imm:$abnt), "abn abnf=$abnf abnt=$abnt">;

// adjust base of predicate registers
def ABP : SSInst<(outs), (ins u8imm:$abpf, u8imm:$abpt), "abp abpf=$abpf abpt=$abpt">;

// adjust loop counters
def ALC : SSInst<(outs), (ins u8imm:$alcf, u8imm:$alct), "alc alcf=$alcf alct=$alct">;

//===----------------------------------------------------------------------===//
// ALS opcodes
//===----------------------------------------------------------------------===//

// logical AND
defm ANDS : ALSInst<0x00, "ands", format.alopf1, SS_S>;
defm ANDD : ALSInst<0x01, "andd", format.alopf1, DD_D>;

// logical AND, inverted operand 2
defm ANDNS : ALSInst<0x02, "andns", format.alopf1, SS_S>;
defm ANDND : ALSInst<0x03, "andnd", format.alopf1, DD_D>;

// logical OR
defm ORS : ALSInst<0x04, "ors", format.alopf1, SS_S>;
defm ORD : ALSInst<0x05, "ord", format.alopf1, DD_D>;

// logical OR, inverted operand 2
defm ORNS : ALSInst<0x06, "orns", format.alopf1, SS_S>;
defm ORND : ALSInst<0x07, "ornd", format.alopf1, DD_D>;

// logical XOR
defm XORS : ALSInst<0x08, "xors", format.alopf1, SS_S>;
defm XORD : ALSInst<0x09, "xord", format.alopf1, DD_D>;

// logical XOR, inverted operand 2
defm XORNS : ALSInst<0x0A, "xorns", format.alopf1, SS_S>;
defm XORND : ALSInst<0x0B, "xornd", format.alopf1, DD_D>;

// sign-extend or zero-extend
defm SXT : ALSInst<0x0C, "sxt", format.alopf1, SS_D>;

// 0x0D ???

// merge, choose operand based on predicate (like ternary operator)
defm MERGES : ALSInst<0x0E, "merges", format.alopf1, SS_S>;
defm MERGED : ALSInst<0x0F, "merged", format.alopf1, DD_D>;

// arithmetic ADD
defm ADDS : ALSInst<0x10, "adds", format.alopf1, SS_S>;
defm ADDD : ALSInst<0x11, "addd", format.alopf1, DD_D>;

// arithmetic SUB
defm SUBS : ALSInst<0x12, "subs", format.alopf1, SS_S>;
defm SUBD : ALSInst<0x13, "subd", format.alopf1, DD_D>;

// shift cyclic left
defm SCLS : ALSInst<0x14, "scls", format.alopf1, SS_S>;
defm SCLD : ALSInst<0x15, "scld", format.alopf1, DD_D>;

// shift cyclic right
defm SCRS : ALSInst<0x16, "scrs", format.alopf1, SS_S>;
defm SCRD : ALSInst<0x17, "scrd", format.alopf1, DD_D>;

// shift logical left
defm SHLS : ALSInst<0x18, "shls", format.alopf1, SS_S>;
defm SHLD : ALSInst<0x19, "shld", format.alopf1, DD_D>;

// shift logical right
defm SHRS : ALSInst<0x1a, "shrs", format.alopf1, SS_S>;
defm SHRD : ALSInst<0x1b, "shrd", format.alopf1, DD_D>;

// shift arithmetic right
defm SARS : ALSInst<0x1c, "sars", format.alopf1, SS_S>;
defm SARD : ALSInst<0x1d, "sard", format.alopf1, DD_D>;

// get field of an arbitrary length
defm GETFS : ALSInst<0x1e, "getfs", format.alopf1, SS_S>;
defm GETFD : ALSInst<0x1f, "getfd", format.alopf1, DD_D>;

// integer comparison
// compare, overflow
defm CMPOSB : ALSCMPInst<0x20, 0, "cmposb", format.alopf7, SS_B>;
defm CMPODB : ALSCMPInst<0x21, 0, "cmpodb", format.alopf7, DD_B>;

// compare, less (without sign)
defm CMPBSB : ALSCMPInst<0x20, 1, "cmpbsb", format.alopf7, SS_B>;
defm CMPBDB : ALSCMPInst<0x21, 1, "cmpbdb", format.alopf7, DD_B>;

// compare, equal
defm CMPESB : ALSCMPInst<0x20, 2, "cmpesb", format.alopf7, SS_B>;
defm CMPEDB : ALSCMPInst<0x21, 2, "cmpedb", format.alopf7, DD_B>;

// compare, below or equal (without sing)
defm CMPBESB : ALSCMPInst<0x20, 3, "cmpbesb", format.alopf7, SS_B>;
defm CMPBEDB : ALSCMPInst<0x21, 3, "cmpbedb", format.alopf7, DD_B>;

// compare, negative
defm CMPSSB : ALSCMPInst<0x20, 4, "cmpssb", format.alopf7, SS_B>;
defm CMPSDB : ALSCMPInst<0x21, 4, "cmpsdb", format.alopf7, DD_B>;

// compare, parity
defm CMPPSB : ALSCMPInst<0x20, 5, "cmppsb", format.alopf7, SS_B>;
defm CMPPDB : ALSCMPInst<0x21, 5, "cmppdb", format.alopf7, DD_B>;

// compare, less (with sign)
defm CMPLSB : ALSCMPInst<0x20, 6, "cmplsb", format.alopf7, SS_B>;
defm CMPLDB : ALSCMPInst<0x21, 6, "cmpldb", format.alopf7, DD_B>;

// compare, less or equal (with sign)
defm CMPLESB : ALSCMPInst<0x20, 7, "cmplesb", format.alopf7, SS_B>;
defm CMPLEDB : ALSCMPInst<0x21, 7, "cmpledb", format.alopf7, DD_B>;

// compare and set predicate, equal
defm CMPANDESB : ALSCMPInst<0x22, 2, "cmpandesb", format.alopf7, SS_B>;
defm CMPANDEDB : ALSCMPInst<0x23, 2, "cmpandedb", format.alopf7, DD_B>;

// compare and set predicate, negative
defm CMPANDSSB : ALSCMPInst<0x22, 4, "cmpandssb", format.alopf7, SS_B>;
defm CMPANDSDB : ALSCMPInst<0x23, 4, "cmpandsdb", format.alopf7, DD_B>;

// compare and set predicate, parity
defm CMPANDPSB : ALSCMPInst<0x22, 5, "cmpandpsb", format.alopf7, SS_B>;
defm CMPANDPDB : ALSCMPInst<0x23, 5, "cmpandpdb", format.alopf7, DD_B>;

// compare and set predicate, less or equal (with sign)
defm CMPANDLESB : ALSCMPInst<0x22, 7, "cmpandlesb", format.alopf7, SS_B>;
defm CMPANDLEDB : ALSCMPInst<0x23, 7, "cmpandledb", format.alopf7, DD_B>;

// store byte
defm STB : ALSInstSRC3<0x24, "stb", format.alopf3, DDS, c.c25>;

defm CCTOPO  : ALSCMPInst<0x24, 0, "cctopo", format.alopf8, S_B, c.c03>;
defm CCTOPB  : ALSCMPInst<0x24, 1, "cctopb", format.alopf8, S_B, c.c03>;
defm CCTOPE  : ALSCMPInst<0x24, 2, "cctope", format.alopf8, S_B, c.c03>;
defm CCTOPBE : ALSCMPInst<0x24, 3, "cctopbe", format.alopf8, S_B, c.c03>;
defm CCTOPS  : ALSCMPInst<0x24, 4, "cctops", format.alopf8, S_B, c.c03>;
defm CCTOPP  : ALSCMPInst<0x24, 5, "cctopp", format.alopf8, S_B, c.c03>;
defm CCTOPL  : ALSCMPInst<0x24, 6, "cctopl", format.alopf8, S_B, c.c03>;
defm CCTOPLE : ALSCMPInst<0x24, 7, "cctople", format.alopf8, S_B, c.c03>;

// store half-word
defm STH : ALSInstSRC3<0x25, "sth", format.alopf3, DDS, c.c25>;

// store word
defm STW : ALSInstSRC3<0x26, "stw", format.alopf3, DDS, c.c25>;

defm BITREVS : ALSEXTInst<0x26, 0xC0, "bitrevs", format.alopf2, S_S, c.c0134>;

// store double-word
defm STD : ALSInstSRC3<0x27, "std", format.alopf3, DDS, c.c25>;

defm BITREVD : ALSEXTInst<0x27, 0xC0, "bitrevd", format.alopf2, D_D, c.c0134>;

defm STCSB : ALSInstSRC3<0x28, "stcsb", format.alopf3, SSS, c.c25>;

defm FXCMPEQSB  : ALSCMPInst<0x28, 0, "fxcmpeqsb", format.alopf7, SS_B>;
defm FXCMPLTSB  : ALSCMPInst<0x28, 1, "fxcmpltsb", format.alopf7, SS_B>;
defm FXCMPLESB  : ALSCMPInst<0x28, 2, "fxcmplesb", format.alopf7, SS_B>;
defm FXCMPUODSB : ALSCMPInst<0x28, 3, "fxcmpuodsb", format.alopf7, SS_B>;
defm FXCMPNEQSB : ALSCMPInst<0x28, 4, "fxcmpneqsb", format.alopf7, SS_B>;
defm FXCMPNLTSB : ALSCMPInst<0x28, 5, "fxcmpnltsb", format.alopf7, SS_B>;
defm FXCMPNLESB : ALSCMPInst<0x28, 6, "fxcmpnlesb", format.alopf7, SS_B>;
defm FXCMPODSB  : ALSCMPInst<0x28, 7, "fxcmpodsb", format.alopf7, SS_B>;

defm STCSH : ALSInstSRC3<0x29, "stcsb", format.alopf3, SSS, c.c25>;

defm FXCMPEQDB  : ALSCMPInst<0x29, 0, "fxcmpeqdb", format.alopf7, DD_B>;
defm FXCMPLTDB  : ALSCMPInst<0x29, 1, "fxcmpltdb", format.alopf7, DD_B>;
defm FXCMPLEDB  : ALSCMPInst<0x29, 2, "fxcmpledb", format.alopf7, DD_B>;
defm FXCMPNEQDB : ALSCMPInst<0x29, 4, "fxcmpneqdb", format.alopf7, DD_B>;
defm FXCMPNLTDB : ALSCMPInst<0x29, 5, "fxcmpnltdb", format.alopf7, DD_B>;
defm FXCMPNLEDB : ALSCMPInst<0x29, 6, "fxcmpnledb", format.alopf7, DD_B>;
defm FXCMPODDB  : ALSCMPInst<0x29, 7, "fxcmpoddb", format.alopf7, DD_B>;

defm STCSW : ALSInstSRC3<0x2a, "stcsw", format.alopf3, SSS, c.c25>;

defm STCSD : ALSInstSRC3<0x2b, "stcsd", format.alopf3, SSD, c.c25>;

defm FXCMPEQXB  : ALSCMPInst<0x2b, 0, "fxcmpeqxb", format.alopf7, DD_B>;
defm FXCMPLTXB  : ALSCMPInst<0x2b, 1, "fxcmpltxb", format.alopf7, DD_B>;
defm FXCMPLEXB  : ALSCMPInst<0x2b, 2, "fxcmplexb", format.alopf7, DD_B>;
defm FXCMPNEQXB : ALSCMPInst<0x2b, 4, "fxcmpneqxb", format.alopf7, DD_B>;
defm FXCMPNLTXB : ALSCMPInst<0x2b, 5, "fxcmpnltxb", format.alopf7, DD_B>;
defm FXCMPNLEXB : ALSCMPInst<0x2b, 6, "fxcmpnlexb", format.alopf7, DD_B>;
defm FXCMPODXB  : ALSCMPInst<0x2b, 7, "fxcmpodxb", format.alopf7, DD_B>;

defm STDSB : ALSInstSRC3<0x2c, "stdsb", format.alopf3, SSS, c.c25>;

defm STDSH : ALSInstSRC3<0x2d, "stdsh", format.alopf3, SSS, c.c25>;

defm STDSW : ALSInstSRC3<0x2e, "stdsw", format.alopf3, SSS, c.c25>;

defm FCMPEQSB  : ALSCMPInst<0x2e, 0, "fcmpeqsb", format.alopf7, SS_B>;
defm FCMPLTSB  : ALSCMPInst<0x2e, 1, "fcmpltsb", format.alopf7, SS_B>;
defm FCMPLESB  : ALSCMPInst<0x2e, 2, "fcmplesb", format.alopf7, SS_B>;
defm FCMPNEQSB : ALSCMPInst<0x2e, 4, "fcmpneqsb", format.alopf7, SS_B>;
defm FCMPNLTSB : ALSCMPInst<0x2e, 5, "fcmpnltsb", format.alopf7, SS_B>;
defm FCMPNLESB : ALSCMPInst<0x2e, 6, "fcmpnlesb", format.alopf7, SS_B>;
defm FCMPODSB  : ALSCMPInst<0x2e, 7, "fcmpodsb", format.alopf7, SS_B>;

defm STDSD : ALSInstSRC3<0x2f, "stdsd", format.alopf3, SSD, c.c25>;

defm FCMPEQDB  : ALSCMPInst<0x2f, 0, "fcmpeqdb", format.alopf7, DD_B>;
defm FCMPLTDB  : ALSCMPInst<0x2f, 1, "fcmpltdb", format.alopf7, DD_B>;
defm FCMPLEDB  : ALSCMPInst<0x2f, 2, "fcmpledb", format.alopf7, DD_B>;
defm FCMPNEQDB : ALSCMPInst<0x2f, 4, "fcmpneqdb", format.alopf7, DD_B>;
defm FCMPNLTDB : ALSCMPInst<0x2f, 5, "fcmpnltdb", format.alopf7, DD_B>;
defm FCMPNLEDB : ALSCMPInst<0x2f, 6, "fcmpnledb", format.alopf7, DD_B>;
defm FCMPODDB  : ALSCMPInst<0x2f, 7, "fcmpoddb", format.alopf7, DD_B>;

defm STESB : ALSInstSRC3<0x30, "stesb", format.alopf3, SSS, c.c25>;
defm STESH : ALSInstSRC3<0x31, "stesh", format.alopf3, SSS, c.c25>;

// floating-point ADD
defm FADDS : ALSInst<0x30, "fadds", format.alopf1, SS_S, c.c0134>;
defm FADDD : ALSInst<0x31, "faddd", format.alopf1, DD_D, c.c0134>;

defm STESW : ALSInstSRC3<0x32, "stesw", format.alopf3, SSS, c.c25>;
defm STESD : ALSInstSRC3<0x33, "stesd", format.alopf3, SSD, c.c25>;

// floating-point SUB
defm FSUBS : ALSInst<0x32, "fsubs", format.alopf1, SS_S, c.c0134>;
defm FSUBD : ALSInst<0x33, "fsubd", format.alopf1, DD_D, c.c0134>;

defm STFSB : ALSInstSRC3<0x34, "stfsb", format.alopf3, SSS, c.c25>;
defm STFSH : ALSInstSRC3<0x35, "stfsh", format.alopf3, SSS, c.c25>;

// floating-point MIN
defm FMINS : ALSInst<0x34, "fmins", format.alopf1, SS_S, c.c0134>;
defm FMIND : ALSInst<0x35, "fmind", format.alopf1, DD_D, c.c0134>;

defm STFSW : ALSInstSRC3<0x36, "stfsw", format.alopf3, SSS, c.c25>;
defm STFSD : ALSInstSRC3<0x37, "stfsd", format.alopf3, SSD, c.c25>;

// floating-point MAX
defm FMAXS : ALSInst<0x36, "fmaxs", format.alopf1, SS_S, c.c0134>;
defm FMAXD : ALSInst<0x37, "fmaxd", format.alopf1, DD_D, c.c0134>;

defm STGSB : ALSInstSRC3<0x38, "stgsb", format.alopf3, SSS, c.c25>;
defm STGSH : ALSInstSRC3<0x39, "stgsh", format.alopf3, SSS, c.c25>;

// floating-point MUL
defm FMULS : ALSInst<0x38, "fmuls", format.alopf1, SS_S, c.c0134>;
defm FMULD : ALSInst<0x39, "fmuld", format.alopf1, DD_D, c.c0134>;

defm STGSW : ALSInstSRC3<0x3a, "stgsw", format.alopf3, SSS, c.c25>;
defm STGSD : ALSInstSRC3<0x3b, "stgsd", format.alopf3, SSD, c.c25>;

defm STSSB : ALSInstSRC3<0x3c, "stssb", format.alopf3, SSS, c.c25>;

// convert fp32 to int32
defm FSTOIS : ALSEXTInst<0x3c, 0xC0, "fstois", format.alopf2, S_S, c.c0134>;

// convert fp32 to int32 (with truncation)
defm FSTOISTR : ALSEXTInst<0x3c, 0xC2, "fstoistr", format.alopf2, S_S, c.c0134>;

// convert int32 to fp32
defm ISTOFS : ALSEXTInst<0x3c, 0xC4, "istofs", format.alopf2, S_S, c.c0134>;

defm STSSH : ALSInstSRC3<0x3d, "stssh", format.alopf3, SSS, c.c25>;

// convert fp64 to int64
defm FDTOID : ALSEXTInst<0x3d, 0xC0, "fdtoid", format.alopf2, D_D, c.c0134>;

// convert fp80 to int64
defm FXTOID : ALSEXTInst<0x3d, 0xC1, "fxtoid", format.alopf2, D_D, c.c0134>;

// convert fp64 to int64 (with truncation)
defm FDTOIDTR : ALSEXTInst<0x3d, 0xC2, "fdtoidtr", format.alopf2, D_D, c.c0134>;

// convert fp80 to int64 (with truncation)
defm FXTOIDTR : ALSEXTInst<0x3d, 0xC3, "fxtoidtr", format.alopf2, D_D, c.c0134>;

// convert int64 to fp64
defm IDTOFD : ALSEXTInst<0x3d, 0xC4, "idtofd", format.alopf2, D_D, c.c0134>;

// convert int64 to fp80
defm IDTOFX : ALSEXTInst<0x3d, 0xC5, "idtofx", format.alopf2, D_D, c.c0134>;

// convert fp80 to fp64
defm FXTOFD : ALSEXTInst<0x3d, 0xC6, "fxtofd", format.alopf2, D_D, c.c0134>;

// convert fp64 to fp80
defm FDTOFX : ALSEXTInst<0x3d, 0xC7, "fdtofx", format.alopf2, D_D, c.c0134>;

// package convert fp32 to int32
defm PFSTOIS : ALSEXTInst<0x3d, 0xC8, "pfstois", format.alopf2, D_D, c.c0134>;

// package convert fp32 to int32 (with truncation)
defm PFSTOISTR : ALSEXTInst<0x3d, 0xCA, "pfstoistr", format.alopf2, D_D, c.c0134>;

// package convert int32 to fp32
defm PISTOFS : ALSEXTInst<0x3d, 0xCC, "pistofs", format.alopf2, D_D, c.c0134>;

defm STSSW : ALSInstSRC3<0x3e, "stssw", format.alopf3, SSS, c.c25>;

// convert fp32 to int64
defm FSTOID : ALSEXTInst<0x3e, 0xC0, "fstoid", format.alopf2, S_D, c.c0134>;

// convert fp32 to int64 (with truncation)
defm FSTOIDTR : ALSEXTInst<0x3e, 0xC2, "fstoidtr", format.alopf2, S_D, c.c0134>;

// convert int32 to fp64
defm ISTOFD : ALSEXTInst<0x3e, 0xC4, "istofd", format.alopf2, S_D, c.c0134>;

// convert int32 to fp80
defm ISTOFX : ALSEXTInst<0x3e, 0xC5, "istofx", format.alopf2, S_D, c.c0134>;

// convert fp32 to fp64
defm FSTOFD : ALSEXTInst<0x3e, 0xC6, "fstofd", format.alopf2, S_D, c.c0134>;

// convert fp32 to fp80
defm FSTOFX : ALSEXTInst<0x3e, 0xC7, "fstofx", format.alopf2, S_D, c.c0134>;

// package convert fp32 to fp64
defm PFSTOFD : ALSEXTInst<0x3e, 0xCE, "pfstofd", format.alopf2, S_D, c.c0134>;

defm STSSD : ALSInstSRC3<0x3f, "stssd", format.alopf3, SSD, c.c25>;

// convert fp64 to int32
defm FDTOIS : ALSEXTInst<0x3f, 0xC0, "fdtois", format.alopf2, D_S, c.c0134>;

// convert fp80 to int32
defm FXTOIS : ALSEXTInst<0x3f, 0xC1, "fxtois", format.alopf2, D_S, c.c0134>;

// convert fp64 to int32 (with truncation)
defm FDTOISTR : ALSEXTInst<0x3f, 0xC2, "fdtoistr", format.alopf2, D_S, c.c0134>;

// convert fp80 to int32 (with truncation)
defm FXTOISTR : ALSEXTInst<0x3f, 0xC3, "fxtoistr", format.alopf2, D_S, c.c0134>;

// convert int64 to fp32
defm IDTOFS : ALSEXTInst<0x3f, 0xC4, "idtofs", format.alopf2, D_S, c.c0134>;

// convert fp64 to fp32
defm FDTOFS : ALSEXTInst<0x3f, 0xC6, "fdtofs", format.alopf2, D_S, c.c0134>;

// convert fp80 to fp32
defm FXTOFS : ALSEXTInst<0x3f, 0xC7, "fxtofs", format.alopf2, D_S, c.c0134>;

// packed convert fp64 to int32
defm PFDTOIS : ALSEXTInst<0x3f, 0xC8, "pfdtois", format.alopf2, D_S, c.c0134>;

// packed convert fp64 to int32 (with truncation)
defm PFDTOISTR : ALSEXTInst<0x3f, 0xCA, "pfdtoistr", format.alopf2, D_S, c.c0134>;

// packed convert fp64 to fp32
defm PFDTOFS : ALSEXTInst<0x3f, 0xCE, "pfdtofs", format.alopf2, D_S, c.c0134>;

defm FXADDSS : ALSInst<0x40, "fxaddss", format.alopf1, DS_S, c.c0134>;
defm FXADDDD : ALSInst<0x41, "fxadddd", format.alopf1, DS_D, c.c0134>;

// unsigned integer DIV
defm UDIVS : ALSInst<0x40, "udivs", format.alopf1, SS_S, c.c5>;
defm UDIVD : ALSInst<0x41, "udivd", format.alopf1, DD_D, c.c5>;
defm UDIVX : ALSInst<0x44, "udivx", format.alopf1, DS_S, c.c5>;

defm FXADDSX : ALSInst<0x42, "fxaddsx", format.alopf1, DS_D, c.c0134>;
defm FXADDDX : ALSInst<0x43, "fxadddx", format.alopf1, DD_D, c.c0134>;

// signed integer DIV
defm SDIVS : ALSInst<0x42, "sdivs", format.alopf1, SS_S, c.c5>;
defm SDIVD : ALSInst<0x43, "sdivd", format.alopf1, DD_D, c.c5>;
defm SDIVX : ALSInst<0x46, "sdivx", format.alopf1, DS_S, c.c5>;

defm FXADDXS : ALSInst<0x44, "fxaddxs", format.alopf1, DD_S, c.c0134>;
defm FXADDXD : ALSInst<0x45, "fxaddxd", format.alopf1, DD_D, c.c0134>;

// unsigned modulo (remainder) of division
defm UMODX : ALSInst<0x45, "umodx", format.alopf1, DS_S, c.c5>;

defm FXADDXX : ALSInst<0x47, "fxaddxx", format.alopf1, DD_D, c.c0134>;

// signed modulo (remainder) of division
defm SMODX : ALSInst<0x47, "smodx", format.alopf1, DS_S, c.c5>;

defm FXSUBSS : ALSInst<0x48, "fxsubss", format.alopf1, DS_S, c.c0134>;
defm FXDIVSS : ALSInst<0x48, "fxdivss", format.alopf1, DS_S, c.c5>;

defm FXSUBDD : ALSInst<0x49, "fxsubdd", format.alopf1, DD_D, c.c0134>;
defm FXDIVDD : ALSInst<0x49, "fxdivdd", format.alopf1, DD_D, c.c5>;

defm FXSUBSX : ALSInst<0x4a, "fxsubsx", format.alopf1, DS_D, c.c0134>;
defm FXDIVSX : ALSInst<0x4a, "fxdivsx", format.alopf1, DS_D, c.c5>;

defm FXSUBDX : ALSInst<0x4b, "fxsubdx", format.alopf1, DD_D, c.c0134>;
defm FXDIVDX : ALSInst<0x4b, "fxdivdx", format.alopf1, DD_D, c.c5>;

defm FXSUBXS : ALSInst<0x4c, "fxsubxs", format.alopf1, DD_S, c.c0134>;
defm FXDIVXS : ALSInst<0x4c, "fxdivxs", format.alopf1, DD_S, c.c5>;

defm FXSUBXD : ALSInst<0x4d, "fxsubxd", format.alopf1, DD_D, c.c0134>;
defm FXDIVXD : ALSInst<0x4d, "fxdivxd", format.alopf1, DD_D, c.c5>;

// 0x4e ???

defm FXSUBXX : ALSInst<0x4f, "fxsubxx", format.alopf1, DD_D, c.c0134>;
defm FXDIVXX : ALSInst<0x4f, "fxdivxdx", format.alopf1, DD_D, c.c5>;

defm FXMULSS : ALSInst<0x50, "fxmulss", format.alopf1, DS_S, c.c0134>;
defm FXMULDD : ALSInst<0x51, "fxmuldd", format.alopf1, DD_D, c.c0134>;
defm FXMULSX : ALSInst<0x52, "fxmulsx", format.alopf1, DS_D, c.c0134>;
defm FXMULDX : ALSInst<0x53, "fxmuldx", format.alopf1, DD_D, c.c0134>;

defm FXSQRTISX : ALSEXTInst<0x52, 0xC0, "fxsqrtisx", format.alopf2, S_D, c.c5>;
defm FXSQRTIDX : ALSEXTInst<0x53, 0xC0, "fxsqrtidx", format.alopf2, D_D, c.c5>;

defm FXMULXS : ALSInst<0x54, "fxmulxs", format.alopf1, SS_S, c.c0134>;
defm FXMULXD : ALSInst<0x55, "fxmulxd", format.alopf1, DD_D, c.c0134>;

// 0x56 ???

defm FXMULXX : ALSInst<0x57, "fxmulxx", format.alopf1, DD_S, c.c0134>;

defm FXSQRTIXX : ALSEXTInst<0x57, 0xC0, "fxsqrtdxx", format.alopf2, D_D, c.c5>;

defm FXRSUBSS : ALSInst<0x58, "fxrsubss", format.alopf1, DS_S, c.c0134>;
defm FXRSUBDD : ALSInst<0x59, "fxrsubdd", format.alopf1, DD_D, c.c0134>;
defm FXRSUBSX : ALSInst<0x5a, "fxrsubsx", format.alopf1, DS_D, c.c0134>;
defm FXRSUBDX : ALSInst<0x5b, "fxrsubdx", format.alopf1, DD_D, c.c0134>;

defm FXSQRTUXX : ALSInst<0x59, "fxsqrtuxx", format.alopf1, DD_D, c.c5>;
defm FXSQRTUSX : ALSInst<0x5a, "fxsqrtusx", format.alopf1, DS_D, c.c5>;
defm FXSQRTUDX : ALSInst<0x5b, "fxsqrtudx", format.alopf1, DD_D, c.c5>;

defm MOVFI : ALSEXTInst<0x5c, 0xC0, "movfi", format.alopf2, D_S, c.c0134>;

defm FXSQRTTXX : ALSInst<0x5d, "fxsqrttxx", format.alopf1, DD_D, c.c5>;

defm MOVIF : ALSInst<0x5e, "movif", format.alopf1, DS_D, c.c0134>;

defm FXSQRTTSX : ALSInst<0x5e, "fxsqrttsx", format.alopf1, DS_D, c.c5>;

defm MOVX : ALSEXTInst<0x5f, 0xC0, "movx", format.alopf2, D_D, c.c0134>;
defm MOVXA : ALSEXTInst<0x5f, 0xC1, "movxa", format.alopf2, D_D, c.c0134>;
defm MOVXC : ALSEXTInst<0x5f, 0xC2, "movxc", format.alopf2, D_D, c.c0134>;

defm FXSQRTTDX : ALSInst<0x5f, "fxsqrttdx", format.alopf1, DD_D, c.c5>;

// move tagged value
defm MOVTS : ALSEXTInst<0x60, 0xC0, "movts", format.alopf2, S_S, c.c0134>;
defm MOVTCS : ALSEXTInst<0x60, 0xC1, "movtcs", format.alopf2, S_S, c.c0134>;
defm MOVTRS : ALSEXTInst<0x60, 0xC2, "movtrs", format.alopf2, S_S, c.c0134>;
defm MOVTRCS : ALSEXTInst<0x60, 0xC3, "movtrcs", format.alopf2, S_S, c.c0134>;

defm FXDIVTSS : ALSInst<0x60, "fxdivtss", format.alopf1, DS_S, c.c5>;

defm MOVTD : ALSEXTInst<0x61, 0xC0, "movtd", format.alopf2, D_D, c.c0134>;
defm MOVTCD : ALSEXTInst<0x61, 0xC1, "movtcd", format.alopf2, D_D, c.c0134>;
defm MOVTRD : ALSEXTInst<0x61, 0xC2, "movtrd", format.alopf2, D_D, c.c0134>;
defm MOVTRCD : ALSEXTInst<0x61, 0xC3, "movtrcd", format.alopf2, D_D, c.c0134>;

defm FXDIVTDD : ALSInst<0x61, "fxdivtdd", format.alopf1, DD_D, c.c5>;

defm GETSAP : ALSEXTInst<0x62, 0xEC, "getsap", format.alopf2, S_Q, c.c0134>;
defm CUDTOAP : ALSEXTInst<0x62, 0xF0, "cudtoap", format.alopf2, S_Q, c.c0134>;
defm GDTOAP : ALSEXTInst<0x62, 0xF2, "gdtoap", format.alopf2, S_Q, c.c0134>;

defm FXDIVTSX : ALSInst<0x62, "fxdivtsx", format.alopf1, DS_D, c.c5>;

defm GETPL : ALSEXTInst<0x63, 0xF0, "getpl", format.alopf2, S_D, c.c03>;

defm VFSI : ALSInst<0x63, "vfsi", format.alopf1, SD_D, c.c14>;

defm FXDIVTDX : ALSInst<0x63, "fxdivtdx", format.alopf1, DD_D, c.c5>;

// load unsigned byte
defm LDB : ALSInst<0x64, "ldb", format.alopf1, DD_D, c.c0235>;

// load unsigned half-word
defm LDH : ALSInst<0x65, "ldh", format.alopf1, DD_D, c.c0235>;

// count number of leading zeroes
defm LZCNTS : ALSEXTInst<0x64, 0xC0, "lzcnts", format.alopf2, S_S, c.c14>;
defm LZCNTD : ALSEXTInst<0x65, 0xC0, "lzcntd", format.alopf2, D_D, c.c14>;

// load unsigned word
defm LDW : ALSInst<0x66, "ldw", format.alopf1, DD_D, c.c0235>;

// load unsigned double-word
defm LDD : ALSInst<0x67, "ldd", format.alopf1, DD_D, c.c0235>;

// count of bits set to one
defm POPCNTS : ALSEXTInst<0x66, 0xC0, "popcnts", format.alopf2, S_S, c.c14>;
defm POPCNTD : ALSEXTInst<0x67, 0xC0, "popcntd", format.alopf2, D_D, c.c14>;

defm LDCSB : ALSInst<0x68, "ldcsb", format.alopf1, SS_D, c.c0235>;

defm VFBGV : ALSInst<0x68, "vfbgv", format.alopf1, SS_S, c.c14>;

defm LDCSH : ALSInst<0x69, "ldcsh", format.alopf1, SS_D, c.c0235>;

defm MODBGV : ALSEXTInst<0x69, 0xC0, "modbgv", format.alopf2, S_S, c.c14>;

defm LDCSW : ALSInst<0x6a, "ldcsw", format.alopf1, SS_D, c.c0235>;

defm MKFSW : ALSInst<0x6a, "mkfsw", format.alopf1, SS_D, c.c14>;

defm LDCSD : ALSInst<0x6b, "ldcsd", format.alopf1, SS_D, c.c0235>;

defm LDDSB : ALSInst<0x6c, "lddsb", format.alopf1, SS_D, c.c0235>;

defm LDDSH : ALSInst<0x6d, "lddsh", format.alopf1, SS_D, c.c0235>;

defm LDDSW : ALSInst<0x6e, "lddsw", format.alopf1, SS_D, c.c0235>;

defm LDDSD : ALSInst<0x6f, "lddsd", format.alopf1, SS_D, c.c0235>;

defm LDESB : ALSInst<0x70, "ldesb", format.alopf1, SS_D, c.c0235>;

defm LDESH : ALSInst<0x71, "ldesh", format.alopf1, SS_D, c.c0235>;

defm LDESW : ALSInst<0x72, "ldesw", format.alopf1, SS_D, c.c0235>;

defm LDESD : ALSInst<0x73, "ldesd", format.alopf1, SS_D, c.c0235>;

defm LDFSB : ALSInst<0x74, "ldfsb", format.alopf1, SS_D, c.c0235>;

defm LDFSH : ALSInst<0x75, "ldfsh", format.alopf1, SS_D, c.c0235>;

defm LDFSW : ALSInst<0x76, "ldfsw", format.alopf1, SS_D, c.c0235>;

defm LDFSD : ALSInst<0x77, "ldfsd", format.alopf1, SS_D, c.c0235>;

defm LDGSB : ALSInst<0x78, "ldgsb", format.alopf1, SS_D, c.c0235>;

defm LDGSH : ALSInst<0x79, "ldgsh", format.alopf1, SS_D, c.c0235>;

defm LDGSW : ALSInst<0x7a, "ldgsw", format.alopf1, SS_D, c.c0235>;

defm LDGSD : ALSInst<0x7b, "ldgsd", format.alopf1, SS_D, c.c0235>;

defm LDSSB : ALSInst<0x7c, "ldssb", format.alopf1, SS_D, c.c0235>;

defm LDSSH : ALSInst<0x7d, "ldssh", format.alopf1, SS_D, c.c0235>;

defm LDSSW : ALSInst<0x7e, "ldssw", format.alopf1, SS_D, c.c0235>;

defm LDSSD : ALSInst<0x7f, "ldssw", format.alopf1, SS_D, c.c0235>;

//===----------------------------------------------------------------------===//
// ALES opcodes, opcode2 = 1
//===----------------------------------------------------------------------===//

defm PMINUB : ALESEXT2Inst<0x00, 0x01, 0xC0, "pminub", format.alopf11, DD_D, c.c03>;
defm PUNPCKHBH : ALESEXT2Inst<0x00, 0x01, 0xC0, "punpckhbh", format.alopf11, DD_S, c.c14>;

defm PMINSH : ALESEXT2Inst<0x01, 0x01, 0xC0, "pminsh", format.alopf11, DD_D, c.c03>;
defm PUNPCKLBH : ALESEXT2Inst<0x01, 0x01, 0xC0, "punpcklbh", format.alopf11, DD_D, c.c14>;
defm STQ : ALESEXT2SRC3Inst<0x01, 0x01, 0xC0, "stq", format.alopf13, DDQ, c.c25>;

defm PMAXUB : ALESEXT2Inst<0x02, 0x01, 0xC0, "pmaxub", format.alopf11, DD_D, c.c03>;
defm PUNPCKHHW : ALESEXT2Inst<0x02, 0x01, 0xC0, "punpckhhw", format.alopf11, DD_D, c.c14>;
defm STCSQ : ALESEXT2SRC3Inst<0x02, 0x01, 0xC0, "stcsq", format.alopf13, SSQ, c.c2>;

defm PMAXSH : ALESEXT2Inst<0x03, 0x01, 0xC0, "pmaxsh", format.alopf11, DD_D, c.c03>;
defm PUNPCKLHW : ALESEXT2Inst<0x03, 0x01, 0xC0, "punpcklhw", format.alopf11, DD_D, c.c14>;
defm STDSQ : ALESEXT2SRC3Inst<0x03, 0x01, 0xC0, "stdsq", format.alopf13, SSQ, c.c2>;

defm PMINSB : ALESEXT2Inst<0x04, 0x01, 0xC0, "pminsb", format.alopf11, DD_D, c.c03>;
defm PUNPCKHWD : ALESEXT2Inst<0x04, 0x01, 0xC0, "punpckhwd", format.alopf11, DD_D, c.c14>;
defm STESQ : ALESEXT2SRC3Inst<0x04, 0x01, 0xC0, "stesq", format.alopf13, SSQ, c.c2>;

defm PMINUH : ALESEXT2Inst<0x05, 0x01, 0xC0, "pminuh", format.alopf11, DD_D, c.c03>;
defm PUNPCKLWD : ALESEXT2Inst<0x05, 0x01, 0xC0, "punpcklwd", format.alopf11, DD_D, c.c14>;
defm STFSQ : ALESEXT2SRC3Inst<0x05, 0x01, 0xC0, "stfsq", format.alopf13, SSQ, c.c2>;

defm PMAXSB : ALESEXT2Inst<0x06, 0x01, 0xC0, "pmaxsb", format.alopf11, DD_D, c.c03>;
defm PMOVMSKPS : ALESEXT2Inst<0x06, 0x01, 0xC0, "pmovmskps", format.alopf11, DD_D, c.c14>;
defm STGSQ : ALESEXT2SRC3Inst<0x06, 0x01, 0xC0, "stgsq", format.alopf13, SSQ, c.c2>;

defm PMAXUH : ALESEXT2Inst<0x07, 0x01, 0xC0, "pmaxuh", format.alopf11, DD_D, c.c03>;
defm PMOVMSKPD : ALESEXT2Inst<0x07, 0x01, 0xC0, "pmovmskpd", format.alopf11, DD_D, c.c14>;
defm STSSQ : ALESEXT2SRC3Inst<0x07, 0x01, 0xC0, "stssq", format.alopf13, SSQ, c.c2>;

defm PADDB : ALESEXT2Inst<0x08, 0x01, 0xC0, "paddb", format.alopf11, DD_D, c.c03>;
defm PACKSSHB : ALESEXT2Inst<0x08, 0x01, 0xC0, "packsshb", format.alopf11, DD_D, c.c14>;
defm GETTAGS : ALESEXT2Inst<0x08, 0x01, 0xC0, "gettags", format.alopf11, SS_S, c.c25>;

defm PADDH : ALESEXT2Inst<0x09, 0x01, 0xC0, "paddh", format.alopf11, DD_D, c.c03>;
defm PACKUSHB : ALESEXT2Inst<0x09, 0x01, 0xC0, "packushb", format.alopf11, DD_D, c.c14>;
defm GETTAGD : ALESEXT2Inst<0x09, 0x01, 0xC0, "gettags", format.alopf11, DD_D, c.c25>;

defm PADDSB : ALESEXT2Inst<0x0a, 0x01, 0xC0, "paddsb", format.alopf11, DD_D, c.c03>;
defm PACKSSWH : ALESEXT2Inst<0x0a, 0x01, 0xC0, "packsswh", format.alopf11, DD_D, c.c14>;
defm PUTTAGS : ALESEXT2Inst<0x0a, 0x01, 0xC0, "puttags", format.alopf11, SS_S, c.c25>;

defm PADDSH : ALESEXT2Inst<0x0b, 0x01, 0xC0, "paddsh", format.alopf11, DD_D, c.c03>;
defm PMOVMSKB : ALESEXT2Inst<0x0b, 0x01, 0xC0, "pmovmskb", format.alopf11, DD_D, c.c14>;
defm PUTTAGD : ALESEXT2Inst<0x0b, 0x01, 0xC0, "puttagd", format.alopf11, DD_D, c.c25>;

defm PADDUSB : ALESEXT2Inst<0x0c, 0x01, 0xC0, "paddusb", format.alopf11, DD_D, c.c03>;
defm PSRLQH : ALESInst<0x0c, 0x01, "psrlqh", format.alopf21, SSS_S, c.c14>;

defm PADDUSH : ALESEXT2Inst<0x0d, 0x01, 0xC0, "paddush", format.alopf11, DD_D, c.c03>;
defm PSRLQL : ALESInst<0x0d, 0x01, "psrlql", format.alopf21, DDD_D, c.c14>;

defm PADDW : ALESEXT2Inst<0x0e, 0x01, 0xC0, "paddw", format.alopf11, DD_D, c.c03>;
defm PSLLQH : ALESInst<0x0e, 0x01, "psllqh", format.alopf21, DDD_D, c.c14>;

defm PADDD : ALESEXT2Inst<0x0f, 0x01, 0xC0, "paddd", format.alopf11, DD_D, c.c03>;
defm PSLLQL : ALESInst<0x0f, 0x01, "psllql", format.alopf21, DDD_D, c.c14>;

defm PSUBB : ALESEXT2Inst<0x10, 0x01, 0xC0, "psubb", format.alopf11, DD_D, c.c03>;
defm PSRLW : ALESEXT2Inst<0x10, 0x01, 0xC0, "psrlw", format.alopf11, DD_D, c.c14>;

defm PSUBH : ALESEXT2Inst<0x11, 0x01, 0xC0, "psubh", format.alopf11, DD_D, c.c03>;
defm PSRLH : ALESEXT2Inst<0x11, 0x01, 0xC0, "psrlh", format.alopf11, DD_D, c.c14>;

defm PSUBSB : ALESEXT2Inst<0x12, 0x01, 0xC0, "psubsb", format.alopf11, DD_D, c.c03>;
defm PSRAW : ALESEXT2Inst<0x12, 0x01, 0xC0, "psraw", format.alopf11, DD_D, c.c14>;

defm PSUBSH : ALESEXT2Inst<0x13, 0x01, 0xC0, "psubsh", format.alopf11, DD_D, c.c03>;
defm PSRAH : ALESEXT2Inst<0x13, 0x01, 0xC0, "psrah", format.alopf11, DD_D, c.c14>;

defm PSUBUSB : ALESEXT2Inst<0x14, 0x01, 0xC0, "psubusb", format.alopf11, DD_D, c.c03>;
defm PSLLW : ALESEXT2Inst<0x14, 0x01, 0xC0, "psllw", format.alopf11, DD_D, c.c14>;

defm PSUBUSH : ALESEXT2Inst<0x15, 0x01, 0xC0, "psubush", format.alopf11, DD_D, c.c03>;
defm PSLLH : ALESEXT2Inst<0x15, 0x01, 0xC0, "psllh", format.alopf11, DD_D, c.c14>;

defm PSUBW : ALESEXT2Inst<0x16, 0x01, 0xC0, "psubw", format.alopf11, DD_D, c.c03>;
defm PSHUFW : ALESInst<0x16, 0x01, "pshufw", format.alopf21, DDD_D, c.c14>;

defm PSUBD : ALESEXT2Inst<0x17, 0x01, 0xC0, "psubd", format.alopf11, DD_D, c.c03>;
// FIXME : PSHUFH has some special encoding?
defm PSHUFH : ALESEXT2Inst<0x17, 0x01, 0xC0, "pshufh", format.alopf11, DD_D, c.c14>;

defm PCMPEQB : ALESEXT2Inst<0x18, 0x01, 0xC0, "pcmpeqb", format.alopf11, DD_D, c.c03>;
defm PMULHH : ALESEXT2Inst<0x18, 0x01, 0xC0, "pmulhh", format.alopf11, DD_D, c.c14>;

defm PCMPEQH : ALESEXT2Inst<0x19, 0x01, 0xC0, "pcmpeqh", format.alopf11, DD_D, c.c03>;
defm PMULLH : ALESEXT2Inst<0x19, 0x01, 0xC0, "pmullh", format.alopf11, DD_D, c.c14>;

defm PCMPEQW : ALESEXT2Inst<0x1a, 0x01, 0xC0, "pcmpeqw", format.alopf11, DD_D, c.c03>;
defm PMADDH : ALESEXT2Inst<0x1a, 0x01, 0xC0, "pmaddh", format.alopf11, DD_D, c.c14>;

defm PCMPGTB : ALESEXT2Inst<0x1b, 0x01, 0xC0, "pcmpgtb", format.alopf11, DD_D, c.c03>;
defm PMULHUH : ALESEXT2Inst<0x1b, 0x01, 0xC0, "pmulhuh", format.alopf11, DD_D, c.c14>;
defm STRD : ALESEXT2SRC3Inst<0x1b, 0x01, 0xC0, "strd", format.alopf13, DDD, c.c25>;

defm PCMPGTH : ALESEXT2Inst<0x1c, 0x01, 0xC0, "pcmpgth", format.alopf11, DD_D, c.c03>;
defm PSADBW : ALESEXT2Inst<0x1c, 0x01, 0xC0, "psadbw", format.alopf11, DD_D, c.c14>;
/* store array byte */
defm STAAB : ALESEXT2SRC3Inst<0x1c, 0x01, 0xC0, "staab", format.alopf10, PPS, c.c25>;

defm PCMPGTW : ALESEXT2Inst<0x1d, 0x01, 0xC0, "pcmpgtw", format.alopf11, DD_D, c.c03>;
defm PMULUBHH : ALESEXT2Inst<0x1d, 0x01, 0xC0, "pmulubhh", format.alopf11, DD_D, c.c14>;
/* store array half-word */
defm STAAH : ALESEXT2SRC3Inst<0x1d, 0x01, 0xC0, "staah", format.alopf10, PPS, c.c25>;

defm PAVGUSB : ALESEXT2Inst<0x1e, 0x01, 0xC0, "pavgusb", format.alopf11, DD_D, c.c03>;
defm PEXTRH : ALESInst<0x1e, 0x01, "pextrh", format.alopf21, DDD_D, c.c14>;
// store array word
defm STAAW : ALESEXT2SRC3Inst<0x1e, 0x01, 0xC0, "staaw", format.alopf10, PPS, c.c25>;

defm PAVGUSH : ALESEXT2Inst<0x1f, 0x01, 0xC0, "pavgush", format.alopf11, DD_D, c.c03>;
defm PINSH : ALESInst<0x1f, 0x01, "pinsh", format.alopf21, DDD_D, c.c14>;
// store array double-word
defm STAAD : ALESEXT2SRC3Inst<0x1f, 0x01, 0xC0, "staad", format.alopf10, PPD, c.c25>;

// integer MUL
defm MULS : ALESEXT2Inst<0x20, 0x01, 0xC0, "muls", format.alopf11, SS_S, c.c0134>;
defm MULD : ALESEXT2Inst<0x21, 0x01, 0xC0, "muld", format.alopf11, DD_D, c.c0134>;

// integer MUL (without sign)
defm UMULX : ALESEXT2Inst<0x22, 0x01, 0xC0, "umulx", format.alopf11, SS_D, c.c0134>;

// integer MUL (with sign)
defm SMULX : ALESEXT2Inst<0x23, 0x01, 0xC0, "smulx", format.alopf11, SS_D, c.c0134>;

// multiply by integer power of two
defm FSCALES : ALESEXT2Inst<0x24, 0x01, 0xC0, "fscales", format.alopf11, SS_S, c.c14>;
defm FSCALED : ALESEXT2Inst<0x25, 0x01, 0xC0, "fscaled", format.alopf11, DS_D, c.c14>;

defm GETTC : ALESEXT2Inst<0x24, 0x01, 0xC0, "gettc", format.alopf11, SS_D, c.c0>;
defm GETTST : ALESEXT2Inst<0x24, 0x01, 0xC0, "gettst", format.alopf11, SS_D, c.c3>;

// store byte (guarded)
defm STGDB : ALESEXT2SRC3Inst<0x24, 0x01, 0xC0, "stgdb", format.alopf13, SSS, c.c25>;

defm PUTTC : ALESEXT2Inst<0x25, 0x01, 0xC0, "puttc", format.alopf11, DD_D, c.c0>;
defm PUTTST : ALESEXT2Inst<0x25, 0x01, 0xC0, "puttc", format.alopf11, DD_D, c.c3>;

// store half-word (guarded)
defm STGDH : ALESEXT2SRC3Inst<0x25, 0x01, 0xC0, "stgdh", format.alopf13, SSS, c.c25>;

defm PMULLW : ALESEXT2Inst<0x26, 0x01, 0xC0, "pmullw", format.alopf11, DD_D, c.c14>;

// store word (guarded)
defm STGDW : ALESEXT2SRC3Inst<0x26, 0x01, 0xC0, "stgdw", format.alopf13, SSS, c.c25>;

defm INVTC : ALESEXT2Inst<0x26, 0x01, 0xC0, "invtc", format.alopf11, DD_D, c.c0>;
defm INVTCE : ALESEXT2Inst<0x26, 0x01, 0xC1, "invtce", format.alopf11, DD_D, c.c0>;

defm FXSCALESX : ALESEXT2Inst<0x27, 0x01, 0xC0, "fxscalesx", format.alopf11, DS_D, c.c14>;

// store double-word (guarded)
defm STGDD : ALESEXT2SRC3Inst<0x27, 0x01, 0xC0, "stgdd", format.alopf13, SSD, c.c25>;

defm STAPB : ALESEXT2SRC3Inst<0x28, 0x01, 0xC0, "stapb", format.alopf13, QSS, c.c25>;
defm STAPH : ALESEXT2SRC3Inst<0x29, 0x01, 0xC0, "staph", format.alopf13, QSS, c.c25>;
defm STAPW : ALESEXT2SRC3Inst<0x2a, 0x01, 0xC0, "stapw", format.alopf13, QSS, c.c25>;
defm STAPD : ALESEXT2SRC3Inst<0x2b, 0x01, 0xC0, "stapd", format.alopf13, QSD, c.c25>;

defm FXCMPUDSF : ALESEXT2Inst<0x28, 0x01, 0xC3, "fxcmpudsf", format.alopf11, DS_S, c.c0134>;
defm FXCMPODSF : ALESEXT2Inst<0x28, 0x01, 0xC7, "fxcmpodsf", format.alopf11, DS_S, c.c0134>;

defm FXCMPUDDF : ALESEXT2Inst<0x29, 0x01, 0xC3, "fxcmpuddf", format.alopf11, DD_S, c.c0134>;
defm FXCMPODDF : ALESEXT2Inst<0x29, 0x01, 0xC7, "fxcmpoddf", format.alopf11, DD_S, c.c0134>;

defm FXCMPUDXF : ALESEXT2Inst<0x2b, 0x01, 0xC3, "fxcmpudxf", format.alopf11, DD_S, c.c0134>;
defm FXCMPODXF : ALESEXT2Inst<0x2b, 0x01, 0xC7, "fxcmpodxf", format.alopf11, DD_S, c.c0134>;

// floating-point compare, equal
defm FCMPEQS : ALESEXT2Inst<0x2c, 0x01, 0xC0, "fcmpeqs", format.alopf7, SS_S, c.c0134>;
defm FCMPEQD : ALESEXT2Inst<0x2d, 0x01, 0xC0, "fcmpeqd", format.alopf7, DD_S, c.c0134>;

// floating-point compare, less
defm FCMPLTS : ALESEXT2Inst<0x2c, 0x01, 0xC1, "fcmplts", format.alopf7, SS_S, c.c0134>;
defm FCMPLTD : ALESEXT2Inst<0x2d, 0x01, 0xC1, "fcmpltd", format.alopf7, DD_S, c.c0134>;

// floating-point compare, less or equal
defm FCMPLES : ALESEXT2Inst<0x2c, 0x01, 0xC2, "fcmples", format.alopf7, SS_S, c.c0134>;
defm FCMPLED : ALESEXT2Inst<0x2d, 0x01, 0xC2, "fcmpled", format.alopf7, DD_S, c.c0134>;

// floating-point compare, unordered
defm FCMPUODS : ALESEXT2Inst<0x2c, 0x01, 0xC3, "fcmpuods", format.alopf7, SS_S, c.c0134>;
defm FCMPUODD : ALESEXT2Inst<0x2d, 0x01, 0xC3, "fcmpuodd", format.alopf7, DD_S, c.c0134>;

// floating-point compare, not equal
defm FCMPNEQS : ALESEXT2Inst<0x2c, 0x01, 0xC4, "fcmpneqs", format.alopf7, SS_S, c.c0134>;
defm FCMPNEQD : ALESEXT2Inst<0x2d, 0x01, 0xC4, "fcmpneqd", format.alopf7, DD_S, c.c0134>;

// floating-point compare, not less
defm FCMPNLTS : ALESEXT2Inst<0x2c, 0x01, 0xC5, "fcmpnlts", format.alopf7, SS_S, c.c0134>;
defm FCMPNLTD : ALESEXT2Inst<0x2d, 0x01, 0xC5, "fcmpnltd", format.alopf7, DD_S, c.c0134>;

// floating-point compare, not less or equal
defm FCMPNLES : ALESEXT2Inst<0x2c, 0x01, 0xC6, "fcmpnles", format.alopf7, SS_S, c.c0134>;
defm FCMPNLED : ALESEXT2Inst<0x2d, 0x01, 0xC6, "fcmpnled", format.alopf7, DD_S, c.c0134>;

// floating-point compare, ordered
defm FCMPODS : ALESEXT2Inst<0x2c, 0x01, 0xC7, "fcmpods", format.alopf7, SS_S, c.c0134>;
defm FCMPODD : ALESEXT2Inst<0x2d, 0x01, 0xC7, "fcmpodd", format.alopf7, DD_S, c.c0134>;

defm FCMPUDSF : ALESEXT2Inst<0x2e, 0x01, 0xC3, "fcmpudsf", format.alopf7, SS_S, c.c0134>;
defm FCMPODSF : ALESEXT2Inst<0x2e, 0x01, 0xC7, "fcmpodsf", format.alopf7, DD_S, c.c0134>;

defm FCMPUDDF : ALESEXT2Inst<0x2f, 0x01, 0xC3, "fcmpuddf", format.alopf7, DD_S, c.c0134>;
defm FCMPODDF : ALESEXT2Inst<0x2f, 0x01, 0xC7, "fcmpoddf", format.alopf7, DD_S, c.c0134>;

defm PFADDS : ALESEXT2Inst<0x30, 0x01, 0xC0, "pfadds", format.alopf11, DD_D, c.all>;
defm PFADDD : ALESEXT2Inst<0x31, 0x01, 0xC0, "pfaddd", format.alopf11, DD_D, c.all>;

defm PFSUBS : ALESEXT2Inst<0x32, 0x01, 0xC0, "pfsubs", format.alopf11, DD_D, c.all>;
defm PFSUBD : ALESEXT2Inst<0x33, 0x01, 0xC0, "pfsubd", format.alopf11, DD_D, c.all>;

defm PFMINS : ALESEXT2Inst<0x34, 0x01, 0xC0, "pfmins", format.alopf11, DD_D, c.c0134>;
defm PFMIND : ALESEXT2Inst<0x35, 0x01, 0xC0, "pfmind", format.alopf11, DD_D, c.c0134>;

defm PFMAXS : ALESEXT2Inst<0x36, 0x01, 0xC0, "pfmaxs", format.alopf11, DD_D, c.c0134>;
defm PFMAXD : ALESEXT2Inst<0x37, 0x01, 0xC0, "pfmaxd", format.alopf11, DD_D, c.c0134>;

defm PFMULS : ALESEXT2Inst<0x38, 0x01, 0xC0, "pfmuls", format.alopf11, DD_D, c.all>;
defm PFMULD : ALESEXT2Inst<0x39, 0x01, 0xC0, "pfmuld", format.alopf11, DD_D, c.c0134>;

defm STGDQ : ALESEXT2SRC3Inst<0x39, 0x01, 0xC0, "stgdq", format.alopf13, SSQ, c.c2>;

// packed floating-point compare, equal
defm PFCMPEQS : ALESEXT2Inst<0x3a, 0x01, 0xC0, "pfcmpeqs", format.alopf11, SS_S, c.c0134>;
defm PFCMPEQD : ALESEXT2Inst<0x3b, 0x01, 0xC0, "pfcmpeqd", format.alopf11, DD_D, c.c0134>;

// packed floating-point compare, less
defm PFCMPLTS : ALESEXT2Inst<0x3a, 0x01, 0xC1, "pfcmplts", format.alopf11, SS_S, c.c0134>;
defm PFCMPLTD : ALESEXT2Inst<0x3b, 0x01, 0xC1, "pfcmpltd", format.alopf11, DD_D, c.c0134>;

// packed floating-point compare, less or equal
defm PFCMPLES : ALESEXT2Inst<0x3a, 0x01, 0xC2, "pfcmples", format.alopf11, SS_S, c.c0134>;
defm PFCMPLED : ALESEXT2Inst<0x3b, 0x01, 0xC2, "pfcmpled", format.alopf11, DD_D, c.c0134>;

// packed floating-point compare, unordered
defm PFCMPUODS : ALESEXT2Inst<0x3a, 0x01, 0xC4, "pfcmpuods", format.alopf11, SS_S, c.c0134>;
defm PFCMPUODD : ALESEXT2Inst<0x3b, 0x01, 0xC4, "pfcmpuodd", format.alopf11, DD_D, c.c0134>;

// packed floating-point compare, not equal
defm PFCMPNEQS : ALESEXT2Inst<0x3a, 0x01, 0xC5, "pfcmpneqs", format.alopf11, SS_S, c.c0134>;
defm PFCMPNEQD : ALESEXT2Inst<0x3b, 0x01, 0xC5, "pfcmpneqd", format.alopf11, DD_D, c.c0134>;

// packed floating-point compare, not less or equal
defm PFCMPNLES : ALESEXT2Inst<0x3a, 0x01, 0xC6, "pfcmpnles", format.alopf11, SS_S, c.c0134>;
defm PFCMPNLED : ALESEXT2Inst<0x3b, 0x01, 0xC6, "pfcmpnled", format.alopf11, DD_D, c.c0134>;

// packed floating-point compare, ordered
defm PFCMPODS : ALESEXT2Inst<0x3a, 0x01, 0xC7, "pfcmpods", format.alopf11, SS_S, c.c0134>;
defm PFCMPODD : ALESEXT2Inst<0x3b, 0x01, 0xC7, "pfcmpodd", format.alopf11, DD_D, c.c0134>;

defm STAPQ : ALESEXT2SRC3Inst<0x3a, 0x01, 0xC0, "stapq", format.alopf13, QSQ, c.c2>;

// write control register
defm RWS : ALESEXT2Inst<0x3c, 0x01, 0xC0, "rws", format.alopf15, S_R, c.c0>;
defm RWD : ALESEXT2Inst<0x3d, 0x01, 0xC0, "rwd", format.alopf15, D_R, c.c0>;

// read control register
defm RRS : ALESEXT2Inst<0x3e, 0x01, 0xC0, "rrs", format.alopf16, R_S, c.c0>;
defm RRD : ALESEXT2Inst<0x3f, 0x01, 0xC0, "rrd", format.alopf16, R_D, c.c0>;

// store array quad-word
defm STAAQ : ALESEXT2SRC3Inst<0x3f, 0x01, 0xC0, "staaq", format.alopf10, PPQ, c.c2>;

defm PMRGP : ALESEXT2Inst<0x40, 0x01, 0xC0, "pmrgp", format.alopf11, DD_D, c.c0134>;

// FIXME : check channel 2, opcodes 0x41 - 0x47

defm LDQ : ALESEXT2Inst<0x41, 0x01, 0xC0, "ldq", format.alopf11, DD_Q, c.c03>;

defm LDCSQ : ALESEXT2Inst<0x42, 0x01, 0xC0, "ldcsq", format.alopf11, DD_Q, c.c03>;

defm LDDSQ : ALESEXT2Inst<0x43, 0x01, 0xC0, "lddsq", format.alopf11, DD_Q, c.c03>;

defm LDESQ : ALESEXT2Inst<0x44, 0x01, 0xC0, "ldesq", format.alopf11, SS_Q, c.c03>;

defm LDFSQ : ALESEXT2Inst<0x45, 0x01, 0xC0, "ldfsq", format.alopf11, DD_Q, c.c03>;

defm LDGSQ : ALESEXT2Inst<0x46, 0x01, 0xC0, "lddsq", format.alopf11, DD_Q, c.c03>;

defm LDSSQ : ALESEXT2Inst<0x47, 0x01, 0xC0, "ldssq", format.alopf11, DD_Q, c.c03>;

// packed AND
defm PANDD : ALESEXT2Inst<0x48, 0x01, 0xC0, "pandd", format.alopf11, DD_D, c.c0134>;
// packed AND NOT
defm PANDND : ALESEXT2Inst<0x49, 0x01, 0xC0, "pandnd", format.alopf11, DD_D, c.c0134>;

// floating-point DIV
defm FDIVS : ALESEXT2Inst<0x48, 0x01, 0xC0, "fdivs", format.alopf11, SS_S, c.c5>;
defm FDIVD : ALESEXT2Inst<0x49, 0x01, 0xC0, "fdivd", format.alopf11, DD_D, c.c5>;

// packed OR
defm PORD : ALESEXT2Inst<0x4a, 0x01, 0xC0, "pord", format.alopf11, DD_D, c.c0134>;
// packed XOR
defm PXORD : ALESEXT2Inst<0x4b, 0x01, 0xC0, "pxord", format.alopf11, DD_D, c.c0134>;

// packed floating-point DIV
defm PFDIVS : ALESEXT2Inst<0x4a, 0x01, 0xC0, "pfdivs", format.alopf11, SS_S, c.c5>;
defm PFDIVD : ALESEXT2Inst<0x4b, 0x01, 0xC0, "pfdivd", format.alopf11, DD_D, c.c5>;

defm PSRLD : ALESEXT2Inst<0x4c, 0x01, 0xC0, "psrld", format.alopf11, DD_D, c.c14>;

defm FSQRTS : ALESEXT2Inst<0x4c, 0x01, 0xC0, "fsqrts", format.alopf11, SS_S, c.c5>;

defm PCMPEQD : ALESEXT2Inst<0x4d, 0x01, 0xC0, "pcmpeqd", format.alopf11, DD_D, c.c03>;

defm FSQRTID : ALESEXT2Inst<0x4d, 0x01, 0xC0, "fsqrtid", format.alopf11, DD_D, c.c5>;

defm PSLLD : ALESEXT2Inst<0x4e, 0x01, 0xC0, "pslld", format.alopf11, DD_D, c.c14>;

defm PFSQRTS : ALESEXT2Inst<0x4e, 0x01, 0xC0, "pfsqrts", format.alopf11, SS_S, c.c5>;

defm PCMPGTD : ALESEXT2Inst<0x4f, 0x01, 0xC0, "pcmpgtd", format.alopf11, DD_D, c.c03>;

defm APTOAP : ALESEXT2Inst<0x50, 0x01, 0xC0, "aptoap", format.alopf11, QS_Q, c.c03>;
defm APTOAPB : ALESEXT2Inst<0x51, 0x01, 0xC0, "aptoapb", format.alopf11, QS_Q, c.c03>;

defm FCRPS : ALESEXT2Inst<0x50, 0x01, 0xC0, "fcrps", format.alopf11, SS_S, c.c5>;

// square root, final command
defm FSQRTTD : ALESEXT2Inst<0x51, 0x01, 0xC0, "fsqrttd", format.alopf11, DD_D, c.c5>;

defm GETVA : ALESEXT2Inst<0x52, 0x01, 0xC0, "getva", format.alopf11, QS_D, c.c03>;

// packed square root, final command
defm PFSQRTTD : ALESEXT2Inst<0x53, 0x01, 0xC0, "pfsqrttd", format.alopf11, DD_D, c.c5>;

defm IBRANCHD : ALESEXT2Inst<0x53, 0x01, 0xC0, "ibranchd", format.alopf11, DD_D, c.c0>;

defm ICALLD : ALESEXT2Inst<0x54, 0x01, 0xC0, "icalld", format.alopf11, DD_D, c.c0>;

defm MOVTQ : ALESEXT2Inst<0x57, 0x01, 0xC0, "movtq", format.alopf11, QQ_Q, c.c03>;
defm MOVTCQ : ALESEXT2Inst<0x57, 0x01, 0xC1, "movtcq", format.alopf11, QQ_Q, c.c03>;
defm MOVTRQ : ALESEXT2Inst<0x57, 0x01, 0xC2, "movtrq", format.alopf11, QQ_Q, c.c03>;
defm MOVTRCQ : ALESEXT2Inst<0x57, 0x01, 0xC3, "movtrcq", format.alopf11, QQ_Q, c.c03>;

defm GETSP : ALESEXT1EXT2Inst<0x58, 0xEC, 0x01, 0xC0, "getsp", format.alopf12, S_D, c.c0>;

defm MPSADBH : ALESEXT2Inst<0x58, 0x01, 0xC0, "mpsadbh", format.alopf11, DD_D, c.c14>;

defm GETSOD : ALESEXT1EXT2Inst<0x5a, 0xEC, 0x01, 0xC0, "getsod", format.alopf12, S_D, c.c0>;

defm LDRD : ALESEXT2Inst<0x5b, 0x01, 0xC0, "ldrd", format.alopf11, DD_D, c.c0235>;

// read AAU register
defm AAURR : ALESEXT2Inst<0x5e, 0x01, 0xC0, "aaurr", format.alopf19, PP_S, c.c25>;
defm AAURRD : ALESEXT2Inst<0x5f, 0x01, 0xC0, "aaurrd", format.alopf19, PP_D, c.c25>;

defm LDCUDB : ALESEXT2Inst<0x60, 0x01, 0xC0, "ldcudb", format.alopf11, SS_D, c.c0235>;
defm LDCUDH : ALESEXT2Inst<0x61, 0x01, 0xC0, "ldcudh", format.alopf11, SS_D, c.c0235>;
defm LDCUDW : ALESEXT2Inst<0x62, 0x01, 0xC0, "ldcudw", format.alopf11, SS_D, c.c0235>;
defm LDCUDD : ALESEXT2Inst<0x63, 0x01, 0xC0, "ldcudd", format.alopf11, SS_D, c.c0235>;

defm ADDCD : ALESInst<0x60, 0x01, "addcd", format.alopf21, DDS_D, c.c14>;
defm ADDCD_C : ALESInst<0x61, 0x01, "addcd_c", format.alopf21, DDS_D, c.c14>;
defm SUBCD : ALESInst<0x62, 0x01, "subcd", format.alopf21, DDS_D, c.c14>;
defm SUBCD_C : ALESInst<0x63, 0x01, "subcd_c", format.alopf21, DDS_D, c.c14>;

// load byte (guarded)
defm LDGDB : ALESEXT2Inst<0x64, 0x01, 0xC0, "ldgdb", format.alopf11, SS_S, c.c0235>;

// load half-word (guarded)
defm LDGDH : ALESEXT2Inst<0x65, 0x01, 0xC0, "ldgdh", format.alopf11, SS_S, c.c0235>;

// load word (guarded)
defm LDGDW : ALESEXT2Inst<0x66, 0x01, 0xC0, "ldgdw", format.alopf11, SS_S, c.c0235>;

// load double-word (guarded)
defm LDGDD : ALESEXT2Inst<0x67, 0x01, 0xC0, "ldgdd", format.alopf11, SS_S, c.c0235>;

defm LDAPB : ALESEXT2Inst<0x68, 0x01, 0xC0, "ldapb", format.alopf11, QS_D, c.c0235>;
defm LDAPH : ALESEXT2Inst<0x69, 0x01, 0xC0, "ldaph", format.alopf11, QS_D, c.c0235>;
defm LDAPW : ALESEXT2Inst<0x6a, 0x01, 0xC0, "ldapw", format.alopf11, QS_D, c.c0235>;
defm LDAPD : ALESEXT2Inst<0x6b, 0x01, 0xC0, "ldapd", format.alopf11, QS_D, c.c0235>;

defm PMADDUBSH : ALESEXT2Inst<0x68, 0x01, 0xC0, "pmaddubsh", format.alopf11, DD_D, c.c14>;

defm PMULHRSH : ALESEXT2Inst<0x69, 0x01, 0xC0, "pmulhrsh", format.alopf11, DD_D, c.c14>;

defm PHMINPOSUH : ALESEXT2Inst<0x6a, 0x01, 0xC0, "phminposuh", format.alopf11, DD_D, c.c14>;

defm PACKUSWH : ALESEXT2Inst<0x6b, 0x01, 0xC0, "packuswh", format.alopf11, DD_D, c.c14>;

// convert fp32 to fp32 (integer part)
defm FSTOIFS : ALESEXT2Inst<0x6c, 0x01, 0xC0, "fstoifs", format.alopf11, SS_S, c.c0134>;

// convert fp64 to fp64 (integer part)
defm FDTOIFD : ALESEXT2Inst<0x6d, 0x01, 0xC0, "fdtoifd", format.alopf11, DD_D, c.c0134>;

// packed convert fp32 to fp32 (integer part)
defm PFSTOIFS : ALESEXT2Inst<0x6e, 0x01, 0xC0, "pfstoifs", format.alopf11, DD_D, c.c0134>;

// packed convert fp64 to fp64 (integer part)
defm PFDTOIFD : ALESEXT2Inst<0x6f, 0x01, 0xC0, "pfdtoifd", format.alopf11, DD_D, c.c0134>;

// integer MUL (without sign, int64 * int64, high part)
defm UMULHD : ALESEXT2Inst<0x70, 0x01, 0xC0, "umulhd", format.alopf11, DD_D, c.c0134>;

// integer MUL (with sign, int64 * int64, high part)
defm SMULHD : ALESEXT2Inst<0x71, 0x01, 0xC0, "smulhd", format.alopf11, DD_D, c.c0134>;

defm PFHADDS : ALESEXT2Inst<0x72, 0x01, 0xC0, "pfhadds", format.alopf11, DD_D, c.all>;

defm PFHSUBS : ALESEXT2Inst<0x73, 0x01, 0xC0, "pfhsubs", format.alopf11, DD_D, c.all>;

defm GETFZS : ALESEXT2Inst<0x74, 0x01, 0xC0, "getfzs", format.alopf11, SS_S, c.all>;

defm GETFZD : ALESEXT2Inst<0x75, 0x01, 0xC0, "getfzd", format.alopf11, DD_D, c.all>;

defm PFADDSUBS : ALESEXT2Inst<0x77, 0x01, 0xC0, "pfaddsubs", format.alopf11, DD_D, c.all>;

defm LDCUDQ : ALESEXT2Inst<0x78, 0x01, 0xC0, "ldqudq", format.alopf11, SS_Q, c.c03>;

// load quad-word (guarded)
defm LDGDQ : ALESEXT2Inst<0x79, 0x01, 0xC0, "ldgdq", format.alopf11, SS_Q, c.c03>;

defm LDAPQ : ALESEXT2Inst<0x7a, 0x01, 0xC0, "ldapq", format.alopf11, SS_Q, c.c03>;

// read AAU register
defm AAURRQ : ALESEXT2Inst<0x7f, 0x01, 0xC0, "aaurrq", format.alopf19, PP_Q, c.c03>;

//===----------------------------------------------------------------------===//
// ALES opcodes, opcode2 = 2
//===----------------------------------------------------------------------===//

defm QPSRLW : ALESEXT2Inst<0x00, 0x02, 0xC0, "qpsrlw", format.alopf11, pS_p, c.c0134>;
defm QPSRLH : ALESEXT2Inst<0x01, 0x02, 0xC0, "qpsrlh", format.alopf11, pS_p, c.c0134>;

defm QPSRAW : ALESEXT2Inst<0x02, 0x02, 0xC0, "qpsraw", format.alopf11, pD_p, c.c0134>;
defm QPSRAH : ALESEXT2Inst<0x03, 0x02, 0xC0, "qpsrah", format.alopf11, pD_p, c.c0134>;

defm QPSLLW : ALESEXT2Inst<0x04, 0x02, 0xC0, "qpsllw", format.alopf11, pD_p, c.c0134>;
defm QPSLLH : ALESEXT2Inst<0x05, 0x02, 0xC0, "qpsllh", format.alopf11, pD_p, c.c0134>;

defm QPSRLD : ALESEXT2Inst<0x06, 0x02, 0xC0, "qpsrld", format.alopf11, pD_p, c.c0134>;
defm QPSLLD : ALESEXT2Inst<0x07, 0x02, 0xC0, "qpslld", format.alopf11, pD_p, c.c0134>;

defm QPAND : ALESEXT2Inst<0x08, 0x02, 0xC0, "qpand", format.alopf11, pp_p, c.c0134>;

defm QPANDN : ALESEXT2Inst<0x09, 0x02, 0xC0, "qpandn", format.alopf11, pp_p, c.c0134>;

defm QPOR : ALESEXT2Inst<0x0a, 0x02, 0xC0, "qpor", format.alopf11, pp_p, c.c0134>;

defm QPXOR : ALESEXT2Inst<0x0b, 0x02, 0xC0, "qpxor", format.alopf11, pp_p, c.c0134>;

defm QPMINUW : ALESEXT2Inst<0x0c, 0x02, 0xC0, "qpminuw", format.alopf11, pp_p, c.c03>;
defm QPMINSW : ALESEXT2Inst<0x0d, 0x02, 0xC0, "qpminsw", format.alopf11, pp_p, c.c03>;

defm QPMAXUW : ALESEXT2Inst<0x0e, 0x02, 0xC0, "qpmaxuw", format.alopf11, pp_p, c.c03>;
defm QPMAXSW : ALESEXT2Inst<0x0f, 0x02, 0xC0, "qpmaxsw", format.alopf11, pp_p, c.c03>;

defm PSRLW2 : ALESEXT2Inst<0x10, 0x02, 0xC0, "psrlw", format.alopf11, DD_D, c.c03>;

defm PSRLH2 : ALESEXT2Inst<0x11, 0x02, 0xC0, "psrlh", format.alopf11, DD_D, c.c03>;

defm STMQP : ALESEXT2SRC3Inst<0x11, 0x02, 0xC0, "stmqp", format.alopf13, DDp, c.c25>;

defm PSRAW2 : ALESEXT2Inst<0x12, 0x02, 0xC0, "psraw", format.alopf11, DD_D, c.c03>;

defm STCSMQP : ALESEXT2SRC3Inst<0x12, 0x02, 0xC0, "stcsmqp", format.alopf13, DDp, c.c25>;

defm PSRAH2 : ALESEXT2Inst<0x13, 0x02, 0xC0, "psrah", format.alopf11, DD_D, c.c03>;

defm STDSMQP : ALESEXT2SRC3Inst<0x13, 0x02, 0xC0, "stdsmqp", format.alopf13, DDp, c.c25>;

defm PSLLW2 : ALESEXT2Inst<0x14, 0x02, 0xC0, "psllw", format.alopf11, DD_D, c.c03>;

defm STESMQP : ALESEXT2SRC3Inst<0x14, 0x02, 0xC0, "stesmqp", format.alopf13, SSp, c.c25>;

defm PSLLH2 : ALESEXT2Inst<0x15, 0x02, 0xC0, "psllh", format.alopf11, DD_D, c.c03>;

defm STFSMQP : ALESEXT2SRC3Inst<0x15, 0x02, 0xC0, "stfsmqp", format.alopf13, SSp, c.c25>;

defm PSRCW : ALESEXT2Inst<0x16, 0x02, 0xC0, "psrcw", format.alopf11, DD_D, c.c0134>;

defm STGSMQP : ALESEXT2SRC3Inst<0x16, 0x02, 0xC0, "stgsmqp", format.alopf13, DDp, c.c25>;

defm PSRCD : ALESEXT2Inst<0x17, 0x02, 0xC0, "psrcd", format.alopf11, DD_D, c.c0134>;

defm STSSMQP : ALESEXT2SRC3Inst<0x17, 0x02, 0xC0, "stssmqp", format.alopf13, DDp, c.c25>;

defm QPCMPEQD : ALESEXT2Inst<0x18, 0x02, 0xC0, "qpcmpeqd", format.alopf11, pp_p, c.c03>;

defm QPHADDH : ALESEXT2Inst<0x19, 0x02, 0xC0, "qphaddh", format.alopf11, pp_p, c.c03>;

defm STGDMQP : ALESEXT2SRC3Inst<0x19, 0x02, 0xC0, "stgdmqp", format.alopf13, SSp, c.c25>;

defm QPHADDW : ALESEXT2Inst<0x1a, 0x02, 0xC0, "qphaddw", format.alopf11, pp_p, c.c03>;

defm STAPMQP : ALESEXT2SRC3Inst<0x1a, 0x02, 0xC0, "stapmqp", format.alopf13, QSp, c.c25>;

defm QPHADDSH : ALESEXT2Inst<0x1b, 0x02, 0xC0, "qphaddsh", format.alopf11, pp_p, c.c03>;

defm QPCMPGTD : ALESEXT2Inst<0x1c, 0x02, 0xC0, "qpcmpgtd", format.alopf11, pp_p, c.c03>;

defm QPHSUBH : ALESEXT2Inst<0x1d, 0x02, 0xC0, "qphsubh", format.alopf11, pp_p, c.c03>;

defm QPHSUBW : ALESEXT2Inst<0x1e, 0x02, 0xC0, "qphsubw", format.alopf11, pp_p, c.c03>;

defm QPHSUBSH : ALESEXT2Inst<0x1f, 0x02, 0xC0, "qphsubsh", format.alopf11, pp_p, c.c03>;

defm PMINUW : ALESEXT2Inst<0x20, 0x02, 0xC0, "pminuw", format.alopf11, DD_D, c.c03>;

defm STAAQP : ALESEXT2SRC3Inst<0x20, 0x02, 0xC0, "staaqp", format.alopf10, pDD, c.c25>;

defm PMINSW : ALESEXT2Inst<0x21, 0x02, 0xC0, "pminsw", format.alopf11, DD_D, c.c03>;

defm STQP : ALESEXT2SRC3Inst<0x21, 0x02, 0xC0, "stqp", format.alopf10, pDD, c.c25>;

defm PMAXUW : ALESEXT2Inst<0x22, 0x02, 0xC0, "pmaxuw", format.alopf11, DD_D, c.c03>;

defm STCSQP : ALESEXT2SRC3Inst<0x22, 0x02, 0xC0, "stcsqp", format.alopf10, pDD, c.c25>;

defm PMAXSW : ALESEXT2Inst<0x23, 0x02, 0xC0, "pmaxsw", format.alopf11, DD_D, c.c03>;

defm STDSQP : ALESEXT2SRC3Inst<0x23, 0x02, 0xC0, "stdsqp", format.alopf10, pDD, c.c25>;

defm STESQP : ALESEXT2SRC3Inst<0x24, 0x02, 0xC0, "stesqp", format.alopf10, pDD, c.c25>;

defm QPSRAD : ALESEXT2Inst<0x25, 0x02, 0xC0, "qpsrad", format.alopf11, pD_p, c.c0134>;

defm STFSQP : ALESEXT2SRC3Inst<0x25, 0x02, 0xC0, "stfsqp", format.alopf10, pDD, c.c25>;

defm QPSRCW : ALESEXT2Inst<0x26, 0x02, 0xC0, "qpsrcw", format.alopf11, pD_p, c.c0134>;

defm STGSQP : ALESEXT2SRC3Inst<0x26, 0x02, 0xC0, "stgsqp", format.alopf10, pDD, c.c25>;

defm QPSRCD : ALESEXT2Inst<0x27, 0x02, 0xC0, "qpsrcd", format.alopf11, pD_p, c.c0134>;

defm STSSQP : ALESEXT2SRC3Inst<0x27, 0x02, 0xC0, "stssqp", format.alopf10, pDD, c.c25>;

defm PCMPEQBOP : ALESEXT2Inst<0x28, 0x02, 0xC0, "pcmpeqbop", format.alopf17, DD_B, c.c03>;
defm PCMPEQHOP : ALESEXT2Inst<0x28, 0x02, 0xC1, "pcmpeqhop", format.alopf17, DD_B, c.c03>;
defm PCMPEQWOP : ALESEXT2Inst<0x28, 0x02, 0xC2, "pcmpeqwop", format.alopf17, DD_B, c.c03>;
defm PCMPEQDOP : ALESEXT2Inst<0x28, 0x02, 0xC3, "pcmpeqdop", format.alopf17, DD_B, c.c03>;
defm PCMPGTBOP : ALESEXT2Inst<0x28, 0x02, 0xC4, "pcmpgtbop", format.alopf17, DD_B, c.c03>;
defm PCMPGTHOP : ALESEXT2Inst<0x28, 0x02, 0xC5, "pcmpgthop", format.alopf17, DD_B, c.c03>;
defm PCMPGTWOP : ALESEXT2Inst<0x28, 0x02, 0xC6, "pcmpgtwop", format.alopf17, DD_B, c.c03>;
defm PCMPGTDOP : ALESEXT2Inst<0x28, 0x02, 0xC7, "pcmpgtdop", format.alopf17, DD_B, c.c03>;
defm PCMPEQBAP : ALESEXT2Inst<0x28, 0x02, 0xC8, "pcmpeqbap", format.alopf17, DD_B, c.c03>;
defm PCMPEQHAP : ALESEXT2Inst<0x28, 0x02, 0xC9, "pcmpeqhap", format.alopf17, DD_B, c.c03>;
defm PCMPEQWAP : ALESEXT2Inst<0x28, 0x02, 0xCA, "pcmpeqwap", format.alopf17, DD_B, c.c03>;
defm PCMPEQDAP : ALESEXT2Inst<0x28, 0x02, 0xCB, "pcmpeqdap", format.alopf17, DD_B, c.c03>;
defm PCMPGTBAP : ALESEXT2Inst<0x28, 0x02, 0xCC, "pcmpgtbap", format.alopf17, DD_B, c.c03>;
defm PCMPGTHAP : ALESEXT2Inst<0x28, 0x02, 0xCD, "pcmpgthap", format.alopf17, DD_B, c.c03>;
defm PCMPGTWAP : ALESEXT2Inst<0x28, 0x02, 0xCE, "pcmpgtwap", format.alopf17, DD_B, c.c03>;
defm PCMPGTDAP : ALESEXT2Inst<0x28, 0x02, 0xCF, "pcmpgtdap", format.alopf17, DD_B, c.c03>;

defm PHADDH : ALESEXT2Inst<0x29, 0x02, 0xC0, "phaddh", format.alopf11, DD_D, c.c03>;

defm STGDQP : ALESEXT2SRC3Inst<0x29, 0x02, 0xC0, "stgdqp", format.alopf10, pDD, c.c25>;

defm PHADDW : ALESEXT2Inst<0x2a, 0x02, 0xC0, "phaddw", format.alopf11, DD_D, c.c03>;

defm STAPQP : ALESEXT2SRC3Inst<0x2a, 0x02, 0xC0, "stapqp", format.alopf10, pDD, c.c25>;

defm PHADDSH : ALESEXT2Inst<0x2b, 0x02, 0xC0, "phaddsh", format.alopf11, DD_D, c.c03>;

defm STRQP : ALESEXT2SRC3Inst<0x2b, 0x02, 0xC0, "strqp", format.alopf10, pDD, c.c25>;

defm QPCMPEQBOP : ALESEXT2Inst<0x2c, 0x02, 0xC0, "qpcmpeqbop", format.alopf17, pp_B, c.c03>;
defm QPCMPEQHOP : ALESEXT2Inst<0x2c, 0x02, 0xC1, "qpcmpeqhop", format.alopf17, pp_B, c.c03>;
defm QPCMPEQWOP : ALESEXT2Inst<0x2c, 0x02, 0xC2, "qpcmpeqwop", format.alopf17, pp_B, c.c03>;
defm QPCMPEQDOP : ALESEXT2Inst<0x2c, 0x02, 0xC3, "qpcmpeqdop", format.alopf17, pp_B, c.c03>;
defm QPCMPGTBOP : ALESEXT2Inst<0x2c, 0x02, 0xC4, "qpcmpgtbop", format.alopf17, pp_B, c.c03>;
defm QPCMPGTHOP : ALESEXT2Inst<0x2c, 0x02, 0xC5, "qpcmpgthop", format.alopf17, pp_B, c.c03>;
defm QPCMPGTWOP : ALESEXT2Inst<0x2c, 0x02, 0xC6, "qpcmpgtwop", format.alopf17, pp_B, c.c03>;
defm QPCMPGTDOP : ALESEXT2Inst<0x2c, 0x02, 0xC7, "qpcmpgtdop", format.alopf17, pp_B, c.c03>;
defm QPCMPEQBAP : ALESEXT2Inst<0x2c, 0x02, 0xC8, "qpcmpeqbap", format.alopf17, pp_B, c.c03>;
defm QPCMPEQHAP : ALESEXT2Inst<0x2c, 0x02, 0xC9, "qpcmpeqhap", format.alopf17, pp_B, c.c03>;
defm QPCMPEQWAP : ALESEXT2Inst<0x2c, 0x02, 0xCA, "qpcmpeqwap", format.alopf17, pp_B, c.c03>;
defm QPCMPEQDAP : ALESEXT2Inst<0x2c, 0x02, 0xCB, "qpcmpeqdap", format.alopf17, pp_B, c.c03>;
defm QPCMPGTBAP : ALESEXT2Inst<0x2c, 0x02, 0xCC, "qpcmpgtbap", format.alopf17, pp_B, c.c03>;
defm QPCMPGTHAP : ALESEXT2Inst<0x2c, 0x02, 0xCD, "qpcmpgthap", format.alopf17, pp_B, c.c03>;
defm QPCMPGTWAP : ALESEXT2Inst<0x2c, 0x02, 0xCE, "qpcmpgtwap", format.alopf17, pp_B, c.c03>;
defm QPCMPGTDAP : ALESEXT2Inst<0x2c, 0x02, 0xCF, "qpcmpgtdap", format.alopf17, pp_B, c.c03>;

defm PHSUBH : ALESEXT2Inst<0x2d, 0x02, 0xC0, "phsubh", format.alopf11, DD_D, c.c03>;
defm PHSUBW : ALESEXT2Inst<0x2e, 0x02, 0xC0, "phsubw", format.alopf11, DD_D, c.c03>;
defm PHSUBSH : ALESEXT2Inst<0x2f, 0x02, 0xC0, "phsubsh", format.alopf11, DD_D, c.c03>;

defm PSIGNB : ALESEXT2Inst<0x30, 0x02, 0xC0, "psignb", format.alopf11, DD_D, c.c03>;
defm PSIGNH : ALESEXT2Inst<0x31, 0x02, 0xC0, "psignh", format.alopf11, DD_D, c.c03>;
defm PSIGNW : ALESEXT2Inst<0x32, 0x02, 0xC0, "psignw", format.alopf11, DD_D, c.c03>;

defm QPCHECKS : ALESEXT2Inst<0x33, 0x02, 0xC0, "qpchecks", format.alopf11, SS_S, c.c03>;

// floating-point ADD
defm FADDS2 : ALESEXT2Inst<0x30, 0x02, 0xC0, "fadds", format.alopf11, SS_S, c.c25>;
defm FADDD2 : ALESEXT2Inst<0x31, 0x02, 0xC0, "faddd", format.alopf11, DD_D, c.c25>;

// floating-point SUB
defm FSUBS2 : ALESEXT2Inst<0x32, 0x02, 0xC0, "fsubs", format.alopf11, SS_S, c.c25>;
defm FSUBD2 : ALESEXT2Inst<0x33, 0x02, 0xC0, "fsubd", format.alopf11, DD_D, c.c25>;

defm QPMSK2SGNB : ALESEXT2Inst<0x34, 0x02, 0xC0, "qpmsk2sgnb", format.alopf11, pS_p, c.c0134>;

defm QPCEXT_0x00 : ALESEXT2Inst<0x35, 0x02, 0xC0, "qpcext_0x00", format.alopf11, DD_D, c.c0134>;
defm QPCEXT_0x7f : ALESEXT2Inst<0x35, 0x02, 0xC2, "qpcext_0x7f", format.alopf11, DD_D, c.c0134>;
defm QPCEXT_0x80 : ALESEXT2Inst<0x35, 0x02, 0xC4, "qpcext_0x80", format.alopf11, DD_D, c.c0134>;
defm QPCEXT_0xff : ALESEXT2Inst<0x35, 0x02, 0xC6, "qpcext_0xff", format.alopf11, DD_D, c.c0134>;

defm QPSIGNB : ALESEXT2Inst<0x38, 0x02, 0xC0, "qpsignb", format.alopf11, pp_p, c.c03>;
defm QPSIGNH : ALESEXT2Inst<0x39, 0x02, 0xC0, "qpsignh", format.alopf11, pp_p, c.c03>;
defm QPSIGNW : ALESEXT2Inst<0x3a, 0x02, 0xC0, "qpsignw", format.alopf11, pp_p, c.c03>;

// floating-point MUL
defm FMULS2 : ALESEXT2Inst<0x38, 0x02, 0xC0, "fmuls", format.alopf11, SS_S, c.c25>;
defm FMULD2 : ALESEXT2Inst<0x39, 0x02, 0xC0, "fmuld", format.alopf11, DD_D, c.c25>;

defm QPPACKDL : ALESEXT2Inst<0x3d, 0x02, 0xC0, "qppackdl", format.alopf11, DD_p, c.c0134>;

defm QPMINUB : ALESEXT2Inst<0x40, 0x02, 0xC0, "qpminub", format.alopf11, pp_p, c.c03>;

defm QPMINSH : ALESEXT2Inst<0x41, 0x02, 0xC0, "qpminsh", format.alopf11, pp_p, c.c03>;

defm QPMAXUB : ALESEXT2Inst<0x42, 0x02, 0xC0, "qpmaxub", format.alopf11, pp_p, c.c03>;

defm QPMAXSH : ALESEXT2Inst<0x43, 0x02, 0xC0, "qpmaxsh", format.alopf11, pp_p, c.c03>;

defm QPMINSB : ALESEXT2Inst<0x44, 0x02, 0xC0, "qpminsb", format.alopf11, pp_p, c.c03>;

defm QPMINUH : ALESEXT2Inst<0x45, 0x02, 0xC0, "qpminuh", format.alopf11, pp_p, c.c03>;

defm QPMAXSB : ALESEXT2Inst<0x46, 0x02, 0xC0, "qpmaxsb", format.alopf11, pp_p, c.c03>;

defm QPMAXUH : ALESEXT2Inst<0x47, 0x02, 0xC0, "qpmaxuh", format.alopf11, pp_p, c.c03>;

defm QPADDB : ALESEXT2Inst<0x48, 0x02, 0xC0, "qpaddb", format.alopf11, pp_p, c.c03>;

defm QPACKSSHB : ALESEXT2Inst<0x48, 0x02, 0xC0, "qpacksshb", format.alopf11, pp_p, c.c14>;

defm QPADDH : ALESEXT2Inst<0x49, 0x02, 0xC0, "qpaddh", format.alopf11, pp_p, c.c03>;

defm QPACKUSHB : ALESEXT2Inst<0x49, 0x02, 0xC0, "qpackushb", format.alopf11, pp_p, c.c14>;

defm QPADDSB : ALESEXT2Inst<0x4a, 0x02, 0xC0, "qpaddsb", format.alopf11, pp_p, c.c03>;

defm QPACKSSWH : ALESEXT2Inst<0x4a, 0x02, 0xC0, "qpacksswh", format.alopf11, pp_p, c.c14>;

defm QPADDSH : ALESEXT2Inst<0x4b, 0x02, 0xC0, "qpaddsh", format.alopf11, pp_p, c.c03>;

defm QPACKUSWH : ALESEXT2Inst<0x4b, 0x02, 0xC0, "qpackuswh", format.alopf11, pp_p, c.c14>;

defm QPADDUSB : ALESEXT2Inst<0x4c, 0x02, 0xC0, "qpaddusb", format.alopf11, pp_p, c.c03>;

defm QPADDUSH : ALESEXT2Inst<0x4d, 0x02, 0xC0, "qpaddush", format.alopf11, pp_p, c.c03>;

defm QPADDW : ALESEXT2Inst<0x4e, 0x02, 0xC0, "qpaddw", format.alopf11, pp_p, c.c03>;

defm QPADDD : ALESEXT2Inst<0x4f, 0x02, 0xC0, "qpaddd", format.alopf11, pp_p, c.c03>;

defm QPSUBB : ALESEXT2Inst<0x50, 0x02, 0xC0, "qpsubb", format.alopf11, pp_p, c.c03>;

defm QPSUBH : ALESEXT2Inst<0x51, 0x02, 0xC0, "qpsubh", format.alopf11, pp_p, c.c03>;

defm QPSUBSB : ALESEXT2Inst<0x52, 0x02, 0xC0, "qpsubsb", format.alopf11, pp_p, c.c03>;

defm QPSUBSH : ALESEXT2Inst<0x53, 0x02, 0xC0, "qpsubsh", format.alopf11, pp_p, c.c03>;

defm QPSUBUSB : ALESEXT2Inst<0x54, 0x02, 0xC0, "qpsubusb", format.alopf11, pp_p, c.c03>;

defm QPSUBUSH : ALESEXT2Inst<0x55, 0x02, 0xC0, "qpsubush", format.alopf11, pp_p, c.c03>;

defm QPSUBW : ALESEXT2Inst<0x56, 0x02, 0xC0, "qpsubw", format.alopf11, pp_p, c.c03>;

defm QPSUBD : ALESEXT2Inst<0x57, 0x02, 0xC0, "qpsubd", format.alopf11, pp_p, c.c03>;

defm QPCMPEQB : ALESEXT2Inst<0x58, 0x02, 0xC0, "qpcmpeqb", format.alopf11, pp_p, c.c03>;

defm QPMULHH : ALESEXT2Inst<0x58, 0x02, 0xC0, "qpmulhh", format.alopf11, pp_p, c.c14>;

defm QPCMPEQH : ALESEXT2Inst<0x59, 0x02, 0xC0, "qpcmpeqh", format.alopf11, pp_p, c.c03>;

defm QPMULLH : ALESEXT2Inst<0x59, 0x02, 0xC0, "qpmullh", format.alopf11, pp_p, c.c14>;

defm QPCMPEQW : ALESEXT2Inst<0x5a, 0x02, 0xC0, "qpcmpeqw", format.alopf11, pp_p, c.c03>;

defm QPMADDH : ALESEXT2Inst<0x5a, 0x02, 0xC0, "qpmaddh", format.alopf11, pp_p, c.c14>;

defm QPCMPGTB : ALESEXT2Inst<0x5b, 0x02, 0xC0, "qpcmpgtb", format.alopf11, pp_p, c.c03>;

defm QPMULHUH : ALESEXT2Inst<0x5b, 0x02, 0xC0, "qpmulhuh", format.alopf11, pp_p, c.c14>;

defm QPCMPGTH : ALESEXT2Inst<0x5c, 0x02, 0xC0, "qpcmpgth", format.alopf11, pp_p, c.c03>;

defm QPSADBW : ALESEXT2Inst<0x5c, 0x02, 0xC0, "qpsadbw", format.alopf11, pp_p, c.c14>;

defm QPCMPGTW : ALESEXT2Inst<0x5d, 0x02, 0xC0, "qpcmpgtw", format.alopf11, pp_p, c.c03>;

defm QPMULUBHH : ALESEXT2Inst<0x5d, 0x02, 0xC0, "qpmulubhh", format.alopf11, pp_p, c.c14>;

defm QPAVGUSB : ALESEXT2Inst<0x5e, 0x02, 0xC0, "qpavgusb", format.alopf11, pp_p, c.c03>;

defm QPMULLW : ALESEXT2Inst<0x5e, 0x02, 0xC0, "qpmull", format.alopf11, pp_p, c.c14>;

defm QPAVGUSH : ALESEXT2Inst<0x5f, 0x02, 0xC0, "qpavgush", format.alopf11, pp_p, c.c03>;

defm QPMRGP : ALESEXT2Inst<0x60, 0x02, 0xC0, "qpmrgp", format.alopf11, pp_p, c.c0134>;

defm LDQP : ALESEXT2Inst<0x61, 0x02, 0xC0, "ldqp", format.alopf11, DD_p, c.c0235>;

defm LDCSQP : ALESEXT2Inst<0x62, 0x02, 0xC0, "ldcsqp", format.alopf11, DD_p, c.c0235>;

defm LDDSQP : ALESEXT2Inst<0x63, 0x02, 0xC0, "lddsqp", format.alopf11, DD_p, c.c0235>;

defm LDESQP : ALESEXT2Inst<0x64, 0x02, 0xC0, "ldesqp", format.alopf11, DD_p, c.c0235>;

defm LDFSQP : ALESEXT2Inst<0x65, 0x02, 0xC0, "ldfsqp", format.alopf11, DD_p, c.c0235>;

defm LDGSQP : ALESEXT2Inst<0x66, 0x02, 0xC0, "ldgsqp", format.alopf11, DD_p, c.c0235>;

defm LDSSQP : ALESEXT2Inst<0x67, 0x02, 0xC0, "ldssqp", format.alopf11, DD_p, c.c0235>;

defm LDCUDQP : ALESEXT2Inst<0x68, 0x02, 0xC0, "ldcudqp", format.alopf11, DD_p, c.c0235>;

defm QPMADDUBSH : ALESEXT2Inst<0x68, 0x02, 0xC0, "qpmaddubsh", format.alopf11, pp_p, c.c14>;

defm LDGDQP : ALESEXT2Inst<0x69, 0x02, 0xC0, "ldgdqp", format.alopf11, SS_p, c.c0235>;

defm QPMULHRSH : ALESEXT2Inst<0x69, 0x02, 0xC0, "qpmulhrsh", format.alopf11, pp_p, c.c14>;

defm LDAPQP : ALESEXT2Inst<0x6a, 0x02, 0xC0, "ldapqp", format.alopf11, QS_p, c.c0235>;

defm QPHMINPOSUH : ALESEXT2Inst<0x6a, 0x02, 0xC0, "qphminposuh", format.alopf11, pp_D, c.c14>;

defm LDRQP : ALESEXT2Inst<0x6b, 0x02, 0xC0, "ldrqp", format.alopf11, DD_p, c.c035>;

defm QPMPSADBH : ALESEXT2Inst<0x6b, 0x02, 0xC0, "qpmpsadbh", format.alopf11, pS_p, c.c14>;

defm PUTTAGQP : ALESEXT2Inst<0x6c, 0x02, 0xC0, "puttagqp", format.alopf11, pS_p, c.c0134>;

defm QPIDTOFS : ALESEXT1EXT2Inst<0x6d, 0xEC, 0x02, 0xC0, "qpidtofs", format.alopf12, p_D, c.c0134>;

defm QPFSTOIFS : ALESEXT2Inst<0x6e, 0x02, 0xC0, "qpfstoifs", format.alopf11, Dp_p, c.c0134>;

defm QPFDTOIFD : ALESEXT2Inst<0x6f, 0x02, 0xC0, "qpfdtoifd", format.alopf11, Dp_p, c.c0134>;

defm QPFADDS : ALESEXT2Inst<0x70, 0x02, 0xC0, "qpfadds", format.alopf11, pp_p, c.all>;
defm QPFADDD : ALESEXT2Inst<0x71, 0x02, 0xC0, "qpfaddd", format.alopf11, pp_p, c.all>;

defm QPFSUBS : ALESEXT2Inst<0x72, 0x02, 0xC0, "qpfsubs", format.alopf11, pp_p, c.all>;
defm QPFSUBD : ALESEXT2Inst<0x73, 0x02, 0xC0, "qpfsubd", format.alopf11, pp_p, c.all>;

defm QPFMINS : ALESEXT2Inst<0x74, 0x02, 0xC0, "qpfmind", format.alopf11, pp_p, c.all>;
defm QPFMIND : ALESEXT2Inst<0x75, 0x02, 0xC0, "qpfmind", format.alopf11, pp_p, c.all>;

defm QPFMAXS : ALESEXT2Inst<0x76, 0x02, 0xC0, "qpfmaxd", format.alopf11, pp_p, c.all>;
defm QPFMAXD : ALESEXT2Inst<0x77, 0x02, 0xC0, "qpfmaxd", format.alopf11, pp_p, c.all>;

defm QPFMULS : ALESEXT2Inst<0x78, 0x02, 0xC0, "qpfmuls", format.alopf11, pp_p, c.all>;
defm QPFMULD : ALESEXT2Inst<0x79, 0x02, 0xC0, "qpfmuld", format.alopf11, pp_p, c.all>;

defm QPFCMPEQS : ALESEXT2Inst<0x7a, 0x02, 0xC0, "qpfcmpeqs", format.alopf11, SSS_S, c.c0134>;
defm QPFCMPEQD : ALESEXT2Inst<0x7b, 0x02, 0xC0, "qpfcmpeqd", format.alopf11, SSS_S, c.c0134>;

defm QPFCMPLTS : ALESEXT2Inst<0x7a, 0x02, 0xC1, "qpfcmplts", format.alopf11, SSS_S, c.c0134>;
defm QPFCMPLTD : ALESEXT2Inst<0x7b, 0x02, 0xC1, "qpfcmpltd", format.alopf11, SSS_S, c.c0134>;

defm QPFCMPLES : ALESEXT2Inst<0x7a, 0x02, 0xC2, "qpfcmples", format.alopf11, SSS_S, c.c0134>;
defm QPFCMPLED : ALESEXT2Inst<0x7b, 0x02, 0xC2, "qpfcmpled", format.alopf11, SSS_S, c.c0134>;

defm QPFCMPUODS : ALESEXT2Inst<0x7a, 0x02, 0xC3, "qpfcmpuods", format.alopf11, SSS_S, c.c0134>;
defm QPFCMPUODD : ALESEXT2Inst<0x7b, 0x02, 0xC3, "qpfcmpuodd", format.alopf11, SSS_S, c.c0134>;

defm QPFCMPNEQS : ALESEXT2Inst<0x7a, 0x02, 0xC4, "qpfcmpneqs", format.alopf11, SSS_S, c.c0134>;
defm QPFCMPNEQD : ALESEXT2Inst<0x7b, 0x02, 0xC4, "qpfcmpneqd", format.alopf11, SSS_S, c.c0134>;

defm QPFCMPNLTS : ALESEXT2Inst<0x7a, 0x02, 0xC5, "qpfcmpnlts", format.alopf11, SSS_S, c.c0134>;
defm QPFCMPNLTD : ALESEXT2Inst<0x7b, 0x02, 0xC5, "qpfcmpnltd", format.alopf11, SSS_S, c.c0134>;

defm QPFCMPNLES : ALESEXT2Inst<0x7a, 0x02, 0xC6, "qpfcmpnles", format.alopf11, SSS_S, c.c0134>;
defm QPFCMPNLED : ALESEXT2Inst<0x7b, 0x02, 0xC6, "qpfcmpnled", format.alopf11, SSS_S, c.c0134>;

defm QPFCMPODS : ALESEXT2Inst<0x7a, 0x02, 0xC7, "qpfcmpods", format.alopf11, SSS_S, c.c0134>;
defm QPFCMPODD : ALESEXT2Inst<0x7b, 0x02, 0xC7, "qpfcmpodd", format.alopf11, SSS_S, c.c0134>;

defm QPFHADDS : ALESEXT2Inst<0x7c, 0x02, 0xC0, "qpfhadds", format.alopf11, pp_p, c.all>;

defm QPFHSUBS : ALESEXT2Inst<0x7d, 0x02, 0xC0, "qpfhsubs", format.alopf11, pp_p, c.all>;

defm QPFADDSUBS : ALESEXT2Inst<0x7e, 0x02, 0xC0, "qpfaddsubs", format.alopf11, pp_p, c.all>;
defm QPFADDSUBD : ALESEXT2Inst<0x7f, 0x02, 0xC0, "qpfaddsubd", format.alopf11, pp_p, c.all>;

//===----------------------------------------------------------------------===//
// ALES opcodes, opcode2 = 3
//===----------------------------------------------------------------------===//

defm FMAS : ALESInst<0x00, 0x03, "fmas", format.alopf21, SSS_S, c.all>;
defm FMAD : ALESInst<0x01, 0x03, "fmad", format.alopf21, DDD_D, c.all>;

defm FMSS : ALESInst<0x02, 0x03, "fmss", format.alopf21, SSS_S, c.all>;
defm FMSD : ALESInst<0x03, 0x03, "fmsd", format.alopf21, DDD_D, c.all>;

defm FNMAS : ALESInst<0x04, 0x03, "fnmas", format.alopf21, SSS_S, c.all>;
defm FNMAD : ALESInst<0x05, 0x03, "fnmad", format.alopf21, DDD_D, c.all>;

defm FNMSS : ALESInst<0x06, 0x03, "fnmss", format.alopf21, SSS_S, c.all>;
defm FNMSD : ALESInst<0x07, 0x03, "fnmsd", format.alopf21, DDD_D, c.all>;

defm QPFMAS : ALESInst<0x08, 0x03, "qpfmas", format.alopf21, SSS_S, c.all>;
defm QPFMAD : ALESInst<0x09, 0x03, "qpfmad", format.alopf21, DDD_D, c.all>;

defm QPFMSS : ALESInst<0x0a, 0x03, "qpfmss", format.alopf21, SSS_S, c.all>;
defm QPFMSD : ALESInst<0x0b, 0x03, "qpfmsd", format.alopf21, DDD_D, c.all>;

defm QPFNMAS : ALESInst<0x0c, 0x03, "qpfnmas", format.alopf21, SSS_S, c.all>;
defm QPFNMAD : ALESInst<0x0d, 0x03, "qpfnmad", format.alopf21, DDD_D, c.all>;

defm QPFNMSS : ALESInst<0x0e, 0x03, "qpfnmss", format.alopf21, SSS_S, c.all>;
defm QPFNMSD : ALESInst<0x0f, 0x03, "qpfnmsd", format.alopf21, DDD_D, c.all>;

defm QPFMASS : ALESInst<0x10, 0x03, "qpfmass", format.alopf21, SSS_S, c.all>;
defm QPFMASD : ALESInst<0x11, 0x03, "qpfmasd", format.alopf21, DDD_D, c.all>;

defm QPFMSAS : ALESInst<0x12, 0x03, "qpfmsas", format.alopf21, SSS_S, c.all>;
defm QPFMSAD : ALESInst<0x13, 0x03, "qpfmsad", format.alopf21, DDD_D, c.all>;

defm CLMULL : ALESEXT2Inst<0x14, 0x03, 0xC0, "clmull", format.alopf11, DD_D, c.c0134>;
defm CLMULH : ALESEXT2Inst<0x15, 0x03, 0xC0, "clmulh", format.alopf11, DD_D, c.c0134>;

//===----------------------------------------------------------------------===//
// ALES opcodes, opcode2 = 4
//===----------------------------------------------------------------------===//

defm ANDS_FB : ALESEXT2Inst<0x00, 0x04, 0xC0, "ands_fb", format.alopf11, SS_S, c.c03>;

defm ANDNS_FB : ALESEXT2Inst<0x02, 0x04, 0xC0, "andns_fb", format.alopf11, SS_S, c.c03>;

defm ORS_FB : ALESEXT2Inst<0x04, 0x04, 0xC0, "ors_fb", format.alopf11, SS_S, c.c03>;

defm ORNS_FB : ALESEXT2Inst<0x06, 0x04, 0xC0, "orns_fb", format.alopf11, SS_S, c.c03>;

defm XORS_FB : ALESEXT2Inst<0x08, 0x04, 0xC0, "xors_fb", format.alopf11, SS_S, c.c03>;

defm XORNS_FB : ALESEXT2Inst<0x0a, 0x04, 0xC0, "xorns_fb", format.alopf11, SS_S, c.c03>;

defm ADDS_FB : ALESEXT2Inst<0x10, 0x04, 0xC0, "adds_fb", format.alopf11, SS_S, c.c03>;

defm SUBS_FB : ALESEXT2Inst<0x12, 0x04, 0xC0, "subs_fb", format.alopf11, SS_S, c.c03>;

defm SCLS_FB : ALESInst<0x14, 0x04, "scls_fb", format.alopf21, SSS_S, c.c03>;

defm SCRS_FB : ALESInst<0x16, 0x04, "scrs_fb", format.alopf21, SSS_S, c.c03>;

defm SHLS_FB : ALESInst<0x18, 0x04, "shls_fb", format.alopf21, SSS_S, c.c03>;

defm SHRS_FB : ALESInst<0x1a, 0x04, "shrs_fb", format.alopf21, SSS_S, c.c03>;

defm SARS_FB : ALESInst<0x1c, 0x04, "sars_fb", format.alopf21, SSS_S, c.c03>;

defm UMULX_FB : ALESEXT2Inst<0x22, 0x04, 0xC0, "umulx_fb", format.alopf11, SS_S, c.c03>;

defm SMULX_FB : ALESEXT2Inst<0x23, 0x04, 0xC0, "smulx_fb", format.alopf11, SS_S, c.c03>;

defm INCS_FB : ALESInst<0x30, 0x04, "incs_fb", format.alopf21, SSS_S, c.c03>;

defm DECS_FB : ALESInst<0x32, 0x04, "decs_fb", format.alopf21, SSS_S, c.c03>;

//===----------------------------------------------------------------------===//
// ALES opcodes, opcode2 = 5
//===----------------------------------------------------------------------===//

defm ANDS_FH : ALESEXT2Inst<0x00, 0x05, 0xC0, "ands_fh", format.alopf11, SS_S, c.c03>;

defm ANDNS_FH : ALESEXT2Inst<0x02, 0x05, 0xC0, "andns_fh", format.alopf11, SS_S, c.c03>;

defm ORS_FH : ALESEXT2Inst<0x04, 0x05, 0xC0, "ors_fh", format.alopf11, SS_S, c.c03>;

defm ORNS_FH : ALESEXT2Inst<0x06, 0x05, 0xC0, "orns_fh", format.alopf11, SS_S, c.c03>;

defm XORS_FH : ALESEXT2Inst<0x08, 0x05, 0xC0, "xors_fh", format.alopf11, SS_S, c.c03>;

defm XORNS_FH : ALESEXT2Inst<0x0a, 0x05, 0xC0, "xorns_fh", format.alopf11, SS_S, c.c03>;

defm ADDS_FH : ALESEXT2Inst<0x10, 0x05, 0xC0, "adds_fh", format.alopf11, SS_S, c.c03>;

defm SUBS_FH : ALESEXT2Inst<0x12, 0x05, 0xC0, "subs_fh", format.alopf11, SS_S, c.c03>;

defm SCLS_FH : ALESInst<0x14, 0x05, "scls_fh", format.alopf21, SSS_S, c.c03>;

defm SCRS_FH : ALESInst<0x16, 0x05, "scrs_fh", format.alopf21, SSS_S, c.c03>;

defm SHLS_FH : ALESInst<0x18, 0x05, "shls_fh", format.alopf21, SSS_S, c.c03>;

defm SHRS_FH : ALESInst<0x1a, 0x05, "shrs_fh", format.alopf21, SSS_S, c.c03>;

defm SARS_FH : ALESInst<0x1c, 0x05, "sars_fh", format.alopf21, SSS_S, c.c03>;

defm UMULX_FH : ALESEXT2Inst<0x22, 0x05, 0xC0, "umulx_fh", format.alopf11, SS_S, c.c03>;

defm SMULX_FH : ALESEXT2Inst<0x23, 0x05, 0xC0, "smulx_fh", format.alopf11, SS_S, c.c03>;

defm INCS_FH : ALESInst<0x30, 0x05, "incs_fh", format.alopf21, SSS_S, c.c03>;

defm DECS_FH : ALESInst<0x32, 0x05, "decs_fh", format.alopf21, SSS_S, c.c03>;

//===----------------------------------------------------------------------===//
// ALES opcodes, opcode2 = 6
//===----------------------------------------------------------------------===//

defm ANDS_FW : ALESEXT2Inst<0x00, 0x06, 0xC0, "ands_fw", format.alopf11, SS_S, c.c03>;

defm ANDNS_FW : ALESEXT2Inst<0x02, 0x06, 0xC0, "andns_fw", format.alopf11, SS_S, c.c03>;

defm ORS_FW : ALESEXT2Inst<0x04, 0x06, 0xC0, "ors_fw", format.alopf11, SS_S, c.c03>;

defm ORNS_FW : ALESEXT2Inst<0x06, 0x06, 0xC0, "orns_fw", format.alopf11, SS_S, c.c03>;

defm XORS_FW : ALESEXT2Inst<0x08, 0x06, 0xC0, "xors_fw", format.alopf11, SS_S, c.c03>;

defm XORNS_FW : ALESEXT2Inst<0x0a, 0x06, 0xC0, "xorns_fw", format.alopf11, SS_S, c.c03>;

defm ADDS_FW : ALESEXT2Inst<0x10, 0x06, 0xC0, "adds_fw", format.alopf11, SS_S, c.c03>;

defm SUBS_FW : ALESEXT2Inst<0x12, 0x06, 0xC0, "subs_fw", format.alopf11, SS_S, c.c03>;

defm SCLS_FW : ALESInst<0x14, 0x06, "scls_fw", format.alopf21, SSS_S, c.c03>;

defm SCRS_FW : ALESInst<0x16, 0x06, "scrs_fw", format.alopf21, SSS_S, c.c03>;

defm SHLS_FW : ALESInst<0x18, 0x06, "shls_fw", format.alopf21, SSS_S, c.c03>;

defm SHRS_FW : ALESInst<0x1a, 0x06, "shrs_fw", format.alopf21, SSS_S, c.c03>;

defm SARS_FW : ALESInst<0x1c, 0x06, "sars_fw", format.alopf21, SSS_S, c.c03>;

defm UMULX_FW : ALESEXT2Inst<0x22, 0x06, 0xC0, "umulx_fw", format.alopf11, SS_S, c.c03>;

defm SMULX_FW : ALESEXT2Inst<0x23, 0x06, 0xC0, "smulx_fw", format.alopf11, SS_S, c.c03>;

defm INCS_FW : ALESInst<0x30, 0x06, "incs_fw", format.alopf21, SSS_S, c.c03>;

defm DECS_FW : ALESInst<0x32, 0x06, "decs_fw", format.alopf21, SSS_S, c.c03>;

//===----------------------------------------------------------------------===//
// ALES opcodes, opcode2 = 7
//===----------------------------------------------------------------------===//

defm ANDD_FD : ALESEXT2Inst<0x01, 0x07, 0xC0, "andd_fd", format.alopf11, DD_S, c.c03>;

defm ANDND_FD : ALESEXT2Inst<0x03, 0x07, 0xC0, "andnd_fd", format.alopf11, DD_S, c.c03>;

defm ORD_FD : ALESEXT2Inst<0x05, 0x07, 0xC0, "ord_fd", format.alopf11, DD_S, c.c03>;

defm ORND_FD : ALESEXT2Inst<0x07, 0x07, 0xC0, "ornd_fd", format.alopf11, DD_S, c.c03>;

defm XORD_FD : ALESEXT2Inst<0x09, 0x07, 0xC0, "xord_fd", format.alopf11, DD_S, c.c03>;

defm XORND_FD : ALESEXT2Inst<0x0b, 0x07, 0xC0, "xornd_fd", format.alopf11, DD_S, c.c03>;

defm ADDD_FD : ALESEXT2Inst<0x11, 0x07, 0xC0, "addd_fd", format.alopf11, DD_S, c.c03>;

defm SUBD_FD : ALESEXT2Inst<0x13, 0x07, 0xC0, "subd_fd", format.alopf11, DD_S, c.c03>;

defm SCLD_FD : ALESInst<0x15, 0x07, "scld_fd", format.alopf21, DDS_S, c.c03>;

defm SCRD_FD : ALESInst<0x17, 0x07, "scrd_fd", format.alopf21, DDS_S, c.c03>;

defm SHLD_FD : ALESInst<0x19, 0x07, "shld_fd", format.alopf21, DDS_S, c.c03>;

defm SHRD_FD : ALESInst<0x1b, 0x07, "shrd_fd", format.alopf21, DDS_S, c.c03>;

defm SARD_FD : ALESInst<0x1d, 0x07, "sard_fd", format.alopf21, DDS_S, c.c03>;

defm INCD_FD : ALESInst<0x31, 0x07, "incd_fd", format.alopf21, DDS_S, c.c03>;

defm DECD_FD : ALESInst<0x33, 0x07, "decd_fd", format.alopf21, DDS_S, c.c03>;

//===----------------------------------------------------------------------===//
// ALES opcodes, opcode2 = 8
//===----------------------------------------------------------------------===//

defm AND_ANDS : ALESInst<0x00, 0x08, "and_ands", format.alopf21, SSS_S, c.c14>;
defm AND_ANDD : ALESInst<0x01, 0x08, "and_andd", format.alopf21, DDD_D, c.c14>;

defm ANDN_ANDS : ALESInst<0x02, 0x08, "andn_ands", format.alopf21, SSS_S, c.c14>;
defm ANDN_ANDD : ALESInst<0x03, 0x08, "andn_andd", format.alopf21, DDD_D, c.c14>;

defm OR_ANDS : ALESInst<0x04, 0x08, "or_ands", format.alopf21, SSS_S, c.c14>;
defm OR_ANDD : ALESInst<0x05, 0x08, "or_andd", format.alopf21, DDD_D, c.c14>;

defm ORN_ANDS : ALESInst<0x06, 0x08, "orn_ands", format.alopf21, SSS_S, c.c14>;
defm ORN_ANDS : ALESInst<0x07, 0x08, "orn_andd", format.alopf21, DDD_D, c.c14>;

defm XOR_ANDS : ALESInst<0x08, 0x08, "xor_ands", format.alopf21, SSS_S, c.c14>;
defm XOR_ANDD : ALESInst<0x09, 0x08, "xor_andd", format.alopf21, DDD_D, c.c14>;

defm XORN_ANDS : ALESInst<0x0a, 0x08, "xorn_ands", format.alopf21, SSS_S, c.c14>;
defm XORN_ANDD : ALESInst<0x0b, 0x08, "xorn_andd", format.alopf21, DDD_D, c.c14>;

defm MERGE_ANDS : ALESInst<0x0e, 0x08, "merge_ands", format.alopf21, SSS_S, c.c14>;
defm MERGE_ANDD : ALESInst<0x0f, 0x08, "merge_andd", format.alopf21, DDD_D, c.c14>;

defm ADD_ANDS : ALESInst<0x10, 0x08, "add_ands", format.alopf21, SSS_S, c.c14>;
defm ADD_ANDD : ALESInst<0x11, 0x08, "add_andd", format.alopf21, DDD_D, c.c14>;

defm SUB_ANDS : ALESInst<0x12, 0x08, "sub_ands", format.alopf21, SSS_S, c.c14>;
defm SUB_ANDD : ALESInst<0x13, 0x08, "sub_andd", format.alopf21, DDD_D, c.c14>;

defm SCL_ANDS : ALESInst<0x14, 0x08, "scl_ands", format.alopf21, SSS_S, c.c14>;
defm SCL_ANDD : ALESInst<0x15, 0x08, "scl_andd", format.alopf21, DDD_D, c.c14>;

defm SCR_ANDS : ALESInst<0x16, 0x08, "scr_ands", format.alopf21, SSS_S, c.c14>;
defm SCR_ANDD : ALESInst<0x17, 0x08, "scr_andd", format.alopf21, DDD_D, c.c14>;

defm SHL_ANDS : ALESInst<0x18, 0x08, "shl_ands", format.alopf21, SSS_S, c.c14>;
defm SHL_ANDD : ALESInst<0x19, 0x08, "shl_andd", format.alopf21, DDD_D, c.c14>;

defm SHR_ANDS : ALESInst<0x1a, 0x08, "shr_ands", format.alopf21, SSS_S, c.c14>;
defm SHR_ANDD : ALESInst<0x1b, 0x08, "shr_andd", format.alopf21, DDD_D, c.c14>;

defm SAR_ANDS : ALESInst<0x1c, 0x08, "sar_ands", format.alopf21, SSS_S, c.c14>;
defm SAR_ANDD : ALESInst<0x1d, 0x08, "sar_andd", format.alopf21, DDD_D, c.c14>;

defm GETF_ANDS : ALESInst<0x1e, 0x08, "getf_ands", format.alopf21, SSS_S, c.c14>;
defm GETF_ANDD : ALESInst<0x1f, 0x08, "getf_andd", format.alopf21, DDD_D, c.c14>;

defm AND_ANDNS : ALESInst<0x20, 0x08, "and_andns", format.alopf21, SSS_S, c.c14>;
defm AND_ANDND : ALESInst<0x21, 0x08, "and_andnd", format.alopf21, DDD_D, c.c14>;

defm ANDN_ANDNS : ALESInst<0x22, 0x08, "andn_andns", format.alopf21, SSS_S, c.c14>;
defm ANDN_ANDND : ALESInst<0x23, 0x08, "andn_andnd", format.alopf21, DDD_D, c.c14>;

defm OR_ANDNS : ALESInst<0x24, 0x08, "or_andns", format.alopf21, SSS_S, c.c14>;
defm OR_ANDND : ALESInst<0x25, 0x08, "or_andnd", format.alopf21, DDD_D, c.c14>;

defm ORN_ANDNS : ALESInst<0x26, 0x08, "orn_andns", format.alopf21, SSS_S, c.c14>;
defm ORN_ANDNS : ALESInst<0x27, 0x08, "orn_andnd", format.alopf21, DDD_D, c.c14>;

defm XOR_ANDNS : ALESInst<0x28, 0x08, "xor_andns", format.alopf21, SSS_S, c.c14>;
defm XOR_ANDND : ALESInst<0x29, 0x08, "xor_andnd", format.alopf21, DDD_D, c.c14>;

defm XORN_ANDNS : ALESInst<0x2a, 0x08, "xorn_andns", format.alopf21, SSS_S, c.c14>;
defm XORN_ANDND : ALESInst<0x2b, 0x08, "xorn_andnd", format.alopf21, DDD_D, c.c14>;

defm MERGE_ANDNS : ALESInst<0x2e, 0x08, "merge_andns", format.alopf21, SSS_S, c.c14>;
defm MERGE_ANDND : ALESInst<0x2f, 0x08, "merge_andnd", format.alopf21, DDD_D, c.c14>;

defm ADD_ANDNS : ALESInst<0x30, 0x08, "add_andns", format.alopf21, SSS_S, c.c14>;
defm ADD_ANDND : ALESInst<0x31, 0x08, "add_andnd", format.alopf21, DDD_D, c.c14>;

defm SUB_ANDNS : ALESInst<0x32, 0x08, "sub_andns", format.alopf21, SSS_S, c.c14>;
defm SUB_ANDND : ALESInst<0x33, 0x08, "sub_andnd", format.alopf21, DDD_D, c.c14>;

defm SCL_ANDNS : ALESInst<0x34, 0x08, "scl_andns", format.alopf21, SSS_S, c.c14>;
defm SCL_ANDND : ALESInst<0x35, 0x08, "scl_andnd", format.alopf21, DDD_D, c.c14>;

defm SCR_ANDNS : ALESInst<0x36, 0x08, "scr_andns", format.alopf21, SSS_S, c.c14>;
defm SCR_ANDND : ALESInst<0x37, 0x08, "scr_andnd", format.alopf21, DDD_D, c.c14>;

defm SHL_ANDNS : ALESInst<0x38, 0x08, "shl_andns", format.alopf21, SSS_S, c.c14>;
defm SHL_ANDND : ALESInst<0x39, 0x08, "shl_andnd", format.alopf21, DDD_D, c.c14>;

defm SHR_ANDNS : ALESInst<0x3a, 0x08, "shr_andns", format.alopf21, SSS_S, c.c14>;
defm SHR_ANDND : ALESInst<0x3b, 0x08, "shr_andnd", format.alopf21, DDD_D, c.c14>;

defm SAR_ANDNS : ALESInst<0x3c, 0x08, "sar_andns", format.alopf21, SSS_S, c.c14>;
defm SAR_ANDND : ALESInst<0x3d, 0x08, "sar_andnd", format.alopf21, DDD_D, c.c14>;

defm GETF_ANDNS : ALESInst<0x3e, 0x08, "getf_andns", format.alopf21, SSS_S, c.c14>;
defm GETF_ANDND : ALESInst<0x3f, 0x08, "getf_andnd", format.alopf21, DDD_D, c.c14>;

defm AND_ORS : ALESInst<0x40, 0x08, "and_ors", format.alopf21, SSS_S, c.c14>;
defm AND_ORD : ALESInst<0x41, 0x08, "and_ord", format.alopf21, DDD_D, c.c14>;

defm ANDN_ORS : ALESInst<0x42, 0x08, "andn_ors", format.alopf21, SSS_S, c.c14>;
defm ANDN_ORD : ALESInst<0x43, 0x08, "andn_ord", format.alopf21, DDD_D, c.c14>;

defm OR_ORS : ALESInst<0x44, 0x08, "or_ors", format.alopf21, SSS_S, c.c14>;
defm OR_ORD : ALESInst<0x45, 0x08, "or_ord", format.alopf21, DDD_D, c.c14>;

defm ORN_ORS : ALESInst<0x46, 0x08, "orn_ors", format.alopf21, SSS_S, c.c14>;
defm ORN_ORS : ALESInst<0x47, 0x08, "orn_ord", format.alopf21, DDD_D, c.c14>;

defm XOR_ORS : ALESInst<0x48, 0x08, "xor_ors", format.alopf21, SSS_S, c.c14>;
defm XOR_ORD : ALESInst<0x49, 0x08, "xor_ord", format.alopf21, DDD_D, c.c14>;

defm XORN_ORS : ALESInst<0x4a, 0x08, "xorn_ors", format.alopf21, SSS_S, c.c14>;
defm XORN_ORD : ALESInst<0x4b, 0x08, "xorn_ord", format.alopf21, DDD_D, c.c14>;

defm MERGE_ORS : ALESInst<0x4e, 0x08, "merge_ors", format.alopf21, SSS_S, c.c14>;
defm MERGE_ORD : ALESInst<0x4f, 0x08, "merge_ord", format.alopf21, DDD_D, c.c14>;

defm ADD_ORS : ALESInst<0x50, 0x08, "add_ors", format.alopf21, SSS_S, c.c14>;
defm ADD_ORD : ALESInst<0x51, 0x08, "add_ord", format.alopf21, DDD_D, c.c14>;

defm SUB_ORS : ALESInst<0x52, 0x08, "sub_ors", format.alopf21, SSS_S, c.c14>;
defm SUB_ORD : ALESInst<0x53, 0x08, "sub_ord", format.alopf21, DDD_D, c.c14>;

defm SCL_ORS : ALESInst<0x54, 0x08, "scl_ors", format.alopf21, SSS_S, c.c14>;
defm SCL_ORD : ALESInst<0x55, 0x08, "scl_ord", format.alopf21, DDD_D, c.c14>;

defm SCR_ORS : ALESInst<0x56, 0x08, "scr_ors", format.alopf21, SSS_S, c.c14>;
defm SCR_ORD : ALESInst<0x57, 0x08, "scr_ord", format.alopf21, DDD_D, c.c14>;

defm SHL_ORS : ALESInst<0x58, 0x08, "shl_ors", format.alopf21, SSS_S, c.c14>;
defm SHL_ORD : ALESInst<0x59, 0x08, "shl_ord", format.alopf21, DDD_D, c.c14>;

defm SHR_ORS : ALESInst<0x5a, 0x08, "shr_ors", format.alopf21, SSS_S, c.c14>;
defm SHR_ORD : ALESInst<0x5b, 0x08, "shr_ord", format.alopf21, DDD_D, c.c14>;

defm SAR_ORS : ALESInst<0x5c, 0x08, "sar_ors", format.alopf21, SSS_S, c.c14>;
defm SAR_ORD : ALESInst<0x5d, 0x08, "sar_ord", format.alopf21, DDD_D, c.c14>;

defm GETF_ORS : ALESInst<0x5e, 0x08, "getf_ors", format.alopf21, SSS_S, c.c14>;
defm GETF_ORD : ALESInst<0x5f, 0x08, "getf_ord", format.alopf21, DDD_D, c.c14>;

defm AND_ORNS : ALESInst<0x60, 0x08, "and_orns", format.alopf21, SSS_S, c.c14>;
defm AND_ORND : ALESInst<0x61, 0x08, "and_ornd", format.alopf21, DDD_D, c.c14>;

defm ANDN_ORNS : ALESInst<0x62, 0x08, "andn_orns", format.alopf21, SSS_S, c.c14>;
defm ANDN_ORND : ALESInst<0x63, 0x08, "andn_ornd", format.alopf21, DDD_D, c.c14>;

defm OR_ORNS : ALESInst<0x64, 0x08, "or_orns", format.alopf21, SSS_S, c.c14>;
defm OR_ORND : ALESInst<0x65, 0x08, "or_ornd", format.alopf21, DDD_D, c.c14>;

defm ORN_ORNS : ALESInst<0x66, 0x08, "orn_orns", format.alopf21, SSS_S, c.c14>;
defm ORN_ORNS : ALESInst<0x67, 0x08, "orn_ornd", format.alopf21, DDD_D, c.c14>;

defm XOR_ORNS : ALESInst<0x68, 0x08, "xor_orns", format.alopf21, SSS_S, c.c14>;
defm XOR_ORND : ALESInst<0x69, 0x08, "xor_ornd", format.alopf21, DDD_D, c.c14>;

defm XORN_ORNS : ALESInst<0x6a, 0x08, "xorn_orns", format.alopf21, SSS_S, c.c14>;
defm XORN_ORND : ALESInst<0x6b, 0x08, "xorn_ornd", format.alopf21, DDD_D, c.c14>;

defm MERGE_ORNS : ALESInst<0x6e, 0x08, "merge_orns", format.alopf21, SSS_S, c.c14>;
defm MERGE_ORND : ALESInst<0x6f, 0x08, "merge_ornd", format.alopf21, DDD_D, c.c14>;

defm ADD_ORNS : ALESInst<0x70, 0x08, "add_orns", format.alopf21, SSS_S, c.c14>;
defm ADD_ORND : ALESInst<0x71, 0x08, "add_ornd", format.alopf21, DDD_D, c.c14>;

defm SUB_ORNS : ALESInst<0x72, 0x08, "sub_orns", format.alopf21, SSS_S, c.c14>;
defm SUB_ORND : ALESInst<0x73, 0x08, "sub_ornd", format.alopf21, DDD_D, c.c14>;

defm SCL_ORNS : ALESInst<0x74, 0x08, "scl_orns", format.alopf21, SSS_S, c.c14>;
defm SCL_ORND : ALESInst<0x75, 0x08, "scl_ornd", format.alopf21, DDD_D, c.c14>;

defm SCR_ORNS : ALESInst<0x76, 0x08, "scr_orns", format.alopf21, SSS_S, c.c14>;
defm SCR_ORND : ALESInst<0x77, 0x08, "scr_ornd", format.alopf21, DDD_D, c.c14>;

defm SHL_ORNS : ALESInst<0x78, 0x08, "shl_orns", format.alopf21, SSS_S, c.c14>;
defm SHL_ORND : ALESInst<0x79, 0x08, "shl_ornd", format.alopf21, DDD_D, c.c14>;

defm SHR_ORNS : ALESInst<0x7a, 0x08, "shr_orns", format.alopf21, SSS_S, c.c14>;
defm SHR_ORND : ALESInst<0x7b, 0x08, "shr_ornd", format.alopf21, DDD_D, c.c14>;

defm SAR_ORNS : ALESInst<0x7c, 0x08, "sar_orns", format.alopf21, SSS_S, c.c14>;
defm SAR_ORND : ALESInst<0x7d, 0x08, "sar_ornd", format.alopf21, DDD_D, c.c14>;

defm GETF_ORNS : ALESInst<0x7e, 0x08, "getf_orns", format.alopf21, SSS_S, c.c14>;
defm GETF_ORND : ALESInst<0x7f, 0x08, "getf_ornd", format.alopf21, DDD_D, c.c14>;

//===----------------------------------------------------------------------===//
// ALES opcodes, opcode2 = 9
//===----------------------------------------------------------------------===//

defm AND_XORS : ALESInst<0x00, 0x09, "and_xors", format.alopf21, SSS_S, c.c14>;
defm AND_XORD : ALESInst<0x01, 0x09, "and_xord", format.alopf21, DDD_D, c.c14>;

defm ANDN_XORS : ALESInst<0x02, 0x09, "andn_xors", format.alopf21, SSS_S, c.c14>;
defm ANDN_XORD : ALESInst<0x03, 0x09, "andn_xord", format.alopf21, DDD_D, c.c14>;

defm OR_XORS : ALESInst<0x04, 0x09, "or_xors", format.alopf21, SSS_S, c.c14>;
defm OR_XORD : ALESInst<0x05, 0x09, "or_xord", format.alopf21, DDD_D, c.c14>;

defm ORN_XORS : ALESInst<0x06, 0x09, "orn_xors", format.alopf21, SSS_S, c.c14>;
defm ORN_XORS : ALESInst<0x07, 0x09, "orn_xord", format.alopf21, DDD_D, c.c14>;

defm XOR_XORS : ALESInst<0x08, 0x09, "xor_xors", format.alopf21, SSS_S, c.c14>;
defm XOR_XORD : ALESInst<0x09, 0x09, "xor_xord", format.alopf21, DDD_D, c.c14>;

defm XORN_XORS : ALESInst<0x0a, 0x09, "xorn_xors", format.alopf21, SSS_S, c.c14>;
defm XORN_XORD : ALESInst<0x0b, 0x09, "xorn_xord", format.alopf21, DDD_D, c.c14>;

defm MERGE_XORS : ALESInst<0x0e, 0x09, "merge_xors", format.alopf21, SSS_S, c.c14>;
defm MERGE_XORD : ALESInst<0x0f, 0x09, "merge_xord", format.alopf21, DDD_D, c.c14>;

defm ADD_XORS : ALESInst<0x10, 0x09, "add_xors", format.alopf21, SSS_S, c.c14>;
defm ADD_XORD : ALESInst<0x11, 0x09, "add_xord", format.alopf21, DDD_D, c.c14>;

defm SUB_XORS : ALESInst<0x12, 0x09, "sub_xors", format.alopf21, SSS_S, c.c14>;
defm SUB_XORD : ALESInst<0x13, 0x09, "sub_xord", format.alopf21, DDD_D, c.c14>;

defm SCL_XORS : ALESInst<0x14, 0x09, "scl_xors", format.alopf21, SSS_S, c.c14>;
defm SCL_XORD : ALESInst<0x15, 0x09, "scl_xord", format.alopf21, DDD_D, c.c14>;

defm SCR_XORS : ALESInst<0x16, 0x09, "scr_xors", format.alopf21, SSS_S, c.c14>;
defm SCR_XORD : ALESInst<0x17, 0x09, "scr_xord", format.alopf21, DDD_D, c.c14>;

defm SHL_XORS : ALESInst<0x18, 0x09, "shl_xors", format.alopf21, SSS_S, c.c14>;
defm SHL_XORD : ALESInst<0x19, 0x09, "shl_xord", format.alopf21, DDD_D, c.c14>;

defm SHR_XORS : ALESInst<0x1a, 0x09, "shr_xors", format.alopf21, SSS_S, c.c14>;
defm SHR_XORD : ALESInst<0x1b, 0x09, "shr_xord", format.alopf21, DDD_D, c.c14>;

defm SAR_XORS : ALESInst<0x1c, 0x09, "sar_xors", format.alopf21, SSS_S, c.c14>;
defm SAR_XORD : ALESInst<0x1d, 0x09, "sar_xord", format.alopf21, DDD_D, c.c14>;

defm GETF_XORS : ALESInst<0x1e, 0x09, "getf_xors", format.alopf21, SSS_S, c.c14>;
defm GETF_XORD : ALESInst<0x1f, 0x09, "getf_xord", format.alopf21, DDD_D, c.c14>;

defm AND_XORNS : ALESInst<0x20, 0x09, "and_xorns", format.alopf21, SSS_S, c.c14>;
defm AND_XORND : ALESInst<0x21, 0x09, "and_xornd", format.alopf21, DDD_D, c.c14>;

defm ANDN_XORNS : ALESInst<0x22, 0x09, "andn_xorns", format.alopf21, SSS_S, c.c14>;
defm ANDN_XORND : ALESInst<0x23, 0x09, "andn_xornd", format.alopf21, DDD_D, c.c14>;

defm OR_XORNS : ALESInst<0x24, 0x09, "or_xorns", format.alopf21, SSS_S, c.c14>;
defm OR_XORND : ALESInst<0x25, 0x09, "or_xornd", format.alopf21, DDD_D, c.c14>;

defm ORN_XORNS : ALESInst<0x26, 0x09, "orn_xorns", format.alopf21, SSS_S, c.c14>;
defm ORN_XORNS : ALESInst<0x27, 0x09, "orn_xornd", format.alopf21, DDD_D, c.c14>;

defm XOR_XORNS : ALESInst<0x28, 0x09, "xor_xorns", format.alopf21, SSS_S, c.c14>;
defm XOR_XORND : ALESInst<0x29, 0x09, "xor_xornd", format.alopf21, DDD_D, c.c14>;

defm XORN_XORNS : ALESInst<0x2a, 0x09, "xorn_xorns", format.alopf21, SSS_S, c.c14>;
defm XORN_XORND : ALESInst<0x2b, 0x09, "xorn_xornd", format.alopf21, DDD_D, c.c14>;

defm MERGE_XORNS : ALESInst<0x2e, 0x09, "merge_xorns", format.alopf21, SSS_S, c.c14>;
defm MERGE_XORND : ALESInst<0x2f, 0x09, "merge_xornd", format.alopf21, DDD_D, c.c14>;

defm ADD_XORNS : ALESInst<0x30, 0x09, "add_xorns", format.alopf21, SSS_S, c.c14>;
defm ADD_XORND : ALESInst<0x31, 0x09, "add_xornd", format.alopf21, DDD_D, c.c14>;

defm SUB_XORNS : ALESInst<0x32, 0x09, "sub_xorns", format.alopf21, SSS_S, c.c14>;
defm SUB_XORND : ALESInst<0x33, 0x09, "sub_xornd", format.alopf21, DDD_D, c.c14>;

defm SCL_XORNS : ALESInst<0x34, 0x09, "scl_xorns", format.alopf21, SSS_S, c.c14>;
defm SCL_XORND : ALESInst<0x35, 0x09, "scl_xornd", format.alopf21, DDD_D, c.c14>;

defm SCR_XORNS : ALESInst<0x36, 0x09, "scr_xorns", format.alopf21, SSS_S, c.c14>;
defm SCR_XORND : ALESInst<0x37, 0x09, "scr_xornd", format.alopf21, DDD_D, c.c14>;

defm SHL_XORNS : ALESInst<0x38, 0x09, "shl_xorns", format.alopf21, SSS_S, c.c14>;
defm SHL_XORND : ALESInst<0x39, 0x09, "shl_xornd", format.alopf21, DDD_D, c.c14>;

defm SHR_XORNS : ALESInst<0x3a, 0x09, "shr_xorns", format.alopf21, SSS_S, c.c14>;
defm SHR_XORND : ALESInst<0x3b, 0x09, "shr_xornd", format.alopf21, DDD_D, c.c14>;

defm SAR_XORNS : ALESInst<0x3c, 0x09, "sar_xorns", format.alopf21, SSS_S, c.c14>;
defm SAR_XORND : ALESInst<0x3d, 0x09, "sar_xornd", format.alopf21, DDD_D, c.c14>;

defm GETF_XORNS : ALESInst<0x3e, 0x09, "getf_xorns", format.alopf21, SSS_S, c.c14>;
defm GETF_XORND : ALESInst<0x3f, 0x09, "getf_xornd", format.alopf21, DDD_D, c.c14>;

defm AND_RSUBS : ALESInst<0x40, 0x09, "and_rsubs", format.alopf21, SSS_S, c.c14>;
defm AND_RSUBD : ALESInst<0x41, 0x09, "and_rsubd", format.alopf21, DDD_D, c.c14>;

defm ANDN_RSUBS : ALESInst<0x42, 0x09, "andn_rsubs", format.alopf21, SSS_S, c.c14>;
defm ANDN_RSUBD : ALESInst<0x43, 0x09, "andn_rsubd", format.alopf21, DDD_D, c.c14>;

defm OR_RSUBS : ALESInst<0x44, 0x09, "or_rsubs", format.alopf21, SSS_S, c.c14>;
defm OR_RSUBD : ALESInst<0x45, 0x09, "or_rsubd", format.alopf21, DDD_D, c.c14>;

defm ORN_RSUBS : ALESInst<0x46, 0x09, "orn_rsubs", format.alopf21, SSS_S, c.c14>;
defm ORN_RSUBS : ALESInst<0x47, 0x09, "orn_rsubd", format.alopf21, DDD_D, c.c14>;

defm XOR_RSUBS : ALESInst<0x48, 0x09, "xor_rsubs", format.alopf21, SSS_S, c.c14>;
defm XOR_RSUBD : ALESInst<0x49, 0x09, "xor_rsubd", format.alopf21, DDD_D, c.c14>;

defm XORN_RSUBS : ALESInst<0x4a, 0x09, "xorn_rsubs", format.alopf21, SSS_S, c.c14>;
defm XORN_RSUBD : ALESInst<0x4b, 0x09, "xorn_rsubd", format.alopf21, DDD_D, c.c14>;

defm MERGE_RSUBS : ALESInst<0x4e, 0x09, "merge_rsubs", format.alopf21, SSS_S, c.c14>;
defm MERGE_RSUBD : ALESInst<0x4f, 0x09, "merge_rsubd", format.alopf21, DDD_D, c.c14>;

defm ADD_RSUBS : ALESInst<0x50, 0x09, "add_rsubs", format.alopf21, SSS_S, c.c14>;
defm ADD_RSUBD : ALESInst<0x51, 0x09, "add_rsubd", format.alopf21, DDD_D, c.c14>;

defm SUB_RSUBS : ALESInst<0x52, 0x09, "sub_rsubs", format.alopf21, SSS_S, c.c14>;
defm SUB_RSUBD : ALESInst<0x53, 0x09, "sub_rsubd", format.alopf21, DDD_D, c.c14>;

defm SCL_RSUBS : ALESInst<0x54, 0x09, "scl_rsubs", format.alopf21, SSS_S, c.c14>;
defm SCL_RSUBD : ALESInst<0x55, 0x09, "scl_rsubd", format.alopf21, DDD_D, c.c14>;

defm SCR_RSUBS : ALESInst<0x56, 0x09, "scr_rsubs", format.alopf21, SSS_S, c.c14>;
defm SCR_RSUBD : ALESInst<0x57, 0x09, "scr_rsubd", format.alopf21, DDD_D, c.c14>;

defm SHL_RSUBS : ALESInst<0x58, 0x09, "shl_rsubs", format.alopf21, SSS_S, c.c14>;
defm SHL_RSUBD : ALESInst<0x59, 0x09, "shl_rsubd", format.alopf21, DDD_D, c.c14>;

defm SHR_RSUBS : ALESInst<0x5a, 0x09, "shr_rsubs", format.alopf21, SSS_S, c.c14>;
defm SHR_RSUBD : ALESInst<0x5b, 0x09, "shr_rsubd", format.alopf21, DDD_D, c.c14>;

defm SAR_RSUBS : ALESInst<0x5c, 0x09, "sar_rsubs", format.alopf21, SSS_S, c.c14>;
defm SAR_RSUBD : ALESInst<0x5d, 0x09, "sar_rsubd", format.alopf21, DDD_D, c.c14>;

defm GETF_RSUBS : ALESInst<0x5e, 0x09, "getf_rsubs", format.alopf21, SSS_S, c.c14>;
defm GETF_RSUBD : ALESInst<0x5f, 0x09, "getf_rsubd", format.alopf21, DDD_D, c.c14>;

//===----------------------------------------------------------------------===//
// ALES opcodes, opcode2 = 10
//===----------------------------------------------------------------------===//

defm AND_ADDS : ALESInst<0x00, 0x0a, "and_adds", format.alopf21, SSS_S, c.c14>;
defm AND_ADDD : ALESInst<0x01, 0x0a, "and_addd", format.alopf21, DDD_D, c.c14>;

defm ANDN_ADDS : ALESInst<0x02, 0x0a, "andn_adds", format.alopf21, SSS_S, c.c14>;
defm ANDN_ADDD : ALESInst<0x03, 0x0a, "andn_addd", format.alopf21, DDD_D, c.c14>;

defm OR_ADDS : ALESInst<0x04, 0x0a, "or_adds", format.alopf21, SSS_S, c.c14>;
defm OR_ADDD : ALESInst<0x05, 0x0a, "or_addd", format.alopf21, DDD_D, c.c14>;

defm ORN_ADDS : ALESInst<0x06, 0x0a, "orn_adds", format.alopf21, SSS_S, c.c14>;
defm ORN_ADDS : ALESInst<0x07, 0x0a, "orn_addd", format.alopf21, DDD_D, c.c14>;

defm XOR_ADDS : ALESInst<0x08, 0x0a, "xor_adds", format.alopf21, SSS_S, c.c14>;
defm XOR_ADDD : ALESInst<0x09, 0x0a, "xor_addd", format.alopf21, DDD_D, c.c14>;

defm XORN_ADDS : ALESInst<0x0a, 0x0a, "xorn_adds", format.alopf21, SSS_S, c.c14>;
defm XORN_ADDD : ALESInst<0x0b, 0x0a, "xorn_addd", format.alopf21, DDD_D, c.c14>;

defm MERGE_ADDS : ALESInst<0x0e, 0x0a, "merge_adds", format.alopf21, SSS_S, c.c14>;
defm MERGE_ADDD : ALESInst<0x0f, 0x0a, "merge_addd", format.alopf21, DDD_D, c.c14>;

defm ADD_ADDS : ALESInst<0x10, 0x0a, "add_adds", format.alopf21, SSS_S, c.c14>;
defm ADD_ADDD : ALESInst<0x11, 0x0a, "add_addd", format.alopf21, DDD_D, c.c14>;

defm SUB_ADDS : ALESInst<0x12, 0x0a, "sub_adds", format.alopf21, SSS_S, c.c14>;
defm SUB_ADDD : ALESInst<0x13, 0x0a, "sub_addd", format.alopf21, DDD_D, c.c14>;

defm SCL_ADDS : ALESInst<0x14, 0x0a, "scl_adds", format.alopf21, SSS_S, c.c14>;
defm SCL_ADDD : ALESInst<0x15, 0x0a, "scl_addd", format.alopf21, DDD_D, c.c14>;

defm SCR_ADDS : ALESInst<0x16, 0x0a, "scr_adds", format.alopf21, SSS_S, c.c14>;
defm SCR_ADDD : ALESInst<0x17, 0x0a, "scr_addd", format.alopf21, DDD_D, c.c14>;

defm SHL_ADDS : ALESInst<0x18, 0x0a, "shl_adds", format.alopf21, SSS_S, c.c14>;
defm SHL_ADDD : ALESInst<0x19, 0x0a, "shl_addd", format.alopf21, DDD_D, c.c14>;

defm SHR_ADDS : ALESInst<0x1a, 0x0a, "shr_adds", format.alopf21, SSS_S, c.c14>;
defm SHR_ADDD : ALESInst<0x1b, 0x0a, "shr_addd", format.alopf21, DDD_D, c.c14>;

defm SAR_ADDS : ALESInst<0x1c, 0x0a, "sar_adds", format.alopf21, SSS_S, c.c14>;
defm SAR_ADDD : ALESInst<0x1d, 0x0a, "sar_addd", format.alopf21, DDD_D, c.c14>;

defm GETF_ADDS : ALESInst<0x1e, 0x0a, "getf_adds", format.alopf21, SSS_S, c.c14>;
defm GETF_ADDD : ALESInst<0x1f, 0x0a, "getf_addd", format.alopf21, DDD_D, c.c14>;

defm AND_SUBS : ALESInst<0x20, 0x0a, "and_subs", format.alopf21, SSS_S, c.c14>;
defm AND_SUBD : ALESInst<0x21, 0x0a, "and_subd", format.alopf21, DDD_D, c.c14>;

defm ANDN_SUBS : ALESInst<0x22, 0x0a, "andn_subs", format.alopf21, SSS_S, c.c14>;
defm ANDN_SUBD : ALESInst<0x23, 0x0a, "andn_subd", format.alopf21, DDD_D, c.c14>;

defm OR_SUBS : ALESInst<0x24, 0x0a, "or_subs", format.alopf21, SSS_S, c.c14>;
defm OR_SUBD : ALESInst<0x25, 0x0a, "or_subd", format.alopf21, DDD_D, c.c14>;

defm ORN_SUBS : ALESInst<0x26, 0x0a, "orn_subs", format.alopf21, SSS_S, c.c14>;
defm ORN_SUBS : ALESInst<0x27, 0x0a, "orn_subd", format.alopf21, DDD_D, c.c14>;

defm XOR_SUBS : ALESInst<0x28, 0x0a, "xor_subs", format.alopf21, SSS_S, c.c14>;
defm XOR_SUBD : ALESInst<0x29, 0x0a, "xor_subd", format.alopf21, DDD_D, c.c14>;

defm XORN_SUBS : ALESInst<0x2a, 0x0a, "xorn_subs", format.alopf21, SSS_S, c.c14>;
defm XORN_SUBD : ALESInst<0x2b, 0x0a, "xorn_subd", format.alopf21, DDD_D, c.c14>;

defm MERGE_SUBS : ALESInst<0x2e, 0x0a, "merge_subs", format.alopf21, SSS_S, c.c14>;
defm MERGE_SUBD : ALESInst<0x2f, 0x0a, "merge_subd", format.alopf21, DDD_D, c.c14>;

defm ADD_SUBS : ALESInst<0x30, 0x0a, "add_subs", format.alopf21, SSS_S, c.c14>;
defm ADD_SUBD : ALESInst<0x31, 0x0a, "add_subd", format.alopf21, DDD_D, c.c14>;

defm SUB_SUBS : ALESInst<0x32, 0x0a, "sub_subs", format.alopf21, SSS_S, c.c14>;
defm SUB_SUBD : ALESInst<0x33, 0x0a, "sub_subd", format.alopf21, DDD_D, c.c14>;

defm SCL_SUBS : ALESInst<0x34, 0x0a, "scl_subs", format.alopf21, SSS_S, c.c14>;
defm SCL_SUBD : ALESInst<0x35, 0x0a, "scl_subd", format.alopf21, DDD_D, c.c14>;

defm SCR_SUBS : ALESInst<0x36, 0x0a, "scr_subs", format.alopf21, SSS_S, c.c14>;
defm SCR_SUBD : ALESInst<0x37, 0x0a, "scr_subd", format.alopf21, DDD_D, c.c14>;

defm SHL_SUBS : ALESInst<0x38, 0x0a, "shl_subs", format.alopf21, SSS_S, c.c14>;
defm SHL_SUBD : ALESInst<0x39, 0x0a, "shl_subd", format.alopf21, DDD_D, c.c14>;

defm SHR_SUBS : ALESInst<0x3a, 0x0a, "shr_subs", format.alopf21, SSS_S, c.c14>;
defm SHR_SUBD : ALESInst<0x3b, 0x0a, "shr_subd", format.alopf21, DDD_D, c.c14>;

defm SAR_SUBS : ALESInst<0x3c, 0x0a, "sar_subs", format.alopf21, SSS_S, c.c14>;
defm SAR_SUBD : ALESInst<0x3d, 0x0a, "sar_subd", format.alopf21, DDD_D, c.c14>;

defm GETF_SUBS : ALESInst<0x3e, 0x0a, "getf_subs", format.alopf21, SSS_S, c.c14>;
defm GETF_SUBD : ALESInst<0x3f, 0x0a, "getf_subd", format.alopf21, DDD_D, c.c14>;

//===----------------------------------------------------------------------===//
// ALES opcodes, opcode2 = 11
//===----------------------------------------------------------------------===//

// insert field
defm INSFS : ALESInst<0x6c, 0x0b, "insfs", format.alopf21, SSS_S, c.c0134>;
defm INSFD : ALESInst<0x6d, 0x0b, "insfd", format.alopf21, DDD_D, c.c0134>;

//===----------------------------------------------------------------------===//
// ALES opcodes, opcode2 = 12
//===----------------------------------------------------------------------===//

defm FADD_ADDS : ALESInst<0x00, 0x0c, "fadd_adds", format.alopf21, SSS_S, c.all>;
defm FADD_ADDD : ALESInst<0x01, 0x0c, "fadd_addd", format.alopf21, DDD_D, c.all>;

defm FSUB_ADDS : ALESInst<0x02, 0x0c, "fsub_adds", format.alopf21, SSS_S, c.all>;
defm FSUB_ADDD : ALESInst<0x03, 0x0c, "fsub_addd", format.alopf21, DDD_D, c.all>;

defm FMUL_ADDS : ALESInst<0x08, 0x0c, "fmul_adds", format.alopf21, SSS_S, c.all>;
defm FMUL_ADDD : ALESInst<0x09, 0x0c, "fmul_addd", format.alopf21, DDD_D, c.all>;

defm FADD_SUBS : ALESInst<0x20, 0x0c, "fadd_subs", format.alopf21, SSS_S, c.all>;
defm FADD_SUBD : ALESInst<0x21, 0x0c, "fadd_subd", format.alopf21, DDD_D, c.all>;

defm FSUB_SUBS : ALESInst<0x22, 0x0c, "fsub_subs", format.alopf21, SSS_S, c.all>;
defm FSUB_SUBD : ALESInst<0x23, 0x0c, "fsub_subd", format.alopf21, DDD_D, c.all>;

defm FMUL_SUBS : ALESInst<0x28, 0x0c, "fmul_subs", format.alopf21, SSS_S, c.all>;
defm FMUL_SUBD : ALESInst<0x29, 0x0c, "fmul_subd", format.alopf21, DDD_D, c.all>;

//===----------------------------------------------------------------------===//
// ALES opcodes, opcode2 = 13
//===----------------------------------------------------------------------===//

defm FADD_RSUBS : ALESInst<0x00, 0x0d, "fadd_rsubs", format.alopf21, SSS_S, c.all>;
defm FADD_RSUBD : ALESInst<0x01, 0x0d, "fadd_rsubd", format.alopf21, DDD_D, c.all>;

defm FSUB_RSUBS : ALESInst<0x02, 0x0d, "fsub_rsubs", format.alopf21, SSS_S, c.all>;
defm FSUB_RSUBD : ALESInst<0x03, 0x0d, "fsub_rsubd", format.alopf21, DDD_D, c.all>;

defm FMUL_RSUBS : ALESInst<0x08, 0x0d, "fmul_rsubs", format.alopf21, SSS_S, c.all>;
defm FMUL_RSUBD : ALESInst<0x09, 0x0d, "fmul_rsubd", format.alopf21, DDD_D, c.all>;

//===----------------------------------------------------------------------===//
// ALES opcodes, opcode2 = 14
//===----------------------------------------------------------------------===//

defm PFADD_ADDS : ALESInst<0x00, 0x0e, "pfadd_adds", format.alopf21, SSS_S, c.all>;
defm PFADD_ADDD : ALESInst<0x01, 0x0e, "pfadd_addd", format.alopf21, DDD_D, c.all>;

defm PFSUB_ADDS : ALESInst<0x02, 0x0e, "pfsub_adds", format.alopf21, SSS_S, c.all>;
defm PFSUB_ADDD : ALESInst<0x03, 0x0e, "pfsub_addd", format.alopf21, DDD_D, c.all>;

defm PFHADD_ADDS : ALESInst<0x04, 0x0e, "pfhadd_adds", format.alopf21, SSS_S, c.all>;
defm PFHSUB_ADDS : ALESInst<0x06, 0x0e, "pfhsub_adds", format.alopf21, SSS_S, c.all>;

defm PFMUL_ADDS : ALESInst<0x08, 0x0e, "pfmul_adds", format.alopf21, SSS_S, c.all>;
defm PFMUL_ADDD : ALESInst<0x09, 0x0e, "pfmul_addd", format.alopf21, DDD_D, c.all>;

defm PFHADDSUB_ADDS : ALESInst<0x0e, 0x0e, "pfhadd_adds", format.alopf21, SSS_S, c.all>;

defm PFADD_SUBS : ALESInst<0x20, 0x0e, "pfadd_subs", format.alopf21, SSS_S, c.all>;
defm PFADD_SUBD : ALESInst<0x21, 0x0e, "pfadd_subd", format.alopf21, DDD_D, c.all>;

defm PFSUB_SUBS : ALESInst<0x22, 0x0e, "pfsub_subs", format.alopf21, SSS_S, c.all>;
defm PFSUB_SUBD : ALESInst<0x23, 0x0e, "pfsub_subd", format.alopf21, DDD_D, c.all>;

defm PFHADD_SUBS : ALESInst<0x24, 0x0e, "pfhadd_subs", format.alopf21, SSS_S, c.all>;
defm PFHSUB_SUBS : ALESInst<0x26, 0x0e, "pfhsub_subs", format.alopf21, SSS_S, c.all>;

defm PFMUL_SUBS : ALESInst<0x28, 0x0e, "pfmul_subs", format.alopf21, SSS_S, c.all>;
defm PFMUL_SUBD : ALESInst<0x29, 0x0e, "pfmul_subd", format.alopf21, DDD_D, c.all>;

defm PFHADDSUB_SUBS : ALESInst<0x2e, 0x0e, "pfhadd_subs", format.alopf21, SSS_S, c.all>;

defm PFADD_HADDS : ALESInst<0x40, 0x0e, "pfadd_hadds", format.alopf21, SSS_S, c.all>;

defm PFSUB_HADDS : ALESInst<0x42, 0x0e, "pfsub_hadds", format.alopf21, SSS_S, c.all>;

defm PFHADD_HADDS : ALESInst<0x44, 0x0e, "pfhadd_hadds", format.alopf21, SSS_S, c.all>;

defm PFHSUB_HADDS : ALESInst<0x46, 0x0e, "pfhsub_hadds", format.alopf21, SSS_S, c.all>;

defm PFMUL_HADDS : ALESInst<0x48, 0x0e, "pfmul_hadds", format.alopf21, SSS_S, c.all>;

defm PFADDSUB_HADDS : ALESInst<0x4e, 0x0e, "pfhadd_hadds", format.alopf21, SSS_S, c.all>;

defm PFADD_HSUBS : ALESInst<0x60, 0x0e, "pfadd_hsubs", format.alopf21, SSS_S, c.all>;

defm PFSUB_HSUBS : ALESInst<0x62, 0x0e, "pfsub_hsubs", format.alopf21, SSS_S, c.all>;

defm PFHADD_HSUBS : ALESInst<0x64, 0x0e, "pfhadd_hsubs", format.alopf21, SSS_S, c.all>;

defm PFHSUB_HSUBS : ALESInst<0x66, 0x0e, "pfhsub_hsubs", format.alopf21, SSS_S, c.all>;

defm PFMUL_HSUBS : ALESInst<0x68, 0x0e, "pfmul_hsubs", format.alopf21, SSS_S, c.all>;

defm PFADDSUB_HSUBS : ALESInst<0x6e, 0x0e, "pfadd_hsubs", format.alopf21, SSS_S, c.all>;

//===----------------------------------------------------------------------===//
// ALES opcodes, opcode2 = 15
//===----------------------------------------------------------------------===//

defm PFADD_RSUBS : ALESInst<0x20, 0x0f, "pfadd_rsubs", format.alopf21, SSS_S, c.all>;
defm PFADD_RSUBD : ALESInst<0x21, 0x0f, "pfadd_rsubd", format.alopf21, DDD_D, c.all>;

defm PFSUB_RSUBS : ALESInst<0x22, 0x0f, "pfsub_rsubs", format.alopf21, SSS_S, c.all>;
defm PFSUB_RSUBD : ALESInst<0x23, 0x0f, "pfsub_rsubd", format.alopf21, DDD_D, c.all>;

defm PFHADD_RSUBS : ALESInst<0x24, 0x0f, "pfhadd_rsubs", format.alopf21, SSS_S, c.all>;
defm PFHSUB_RSUBS : ALESInst<0x26, 0x0f, "pfhsub_rsubs", format.alopf21, SSS_S, c.all>;

defm PFMUL_RSUBS : ALESInst<0x28, 0x0f, "pfmul_rsubs", format.alopf21, SSS_S, c.all>;
defm PFMUL_RSUBD : ALESInst<0x29, 0x0f, "pfmul_rsubd", format.alopf21, DDD_D, c.all>;

defm PFHADDSUB_RSUBS : ALESInst<0x2e, 0x0f, "pfhadd_rsubs", format.alopf21, SSS_S, c.all>;

defm PSHUFB : ALESInst<0x4d, 0x0f, "pshufb", format.alopf21, DDD_D, c.c0134>;

defm PFADD_ADDSUBS : ALESInst<0x60, 0x0f, "pfadd_addsubs", format.alopf21, SSS_S, c.all>;

defm PFSUB_ADDSUBS : ALESInst<0x62, 0x0f, "pfsub_addsubs", format.alopf21, SSS_S, c.all>;

defm PFHADD_ADDSUBS : ALESInst<0x64, 0x0f, "pfhadd_addsubs", format.alopf21, SSS_S, c.all>;

defm PFHSUB_ADDSUBS : ALESInst<0x66, 0x0f, "pfhsub_addsubs", format.alopf21, SSS_S, c.all>;

defm PFMUL_ADDSUBS : ALESInst<0x68, 0x0f, "pfmul_addsubs", format.alopf21, SSS_S, c.all>;

defm PMERGE : ALESInst<0x6d, 0x0f, "pmerge", format.alopf21, DDD_D, c.c0134>;

defm PFHADDSUB_ADDSUBS : ALESInst<0x6e, 0x0f, "pfhadd_addsubs", format.alopf21, SSS_S, c.all>;


//===----------------------------------------------------------------------===//
// ALES opcodes, opcode2 = 16
//===----------------------------------------------------------------------===//

foreach Index = 0-127 in {
  defm PLOG_#Index : ALESInst<Index, 0x10, "plog_"#Index, format.alopf21, DDD_D, c.c0134>;
}

//===----------------------------------------------------------------------===//
// ALES opcodes, opcode2 = 17
//===----------------------------------------------------------------------===//

foreach Index = 0-127 in {
  defm PLOG_#!add(Index, 128) : ALESInst<Index, 0x11, "plog_"#!add(Index, 128), format.alopf21, DDD_D, c.c0134>;
}

//===----------------------------------------------------------------------===//
// ALES opcodes, opcode2 = 18
//===----------------------------------------------------------------------===//

foreach Index = 0-127 in {
  defm QPLOG_#Index : ALESInst<Index, 0x12, "qplog_"#Index, format.alopf21, DDD_D, c.c0134>;
}

//===----------------------------------------------------------------------===//
// ALES opcodes, opcode2 = 19
//===----------------------------------------------------------------------===//

foreach Index = 0-127 in {
  defm QPLOG_#!add(Index, 128) : ALESInst<Index, 0x13, "qplog_"#!add(Index, 128), format.alopf21, DDD_D, c.c0134>;
}

//===----------------------------------------------------------------------===//
// CS0 opcodes
//===----------------------------------------------------------------------===//

// non-procedure immediate flow control
def IBRANCH : CS0Inst<0x00, (outs), (ins), "ibranch">;

// prefetch code
def PREF : CS0Inst<0x01, (outs), (ins), "pref">;

def PUTTSD : CS0Inst<0x02, (outs), (ins), "puttsd">;

// return from the hardware interrupt handler
def DONE : CS0Inst<0x03, (outs), (ins), "done">;

// prepare literal flow control (BRANCH/CALL type)
def DISP : CS0Inst<0x04, (outs), (ins), "disp">;
def DISP2 : CS0Inst<0x08, (outs), (ins), "disp">;
def DISP3 : CS0Inst<0x0c, (outs), (ins), "disp">;

def NOTHING : CS0Inst<0x05, (outs), (ins), "?">;

// prepare global OS procedure flow control (SCALL type)
def SDISP : CS0Inst<0x06, (outs), (ins), "sdisp">;
def SDISP2 : CS0Inst<0x0a, (outs), (ins), "sdisp">;
def SDISP3 : CS0Inst<0x0e, (outs), (ins), "sdisp">;

def GETTSD : CS0Inst<0x07, (outs), (ins), "gettsd">;
def GETTSD2 : CS0Inst<0x0b, (outs), (ins), "gettsd">;

// prepare array pefetch
def LDISP : CS0Inst<0x09, (outs), (ins), "ldisp">;

def NOTHING2 : CS0Inst<0x0d, (outs), (ins), "?">;

// prepare return from the procedure
def RETURN : CS0Inst<0x0f, (outs), (ins), "return">;

//===----------------------------------------------------------------------===//
// CS1 opcodes
//===----------------------------------------------------------------------===//

// set size of procedure stack window
def SETWD : CS1Inst<0x00, (outs), (ins), "setwd">;

// verify size of procedure register area
def VFRPSZ : CS1Inst<0x01, (outs), (ins), "vfrpsz">;

def SETEI : CS1InstF1<0x02, 0x00, (outs), (ins), "setei">;

def SETFST : CS1InstF1<0x02, 0x01, (outs), (ins), "setfst">;

// wait for the execution of previous operations
def WAIT : CS1Inst<0x03, (outs), (ins), "wait">;

// set rotation base for number registers
def SETBN : CS1InstF2<0x04, 0x01, 0x00, (outs), (ins), "setbn">;

// set rotation base for predicate registers
def SETBP : CS1InstF2<0x04, 0x00, 0x01, (outs), (ins), "setbp">;

def CALL : CS1Inst<0x05, (outs), (ins), "call">;

// memory address specifier, it's not decoded as an instruction itself
def MAS : CS1Inst<0x06, (outs), (ins), "mas">;

// this are not fully mutually exclusive, so maybe move to C++ code
def FLUSHR : FLUSHInst<0x07, 1, 0, 0, 0, (outs), (ins), "flushr">;
def FLUSHC : FLUSHInst<0x07, 0, 1, 0, 0, (outs), (ins), "flushc">;
def FILLR : FLUSHInst<0x07, 0, 0, 1, 0, (outs), (ins), "fillr">;
def FILLC : FLUSHInst<0x07, 0, 0, 0, 1, (outs), (ins), "fillc">;

def VFBG : CS1Inst<0x08, (outs), (ins), "vfbg">;

//===----------------------------------------------------------------------===//
// Feature predicates.
//===----------------------------------------------------------------------===//

// True when generating 32-bit code.
def Is32Bit : Predicate<"!Subtarget->is64Bit()">;

// True when generating 64-bit code. This also implies HasV9.
def Is64Bit : Predicate<"Subtarget->is64Bit()">;

//===----------------------------------------------------------------------===//
// Instruction Pattern Stuff
//===----------------------------------------------------------------------===//

def simm11  : PatLeaf<(imm), [{ return isInt<11>(N->getSExtValue()); }]>;

def simm13  : PatLeaf<(imm), [{ return isInt<13>(N->getSExtValue()); }]>;

def LO10 : SDNodeXForm<imm, [{
  return CurDAG->getTargetConstant((unsigned)N->getZExtValue() & 1023, SDLoc(N),
                                   MVT::i32);
}]>;

def HI22 : SDNodeXForm<imm, [{
  // Transformation function: shift the immediate value down into the low bits.
  return CurDAG->getTargetConstant((unsigned)N->getZExtValue() >> 10, SDLoc(N),
                                   MVT::i32);
}]>;

// Return the complement of a HI22 immediate value.
def HI22_not : SDNodeXForm<imm, [{
  return CurDAG->getTargetConstant(~(unsigned)N->getZExtValue() >> 10, SDLoc(N),
                                   MVT::i32);
}]>;

def SETHIimm : PatLeaf<(imm), [{
  return isShiftedUInt<22, 10>(N->getZExtValue());
}], HI22>;

// The N->hasOneUse() prevents the immediate from being instantiated in both
// normal and complement form.
def SETHIimm_not : PatLeaf<(i32 imm), [{
  return N->hasOneUse() && isShiftedUInt<22, 10>(~(unsigned)N->getZExtValue());
}], HI22_not>;

// Addressing modes.
def ADDRrr : ComplexPattern<iPTR, 2, "SelectADDRrr", [], []>;
def ADDRri : ComplexPattern<iPTR, 2, "SelectADDRri", [frameindex], []>;

// Constrained operands for the shift operations.
class ShiftAmtImmAsmOperand<int Bits> : AsmOperandClass {
    let Name = "ShiftAmtImm" # Bits;
    let ParserMethod = "parseShiftAmtImm<" # Bits # ">";
}
def shift_imm5 : Operand<i32> {
  let ParserMatchClass = ShiftAmtImmAsmOperand<5>;
}
def shift_imm6 : Operand<i32> {
  let ParserMatchClass = ShiftAmtImmAsmOperand<6>;
}

// Address operands
def E2KMEMrrAsmOperand : AsmOperandClass {
  let Name = "MEMrr";
  let ParserMethod = "parseMEMOperand";
}

def E2KMEMriAsmOperand : AsmOperandClass {
  let Name = "MEMri";
  let ParserMethod = "parseMEMOperand";
}

def MEMrr : Operand<iPTR> {
  let PrintMethod = "printMemOperand";
  let MIOperandInfo = (ops ptr_rc:$rs1, ptr_rc:$rs2);
  let ParserMatchClass = E2KMEMrrAsmOperand;
}
def MEMri : Operand<iPTR> {
  let PrintMethod = "printMemOperand";
  let MIOperandInfo = (ops ptr_rc:$rs1, i32imm:$simm13);
  let ParserMatchClass = E2KMEMriAsmOperand;
}

// Represents a tail relocation operand for instructions such as add, ld, call.
class E2KTailRelocSymAsmOperand<string Kind> : AsmOperandClass {
  let Name = "TailRelocSym" # Kind;
  let RenderMethod = "addTailRelocSymOperands";
  let PredicateMethod = "isTailRelocSym";
  let ParserMethod = "parseTailRelocSym<TailRelocKind::" # Kind # ">";
}

def TailRelocSymGOTLoad : Operand<iPTR> {
  let ParserMatchClass = E2KTailRelocSymAsmOperand<"Load_GOT">;
}

def TailRelocSymTLSAdd : Operand<iPTR> {
  let ParserMatchClass = E2KTailRelocSymAsmOperand<"Add_TLS">;
}

def TailRelocSymTLSLoad : Operand<iPTR> {
  let ParserMatchClass = E2KTailRelocSymAsmOperand<"Load_TLS">;
}

def TailRelocSymTLSCall : Operand<iPTR> {
  let ParserMatchClass = E2KTailRelocSymAsmOperand<"Call_TLS">;
}

def E2KMembarTagAsmOperand : AsmOperandClass {
  let Name = "MembarTag";
  let ParserMethod = "parseMembarTag";
}

def MembarTag : Operand<i32> {
  let PrintMethod = "printMembarTag";
  let ParserMatchClass = E2KMembarTagAsmOperand;
}

// Branch targets have OtherVT type.
def brtarget : Operand<OtherVT> {
  let EncoderMethod = "getBranchTargetOpValue";
}

def bprtarget : Operand<OtherVT> {
  let EncoderMethod = "getBranchPredTargetOpValue";
}

def bprtarget16 : Operand<OtherVT> {
  let EncoderMethod = "getBranchOnRegTargetOpValue";
}

def E2KCallTargetAsmOperand : AsmOperandClass {
  let Name = "CallTarget";
  let ParserMethod = "parseCallTarget";
}

def calltarget : Operand<i32> {
  let EncoderMethod = "getCallTargetOpValue";
  let DecoderMethod = "DecodeCall";
  let ParserMatchClass = E2KCallTargetAsmOperand;
}

def simm13Op : Operand<i32> {
  let DecoderMethod = "DecodeSIMM13";
  let EncoderMethod = "getSImm13OpValue";
}

// Operand for printing out a condition code.
let PrintMethod = "printCCOperand" in
  def CCOp : Operand<i32>;

def SDTSPcmpicc :
SDTypeProfile<0, 2, [SDTCisInt<0>, SDTCisSameAs<0, 1>]>;
def SDTSPcmpfcc :
SDTypeProfile<0, 2, [SDTCisFP<0>, SDTCisSameAs<0, 1>]>;
def SDTSPbrcc :
SDTypeProfile<0, 2, [SDTCisVT<0, OtherVT>, SDTCisVT<1, i32>]>;
def SDTSPselectcc :
SDTypeProfile<1, 3, [SDTCisSameAs<0, 1>, SDTCisSameAs<1, 2>, SDTCisVT<3, i32>]>;
def SDTSPFTOI :
SDTypeProfile<1, 1, [SDTCisVT<0, f32>, SDTCisFP<1>]>;
def SDTSPITOF :
SDTypeProfile<1, 1, [SDTCisFP<0>, SDTCisVT<1, f32>]>;
def SDTSPFTOX :
SDTypeProfile<1, 1, [SDTCisVT<0, f64>, SDTCisFP<1>]>;
def SDTSPXTOF :
SDTypeProfile<1, 1, [SDTCisFP<0>, SDTCisVT<1, f64>]>;

def SDTSPtlsadd :
SDTypeProfile<1, 3, [SDTCisInt<0>, SDTCisSameAs<0, 1>, SDTCisPtrTy<2>]>;
def SDTSPtlsld :
SDTypeProfile<1, 2, [SDTCisPtrTy<0>, SDTCisPtrTy<1>]>;

def SDTSPloadgdop :
SDTypeProfile<1, 2, [SDTCisPtrTy<0>, SDTCisPtrTy<1>]>;

def SPcmpicc : SDNode<"SPISD::CMPICC", SDTSPcmpicc, [SDNPOutGlue]>;
def SPcmpfcc : SDNode<"SPISD::CMPFCC", SDTSPcmpfcc, [SDNPOutGlue]>;
def SPcmpfccv9 : SDNode<"SPISD::CMPFCC_V9", SDTSPcmpfcc, [SDNPOutGlue]>;
def SPbricc : SDNode<"SPISD::BRICC", SDTSPbrcc, [SDNPHasChain, SDNPInGlue]>;
def SPbpicc : SDNode<"SPISD::BPICC", SDTSPbrcc, [SDNPHasChain, SDNPInGlue]>;
def SPbpxcc : SDNode<"SPISD::BPXCC", SDTSPbrcc, [SDNPHasChain, SDNPInGlue]>;
def SPbrfcc : SDNode<"SPISD::BRFCC", SDTSPbrcc, [SDNPHasChain, SDNPInGlue]>;
def SPbrfccv9 : SDNode<"SPISD::BRFCC_V9", SDTSPbrcc, [SDNPHasChain, SDNPInGlue]>;

def SPhi    : SDNode<"SPISD::Hi", SDTIntUnaryOp>;
def SPlo    : SDNode<"SPISD::Lo", SDTIntUnaryOp>;

def SPftoi  : SDNode<"SPISD::FTOI", SDTSPFTOI>;
def SPitof  : SDNode<"SPISD::ITOF", SDTSPITOF>;
def SPftox  : SDNode<"SPISD::FTOX", SDTSPFTOX>;
def SPxtof  : SDNode<"SPISD::XTOF", SDTSPXTOF>;

def SPselecticc : SDNode<"SPISD::SELECT_ICC", SDTSPselectcc, [SDNPInGlue]>;
def SPselectxcc : SDNode<"SPISD::SELECT_XCC", SDTSPselectcc, [SDNPInGlue]>;
def SPselectfcc : SDNode<"SPISD::SELECT_FCC", SDTSPselectcc, [SDNPInGlue]>;

//  These are target-independent nodes, but have target-specific formats.
def SDT_SPCallSeqStart : SDCallSeqStart<[ SDTCisVT<0, i32>,
                                          SDTCisVT<1, i32> ]>;
def SDT_SPCallSeqEnd   : SDCallSeqEnd<[ SDTCisVT<0, i32>,
                                        SDTCisVT<1, i32> ]>;

def callseq_start : SDNode<"ISD::CALLSEQ_START", SDT_SPCallSeqStart,
                           [SDNPHasChain, SDNPOutGlue]>;
def callseq_end   : SDNode<"ISD::CALLSEQ_END",   SDT_SPCallSeqEnd,
                           [SDNPHasChain, SDNPOptInGlue, SDNPOutGlue]>;

def SDT_SPCall    : SDTypeProfile<0, -1, [SDTCisVT<0, i32>]>;
def call          : SDNode<"SPISD::CALL", SDT_SPCall,
                           [SDNPHasChain, SDNPOptInGlue, SDNPOutGlue,
                            SDNPVariadic]>;

def tailcall      : SDNode<"SPISD::TAIL_CALL", SDT_SPCall,
                           [SDNPHasChain, SDNPOptInGlue, SDNPOutGlue,
                            SDNPVariadic]>;

def SDT_SPRet     : SDTypeProfile<0, 1, [SDTCisVT<0, i32>]>;
def retflag       : SDNode<"SPISD::RET_FLAG", SDT_SPRet,
                           [SDNPHasChain, SDNPOptInGlue, SDNPVariadic]>;

def flushw        : SDNode<"SPISD::FLUSHW", SDTNone,
                           [SDNPHasChain, SDNPSideEffect, SDNPMayStore]>;

def tlsadd        : SDNode<"SPISD::TLS_ADD", SDTSPtlsadd>;
def tlsld         : SDNode<"SPISD::TLS_LD",  SDTSPtlsld>;
def tlscall       : SDNode<"SPISD::TLS_CALL", SDT_SPCall,
                            [SDNPHasChain, SDNPOptInGlue, SDNPOutGlue,
                             SDNPVariadic]>;

def load_gdop : SDNode<"SPISD::LOAD_GDOP",  SDTSPloadgdop>;

def getPCX        : Operand<iPTR> {
  let PrintMethod = "printGetPCX";
}

//===----------------------------------------------------------------------===//
// E2K Flag Conditions
//===----------------------------------------------------------------------===//

// Note that these values must be kept in sync with the CCOp::CondCode enum
// values.
class ICC_VAL<int N> : PatLeaf<(i32 N)>;
def ICC_NE  : ICC_VAL< 9>;  // Not Equal
def ICC_E   : ICC_VAL< 1>;  // Equal
def ICC_G   : ICC_VAL<10>;  // Greater
def ICC_LE  : ICC_VAL< 2>;  // Less or Equal
def ICC_GE  : ICC_VAL<11>;  // Greater or Equal
def ICC_L   : ICC_VAL< 3>;  // Less
def ICC_GU  : ICC_VAL<12>;  // Greater Unsigned
def ICC_LEU : ICC_VAL< 4>;  // Less or Equal Unsigned
def ICC_CC  : ICC_VAL<13>;  // Carry Clear/Great or Equal Unsigned
def ICC_CS  : ICC_VAL< 5>;  // Carry Set/Less Unsigned
def ICC_POS : ICC_VAL<14>;  // Positive
def ICC_NEG : ICC_VAL< 6>;  // Negative
def ICC_VC  : ICC_VAL<15>;  // Overflow Clear
def ICC_VS  : ICC_VAL< 7>;  // Overflow Set

class FCC_VAL<int N> : PatLeaf<(i32 N)>;
def FCC_U   : FCC_VAL<23>;  // Unordered
def FCC_G   : FCC_VAL<22>;  // Greater
def FCC_UG  : FCC_VAL<21>;  // Unordered or Greater
def FCC_L   : FCC_VAL<20>;  // Less
def FCC_UL  : FCC_VAL<19>;  // Unordered or Less
def FCC_LG  : FCC_VAL<18>;  // Less or Greater
def FCC_NE  : FCC_VAL<17>;  // Not Equal
def FCC_E   : FCC_VAL<25>;  // Equal
def FCC_UE  : FCC_VAL<26>;  // Unordered or Equal
def FCC_GE  : FCC_VAL<27>;  // Greater or Equal
def FCC_UGE : FCC_VAL<28>;  // Unordered or Greater or Equal
def FCC_LE  : FCC_VAL<29>;  // Less or Equal
def FCC_ULE : FCC_VAL<30>;  // Unordered or Less or Equal
def FCC_O   : FCC_VAL<31>;  // Ordered

class CPCC_VAL<int N> : PatLeaf<(i32 N)>;
def CPCC_3   : CPCC_VAL<39>;  // 3
def CPCC_2   : CPCC_VAL<38>;  // 2
def CPCC_23  : CPCC_VAL<37>;  // 2 or 3
def CPCC_1   : CPCC_VAL<36>;  // 1
def CPCC_13  : CPCC_VAL<35>;  // 1 or 3
def CPCC_12  : CPCC_VAL<34>;  // 1 or 2
def CPCC_123 : CPCC_VAL<33>;  // 1 or 2 or 3
def CPCC_0   : CPCC_VAL<41>;  // 0
def CPCC_03  : CPCC_VAL<42>;  // 0 or 3
def CPCC_02  : CPCC_VAL<43>;  // 0 or 2
def CPCC_023 : CPCC_VAL<44>;  // 0 or 2 or 3
def CPCC_01  : CPCC_VAL<45>;  // 0 or 1
def CPCC_013 : CPCC_VAL<46>;  // 0 or 1 or 3
def CPCC_012 : CPCC_VAL<47>;  // 0 or 1 or 2

//===----------------------------------------------------------------------===//
// Instruction Class Templates
//===----------------------------------------------------------------------===//

/// F3_12 multiclass - Define a normal F3_1/F3_2 pattern in one shot.
multiclass F3_12<string OpcStr, bits<6> Op3Val, SDNode OpNode,
                 RegisterClass RC, ValueType Ty, Operand immOp,
                 InstrItinClass itin = IIC_iu_instr> {
  def rr  : F3_1<2, Op3Val,
                 (outs RC:$rd), (ins RC:$rs1, RC:$rs2),
                 !strconcat(OpcStr, " $rs1, $rs2, $rd"),
                 [(set Ty:$rd, (OpNode Ty:$rs1, Ty:$rs2))],
                 itin>;
  def ri  : F3_2<2, Op3Val,
                 (outs RC:$rd), (ins RC:$rs1, immOp:$simm13),
                 !strconcat(OpcStr, " $rs1, $simm13, $rd"),
                 [(set Ty:$rd, (OpNode Ty:$rs1, (Ty simm13:$simm13)))],
                 itin>;
}

/// F3_12np multiclass - Define a normal F3_1/F3_2 pattern in one shot, with no
/// pattern.
multiclass F3_12np<string OpcStr, bits<6> Op3Val, InstrItinClass itin = IIC_iu_instr> {
  def rr  : F3_1<2, Op3Val,
                 (outs IntRegs:$rd), (ins IntRegs:$rs1, IntRegs:$rs2),
                 !strconcat(OpcStr, " $rs1, $rs2, $rd"), [],
                 itin>;
  def ri  : F3_2<2, Op3Val,
                 (outs IntRegs:$rd), (ins IntRegs:$rs1, simm13Op:$simm13),
                 !strconcat(OpcStr, " $rs1, $simm13, $rd"), [],
                 itin>;
}

// Load multiclass - Define both Reg+Reg/Reg+Imm patterns in one shot.
multiclass Load<string OpcStr, bits<6> Op3Val, SDPatternOperator OpNode,
           RegisterClass RC, ValueType Ty, InstrItinClass itin = IIC_iu_instr> {
  def rr  : F3_1<3, Op3Val,
                 (outs RC:$rd), (ins (MEMrr $rs1, $rs2):$addr),
                 !strconcat(OpcStr, " [$addr], $rd"),
                 [(set Ty:$rd, (OpNode ADDRrr:$addr))],
                 itin>;
  def ri  : F3_2<3, Op3Val,
                 (outs RC:$rd), (ins (MEMri $rs1, $simm13):$addr),
                 !strconcat(OpcStr, " [$addr], $rd"),
                 [(set Ty:$rd, (OpNode ADDRri:$addr))],
                 itin>;
}

// TODO: Instructions of the LoadASI class are currently asm only; hooking up
// CodeGen's address spaces to use these is a future task.
class LoadASI<string OpcStr, bits<6> Op3Val, RegisterClass RC> :
  F3_1_asi<3, Op3Val, (outs RC:$rd), (ins (MEMrr $rs1, $rs2):$addr, i8imm:$asi),
                !strconcat(OpcStr, "a [$addr] $asi, $rd"),
                []>;

// LoadA multiclass - As above, but also define alternate address space variant
multiclass LoadA<string OpcStr, bits<6> Op3Val, bits<6> LoadAOp3Val,
                 SDPatternOperator OpNode, RegisterClass RC, ValueType Ty,
                 InstrItinClass itin = NoItinerary> :
             Load<OpcStr, Op3Val, OpNode, RC, Ty, itin> {
  def Arr  : LoadASI<OpcStr, LoadAOp3Val, RC>;
}

// The LDSTUB instruction is supported for asm only.
// It is unlikely that general-purpose code could make use of it.
// CAS is preferred for E2K v9.
def LDSTUBrr : F3_1<3, 0b001101, (outs IntRegs:$rd), (ins (MEMrr $rs1, $rs2):$addr),
                    "ldstub [$addr], $rd", []>;
def LDSTUBri : F3_2<3, 0b001101, (outs IntRegs:$rd), (ins (MEMri $rs1, $simm13):$addr),
                    "ldstub [$addr], $rd", []>;
def LDSTUBArr : F3_1_asi<3, 0b011101, (outs IntRegs:$rd),
                         (ins (MEMrr $rs1, $rs2):$addr, i8imm:$asi),
                         "ldstuba [$addr] $asi, $rd", []>;

// Store multiclass - Define both Reg+Reg/Reg+Imm patterns in one shot.
multiclass Store<string OpcStr, bits<6> Op3Val, SDPatternOperator OpNode,
           RegisterClass RC, ValueType Ty, InstrItinClass itin = IIC_st> {
  def rr  : F3_1<3, Op3Val,
                 (outs), (ins (MEMrr $rs1, $rs2):$addr, RC:$rd),
                 !strconcat(OpcStr, " $rd, [$addr]"),
                 [(OpNode Ty:$rd, ADDRrr:$addr)],
                 itin>;
  def ri  : F3_2<3, Op3Val,
                 (outs), (ins (MEMri $rs1, $simm13):$addr, RC:$rd),
                 !strconcat(OpcStr, " $rd, [$addr]"),
                 [(OpNode Ty:$rd, ADDRri:$addr)],
                 itin>;
}

// TODO: Instructions of the StoreASI class are currently asm only; hooking up
// CodeGen's address spaces to use these is a future task.
class StoreASI<string OpcStr, bits<6> Op3Val, RegisterClass RC,
               InstrItinClass itin = IIC_st> :
  F3_1_asi<3, Op3Val, (outs), (ins (MEMrr $rs1, $rs2):$addr, RC:$rd, i8imm:$asi),
           !strconcat(OpcStr, "a $rd, [$addr] $asi"),
           [],
           itin>;

multiclass StoreA<string OpcStr, bits<6> Op3Val, bits<6> StoreAOp3Val,
                  SDPatternOperator OpNode, RegisterClass RC, ValueType Ty> :
             Store<OpcStr, Op3Val, OpNode, RC, Ty> {
  def Arr : StoreASI<OpcStr, StoreAOp3Val, RC>;
}

//===----------------------------------------------------------------------===//
// Instructions
//===----------------------------------------------------------------------===//

// Pseudo instructions.
class Pseudo<dag outs, dag ins, string asmstr, list<dag> pattern>
   : InstSP<outs, ins, asmstr, pattern> {
  let isCodeGenOnly = 1;
  let isPseudo = 1;
}

// GETPCX for PIC
let Defs = [O7] in {
  def GETPCX : Pseudo<(outs getPCX:$getpcseq), (ins), "$getpcseq", [] >;
}

let Defs = [O6], Uses = [O6] in {
def ADJCALLSTACKDOWN : Pseudo<(outs), (ins i32imm:$amt1, i32imm:$amt2),
                               "!ADJCALLSTACKDOWN $amt1, $amt2",
                               [(callseq_start timm:$amt1, timm:$amt2)]>;
def ADJCALLSTACKUP : Pseudo<(outs), (ins i32imm:$amt1, i32imm:$amt2),
                            "!ADJCALLSTACKUP $amt1",
                            [(callseq_end timm:$amt1, timm:$amt2)]>;
}

let hasSideEffects = 1, mayStore = 1 in {
  let rd = 0, rs1 = 0, rs2 = 0 in
    def FLUSHW : F3_1<0b10, 0b101011, (outs), (ins),
                      "flushw",
                      [(flushw)]>;
  let rd = 8, rs1 = 0, simm13 = 3 in
    def TA3 : F3_2<0b10, 0b111010, (outs), (ins),
                   "ta 3",
                   [(flushw)]>;
}

// SELECT_CC_* - Used to implement the SELECT_CC DAG operation.  Expanded after
// instruction selection into a branch sequence.  This has to handle all
// permutations of selection between i32/f32/f64 on ICC and FCC.
// Expanded after instruction selection.
let Uses = [ICC], usesCustomInserter = 1 in {
  def SELECT_CC_Int_ICC
   : Pseudo<(outs IntRegs:$dst), (ins IntRegs:$T, IntRegs:$F, i32imm:$Cond),
            "; SELECT_CC_Int_ICC PSEUDO!",
            [(set i32:$dst, (SPselecticc i32:$T, i32:$F, imm:$Cond))]>;
  def SELECT_CC_FP_ICC
   : Pseudo<(outs FPRegs:$dst), (ins FPRegs:$T, FPRegs:$F, i32imm:$Cond),
            "; SELECT_CC_FP_ICC PSEUDO!",
            [(set f32:$dst, (SPselecticc f32:$T, f32:$F, imm:$Cond))]>;

  def SELECT_CC_DFP_ICC
   : Pseudo<(outs DFPRegs:$dst), (ins DFPRegs:$T, DFPRegs:$F, i32imm:$Cond),
            "; SELECT_CC_DFP_ICC PSEUDO!",
            [(set f64:$dst, (SPselecticc f64:$T, f64:$F, imm:$Cond))]>;

  def SELECT_CC_QFP_ICC
   : Pseudo<(outs QFPRegs:$dst), (ins QFPRegs:$T, QFPRegs:$F, i32imm:$Cond),
            "; SELECT_CC_QFP_ICC PSEUDO!",
            [(set f128:$dst, (SPselecticc f128:$T, f128:$F, imm:$Cond))]>;
}

let Uses = [ICC], usesCustomInserter = 1 in {
  def SELECT_CC_Int_XCC
   : Pseudo<(outs IntRegs:$dst), (ins IntRegs:$T, IntRegs:$F, i32imm:$Cond),
            "; SELECT_CC_Int_XCC PSEUDO!",
            [(set i32:$dst, (SPselectxcc i32:$T, i32:$F, imm:$Cond))]>;
  def SELECT_CC_FP_XCC
   : Pseudo<(outs FPRegs:$dst), (ins FPRegs:$T, FPRegs:$F, i32imm:$Cond),
            "; SELECT_CC_FP_XCC PSEUDO!",
            [(set f32:$dst, (SPselectxcc f32:$T, f32:$F, imm:$Cond))]>;

  def SELECT_CC_DFP_XCC
   : Pseudo<(outs DFPRegs:$dst), (ins DFPRegs:$T, DFPRegs:$F, i32imm:$Cond),
            "; SELECT_CC_DFP_XCC PSEUDO!",
            [(set f64:$dst, (SPselectxcc f64:$T, f64:$F, imm:$Cond))]>;

  def SELECT_CC_QFP_XCC
   : Pseudo<(outs QFPRegs:$dst), (ins QFPRegs:$T, QFPRegs:$F, i32imm:$Cond),
            "; SELECT_CC_QFP_XCC PSEUDO!",
            [(set f128:$dst, (SPselectxcc f128:$T, f128:$F, imm:$Cond))]>;
}

let usesCustomInserter = 1, Uses = [FCC0] in {

  def SELECT_CC_Int_FCC
   : Pseudo<(outs IntRegs:$dst), (ins IntRegs:$T, IntRegs:$F, i32imm:$Cond),
            "; SELECT_CC_Int_FCC PSEUDO!",
            [(set i32:$dst, (SPselectfcc i32:$T, i32:$F, imm:$Cond))]>;

  def SELECT_CC_FP_FCC
   : Pseudo<(outs FPRegs:$dst), (ins FPRegs:$T, FPRegs:$F, i32imm:$Cond),
            "; SELECT_CC_FP_FCC PSEUDO!",
            [(set f32:$dst, (SPselectfcc f32:$T, f32:$F, imm:$Cond))]>;
  def SELECT_CC_DFP_FCC
   : Pseudo<(outs DFPRegs:$dst), (ins DFPRegs:$T, DFPRegs:$F, i32imm:$Cond),
            "; SELECT_CC_DFP_FCC PSEUDO!",
            [(set f64:$dst, (SPselectfcc f64:$T, f64:$F, imm:$Cond))]>;
  def SELECT_CC_QFP_FCC
   : Pseudo<(outs QFPRegs:$dst), (ins QFPRegs:$T, QFPRegs:$F, i32imm:$Cond),
            "; SELECT_CC_QFP_FCC PSEUDO!",
            [(set f128:$dst, (SPselectfcc f128:$T, f128:$F, imm:$Cond))]>;
}

// Section B.1 - Load Integer Instructions, p. 90
let DecoderMethod = "DecodeLoadInt" in {
  defm LDSB : LoadA<"ldsb", 0b001001, 0b011001, sextloadi8,  IntRegs, i32>;
  defm LDSH : LoadA<"ldsh", 0b001010, 0b011010, sextloadi16, IntRegs, i32>;
  defm LDUB : LoadA<"ldub", 0b000001, 0b010001, zextloadi8,  IntRegs, i32>;
  defm LDUH : LoadA<"lduh", 0b000010, 0b010010, zextloadi16, IntRegs, i32>;
  defm LD   : LoadA<"ld",   0b000000, 0b010000, load,        IntRegs, i32>;
}

let DecoderMethod = "DecodeLoadIntPair" in
  defm LDD : LoadA<"ldd", 0b000011, 0b010011, load, IntPair, v2i32, IIC_ldd>;

// Section B.2 - Load Floating-point Instructions, p. 92
let DecoderMethod = "DecodeLoadFP" in {
  defm LDF   : Load<"ld",  0b100000, load,    FPRegs,  f32, IIC_iu_or_fpu_instr>;
  def LDFArr : LoadASI<"ld",  0b110000, FPRegs>;
}
let DecoderMethod = "DecodeLoadDFP" in {
  defm LDDF   : Load<"ldd", 0b100011, load,    DFPRegs, f64, IIC_ldd>;
  def LDDFArr : LoadASI<"ldd", 0b110011, DFPRegs>;
}
let DecoderMethod = "DecodeLoadQFP" in
  defm LDQF  : LoadA<"ldq", 0b100010, 0b110010, load, QFPRegs, f128>;

let DecoderMethod = "DecodeLoadCP" in
  defm LDC   : Load<"ld", 0b110000, load, CoprocRegs, i32>;
let DecoderMethod = "DecodeLoadCPPair" in
  defm LDDC   : Load<"ldd", 0b110011, load, CoprocPair, v2i32, IIC_ldd>;

let DecoderMethod = "DecodeLoadCP", Defs = [CPSR] in {
  let rd = 0 in {
    def LDCSRrr : F3_1<3, 0b110001, (outs), (ins (MEMrr $rs1, $rs2):$addr),
                       "ld [$addr], %csr", []>;
    def LDCSRri : F3_2<3, 0b110001, (outs), (ins (MEMri $rs1, $simm13):$addr),
                       "ld [$addr], %csr", []>;
  }
}

let DecoderMethod = "DecodeLoadFP" in
  let Defs = [FSR] in {
    let rd = 0 in {
      def LDFSRrr : F3_1<3, 0b100001, (outs), (ins (MEMrr $rs1, $rs2):$addr),
                     "ld [$addr], %fsr", [], IIC_iu_or_fpu_instr>;
      def LDFSRri : F3_2<3, 0b100001, (outs), (ins (MEMri $rs1, $simm13):$addr),
                     "ld [$addr], %fsr", [], IIC_iu_or_fpu_instr>;
    }
    let rd = 1 in {
      def LDXFSRrr : F3_1<3, 0b100001, (outs), (ins (MEMrr $rs1, $rs2):$addr),
                     "ldx [$addr], %fsr", []>;
      def LDXFSRri : F3_2<3, 0b100001, (outs), (ins (MEMri $rs1, $simm13):$addr),
                     "ldx [$addr], %fsr", []>;
    }
  }

let mayLoad = 1, isAsmParserOnly = 1 in {
  def GDOP_LDrr : F3_1<3, 0b000000,
                      (outs IntRegs:$rd),
                      (ins (MEMrr $rs1, $rs2):$addr, TailRelocSymGOTLoad:$sym),
                      "ld [$addr], $rd, $sym",
                      [(set i32:$rd,
                          (load_gdop ADDRrr:$addr, tglobaladdr:$sym))]>;
}

// Section B.4 - Store Integer Instructions, p. 95
let DecoderMethod = "DecodeStoreInt" in {
  defm STB   : StoreA<"stb", 0b000101, 0b010101, truncstorei8,  IntRegs, i32>;
  defm STH   : StoreA<"sth", 0b000110, 0b010110, truncstorei16, IntRegs, i32>;
  defm ST    : StoreA<"st",  0b000100, 0b010100, store,         IntRegs, i32>;
}

let DecoderMethod = "DecodeStoreIntPair" in
  defm STD   : StoreA<"std", 0b000111, 0b010111, store, IntPair, v2i32>;

// Section B.5 - Store Floating-point Instructions, p. 97
let DecoderMethod = "DecodeStoreFP" in {
  defm STF   : Store<"st",  0b100100, store,         FPRegs,  f32>;
  def STFArr : StoreASI<"st",  0b110100, FPRegs>;
}
let DecoderMethod = "DecodeStoreDFP" in {
  defm STDF   : Store<"std", 0b100111, store,         DFPRegs, f64, IIC_std>;
  def STDFArr : StoreASI<"std", 0b110111, DFPRegs>;
}
let DecoderMethod = "DecodeStoreQFP" in
  defm STQF  : StoreA<"stq", 0b100110, 0b110110, store, QFPRegs, f128>;

let DecoderMethod = "DecodeStoreCP" in
  defm STC   : Store<"st", 0b110100, store, CoprocRegs, i32>;

let DecoderMethod = "DecodeStoreCPPair" in
  defm STDC   : Store<"std", 0b110111, store, CoprocPair, v2i32, IIC_std>;

let DecoderMethod = "DecodeStoreCP", rd = 0 in {
  let Defs = [CPSR] in {
    def STCSRrr : F3_1<3, 0b110101, (outs (MEMrr $rs1, $rs2):$addr), (ins),
                       "st %csr, [$addr]", [], IIC_st>;
    def STCSRri : F3_2<3, 0b110101, (outs (MEMri $rs1, $simm13):$addr), (ins),
                       "st %csr, [$addr]", [], IIC_st>;
  }
  let Defs = [CPQ] in {
    def STDCQrr : F3_1<3, 0b110110, (outs (MEMrr $rs1, $rs2):$addr), (ins),
                       "std %cq, [$addr]", [], IIC_std>;
    def STDCQri : F3_2<3, 0b110110, (outs (MEMri $rs1, $simm13):$addr), (ins),
                       "std %cq, [$addr]", [], IIC_std>;
  }
}

let DecoderMethod = "DecodeStoreFP" in {
  let rd = 0 in {
    let Defs = [FSR] in {
      def STFSRrr : F3_1<3, 0b100101, (outs (MEMrr $rs1, $rs2):$addr), (ins),
                     "st %fsr, [$addr]", [], IIC_st>;
      def STFSRri : F3_2<3, 0b100101, (outs (MEMri $rs1, $simm13):$addr), (ins),
                     "st %fsr, [$addr]", [], IIC_st>;
    }
    let Defs = [FQ] in {
      def STDFQrr : F3_1<3, 0b100110, (outs (MEMrr $rs1, $rs2):$addr), (ins),
                     "std %fq, [$addr]", [], IIC_std>;
      def STDFQri : F3_2<3, 0b100110, (outs (MEMri $rs1, $simm13):$addr), (ins),
                     "std %fq, [$addr]", [], IIC_std>;
    }
  }
  let rd = 1, Defs = [FSR] in {
    def STXFSRrr : F3_1<3, 0b100101, (outs (MEMrr $rs1, $rs2):$addr), (ins),
                   "stx %fsr, [$addr]", []>;
    def STXFSRri : F3_2<3, 0b100101, (outs (MEMri $rs1, $simm13):$addr), (ins),
                   "stx %fsr, [$addr]", []>;
  }
}

// Section B.8 - SWAP Register with Memory Instruction
// (Atomic swap)
let Constraints = "$val = $rd", DecoderMethod = "DecodeSWAP" in {
  def SWAPrr : F3_1<3, 0b001111,
                 (outs IntRegs:$rd), (ins (MEMrr $rs1, $rs2):$addr, IntRegs:$val),
                 "swap [$addr], $rd",
                 [(set i32:$rd, (atomic_swap_32 ADDRrr:$addr, i32:$val))]>;
  def SWAPri : F3_2<3, 0b001111,
                 (outs IntRegs:$rd), (ins (MEMri $rs1, $simm13):$addr, IntRegs:$val),
                 "swap [$addr], $rd",
                 [(set i32:$rd, (atomic_swap_32 ADDRri:$addr, i32:$val))]>;
  def SWAPArr : F3_1_asi<3, 0b011111,
                 (outs IntRegs:$rd), (ins (MEMrr $rs1, $rs2):$addr, i8imm:$asi, IntRegs:$val),
                 "swapa [$addr] $asi, $rd",
                 [/*FIXME: pattern?*/]>;
}


// Section B.9 - SETHI Instruction, p. 104
def SETHIi: F2_1<0b100,
                 (outs IntRegs:$rd), (ins i32imm:$imm22),
                 "sethi $imm22, $rd",
                 [(set i32:$rd, SETHIimm:$imm22)],
                 IIC_iu_instr>;

// Section B.10 - NOP Instruction, p. 105
// (It's a special case of SETHI)
let rd = 0, imm22 = 0 in
  def NOP : F2_1<0b100, (outs), (ins), "nop", []>;

// Section B.11 - Logical Instructions, p. 106
defm AND    : F3_12<"and", 0b000001, and, IntRegs, i32, simm13Op>;

def ANDNrr  : F3_1<2, 0b000101,
                   (outs IntRegs:$rd), (ins IntRegs:$rs1, IntRegs:$rs2),
                   "andn $rs1, $rs2, $rd",
                   [(set i32:$rd, (and i32:$rs1, (not i32:$rs2)))]>;
def ANDNri  : F3_2<2, 0b000101,
                   (outs IntRegs:$rd), (ins IntRegs:$rs1, simm13Op:$simm13),
                   "andn $rs1, $simm13, $rd", []>;

defm OR     : F3_12<"or", 0b000010, or, IntRegs, i32, simm13Op>;

def ORNrr   : F3_1<2, 0b000110,
                   (outs IntRegs:$rd), (ins IntRegs:$rs1, IntRegs:$rs2),
                   "orn $rs1, $rs2, $rd",
                   [(set i32:$rd, (or i32:$rs1, (not i32:$rs2)))]>;
def ORNri   : F3_2<2, 0b000110,
                   (outs IntRegs:$rd), (ins IntRegs:$rs1, simm13Op:$simm13),
                   "orn $rs1, $simm13, $rd", []>;
defm XOR    : F3_12<"xor", 0b000011, xor, IntRegs, i32, simm13Op>;

def XNORrr  : F3_1<2, 0b000111,
                   (outs IntRegs:$rd), (ins IntRegs:$rs1, IntRegs:$rs2),
                   "xnor $rs1, $rs2, $rd",
                   [(set i32:$rd, (not (xor i32:$rs1, i32:$rs2)))]>;
def XNORri  : F3_2<2, 0b000111,
                   (outs IntRegs:$rd), (ins IntRegs:$rs1, simm13Op:$simm13),
                   "xnor $rs1, $simm13, $rd", []>;

def : Pat<(and IntRegs:$rs1, SETHIimm_not:$rs2),
          (ANDNrr i32:$rs1, (SETHIi SETHIimm_not:$rs2))>;

def : Pat<(or IntRegs:$rs1, SETHIimm_not:$rs2),
          (ORNrr i32:$rs1,  (SETHIi SETHIimm_not:$rs2))>;

let Defs = [ICC] in {
  defm ANDCC  : F3_12np<"andcc",  0b010001>;
  defm ANDNCC : F3_12np<"andncc", 0b010101>;
  defm ORCC   : F3_12np<"orcc",   0b010010>;
  defm ORNCC  : F3_12np<"orncc",  0b010110>;
  defm XORCC  : F3_12np<"xorcc",  0b010011>;
  defm XNORCC : F3_12np<"xnorcc", 0b010111>;
}

// Section B.12 - Shift Instructions, p. 107
defm SLL : F3_S<"sll", 0b100101, 0, shl, i32, shift_imm5, IntRegs>;
defm SRL : F3_S<"srl", 0b100110, 0, srl, i32, shift_imm5, IntRegs>;
defm SRA : F3_S<"sra", 0b100111, 0, sra, i32, shift_imm5, IntRegs>;

// Section B.13 - Add Instructions, p. 108
defm ADD   : F3_12<"add", 0b000000, add, IntRegs, i32, simm13Op>;

// "LEA" forms of add (patterns to make tblgen happy)
let Predicates = [Is32Bit], isCodeGenOnly = 1 in
  def LEA_ADDri   : F3_2<2, 0b000000,
                     (outs IntRegs:$rd), (ins (MEMri $rs1, $simm13):$addr),
                     "add ${addr:arith}, $rd",
                     [(set iPTR:$rd, ADDRri:$addr)]>;

let Defs = [ICC] in
  defm ADDCC  : F3_12<"addcc", 0b010000, addc, IntRegs, i32, simm13Op>;

let Uses = [ICC] in
  defm ADDC   : F3_12np<"addx", 0b001000>;

let Uses = [ICC], Defs = [ICC] in
  defm ADDE  : F3_12<"addxcc", 0b011000, adde, IntRegs, i32, simm13Op>;

// Section B.15 - Subtract Instructions, p. 110
defm SUB    : F3_12  <"sub"  , 0b000100, sub, IntRegs, i32, simm13Op>;
let Uses = [ICC], Defs = [ICC] in
  defm SUBE   : F3_12  <"subxcc" , 0b011100, sube, IntRegs, i32, simm13Op>;

let Defs = [ICC] in
  defm SUBCC  : F3_12  <"subcc", 0b010100, subc, IntRegs, i32, simm13Op>;

let Uses = [ICC] in
  defm SUBC   : F3_12np <"subx", 0b001100>;

// cmp (from Section A.3) is a specialized alias for subcc
let Defs = [ICC], rd = 0 in {
  def CMPrr   : F3_1<2, 0b010100,
                     (outs), (ins IntRegs:$rs1, IntRegs:$rs2),
                     "cmp $rs1, $rs2",
                     [(SPcmpicc i32:$rs1, i32:$rs2)]>;
  def CMPri   : F3_2<2, 0b010100,
                     (outs), (ins IntRegs:$rs1, simm13Op:$simm13),
                     "cmp $rs1, $simm13",
                     [(SPcmpicc i32:$rs1, (i32 simm13:$simm13))]>;
}

// Section B.18 - Multiply Instructions, p. 113
let Defs = [Y] in {
  defm UMUL : F3_12<"umul", 0b001010, umullohi, IntRegs, i32, simm13Op, IIC_iu_umul>;
  defm SMUL : F3_12<"smul", 0b001011, smullohi, IntRegs, i32, simm13Op, IIC_iu_smul>;
}

let Defs = [Y, ICC] in {
  defm UMULCC : F3_12np<"umulcc", 0b011010, IIC_iu_umul>;
  defm SMULCC : F3_12np<"smulcc", 0b011011, IIC_iu_smul>;
}

let Defs = [Y, ICC], Uses = [Y, ICC] in {
  defm MULSCC : F3_12np<"mulscc", 0b100100>;
}

// Section B.19 - Divide Instructions, p. 115
let Uses = [Y], Defs = [Y] in {
  defm UDIV : F3_12np<"udiv", 0b001110, IIC_iu_div>;
  defm SDIV : F3_12np<"sdiv", 0b001111, IIC_iu_div>;
}

let Uses = [Y], Defs = [Y, ICC] in {
  defm UDIVCC : F3_12np<"udivcc", 0b011110, IIC_iu_div>;
  defm SDIVCC : F3_12np<"sdivcc", 0b011111, IIC_iu_div>;
}

// Section B.20 - SAVE and RESTORE, p. 117
defm SAVE    : F3_12np<"save"   , 0b111100>;
defm RESTORE : F3_12np<"restore", 0b111101>;

// Section B.21 - Branch on Integer Condition Codes Instructions, p. 119
// Section A.7 - Branch on Integer Condition Codes with Prediction (E2K v9)

let isBranch = 1, isTerminator = 1, hasDelaySlot = 1, isBarrier = 1 in {
// unconditional branch class.
class BranchAlways<dag ins, string asmstr, list<dag> pattern>
  : F2_2<0b010, 0, (outs), ins, asmstr, pattern>;

// Same as BranchAlways but uses the new v9 encoding
class BranchPredictAlways<dag ins, string asmstr, list<dag> pattern>
  : F2_3<0b001, 0, 1, (outs), ins, asmstr, pattern>;
}

let cond = 8 in {
  // If we're compiling for v9, prefer BPA rather than BA
  // TODO: Disallow BA emission when FeatureV8Deprecated isn't enabled
  let cc = 0b00 in
    def BPA : BranchPredictAlways<(ins bprtarget:$imm19),
      "ba %icc, $imm19", [(br bb:$imm19)]>;

  def BA : BranchAlways<(ins brtarget:$imm22), "ba $imm22", [(br bb:$imm22)]>;
}

let isBranch = 1, isTerminator = 1, hasDelaySlot = 1 in {

// conditional branch class:
class BranchSP<dag ins, string asmstr, list<dag> pattern>
 : F2_2<0b010, 0, (outs), ins, asmstr, pattern, IIC_iu_instr>;

// conditional branch with annul class:
class BranchSPA<dag ins, string asmstr, list<dag> pattern>
 : F2_2<0b010, 1, (outs), ins, asmstr, pattern, IIC_iu_instr>;

// Conditional branch class on %icc|%xcc with predication:
multiclass IPredBranch<string regstr, list<dag> CCPattern> {
  def CC    : F2_3<0b001, 0, 1, (outs), (ins bprtarget:$imm19, CCOp:$cond),
                   !strconcat("b$cond ", !strconcat(regstr, ", $imm19")),
                   CCPattern,
                   IIC_iu_instr>;
  def CCA   : F2_3<0b001, 1, 1, (outs), (ins bprtarget:$imm19, CCOp:$cond),
                   !strconcat("b$cond,a ", !strconcat(regstr, ", $imm19")),
                   [],
                   IIC_iu_instr>;
  def CCNT  : F2_3<0b001, 0, 0, (outs), (ins bprtarget:$imm19, CCOp:$cond),
                   !strconcat("b$cond,pn ", !strconcat(regstr, ", $imm19")),
                   [],
                   IIC_iu_instr>;
  def CCANT : F2_3<0b001, 1, 0, (outs), (ins bprtarget:$imm19, CCOp:$cond),
                   !strconcat("b$cond,a,pn ", !strconcat(regstr, ", $imm19")),
                   [],
                   IIC_iu_instr>;
}

} // let isBranch = 1, isTerminator = 1, hasDelaySlot = 1


// Indirect branch instructions.
let isTerminator = 1, isBarrier = 1,  hasDelaySlot = 1, isBranch =1,
     isIndirectBranch = 1, rd = 0, isCodeGenOnly = 1 in {
  def BINDrr  : F3_1<2, 0b111000,
                   (outs), (ins (MEMrr $rs1, $rs2):$addr),
                   "jmp $addr",
                   [(brind ADDRrr:$addr)]>;
  def BINDri  : F3_2<2, 0b111000,
                   (outs), (ins (MEMri $rs1, $simm13):$addr),
                   "jmp $addr",
                   [(brind ADDRri:$addr)]>;
}

let Uses = [ICC] in {
  def BCOND : BranchSP<(ins brtarget:$imm22, CCOp:$cond),
                         "b$cond $imm22",
                        [(SPbricc bb:$imm22, imm:$cond)]>;
  def BCONDA : BranchSPA<(ins brtarget:$imm22, CCOp:$cond),
                         "b$cond,a $imm22", []>;

  let cc = 0b00 in
    defm BPI : IPredBranch<"%icc", [(SPbpicc bb:$imm19, imm:$cond)]>;
}

// Section B.22 - Branch on Floating-point Condition Codes Instructions, p. 121

let isBranch = 1, isTerminator = 1, hasDelaySlot = 1 in {

// floating-point conditional branch class:
class FPBranchSP<dag ins, string asmstr, list<dag> pattern>
 : F2_2<0b110, 0, (outs), ins, asmstr, pattern, IIC_fpu_normal_instr>;

// floating-point conditional branch with annul class:
class FPBranchSPA<dag ins, string asmstr, list<dag> pattern>
 : F2_2<0b110, 1, (outs), ins, asmstr, pattern, IIC_fpu_normal_instr>;

// Conditional branch class on %fcc0-%fcc3 with predication:
multiclass FPredBranch {
  def CC    : F2_3<0b101, 0, 1, (outs), (ins bprtarget:$imm19, CCOp:$cond,
                                         FCCRegs:$cc),
                  "fb$cond $cc, $imm19", [], IIC_fpu_normal_instr>;
  def CCA   : F2_3<0b101, 1, 1, (outs), (ins bprtarget:$imm19, CCOp:$cond,
                                         FCCRegs:$cc),
                  "fb$cond,a $cc, $imm19", [], IIC_fpu_normal_instr>;
  def CCNT  : F2_3<0b101, 0, 0, (outs), (ins bprtarget:$imm19, CCOp:$cond,
                                         FCCRegs:$cc),
                  "fb$cond,pn $cc, $imm19", [], IIC_fpu_normal_instr>;
  def CCANT : F2_3<0b101, 1, 0, (outs), (ins bprtarget:$imm19, CCOp:$cond,
                                         FCCRegs:$cc),
                  "fb$cond,a,pn $cc, $imm19", [], IIC_fpu_normal_instr>;
}
} // let isBranch = 1, isTerminator = 1, hasDelaySlot = 1

let Uses = [FCC0] in {
  def FBCOND  : FPBranchSP<(ins brtarget:$imm22, CCOp:$cond),
                              "fb$cond $imm22",
                              [(SPbrfcc bb:$imm22, imm:$cond)]>;
  def FBCONDA : FPBranchSPA<(ins brtarget:$imm22, CCOp:$cond),
                             "fb$cond,a $imm22", []>;
}

// Variants of FBCOND that uses V9 opcode
let Uses = [FCC0], cc = 0,
    isBranch = 1, isTerminator = 1, hasDelaySlot = 1 in {
  def FBCOND_V9  : F2_3<0b101, 0, 1, (outs),
                    (ins bprtarget:$imm19, CCOp:$cond),
                    "fb$cond %fcc0, $imm19",
                    [(SPbrfccv9 bb:$imm19, imm:$cond)], IIC_fpu_normal_instr>;
  def FBCONDA_V9 : F2_3<0b101, 1, 1, (outs),
                    (ins bprtarget:$imm19, CCOp:$cond),
                    "fb$cond,a %fcc0, $imm19",
                    [(SPbrfccv9 bb:$imm19, imm:$cond)], IIC_fpu_normal_instr>;
}

defm BPF : FPredBranch;

// Section B.22 - Branch on Co-processor Condition Codes Instructions, p. 123
let isBranch = 1, isTerminator = 1, hasDelaySlot = 1 in {

// co-processor conditional branch class:
class CPBranchSP<dag ins, string asmstr, list<dag> pattern>
 : F2_2<0b111, 0, (outs), ins, asmstr, pattern>;

// co-processor conditional branch with annul class:
class CPBranchSPA<dag ins, string asmstr, list<dag> pattern>
 : F2_2<0b111, 1, (outs), ins, asmstr, pattern>;

} // let isBranch = 1, isTerminator = 1, hasDelaySlot = 1

def CBCOND  : CPBranchSP<(ins brtarget:$imm22, CCOp:$cond),
                          "cb$cond $imm22",
                          [(SPbrfcc bb:$imm22, imm:$cond)]>;
def CBCONDA : CPBranchSPA<(ins brtarget:$imm22, CCOp:$cond),
                           "cb$cond,a $imm22", []>;

// Section B.24 - Call and Link Instruction, p. 125
// This is the only Format 1 instruction
let Uses = [O6],
    hasDelaySlot = 1, isCall = 1 in {
    /*
  def CALL : InstSP<(outs), (ins calltarget:$disp, variable_ops),
                    "call $disp",
                    [],
                    IIC_jmp_or_call> {
    bits<30> disp;
    let op = 1;
    let Inst{29-0} = disp;
  }
  */

  // indirect calls: special cases of JMPL.
  let isCodeGenOnly = 1, rd = 15 in {
    def CALLrr : F3_1<2, 0b111000,
                      (outs), (ins (MEMrr $rs1, $rs2):$addr, variable_ops),
                      "call $addr",
                      [(call ADDRrr:$addr)],
                      IIC_jmp_or_call>;
    def CALLri : F3_2<2, 0b111000,
                      (outs), (ins (MEMri $rs1, $simm13):$addr, variable_ops),
                      "call $addr",
                      [(call ADDRri:$addr)],
                      IIC_jmp_or_call>;
  }
}

// Section B.25 - Jump and Link Instruction

// JMPL Instruction.
let isTerminator = 1, hasDelaySlot = 1, isBarrier = 1,
    DecoderMethod = "DecodeJMPL" in {
  def JMPLrr: F3_1<2, 0b111000,
                   (outs IntRegs:$rd), (ins (MEMrr $rs1, $rs2):$addr),
                   "jmpl $addr, $rd",
                   [],
                   IIC_jmp_or_call>;
  def JMPLri: F3_2<2, 0b111000,
                   (outs IntRegs:$rd), (ins (MEMri $rs1, $simm13):$addr),
                   "jmpl $addr, $rd",
                   [],
                   IIC_jmp_or_call>;
}

// Section A.3 - Synthetic Instructions, p. 85
// special cases of JMPL:
let isReturn = 1, isTerminator = 1, hasDelaySlot = 1, isBarrier = 1,
    isCodeGenOnly = 1 in {
  let rd = 0, rs1 = 15 in
    def RETL: F3_2<2, 0b111000,
                   (outs), (ins i32imm:$simm13),
                   "jmp %o7+$simm13",
                   [(retflag simm13:$simm13)],
                   IIC_jmp_or_call>;

  let rd = 0, rs1 = 31 in
    def RET: F3_2<2, 0b111000,
                  (outs), (ins i32imm:$simm13),
                  "jmp %i7+$simm13",
                  [],
                  IIC_jmp_or_call>;
}

// Section B.26 - Return from Trap Instruction
let isReturn = 1, isTerminator = 1, hasDelaySlot = 1,
     isBarrier = 1, rd = 0, DecoderMethod = "DecodeReturn" in {
  def RETTrr : F3_1<2, 0b111001,
                   (outs), (ins (MEMrr $rs1, $rs2):$addr),
                   "rett $addr",
                   [],
                   IIC_jmp_or_call>;
  def RETTri : F3_2<2, 0b111001,
                    (outs), (ins (MEMri $rs1, $simm13):$addr),
                    "rett $addr",
                    [],
                    IIC_jmp_or_call>;
}


// Section B.27 - Trap on Integer Condition Codes Instruction
// conditional branch class:
let DecoderNamespace = "E2KV8", DecoderMethod = "DecodeTRAP", hasSideEffects = 1, Uses = [ICC], cc = 0b00 in
{
  def TRAPrr : TRAPSPrr<0b111010,
                        (outs), (ins IntRegs:$rs1, IntRegs:$rs2, CCOp:$cond),
                        "t$cond $rs1 + $rs2",
                        []>;
  def TRAPri : TRAPSPri<0b111010,
                        (outs), (ins IntRegs:$rs1, i32imm:$imm, CCOp:$cond),
                        "t$cond $rs1 + $imm",
                        []>;
}

multiclass TRAP<string regStr> {
  def rr : TRAPSPrr<0b111010,
                    (outs), (ins IntRegs:$rs1, IntRegs:$rs2, CCOp:$cond),
                    !strconcat(!strconcat("t$cond ", regStr), ", $rs1 + $rs2"),
                    []>;
  def ri : TRAPSPri<0b111010,
                    (outs), (ins IntRegs:$rs1, i32imm:$imm, CCOp:$cond),
                    !strconcat(!strconcat("t$cond ", regStr), ", $rs1 + $imm"),
                    []>;
}

let DecoderNamespace = "E2KV9", DecoderMethod = "DecodeTRAP", hasSideEffects = 1, Uses = [ICC], cc = 0b00 in
  defm TICC : TRAP<"%icc">;


let isBarrier = 1, isTerminator = 1, rd = 0b01000, rs1 = 0, simm13 = 5 in
  def TA5 : F3_2<0b10, 0b111010, (outs), (ins), "ta 5", [(trap)]>;

let hasSideEffects = 1, rd = 0b01000, rs1 = 0, simm13 = 1 in
  def TA1 : F3_2<0b10, 0b111010, (outs), (ins), "ta 1", [(debugtrap)]>;

// Section B.28 - Read State Register Instructions
let rs2 = 0 in
  def RDASR : F3_1<2, 0b101000,
                 (outs IntRegs:$rd), (ins ASRRegs:$rs1),
                 "rd $rs1, $rd", []>;

// PSR, WIM, and TBR don't exist on the E2KV9, only the V8.

let rs2 = 0, rs1 = 0, Uses=[PSR] in
def RDPSR : F3_1<2, 0b101001,
         (outs IntRegs:$rd), (ins),
         "rd %psr, $rd", []>;

let rs2 = 0, rs1 = 0, Uses=[WIM] in
def RDWIM : F3_1<2, 0b101010,
         (outs IntRegs:$rd), (ins),
         "rd %wim, $rd", []>;

let rs2 = 0, rs1 = 0, Uses=[TBR] in
def RDTBR : F3_1<2, 0b101011,
         (outs IntRegs:$rd), (ins),
         "rd %tbr, $rd", []>;


// PC don't exist on the E2KV8, only the V9.

let rs2 = 0, rs1 = 5 in
def RDPC : F3_1<2, 0b101000,
         (outs IntRegs:$rd), (ins),
         "rd %pc, $rd", []>;


// Section B.29 - Write State Register Instructions
def WRASRrr : F3_1<2, 0b110000,
                 (outs ASRRegs:$rd), (ins IntRegs:$rs1, IntRegs:$rs2),
                 "wr $rs1, $rs2, $rd", []>;
def WRASRri : F3_2<2, 0b110000,
                 (outs ASRRegs:$rd), (ins IntRegs:$rs1, simm13Op:$simm13),
                 "wr $rs1, $simm13, $rd", []>;

// PSR, WIM, and TBR don't exist on the E2KV9, only the V8.

let Defs = [PSR], rd=0 in {
def WRPSRrr : F3_1<2, 0b110001,
         (outs), (ins IntRegs:$rs1, IntRegs:$rs2),
         "wr $rs1, $rs2, %psr", []>;
def WRPSRri : F3_2<2, 0b110001,
         (outs), (ins IntRegs:$rs1, simm13Op:$simm13),
         "wr $rs1, $simm13, %psr", []>;
}

let Defs = [WIM], rd=0 in {
def WRWIMrr : F3_1<2, 0b110010,
         (outs), (ins IntRegs:$rs1, IntRegs:$rs2),
         "wr $rs1, $rs2, %wim", []>;
def WRWIMri : F3_2<2, 0b110010,
         (outs), (ins IntRegs:$rs1, simm13Op:$simm13),
         "wr $rs1, $simm13, %wim", []>;
}

let Defs = [TBR], rd=0 in {
def WRTBRrr : F3_1<2, 0b110011,
         (outs), (ins IntRegs:$rs1, IntRegs:$rs2),
         "wr $rs1, $rs2, %tbr", []>;
def WRTBRri : F3_2<2, 0b110011,
         (outs), (ins IntRegs:$rs1, simm13Op:$simm13),
         "wr $rs1, $simm13, %tbr", []>;
}


// Section B.30 - STBAR Instruction
let hasSideEffects = 1, rd = 0, rs1 = 0b01111, rs2 = 0 in
  def STBAR : F3_1<2, 0b101000, (outs), (ins), "stbar", []>;


// Section B.31 - Unimplemented Instruction
let rd = 0 in
  def UNIMP : F2_1<0b000, (outs), (ins i32imm:$imm22),
                  "unimp $imm22", []>;

// Section B.32 - Flush Instruction Memory
let rd = 0 in {
  def FLUSHrr : F3_1<2, 0b111011, (outs), (ins (MEMrr $rs1, $rs2):$addr),
                       "flush $addr", []>;
  def FLUSHri : F3_2<2, 0b111011, (outs), (ins (MEMri $rs1, $simm13):$addr),
                       "flush $addr", []>;

  // The no-arg FLUSH is only here for the benefit of the InstAlias
  // "flush", which cannot seem to use FLUSHrr, due to the inability
  // to construct a MEMrr with fixed G0 registers.
  let rs1 = 0, rs2 = 0 in
    def FLUSH   : F3_1<2, 0b111011, (outs), (ins), "flush %g0", []>;
}

// Section B.33 - Floating-point Operate (FPop) Instructions

// Convert Integer to Floating-point Instructions, p. 141
def FITOS : F3_3u<2, 0b110100, 0b011000100,
                 (outs FPRegs:$rd), (ins FPRegs:$rs2),
                 "fitos $rs2, $rd",
                 [(set FPRegs:$rd, (SPitof FPRegs:$rs2))],
                 IIC_fpu_fast_instr>;
def FITOD : F3_3u<2, 0b110100, 0b011001000,
                 (outs DFPRegs:$rd), (ins FPRegs:$rs2),
                 "fitod $rs2, $rd",
                 [(set DFPRegs:$rd, (SPitof FPRegs:$rs2))],
                 IIC_fpu_fast_instr>;
def FITOQ : F3_3u<2, 0b110100, 0b011001100,
                 (outs QFPRegs:$rd), (ins FPRegs:$rs2),
                 "fitoq $rs2, $rd",
                 [(set QFPRegs:$rd, (SPitof FPRegs:$rs2))]>;

// Convert Floating-point to Integer Instructions, p. 142
def FSTOI : F3_3u<2, 0b110100, 0b011010001,
                 (outs FPRegs:$rd), (ins FPRegs:$rs2),
                 "fstoi $rs2, $rd",
                 [(set FPRegs:$rd, (SPftoi FPRegs:$rs2))],
                 IIC_fpu_fast_instr>;
def FDTOI : F3_3u<2, 0b110100, 0b011010010,
                 (outs FPRegs:$rd), (ins DFPRegs:$rs2),
                 "fdtoi $rs2, $rd",
                 [(set FPRegs:$rd, (SPftoi DFPRegs:$rs2))],
                 IIC_fpu_fast_instr>;
def FQTOI : F3_3u<2, 0b110100, 0b011010011,
                 (outs FPRegs:$rd), (ins QFPRegs:$rs2),
                 "fqtoi $rs2, $rd",
                 [(set FPRegs:$rd, (SPftoi QFPRegs:$rs2))]>;

// Convert between Floating-point Formats Instructions, p. 143
def FSTOD : F3_3u<2, 0b110100, 0b011001001,
                 (outs DFPRegs:$rd), (ins FPRegs:$rs2),
                 "fstod $rs2, $rd",
                 [(set f64:$rd, (fpextend f32:$rs2))],
                 IIC_fpu_stod>;
def FSTOQ : F3_3u<2, 0b110100, 0b011001101,
                 (outs QFPRegs:$rd), (ins FPRegs:$rs2),
                 "fstoq $rs2, $rd",
                 [(set f128:$rd, (fpextend f32:$rs2))]>;
def FDTOS : F3_3u<2, 0b110100, 0b011000110,
                 (outs FPRegs:$rd), (ins DFPRegs:$rs2),
                 "fdtos $rs2, $rd",
                 [(set f32:$rd, (fpround f64:$rs2))],
                 IIC_fpu_fast_instr>;
def FDTOQ : F3_3u<2, 0b110100, 0b011001110,
                 (outs QFPRegs:$rd), (ins DFPRegs:$rs2),
                 "fdtoq $rs2, $rd",
                 [(set f128:$rd, (fpextend f64:$rs2))]>;
def FQTOS : F3_3u<2, 0b110100, 0b011000111,
                 (outs FPRegs:$rd), (ins QFPRegs:$rs2),
                 "fqtos $rs2, $rd",
                 [(set f32:$rd, (fpround f128:$rs2))]>;
def FQTOD : F3_3u<2, 0b110100, 0b011001011,
                 (outs DFPRegs:$rd), (ins QFPRegs:$rs2),
                 "fqtod $rs2, $rd",
                 [(set f64:$rd, (fpround f128:$rs2))]>;

// Floating-point Move Instructions, p. 144
def FMOVS : F3_3u<2, 0b110100, 0b000000001,
                 (outs FPRegs:$rd), (ins FPRegs:$rs2),
                 "fmovs $rs2, $rd", []>;
def FNEGS : F3_3u<2, 0b110100, 0b000000101,
                 (outs FPRegs:$rd), (ins FPRegs:$rs2),
                 "fnegs $rs2, $rd",
                 [(set f32:$rd, (fneg f32:$rs2))],
                 IIC_fpu_negs>;
def FABSS : F3_3u<2, 0b110100, 0b000001001,
                 (outs FPRegs:$rd), (ins FPRegs:$rs2),
                 "fabss $rs2, $rd",
                 [(set f32:$rd, (fabs f32:$rs2))],
                 IIC_fpu_abs>;




/*
// Floating-point Add and Subtract Instructions, p. 146
def FADDS  : F3_3<2, 0b110100, 0b001000001,
                  (outs FPRegs:$rd), (ins FPRegs:$rs1, FPRegs:$rs2),
                  "fadds $rs1, $rs2, $rd",
                  [(set f32:$rd, (fadd f32:$rs1, f32:$rs2))],
                  IIC_fpu_fast_instr>;
def FADDD  : F3_3<2, 0b110100, 0b001000010,
                  (outs DFPRegs:$rd), (ins DFPRegs:$rs1, DFPRegs:$rs2),
                  "faddd $rs1, $rs2, $rd",
                  [(set f64:$rd, (fadd f64:$rs1, f64:$rs2))],
                  IIC_fpu_fast_instr>;
def FADDQ  : F3_3<2, 0b110100, 0b001000011,
                  (outs QFPRegs:$rd), (ins QFPRegs:$rs1, QFPRegs:$rs2),
                  "faddq $rs1, $rs2, $rd",
                  [(set f128:$rd, (fadd f128:$rs1, f128:$rs2))]>;

def FSUBS  : F3_3<2, 0b110100, 0b001000101,
                  (outs FPRegs:$rd), (ins FPRegs:$rs1, FPRegs:$rs2),
                  "fsubs $rs1, $rs2, $rd",
                  [(set f32:$rd, (fsub f32:$rs1, f32:$rs2))],
                  IIC_fpu_fast_instr>;
def FSUBD  : F3_3<2, 0b110100, 0b001000110,
                  (outs DFPRegs:$rd), (ins DFPRegs:$rs1, DFPRegs:$rs2),
                  "fsubd $rs1, $rs2, $rd",
                  [(set f64:$rd, (fsub f64:$rs1, f64:$rs2))],
                  IIC_fpu_fast_instr>;
def FSUBQ  : F3_3<2, 0b110100, 0b001000111,
                  (outs QFPRegs:$rd), (ins QFPRegs:$rs1, QFPRegs:$rs2),
                  "fsubq $rs1, $rs2, $rd",
                  [(set f128:$rd, (fsub f128:$rs1, f128:$rs2))]>;


// Floating-point Multiply and Divide Instructions, p. 147
def FMULS  : F3_3<2, 0b110100, 0b001001001,
                  (outs FPRegs:$rd), (ins FPRegs:$rs1, FPRegs:$rs2),
                  "fmuls $rs1, $rs2, $rd",
                  [(set f32:$rd, (fmul f32:$rs1, f32:$rs2))],
                  IIC_fpu_muls>;
def FMULD  : F3_3<2, 0b110100, 0b001001010,
                  (outs DFPRegs:$rd), (ins DFPRegs:$rs1, DFPRegs:$rs2),
                  "fmuld $rs1, $rs2, $rd",
                  [(set f64:$rd, (fmul f64:$rs1, f64:$rs2))],
                  IIC_fpu_muld>;
def FMULQ  : F3_3<2, 0b110100, 0b001001011,
                  (outs QFPRegs:$rd), (ins QFPRegs:$rs1, QFPRegs:$rs2),
                  "fmulq $rs1, $rs2, $rd",
                  [(set f128:$rd, (fmul f128:$rs1, f128:$rs2))]>;

def FSMULD : F3_3<2, 0b110100, 0b001101001,
                  (outs DFPRegs:$rd), (ins FPRegs:$rs1, FPRegs:$rs2),
                  "fsmuld $rs1, $rs2, $rd",
                  [(set f64:$rd, (fmul (fpextend f32:$rs1),
                                        (fpextend f32:$rs2)))],
                  IIC_fpu_muld>;
def FDMULQ : F3_3<2, 0b110100, 0b001101110,
                  (outs QFPRegs:$rd), (ins DFPRegs:$rs1, DFPRegs:$rs2),
                  "fdmulq $rs1, $rs2, $rd",
                  [(set f128:$rd, (fmul (fpextend f64:$rs1),
                                         (fpextend f64:$rs2)))]>;

// FDIVS generates an erratum on processors, so by disabling this instruction
// this will be promoted to use FDIVD with doubles instead.
def FDIVS  : F3_3<2, 0b110100, 0b001001101,
                 (outs FPRegs:$rd), (ins FPRegs:$rs1, FPRegs:$rs2),
                 "fdivs $rs1, $rs2, $rd",
                 [(set f32:$rd, (fdiv f32:$rs1, f32:$rs2))],
                 IIC_fpu_divs>;
def FDIVD  : F3_3<2, 0b110100, 0b001001110,
                 (outs DFPRegs:$rd), (ins DFPRegs:$rs1, DFPRegs:$rs2),
                 "fdivd $rs1, $rs2, $rd",
                 [(set f64:$rd, (fdiv f64:$rs1, f64:$rs2))],
                 IIC_fpu_divd>;
def FDIVQ  : F3_3<2, 0b110100, 0b001001111,
                 (outs QFPRegs:$rd), (ins QFPRegs:$rs1, QFPRegs:$rs2),
                 "fdivq $rs1, $rs2, $rd",
                 [(set f128:$rd, (fdiv f128:$rs1, f128:$rs2))]>;
*/
// Floating-point Compare Instructions, p. 148
// Note: the 2nd template arg is different for these guys.
// Note 2: the result of a FCMP is not available until the 2nd cycle
// after the instr is retired, but there is no interlock in E2K V8.
// This behavior is modeled with a forced noop after the instruction in
// DelaySlotFiller.

let Defs = [FCC0], rd = 0, isCodeGenOnly = 1 in {
  def FCMPS  : F3_3c<2, 0b110101, 0b001010001,
                   (outs), (ins FPRegs:$rs1, FPRegs:$rs2),
                   "fcmps $rs1, $rs2",
                   [(SPcmpfcc f32:$rs1, f32:$rs2)],
                   IIC_fpu_fast_instr>;
  def FCMPD  : F3_3c<2, 0b110101, 0b001010010,
                   (outs), (ins DFPRegs:$rs1, DFPRegs:$rs2),
                   "fcmpd $rs1, $rs2",
                   [(SPcmpfcc f64:$rs1, f64:$rs2)],
                   IIC_fpu_fast_instr>;
  def FCMPQ  : F3_3c<2, 0b110101, 0b001010011,
                   (outs), (ins QFPRegs:$rs1, QFPRegs:$rs2),
                   "fcmpq $rs1, $rs2",
                   [(SPcmpfcc f128:$rs1, f128:$rs2)]>;
}

// A.13 Floating-Point Compare (E2K v9)
// Note that these always write to %fcc0 instead of having its destination
// allocated automatically.
// This avoids complications with the scheduler sometimes wanting to spill
// the contents of an FCC, since E2K v9 doesn't have facilities to spill
// an individual FCC.

let Defs = [FCC0], rd = 0, isCodeGenOnly = 1 in {
  def FCMPS_V9  : F3_3c<2, 0b110101, 0b001010001,
                   (outs), (ins FPRegs:$rs1, FPRegs:$rs2),
                   "fcmps %fcc0, $rs1, $rs2",
                   [(SPcmpfccv9 f32:$rs1, f32:$rs2)],
                   IIC_fpu_fast_instr>;
  def FCMPD_V9  : F3_3c<2, 0b110101, 0b001010010,
                   (outs), (ins DFPRegs:$rs1, DFPRegs:$rs2),
                   "fcmpd %fcc0, $rs1, $rs2",
                   [(SPcmpfccv9 f64:$rs1, f64:$rs2)],
                   IIC_fpu_fast_instr>;
  def FCMPQ_V9  : F3_3c<2, 0b110101, 0b001010011,
                   (outs), (ins QFPRegs:$rs1, QFPRegs:$rs2),
                   "fcmpq %fcc0, $rs1, $rs2",
                   [(SPcmpfccv9 f128:$rs1, f128:$rs2)]>;
}

//===----------------------------------------------------------------------===//
// Instructions for Thread Local Storage(TLS).
//===----------------------------------------------------------------------===//
let isAsmParserOnly = 1 in {
def TLS_ADDrr : F3_1<2, 0b000000,
                    (outs IntRegs:$rd),
                    (ins IntRegs:$rs1, IntRegs:$rs2, TailRelocSymTLSAdd:$sym),
                    "add $rs1, $rs2, $rd, $sym",
                    [(set i32:$rd,
                        (tlsadd i32:$rs1, i32:$rs2, tglobaltlsaddr:$sym))]>;

let mayLoad = 1 in {
  def TLS_LDrr : F3_1<3, 0b000000,
                      (outs IntRegs:$rd),
                      (ins (MEMrr $rs1, $rs2):$addr, TailRelocSymTLSLoad:$sym),
                      "ld [$addr], $rd, $sym",
                      [(set i32:$rd,
                          (tlsld ADDRrr:$addr, tglobaltlsaddr:$sym))]>;
}

let Uses = [O6], isCall = 1, hasDelaySlot = 1 in
  def TLS_CALL : InstSP<(outs),
                        (ins calltarget:$disp, TailRelocSymTLSCall:$sym,
                         variable_ops),
                        "call $disp, $sym",
                        [(tlscall texternalsym:$disp, tglobaltlsaddr:$sym)],
                        IIC_jmp_or_call> {
  bits<30> disp;
  let op = 1;
  let Inst{29-0} = disp;
}
}

//===----------------------------------------------------------------------===//
// Instructions for tail calls.
//===----------------------------------------------------------------------===//
let isCodeGenOnly = 1, isReturn = 1,  hasDelaySlot = 1,
    isTerminator = 1, isBarrier = 1 in {
  def TAIL_CALL : InstSP<(outs), (ins calltarget:$disp, variable_ops),
                         "call $disp",
                         [(tailcall tglobaladdr:$disp)]> {
  bits<30> disp;
  let op = 1;
  let Inst{29-0} = disp;
  }
}

def : Pat<(tailcall (iPTR texternalsym:$dst)),
          (TAIL_CALL texternalsym:$dst)>;

let isCodeGenOnly = 1, isReturn = 1,  hasDelaySlot = 1,  isTerminator = 1,
    isBarrier = 1, rd = 0 in {
  def TAIL_CALLri : F3_2<2, 0b111000,
                         (outs), (ins (MEMri $rs1, $simm13):$addr, variable_ops),
                         "jmp $addr",
                         [(tailcall ADDRri:$addr)]>;
}

//===----------------------------------------------------------------------===//
// V9 Instructions
//===----------------------------------------------------------------------===//

// V9 Conditional Moves.
let Constraints = "$f = $rd" in {
  // Move Integer Register on Condition (MOVcc) p. 194 of the V9 manual.
  let Uses = [ICC], intcc = 1, cc = 0b00 in {
    def MOVICCrr
      : F4_1<0b101100, (outs IntRegs:$rd),
             (ins IntRegs:$rs2, IntRegs:$f, CCOp:$cond),
             "mov$cond %icc, $rs2, $rd",
             [(set i32:$rd, (SPselecticc i32:$rs2, i32:$f, imm:$cond))]>;

    def MOVICCri
      : F4_2<0b101100, (outs IntRegs:$rd),
             (ins i32imm:$simm11, IntRegs:$f, CCOp:$cond),
             "mov$cond %icc, $simm11, $rd",
             [(set i32:$rd,
                    (SPselecticc simm11:$simm11, i32:$f, imm:$cond))]>;
  }

  let Uses = [FCC0], intcc = 0, cc = 0b00 in {
    def MOVFCCrr
      : F4_1<0b101100, (outs IntRegs:$rd),
             (ins IntRegs:$rs2, IntRegs:$f, CCOp:$cond),
             "mov$cond %fcc0, $rs2, $rd",
             [(set i32:$rd, (SPselectfcc i32:$rs2, i32:$f, imm:$cond))]>;
    def MOVFCCri
      : F4_2<0b101100, (outs IntRegs:$rd),
             (ins i32imm:$simm11, IntRegs:$f, CCOp:$cond),
             "mov$cond %fcc0, $simm11, $rd",
             [(set i32:$rd,
                    (SPselectfcc simm11:$simm11, i32:$f, imm:$cond))]>;
  }

  let Uses = [ICC], intcc = 1, opf_cc = 0b00 in {
    def FMOVS_ICC
      : F4_3<0b110101, 0b000001, (outs FPRegs:$rd),
             (ins FPRegs:$rs2, FPRegs:$f, CCOp:$cond),
             "fmovs$cond %icc, $rs2, $rd",
             [(set f32:$rd, (SPselecticc f32:$rs2, f32:$f, imm:$cond))]>;
    def FMOVD_ICC
      : F4_3<0b110101, 0b000010, (outs DFPRegs:$rd),
               (ins DFPRegs:$rs2, DFPRegs:$f, CCOp:$cond),
               "fmovd$cond %icc, $rs2, $rd",
               [(set f64:$rd, (SPselecticc f64:$rs2, f64:$f, imm:$cond))]>;
    def FMOVQ_ICC
      : F4_3<0b110101, 0b000011, (outs QFPRegs:$rd),
               (ins QFPRegs:$rs2, QFPRegs:$f, CCOp:$cond),
               "fmovq$cond %icc, $rs2, $rd",
               [(set f128:$rd, (SPselecticc f128:$rs2, f128:$f, imm:$cond))]>;
  }

  let Uses = [FCC0], intcc = 0, opf_cc = 0b00 in {
    def FMOVS_FCC
      : F4_3<0b110101, 0b000001, (outs FPRegs:$rd),
             (ins FPRegs:$rs2, FPRegs:$f, CCOp:$cond),
             "fmovs$cond %fcc0, $rs2, $rd",
             [(set f32:$rd, (SPselectfcc f32:$rs2, f32:$f, imm:$cond))]>;
    def FMOVD_FCC
      : F4_3<0b110101, 0b000010, (outs DFPRegs:$rd),
             (ins DFPRegs:$rs2, DFPRegs:$f, CCOp:$cond),
             "fmovd$cond %fcc0, $rs2, $rd",
             [(set f64:$rd, (SPselectfcc f64:$rs2, f64:$f, imm:$cond))]>;
    def FMOVQ_FCC
      : F4_3<0b110101, 0b000011, (outs QFPRegs:$rd),
             (ins QFPRegs:$rs2, QFPRegs:$f, CCOp:$cond),
             "fmovq$cond %fcc0, $rs2, $rd",
             [(set f128:$rd, (SPselectfcc f128:$rs2, f128:$f, imm:$cond))]>;
  }

}

// Floating-Point Move Instructions, p. 164 of the V9 manual.
def FMOVD : F3_3u<2, 0b110100, 0b000000010,
               (outs DFPRegs:$rd), (ins DFPRegs:$rs2),
               "fmovd $rs2, $rd", []>;
def FMOVQ : F3_3u<2, 0b110100, 0b000000011,
               (outs QFPRegs:$rd), (ins QFPRegs:$rs2),
               "fmovq $rs2, $rd", []>;
def FNEGD : F3_3u<2, 0b110100, 0b000000110,
               (outs DFPRegs:$rd), (ins DFPRegs:$rs2),
               "fnegd $rs2, $rd",
               [(set f64:$rd, (fneg f64:$rs2))]>;
def FNEGQ : F3_3u<2, 0b110100, 0b000000111,
               (outs QFPRegs:$rd), (ins QFPRegs:$rs2),
               "fnegq $rs2, $rd",
               [(set f128:$rd, (fneg f128:$rs2))]>;
def FABSD : F3_3u<2, 0b110100, 0b000001010,
               (outs DFPRegs:$rd), (ins DFPRegs:$rs2),
               "fabsd $rs2, $rd",
               [(set f64:$rd, (fabs f64:$rs2))]>;
def FABSQ : F3_3u<2, 0b110100, 0b000001011,
               (outs QFPRegs:$rd), (ins QFPRegs:$rs2),
               "fabsq $rs2, $rd",
               [(set f128:$rd, (fabs f128:$rs2))]>;

// Floating-point compare instruction with %fcc0-%fcc3.
def V9FCMPS  : F3_3c<2, 0b110101, 0b001010001,
               (outs FCCRegs:$rd), (ins FPRegs:$rs1, FPRegs:$rs2),
               "fcmps $rd, $rs1, $rs2", []>;
def V9FCMPD  : F3_3c<2, 0b110101, 0b001010010,
                (outs FCCRegs:$rd), (ins DFPRegs:$rs1, DFPRegs:$rs2),
                "fcmpd $rd, $rs1, $rs2", []>;
def V9FCMPQ  : F3_3c<2, 0b110101, 0b001010011,
                (outs FCCRegs:$rd), (ins QFPRegs:$rs1, QFPRegs:$rs2),
                "fcmpq $rd, $rs1, $rs2", []>;

let hasSideEffects = 1 in {
  def V9FCMPES  : F3_3c<2, 0b110101, 0b001010101,
                   (outs FCCRegs:$rd), (ins FPRegs:$rs1, FPRegs:$rs2),
                   "fcmpes $rd, $rs1, $rs2", []>;
  def V9FCMPED  : F3_3c<2, 0b110101, 0b001010110,
                   (outs FCCRegs:$rd), (ins DFPRegs:$rs1, DFPRegs:$rs2),
                   "fcmped $rd, $rs1, $rs2", []>;
  def V9FCMPEQ  : F3_3c<2, 0b110101, 0b001010111,
                   (outs FCCRegs:$rd), (ins QFPRegs:$rs1, QFPRegs:$rs2),
                   "fcmpeq $rd, $rs1, $rs2", []>;
}

// Floating point conditional move instrucitons with %fcc0-%fcc3.
let Constraints = "$f = $rd", intcc = 0 in {
def V9MOVFCCrr
  : F4_1<0b101100, (outs IntRegs:$rd),
         (ins FCCRegs:$cc, IntRegs:$rs2, IntRegs:$f, CCOp:$cond),
         "mov$cond $cc, $rs2, $rd", []>;
def V9MOVFCCri
  : F4_2<0b101100, (outs IntRegs:$rd),
         (ins FCCRegs:$cc, i32imm:$simm11, IntRegs:$f, CCOp:$cond),
         "mov$cond $cc, $simm11, $rd", []>;
def V9FMOVS_FCC
  : F4_3<0b110101, 0b000001, (outs FPRegs:$rd),
         (ins FCCRegs:$opf_cc, FPRegs:$rs2, FPRegs:$f, CCOp:$cond),
         "fmovs$cond $opf_cc, $rs2, $rd", []>;
def V9FMOVD_FCC
  : F4_3<0b110101, 0b000010, (outs DFPRegs:$rd),
         (ins FCCRegs:$opf_cc, DFPRegs:$rs2, DFPRegs:$f, CCOp:$cond),
         "fmovd$cond $opf_cc, $rs2, $rd", []>;
def V9FMOVQ_FCC
  : F4_3<0b110101, 0b000011, (outs QFPRegs:$rd),
         (ins FCCRegs:$opf_cc, QFPRegs:$rs2, QFPRegs:$f, CCOp:$cond),
         "fmovq$cond $opf_cc, $rs2, $rd", []>;
} // Constraints = "$f = $rd", ...


// POPCrr - This does a ctpop of a 64-bit register.  As such, we have to clear
// the top 32-bits before using it.  To do this clearing, we use a SRLri X,0.
let rs1 = 0 in
  def POPCrr : F3_1<2, 0b101110,
                    (outs IntRegs:$rd), (ins IntRegs:$rs2),
                    "popc $rs2, $rd", []>;
def : Pat<(i32 (ctpop i32:$src)),
          (POPCrr (SRLri $src, 0))>;

let hasSideEffects = 1, rd = 0, rs1 = 0b01111 in
 def MEMBARi : F3_2<2, 0b101000, (outs), (ins MembarTag:$simm13),
                    "membar $simm13", []>;

let rd = 15, rs1 = 0b00000 in
  def SIR: F3_2<2, 0b110000, (outs),
                (ins simm13Op:$simm13),
                 "sir $simm13", []>;

// The CAS instruction, unlike other instructions, only comes in a
// form which requires an ASI be provided. The ASI value hardcoded
// here is ASI_PRIMARY, the default unprivileged ASI for E2KV9.
let Constraints = "$swap = $rd", asi = 0b10000000 in
  def CASrr: F3_1_asi<3, 0b111100,
                (outs IntRegs:$rd), (ins IntRegs:$rs1, IntRegs:$rs2,
                                     IntRegs:$swap),
                 "cas [$rs1], $rs2, $rd",
                 [(set i32:$rd,
                     (atomic_cmp_swap_32 iPTR:$rs1, i32:$rs2, i32:$swap))]>;



// TODO: Add DAG sequence to lower these instructions. Currently, only provided
// as inline assembler-supported instructions.
let Defs = [Y, ASR18], Uses = [Y, ASR18] in {
  def SMACrr :  F3_1<2, 0b111111,
                   (outs IntRegs:$rd), (ins IntRegs:$rs1, IntRegs:$rs2, ASRRegs:$asr18),
                   "smac $rs1, $rs2, $rd",
                   [], IIC_smac_umac>;

  def SMACri :  F3_2<2, 0b111111,
                  (outs IntRegs:$rd), (ins IntRegs:$rs1, simm13Op:$simm13, ASRRegs:$asr18),
                   "smac $rs1, $simm13, $rd",
                   [], IIC_smac_umac>;

  def UMACrr :  F3_1<2, 0b111110,
                  (outs IntRegs:$rd), (ins IntRegs:$rs1, IntRegs:$rs2, ASRRegs:$asr18),
                   "umac $rs1, $rs2, $rd",
                   [], IIC_smac_umac>;

  def UMACri :  F3_2<2, 0b111110,
                  (outs IntRegs:$rd), (ins IntRegs:$rs1, simm13Op:$simm13, ASRRegs:$asr18),
                   "umac $rs1, $simm13, $rd",
                   [], IIC_smac_umac>;
}

let Defs = [ICC] in {
defm TADDCC   : F3_12np<"taddcc",   0b100000>;
defm TSUBCC   : F3_12np<"tsubcc",   0b100001>;

let hasSideEffects = 1 in {
  defm TADDCCTV : F3_12np<"taddcctv", 0b100010>;
  defm TSUBCCTV : F3_12np<"tsubcctv", 0b100011>;
}
}

// Section A.42 - Prefetch Data
def PREFETCHr : F3_1<3, 0b101101,
               (outs), (ins (MEMrr $rs1, $rs2):$addr, shift_imm5:$rd),
               "prefetch [$addr], $rd", []>;
def PREFETCHi : F3_2<3, 0b101101,
               (outs), (ins (MEMri $rs1, $simm13):$addr, shift_imm5:$rd),
               "prefetch [$addr], $rd", []>;


// Section A.43 - Read Privileged Register Instructions
let rs2 = 0 in
  def RDPR : F3_1<2, 0b101010,
                 (outs IntRegs:$rd), (ins PRRegs:$rs1),
                 "rdpr $rs1, $rd", []>;


// Section A.62 - Write Privileged Register Instructions
def WRPRrr : F3_1<2, 0b110010,
               (outs PRRegs:$rd), (ins IntRegs:$rs1, IntRegs:$rs2),
               "wrpr $rs1, $rs2, $rd", []>;
def WRPRri : F3_2<2, 0b110010,
               (outs PRRegs:$rd), (ins IntRegs:$rs1, simm13Op:$simm13),
               "wrpr $rs1, $simm13, $rd", []>;

//===----------------------------------------------------------------------===//
// Non-Instruction Patterns
//===----------------------------------------------------------------------===//

// Zero immediate.
def : Pat<(i32 0),
          (ORrr (i32 G0), (i32 G0))>;
// Small immediates.
def : Pat<(i32 simm13:$val),
          (ORri (i32 G0), imm:$val)>;
// Arbitrary immediates.
def : Pat<(i32 imm:$val),
          (ORri (SETHIi (HI22 imm:$val)), (LO10 imm:$val))>;


// Global addresses, constant pool entries

def : Pat<(SPhi tglobaladdr:$in), (SETHIi tglobaladdr:$in)>;
def : Pat<(SPlo tglobaladdr:$in), (ORri (i32 G0), tglobaladdr:$in)>;
def : Pat<(SPhi tconstpool:$in), (SETHIi tconstpool:$in)>;
def : Pat<(SPlo tconstpool:$in), (ORri (i32 G0), tconstpool:$in)>;

// GlobalTLS addresses
def : Pat<(SPhi tglobaltlsaddr:$in), (SETHIi tglobaltlsaddr:$in)>;
def : Pat<(SPlo tglobaltlsaddr:$in), (ORri (i32 G0), tglobaltlsaddr:$in)>;
def : Pat<(add (SPhi tglobaltlsaddr:$in1), (SPlo tglobaltlsaddr:$in2)),
          (ADDri (SETHIi tglobaltlsaddr:$in1), (tglobaltlsaddr:$in2))>;
def : Pat<(xor (SPhi tglobaltlsaddr:$in1), (SPlo tglobaltlsaddr:$in2)),
          (XORri (SETHIi tglobaltlsaddr:$in1), (tglobaltlsaddr:$in2))>;

// Blockaddress
def : Pat<(SPhi tblockaddress:$in), (SETHIi tblockaddress:$in)>;
def : Pat<(SPlo tblockaddress:$in), (ORri (i32 G0), tblockaddress:$in)>;

// Add reg, lo.  This is used when taking the addr of a global/constpool entry.
def : Pat<(add iPTR:$r, (SPlo tglobaladdr:$in)), (ADDri $r, tglobaladdr:$in)>;
def : Pat<(add iPTR:$r, (SPlo tconstpool:$in)),  (ADDri $r, tconstpool:$in)>;
def : Pat<(add iPTR:$r, (SPlo tblockaddress:$in)),
                        (ADDri $r, tblockaddress:$in)>;

// Calls:
/*
def : Pat<(call tglobaladdr:$dst),
          (CALL tglobaladdr:$dst)>;
def : Pat<(call texternalsym:$dst),
          (CALL texternalsym:$dst)>;
*/
// Map integer extload's to zextloads.
def : Pat<(i32 (extloadi1 ADDRrr:$src)), (LDUBrr ADDRrr:$src)>;
def : Pat<(i32 (extloadi1 ADDRri:$src)), (LDUBri ADDRri:$src)>;
def : Pat<(i32 (extloadi8 ADDRrr:$src)), (LDUBrr ADDRrr:$src)>;
def : Pat<(i32 (extloadi8 ADDRri:$src)), (LDUBri ADDRri:$src)>;
def : Pat<(i32 (extloadi16 ADDRrr:$src)), (LDUHrr ADDRrr:$src)>;
def : Pat<(i32 (extloadi16 ADDRri:$src)), (LDUHri ADDRri:$src)>;

// zextload bool -> zextload byte
def : Pat<(i32 (zextloadi1 ADDRrr:$src)), (LDUBrr ADDRrr:$src)>;
def : Pat<(i32 (zextloadi1 ADDRri:$src)), (LDUBri ADDRri:$src)>;

// store 0, addr -> store %g0, addr
def : Pat<(store (i32 0), ADDRrr:$dst), (STrr ADDRrr:$dst, (i32 G0))>;
def : Pat<(store (i32 0), ADDRri:$dst), (STri ADDRri:$dst, (i32 G0))>;

// store bar for all atomic_fence in V8.
def : Pat<(atomic_fence timm, timm), (STBAR)>;

def : Pat<(atomic_fence timm, timm), (MEMBARi 0xf)>;

// atomic_load addr -> load addr
def : Pat<(i32 (atomic_load_8 ADDRrr:$src)), (LDUBrr ADDRrr:$src)>;
def : Pat<(i32 (atomic_load_8 ADDRri:$src)), (LDUBri ADDRri:$src)>;
def : Pat<(i32 (atomic_load_16 ADDRrr:$src)), (LDUHrr ADDRrr:$src)>;
def : Pat<(i32 (atomic_load_16 ADDRri:$src)), (LDUHri ADDRri:$src)>;
def : Pat<(i32 (atomic_load_32 ADDRrr:$src)), (LDrr ADDRrr:$src)>;
def : Pat<(i32 (atomic_load_32 ADDRri:$src)), (LDri ADDRri:$src)>;

// atomic_store val, addr -> store val, addr
def : Pat<(atomic_store_8 ADDRrr:$dst, i32:$val), (STBrr ADDRrr:$dst, $val)>;
def : Pat<(atomic_store_8 ADDRri:$dst, i32:$val), (STBri ADDRri:$dst, $val)>;
def : Pat<(atomic_store_16 ADDRrr:$dst, i32:$val), (STHrr ADDRrr:$dst, $val)>;
def : Pat<(atomic_store_16 ADDRri:$dst, i32:$val), (STHri ADDRri:$dst, $val)>;
def : Pat<(atomic_store_32 ADDRrr:$dst, i32:$val), (STrr ADDRrr:$dst, $val)>;
def : Pat<(atomic_store_32 ADDRri:$dst, i32:$val), (STri ADDRri:$dst, $val)>;

// extract_vector
def : Pat<(extractelt (v2i32 IntPair:$Rn), 0),
          (i32 (EXTRACT_SUBREG IntPair:$Rn, sub_even))>;
def : Pat<(extractelt (v2i32 IntPair:$Rn), 1),
          (i32 (EXTRACT_SUBREG IntPair:$Rn, sub_odd))>;

// build_vector
def : Pat<(build_vector (i32 IntRegs:$a1), (i32 IntRegs:$a2)),
          (INSERT_SUBREG
	    (INSERT_SUBREG (v2i32 (IMPLICIT_DEF)), (i32 IntRegs:$a1), sub_even),
            (i32 IntRegs:$a2), sub_odd)>;


include "E2KInstr64Bit.td"
include "E2KInstrVIS.td"
include "E2KInstrAliases.td"
