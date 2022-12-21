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

// logical AND
def ANDS : E2KInst<0x00, (outs), (ins), "ands">;
def ANDD : E2KInst<0x01, (outs), (ins), "andd">;

// logical AND, inverted operand 2
def ANDNS : E2KInst<0x02, (outs), (ins), "andns">;
def ANDND : E2KInst<0x03, (outs), (ins), "andnd">;

// logical OR
def ORS : E2KInst<0x04, (outs), (ins), "ors">;
def ORD : E2KInst<0x05, (outs), (ins), "ord">;

// logical OR, inverted operand 2
def ORNS : E2KInst<0x06, (outs), (ins), "orns">;
def ORND : E2KInst<0x07, (outs), (ins), "ornd">;

// logical XOR
def XORS : E2KInst<0x08, (outs), (ins), "xors">;
def XORD : E2KInst<0x09, (outs), (ins), "xord">;

// logical XOR, inverted operand 2
def XORNS : E2KInst<0x0A, (outs), (ins), "xorns">;
def XORND : E2KInst<0x0B, (outs), (ins), "xornd">;

// sign-extend or zero-extend
def SXT : E2KInst<0x0C, (outs), (ins), "sxt">;

// 0x0D ???

// merge, choose operand based on predicate (like ternary operator)
def MERGES : E2KInst<0x0E, (outs), (ins), "merges">;
def MERGED : E2KInst<0x0F, (outs), (ins), "merged">;

// arithmetic ADD
def ADDS : E2KInst<0x10, (outs), (ins), "adds">;
def ADDD : E2KInst<0x11, (outs), (ins), "addd">;

// arithmetic SUB
def SUBS : E2KInst<0x12, (outs), (ins), "subs">;
def SUBD : E2KInst<0x13, (outs), (ins), "subd">;

// shift cyclic left
def SCLS : E2KInst<0x14, (outs), (ins), "scls">;
def SCLD : E2KInst<0x15, (outs), (ins), "scld">;

// shift cyclic right
def SCRS : E2KInst<0x16, (outs), (ins), "scrs">;
def SCRD : E2KInst<0x17, (outs), (ins), "scrd">;

// shift logical left
def SHLS : E2KInst<0x18, (outs), (ins), "shls">;
def SHLD : E2KInst<0x19, (outs), (ins), "shld">;

// shift logical right
def SHRS : E2KInst<0x1a, (outs), (ins), "shrs">;
def SHRD : E2KInst<0x1b, (outs), (ins), "shrd">;

// shift arithmetic right
def SARS : E2KInst<0x1c, (outs), (ins), "sars">;
def SARD : E2KInst<0x1d, (outs), (ins), "sard">;

// get field of an arbitrary length
def GETFS : E2KInst<0x1e, (outs), (ins), "getfs">;
def GETFD : E2KInst<0x1f, (outs), (ins), "getfd">;

// integer comparison
// compare, overflow
def CMPOSB : E2KCCInst<0x20, 0, (outs), (ins), "cmposb">;
def CMPODB : E2KCCInst<0x21, 0, (outs), (ins), "cmpodb">;

// compare, less (without sign)
def CMPBSB : E2KCCInst<0x20, 1, (outs), (ins), "cmpbsb">;
def CMPBDB : E2KCCInst<0x21, 1, (outs), (ins), "cmpbdb">;

// compare, equal
def CMPESB : E2KCCInst<0x20, 2, (outs), (ins), "cmpesb">;
def CMPEDB : E2KCCInst<0x21, 2, (outs), (ins), "cmpedb">;

// compare, below or equal (without sing)
def CMPBESB : E2KCCInst<0x20, 3, (outs), (ins), "cmpbesb">;
def CMPBEDB : E2KCCInst<0x21, 3, (outs), (ins), "cmpbedb">;

// compare, negative
def CMPSSB : E2KCCInst<0x20, 4, (outs), (ins), "cmpssb">;
def CMPSDB : E2KCCInst<0x21, 4, (outs), (ins), "cmpsdb">;

// compare, parity
def CMPPSB : E2KCCInst<0x20, 5, (outs), (ins), "cmppsb">;
def CMPPDB : E2KCCInst<0x21, 5, (outs), (ins), "cmppdb">;

// compare, less (with sign)
def CMPLSB : E2KCCInst<0x20, 6, (outs), (ins), "cmplsb">;
def CMPLDB : E2KCCInst<0x21, 6, (outs), (ins), "cmpldb">;

// compare, less or equal (with sign)
def CMPLESB : E2KCCInst<0x20, 7, (outs), (ins), "cmplesb">;
def CMPLEDB : E2KCCInst<0x21, 7, (outs), (ins), "cmpledb">;

// 0x22 ???
// 0x23 ???

// store byte
def STB : E2KInst<0x24, (outs), (ins), "stb", [2, 5]>;

def CCTOPO  : E2KCCInst<0x24, 0, (outs), (ins), "cctopo", [0, 3]>;
def CCTOPB  : E2KCCInst<0x24, 1, (outs), (ins), "cctopb", [0, 3]>;
def CCTOPE  : E2KCCInst<0x24, 2, (outs), (ins), "cctope", [0, 3]>;
def CCTOPBE : E2KCCInst<0x24, 3, (outs), (ins), "cctopbe", [0, 3]>;
def CCTOPS  : E2KCCInst<0x24, 4, (outs), (ins), "cctops", [0, 3]>;
def CCTOPP  : E2KCCInst<0x24, 5, (outs), (ins), "cctopp", [0, 3]>;
def CCTOPL  : E2KCCInst<0x24, 6, (outs), (ins), "cctopl", [0, 3]>;
def CCTOPLE : E2KCCInst<0x24, 7, (outs), (ins), "cctople", [0, 3]>;

// store half-word
def STH : E2KInst<0x25, (outs), (ins), "sth", [2, 5]>;

// store word
def STW : E2KInst<0x26, (outs), (ins), "stw", [2, 5]>;

def BITREVS : E2KEXTInst<0x26, 0xC0, (outs), (ins), "bitrevs", [0, 1, 3, 4]>;

// store double-word
def STD : E2KInst<0x27, (outs), (ins), "std", [2, 5]>;

def BITREVD : E2KEXTInst<0x27, 0xC0, (outs), (ins), "bitrevd", [0, 1, 3, 4]>;

def STCSB : E2KInst<0x28, (outs), (ins), "stcsb", [2, 5]>;

def FXCMPEQSB  : E2KCCInst<0x28, 0, (outs), (ins), "fxcmpeqsb">;
def FXCMPLTSB  : E2KCCInst<0x28, 1, (outs), (ins), "fxcmpltsb">;
def FXCMPLESB  : E2KCCInst<0x28, 2, (outs), (ins), "fxcmplesb">;
def FXCMPUODSB : E2KCCInst<0x28, 3, (outs), (ins), "fxcmpuodsb">;
def FXCMPNEQSB : E2KCCInst<0x28, 4, (outs), (ins), "fxcmpneqsb">;
def FXCMPNLTSB : E2KCCInst<0x28, 5, (outs), (ins), "fxcmpnltsb">;
def FXCMPNLESB : E2KCCInst<0x28, 6, (outs), (ins), "fxcmpnlesb">;
def FXCMPODSB  : E2KCCInst<0x28, 7, (outs), (ins), "fxcmpodsb">;

def STCSH : E2KInst<0x29, (outs), (ins), "stcsb", [2, 5]>;

def FXCMPEQDB  : E2KCCInst<0x29, 0, (outs), (ins), "fxcmpeqdb">;
def FXCMPLTDB  : E2KCCInst<0x29, 1, (outs), (ins), "fxcmpltdb">;
def FXCMPLEDB  : E2KCCInst<0x29, 2, (outs), (ins), "fxcmpledb">;
def FXCMPNEQDB : E2KCCInst<0x29, 4, (outs), (ins), "fxcmpneqdb">;
def FXCMPNLTDB : E2KCCInst<0x29, 5, (outs), (ins), "fxcmpnltdb">;
def FXCMPNLEDB : E2KCCInst<0x29, 6, (outs), (ins), "fxcmpnledb">;
def FXCMPODDB  : E2KCCInst<0x29, 7, (outs), (ins), "fxcmpoddb">;

def STCSW : E2KInst<0x2a, (outs), (ins), "stcsw", [2, 5]>;

def STCSD : E2KInst<0x2b, (outs), (ins), "stcsd", [2, 5]>;

def FXCMPEQXB  : E2KCCInst<0x2b, 0, (outs), (ins), "fxcmpeqxb">;
def FXCMPLTXB  : E2KCCInst<0x2b, 1, (outs), (ins), "fxcmpltxb">;
def FXCMPLEXB  : E2KCCInst<0x2b, 2, (outs), (ins), "fxcmplexb">;
def FXCMPNEQXB : E2KCCInst<0x2b, 4, (outs), (ins), "fxcmpneqxb">;
def FXCMPNLTXB : E2KCCInst<0x2b, 5, (outs), (ins), "fxcmpnltxb">;
def FXCMPNLEXB : E2KCCInst<0x2b, 6, (outs), (ins), "fxcmpnlexb">;
def FXCMPODXB  : E2KCCInst<0x2b, 7, (outs), (ins), "fxcmpodxb">;

def STDSB : E2KInst<0x2c, (outs), (ins), "stdsb", [2, 5]>;

def STDSH : E2KInst<0x2d, (outs), (ins), "stdsh", [2, 5]>;

def STDSW : E2KInst<0x2e, (outs), (ins), "stdsw", [2, 5]>;

def FCMPEQSB  : E2KCCInst<0x2e, 0, (outs), (ins), "fcmpeqsb">;
def FCMPLTSB  : E2KCCInst<0x2e, 1, (outs), (ins), "fcmpltsb">;
def FCMPLESB  : E2KCCInst<0x2e, 2, (outs), (ins), "fcmplesb">;
def FCMPNEQSB : E2KCCInst<0x2e, 4, (outs), (ins), "fcmpneqsb">;
def FCMPNLTSB : E2KCCInst<0x2e, 5, (outs), (ins), "fcmpnltsb">;
def FCMPNLESB : E2KCCInst<0x2e, 6, (outs), (ins), "fcmpnlesb">;
def FCMPODSB  : E2KCCInst<0x2e, 7, (outs), (ins), "fcmpodsb">;

def STESB : E2KInst<0x30, (outs), (ins), "stesb", [2, 5]>;
def STESH : E2KInst<0x31, (outs), (ins), "stesh", [2, 5]>;

// floating-point ADD
def FADDS : E2KInst<0x30, (outs), (ins), "fadds", [0, 1, 3, 4]>;
def FADDD : E2KInst<0x31, (outs), (ins), "faddd", [0, 1, 3, 4]>;

def STESW : E2KInst<0x32, (outs), (ins), "stesw", [2, 5]>;
def STESD : E2KInst<0x33, (outs), (ins), "stesd", [2, 5]>;

// floating-point SUB
def FSUBS : E2KInst<0x32, (outs), (ins), "fsubs", [0, 1, 3, 4]>;
def FSUBD : E2KInst<0x33, (outs), (ins), "fsubd", [0, 1, 3, 4]>;

def STFSB : E2KInst<0x34, (outs), (ins), "stfsb", [2, 5]>;
def STFSH : E2KInst<0x35, (outs), (ins), "stfsh", [2, 5]>;

// floating-point MIN
def FMINS : E2KInst<0x34, (outs), (ins), "fmins", [0, 1, 3, 4]>;
def FMIND : E2KInst<0x35, (outs), (ins), "fmind", [0, 1, 3, 4]>;

def STFSW : E2KInst<0x36, (outs), (ins), "stfsw", [2, 5]>;
def STFSD : E2KInst<0x37, (outs), (ins), "stfsd", [2, 5]>;

// floating-point MAX
def FMAXS : E2KInst<0x36, (outs), (ins), "fmaxs", [0, 1, 3, 4]>;
def FMAXD : E2KInst<0x37, (outs), (ins), "fmaxd", [0, 1, 3, 4]>;

def STGSB : E2KInst<0x38, (outs), (ins), "stgsb", [2, 5]>;
def STGSH : E2KInst<0x38, (outs), (ins), "stgsh", [2, 5]>;

// floating-point MUL
def FMULS : E2KInst<0x38, (outs), (ins), "fmuls", [0, 1, 3, 4]>;
def FMULD : E2KInst<0x39, (outs), (ins), "fmuld", [0, 1, 3, 4]>;

def STGSW : E2KInst<0x3a, (outs), (ins), "stgsw", [2, 5]>;
def STGSD : E2KInst<0x3b, (outs), (ins), "stgsd", [2, 5]>;

def STSSB : E2KInst<0x3c, (outs), (ins), "stssb", [2, 5]>;

// convert fp32 to int32
def FSTOIS : E2KEXTInst<0x3c, 0xC0, (outs), (ins), "fstois", [0, 1, 3, 4]>;

// convert fp32 to int32 (with truncation)
def FSTOISTR : E2KEXTInst<0x3c, 0xC2, (outs), (ins), "fstoistr", [0, 1, 3, 4]>;

// convert int32 to fp32
def ISTOFS : E2KEXTInst<0x3c, 0xC4, (outs), (ins), "istofs", [0, 1, 3, 4]>;

def STSSH : E2KInst<0x3d, (outs), (ins), "stssh", [2, 5]>;

// convert fp64 to int64
def FDTOID : E2KEXTInst<0x3d, 0xC0, (outs), (ins), "fdtoid", [0, 1, 3, 4]>;

// convert fp80 to int64
def FXTOID : E2KEXTInst<0x3d, 0xC1, (outs), (ins), "fxtoid", [0, 1, 3, 4]>;

// convert fp64 to int64 (with truncation)
def FDTOIDTR : E2KEXTInst<0x3d, 0xC2, (outs), (ins), "fdtoidtr", [0, 1, 3, 4]>;

// convert fp80 to int64 (with truncation)
def FXTOIDTR : E2KEXTInst<0x3d, 0xC3, (outs), (ins), "fxtoidtr", [0, 1, 3, 4]>;

// convert int64 to fp64
def IDTOFD : E2KEXTInst<0x3d, 0xC4, (outs), (ins), "idtofd", [0, 1, 3, 4]>;

// convert int64 to fp80
def IDTOFX : E2KEXTInst<0x3d, 0xC5, (outs), (ins), "idtofx", [0, 1, 3, 4]>;

// convert fp80 to fp64
def FXTOFD : E2KEXTInst<0x3d, 0xC6, (outs), (ins), "fxtofd", [0, 1, 3, 4]>;

// convert fp64 to fp80
def FDTOFX : E2KEXTInst<0x3d, 0xC7, (outs), (ins), "fdtofx", [0, 1, 3, 4]>;

// package convert fp32 to int32
def PFSTOIS : E2KEXTInst<0x3d, 0xC8, (outs), (ins), "pfstois", [0, 1, 3, 4]>;

// package convert fp32 to int32 (with truncation)
def PFSTOISTR : E2KEXTInst<0x3d, 0xCA, (outs), (ins), "pfstoistr", [0, 1, 3, 4]>;

// package convert int32 to fp32
def PISTOFS : E2KEXTInst<0x3d, 0xCC, (outs), (ins), "pistofs", [0, 1, 3, 4]>;

def STSSW : E2KInst<0x3e, (outs), (ins), "stssw", [2, 5]>;

// convert fp32 to int64
def FSTOID : E2KEXTInst<0x3e, 0xC0, (outs), (ins), "fstoid", [0, 1, 3, 4]>;

// convert fp32 to int64 (with truncation)
def FSTOIDTR : E2KEXTInst<0x3e, 0xC2, (outs), (ins), "fstoidtr", [0, 1, 3, 4]>;

// convert int32 to fp64
def ISTOFD : E2KEXTInst<0x3e, 0xC4, (outs), (ins), "istofd", [0, 1, 3, 4]>;

// convert int32 to fp80
def ISTOFX : E2KEXTInst<0x3e, 0xC5, (outs), (ins), "istofx", [0, 1, 3, 4]>;

// convert fp32 to fp64
def FSTOFD : E2KEXTInst<0x3e, 0xC6, (outs), (ins), "fstofd", [0, 1, 3, 4]>;

// convert fp32 to fp80
def FSTOFX : E2KEXTInst<0x3e, 0xC7, (outs), (ins), "fstofx", [0, 1, 3, 4]>;

// package convert fp32 to fp64
def PFSTOFD : E2KEXTInst<0x3e, 0xCE, (outs), (ins), "pfstofd", [0, 1, 3, 4]>;

def STSSD : E2KInst<0x3f, (outs), (ins), "stssd", [2, 5]>;

// convert fp64 to int32
def FDTOIS : E2KEXTInst<0x3f, 0xC0, (outs), (ins), "fdtois", [0, 1, 3, 4]>;

// convert fp80 to int32
def FXTOIS : E2KEXTInst<0x3f, 0xC1, (outs), (ins), "fxtois", [0, 1, 3, 4]>;

// convert fp64 to int32 (with truncation)
def FDTOISTR : E2KEXTInst<0x3f, 0xC2, (outs), (ins), "fdtoistr", [0, 1, 3, 4]>;

// convert fp80 to int32 (with truncation)
def FXTOISTR : E2KEXTInst<0x3f, 0xC3, (outs), (ins), "fxtoistr", [0, 1, 3, 4]>;

// convert int64 to fp32
def IDTOFS : E2KEXTInst<0x3f, 0xC4, (outs), (ins), "idtofs", [0, 1, 3, 4]>;

// convert fp64 to fp32
def FDTOFS : E2KEXTInst<0x3f, 0xC6, (outs), (ins), "fdtofs", [0, 1, 3, 4]>;

// convert fp80 to fp32
def FXTOFS : E2KEXTInst<0x3f, 0xC7, (outs), (ins), "fxtofs", [0, 1, 3, 4]>;

// packed convert fp64 to int32
def PFDTOIS : E2KEXTInst<0x3f, 0xC8, (outs), (ins), "pfdtois", [0, 1, 3, 4]>;

// packed convert fp64 to int32 (with truncation)
def PFDTOISTR : E2KEXTInst<0x3f, 0xCA, (outs), (ins), "pfdtoistr", [0, 1, 3, 4]>;

// packed convert fp64 to fp32
def PFDTOFS : E2KEXTInst<0x3f, 0xCE, (outs), (ins), "pfdtofs", [0, 1, 3, 4]>;

def FXADDSS : E2KInst<0x40, (outs), (ins), "fxaddss", [0, 1, 3, 4]>;
def FXADDDD : E2KInst<0x41, (outs), (ins), "fxadddd", [0, 1, 3, 4]>;

// unsigned integer DIV
def UDIVS : E2KInst<0x40, (outs), (ins), "udivs", [5]>;
def UDIVD : E2KInst<0x41, (outs), (ins), "udivd", [5]>;
def UDIVX : E2KInst<0x44, (outs), (ins), "udivx", [5]>;

def FXADDSX : E2KInst<0x42, (outs), (ins), "fxaddsx", [0, 1, 3, 4]>;
def FXADDDX : E2KInst<0x43, (outs), (ins), "fxadddx", [0, 1, 3, 4]>;

// signed integer DIV
def SDIVS : E2KInst<0x42, (outs), (ins), "sdivs", [5]>;
def SDIVD : E2KInst<0x43, (outs), (ins), "sdivd", [5]>;
def SDIVX : E2KInst<0x46, (outs), (ins), "sdivx", [5]>;

def FXADDXS : E2KInst<0x44, (outs), (ins), "fxaddxs", [0, 1, 3, 4]>;
def FXADDXD : E2KInst<0x45, (outs), (ins), "fxaddxd", [0, 1, 3, 4]>;

// unsigned modulo (remainder) of division
def UMODX : E2KInst<0x45, (outs), (ins), "umodx", [5]>;

def FXADDXX : E2KInst<0x47, (outs), (ins), "fxaddxx", [0, 1, 3, 4]>;

// signed modulo (remainder) of division
def SMODX : E2KInst<0x47, (outs), (ins), "smodx", [5]>;

def FXSUBSS : E2KInst<0x48, (outs), (ins), "fxsubss", [0, 1, 3, 4]>;
def FXDIVSS : E2KInst<0x48, (outs), (ins), "fxdivss", [5]>;

def FXSUBDD : E2KInst<0x49, (outs), (ins), "fxsubdd", [0, 1, 3, 4]>;
def FXDIVDD : E2KInst<0x49, (outs), (ins), "fxdivdd", [5]>;

def FXSUBSX : E2KInst<0x4a, (outs), (ins), "fxsubsx", [0, 1, 3, 4]>;
def FXDIVSX : E2KInst<0x4a, (outs), (ins), "fxdivsx", [5]>;

def FXSUBDX : E2KInst<0x4b, (outs), (ins), "fxsubdx", [0, 1, 3, 4]>;
def FXDIVDX : E2KInst<0x4b, (outs), (ins), "fxdivdx", [5]>;

def FXSUBXS : E2KInst<0x4c, (outs), (ins), "fxsubxs", [0, 1, 3, 4]>;
def FXDIVXS : E2KInst<0x4c, (outs), (ins), "fxdivxs", [5]>;

def FXSUBXD : E2KInst<0x4d, (outs), (ins), "fxsubxd", [0, 1, 3, 4]>;
def FXDIVXD : E2KInst<0x4d, (outs), (ins), "fxdivxd", [5]>;

// 0x4e ???

def FXSUBXX : E2KInst<0x4f, (outs), (ins), "fxsubxx", [0, 1, 3, 4]>;
def FXDIVXX : E2KInst<0x4f, (outs), (ins), "fxdivxdx", [5]>;

def FXMULSS : E2KInst<0x50, (outs), (ins), "fxmulss", [0, 1, 3, 4]>;
def FXMULDD : E2KInst<0x51, (outs), (ins), "fxmuldd", [0, 1, 3, 4]>;
def FXMULSX : E2KInst<0x52, (outs), (ins), "fxmulsx", [0, 1, 3, 4]>;
def FXMULDX : E2KInst<0x53, (outs), (ins), "fxmuldx", [0, 1, 3, 4]>;

def FXSQRTISX : E2KEXTInst<0x52, 0xC0, (outs), (ins), "fxsqrtisx", [5]>;
def FXSQRTIDX : E2KEXTInst<0x53, 0xC0, (outs), (ins), "fxsqrtdsx", [5]>;

def FXMULXS : E2KInst<0x54, (outs), (ins), "fxmulxs", [0, 1, 3, 4]>;
def FXMULXD : E2KInst<0x55, (outs), (ins), "fxmulxd", [0, 1, 3, 4]>;

// 0x56 ???

def FXMULXX : E2KInst<0x57, (outs), (ins), "fxmulxx", [0, 1, 3, 4]>;

def FXSQRTIXX : E2KEXTInst<0x57, 0xC0, (outs), (ins), "fxsqrtdxx", [5]>;

def FXRSUBSS : E2KInst<0x58, (outs), (ins), "fxrsubss", [0, 1, 3, 4]>;
def FXRSUBDD : E2KInst<0x59, (outs), (ins), "fxrsubdd", [0, 1, 3, 4]>;
def FXRSUBSX : E2KInst<0x5a, (outs), (ins), "fxrsubsx", [0, 1, 3, 4]>;
def FXRSUBDX : E2KInst<0x5b, (outs), (ins), "fxrsubdx", [0, 1, 3, 4]>;

def FXSQRTUXX : E2KInst<0x59, (outs), (ins), "fxsqrtuxx", [5]>;
def FXSQRTUSX : E2KInst<0x5a, (outs), (ins), "fxsqrtusx", [5]>;
def FXSQRTUDX : E2KInst<0x5b, (outs), (ins), "fxsqrtudx", [5]>;

def MOVFI : E2KEXTInst<0x5c, 0xC0, (outs), (ins), "movfi", [0, 1, 3, 4]>;

def FXSQRTTXX : E2KInst<0x5d, (outs), (ins), "fxsqrttxx", [5]>;

def MOVIF : E2KInst<0x5e, (outs), (ins), "movif", [0, 1, 3, 4]>;

def FXSQRTTSX : E2KInst<0x5e, (outs), (ins), "fxsqrttsx", [5]>;

def MOVX : E2KEXTInst<0x5f, 0xC0, (outs), (ins), "movx", [0, 1, 3, 4]>;
def MOVXA : E2KEXTInst<0x5f, 0xC1, (outs), (ins), "movxa", [0, 1, 3, 4]>;
def MOVXC : E2KEXTInst<0x5f, 0xC2, (outs), (ins), "movxc", [0, 1, 3, 4]>;

def FXSQRTTDX : E2KInst<0x5f, (outs), (ins), "fxsqrttdx", [5]>;

// move tagged value
def MOVTS : E2KEXTInst<0x60, 0xC0, (outs), (ins), "movts", [0, 1, 3, 4]>;
def MOVTCS : E2KEXTInst<0x60, 0xC1, (outs), (ins), "movtcs", [0, 1, 3, 4]>;
def MOVTRS : E2KEXTInst<0x60, 0xC2, (outs), (ins), "movtrs", [0, 1, 3, 4]>;
def MOVTRCS : E2KEXTInst<0x60, 0xC3, (outs), (ins), "movtrcs", [0, 1, 3, 4]>;

def FXDIVTSS : E2KInst<0x60, (outs), (ins), "fxdivtss", [5]>;

def MOVTD : E2KEXTInst<0x61, 0xC0, (outs), (ins), "movtd", [0, 1, 3, 4]>;
def MOVTCD : E2KEXTInst<0x61, 0xC1, (outs), (ins), "movtcd", [0, 1, 3, 4]>;
def MOVTRD : E2KEXTInst<0x61, 0xC2, (outs), (ins), "movtrd", [0, 1, 3, 4]>;
def MOVTRCD : E2KEXTInst<0x61, 0xC3, (outs), (ins), "movtrcd", [0, 1, 3, 4]>;

def FXDIVTDD : E2KInst<0x61, (outs), (ins), "fxdivtdd", [5]>;

def GETSAP : E2KEXTInst<0x62, 0xEC, (outs), (ins), "getsap", [0, 1, 3, 4]>;
def CUDTOAP : E2KEXTInst<0x62, 0xF0, (outs), (ins), "cudtoap", [0, 1, 3, 4]>;
def GDTOAP : E2KEXTInst<0x62, 0xF2, (outs), (ins), "gdtoap", [0, 1, 3, 4]>;

def FXDIVTSX : E2KInst<0x62, (outs), (ins), "fxdivtsx", [5]>;

def GETPL : E2KEXTInst<0x63, 0xF0, (outs), (ins), "gdtoap", [0, 3]>;

def VFSI : E2KInst<0x63, (outs), (ins), "gdtoap", [1, 4]>;

def FXDIVTDX : E2KInst<0x63, (outs), (ins), "fxdivtdx", [5]>;

// load unsigned byte
def LDB : E2KInst<0x64, (outs), (ins), "ldb", [0, 2, 3, 5]>;

// load unsigned half-word
def LDH : E2KInst<0x65, (outs), (ins), "ldh", [0, 2, 3, 5]>;

// count number of leading zeroes
def LZCNTS : E2KEXTInst<0x64, 0xC0, (outs), (ins), "lzcnts", [1, 4]>;
def LZCNTD : E2KEXTInst<0x65, 0xC0, (outs), (ins), "lzcntd", [1, 4]>;

// load unsigned word
def LDW : E2KInst<0x66, (outs), (ins), "ldw", [0, 2, 3, 5]>;

// load unsigned double-word
def LDD : E2KInst<0x67, (outs), (ins), "ldd", [0, 2, 3, 5]>;

// count of bits set to one
def POPCNTS : E2KEXTInst<0x66, 0xC0, (outs), (ins), "popcnts", [1, 4]>;
def POPCNTD : E2KEXTInst<0x67, 0xC0, (outs), (ins), "popcntd", [1, 4]>;

def LDCSB : E2KInst<0x68, (outs), (ins), "ldcsb", [0, 2, 3, 5]>;

def VFBGV : E2KInst<0x68, (outs), (ins), "vfbgv", [1, 4]>;

def LDCSH : E2KInst<0x69, (outs), (ins), "ldcsh", [0, 2, 3, 5]>;

def MODBGV : E2KEXTInst<0x69, 0xC0, (outs), (ins), "modbgv", [1, 4]>;

def LDCSW : E2KInst<0x6a, (outs), (ins), "ldcsw", [0, 2, 3, 5]>;

def MKFSW : E2KInst<0x6a, (outs), (ins), "mkfsw", [1, 4]>;

def LDCSD : E2KInst<0x6b, (outs), (ins), "ldcsd", [0, 2, 3, 5]>;

def LDDSB : E2KInst<0x6c, (outs), (ins), "lddsb", [0, 2, 3, 5]>;

def LDDSH : E2KInst<0x6d, (outs), (ins), "lddsh", [0, 2, 3, 5]>;

def LDDSW : E2KInst<0x6e, (outs), (ins), "lddsw", [0, 2, 3, 5]>;

def LDDSD : E2KInst<0x6f, (outs), (ins), "lddsd", [0, 2, 3, 5]>;

def LDESB : E2KInst<0x70, (outs), (ins), "ldesb", [0, 2, 3, 5]>;

def LDESH : E2KInst<0x71, (outs), (ins), "ldesh", [0, 2, 3, 5]>;

def LDESW : E2KInst<0x72, (outs), (ins), "ldesw", [0, 2, 3, 5]>;

def LDESD : E2KInst<0x73, (outs), (ins), "ldesd", [0, 2, 3, 5]>;

def LDFSB : E2KInst<0x74, (outs), (ins), "ldfsb", [0, 2, 3, 5]>;

def LDFSH : E2KInst<0x75, (outs), (ins), "ldfsh", [0, 2, 3, 5]>;

def LDFSW : E2KInst<0x76, (outs), (ins), "ldfsw", [0, 2, 3, 5]>;

def LDFSD : E2KInst<0x77, (outs), (ins), "ldfsd", [0, 2, 3, 5]>;

def LDGSB : E2KInst<0x78, (outs), (ins), "ldgsb", [0, 2, 3, 5]>;

def LDGSH : E2KInst<0x79, (outs), (ins), "ldgsh", [0, 2, 3, 5]>;

def LDGSW : E2KInst<0x7a, (outs), (ins), "ldgsw", [0, 2, 3, 5]>;

def LDGSD : E2KInst<0x7b, (outs), (ins), "ldgsd", [0, 2, 3, 5]>;

def LDSSB : E2KInst<0x7c, (outs), (ins), "ldssb", [0, 2, 3, 5]>;

def LDSSH : E2KInst<0x7d, (outs), (ins), "ldssh", [0, 2, 3, 5]>;

def LDSSW : E2KInst<0x7e, (outs), (ins), "ldssw", [0, 2, 3, 5]>;

def LDSSD : E2KInst<0x7f, (outs), (ins), "ldssw", [0, 2, 3, 5]>;

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
  def CALL : InstSP<(outs), (ins calltarget:$disp, variable_ops),
                    "call $disp",
                    [],
                    IIC_jmp_or_call> {
    bits<30> disp;
    let op = 1;
    let Inst{29-0} = disp;
  }

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
def : Pat<(call tglobaladdr:$dst),
          (CALL tglobaladdr:$dst)>;
def : Pat<(call texternalsym:$dst),
          (CALL texternalsym:$dst)>;

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
