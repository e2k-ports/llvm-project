//===-- E2KRegisterInfo.td - E2K Register defs ---------*- tablegen -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
//  Declarations that describe the E2K register file
//===----------------------------------------------------------------------===//

class E2KReg<bits<16> Enc, string n, list<Register> subregs = []> : Register<n> {
  let HWEncoding = Enc;
  let Namespace = "E2K";
  let SubRegs = subregs;
}

let Namespace = "E2K" in {
  def sub_32 : SubRegIndex<32>;
  def sub_64 : SubRegIndex<64>;
}

// current window registers, 0-63

foreach Index = 0-63 in {
  // current window registers, SR (single register)
  def R#Index : E2KReg<Index, "r"#Index>, DwarfRegNum<[Index]>;
  // current window registers, DR (double register)
  let SubRegIndices = [sub_32] in {
    def DR#Index : E2KReg<Index, "dr"#Index, [!cast<E2KReg>("R"#Index)]>, DwarfRegNum<[Index]>;
  }
  // current window registers, QR (quad register)
  let SubRegIndices = [sub_64] in {
    def QR#Index : E2KReg<Index, "qr"#Index, [!cast<E2KReg>("DR"#Index)]>, DwarfRegNum<[Index]>;
  }
  def QPR#Index : E2KReg<Index, "qpr"#Index>, DwarfRegNum<[Index]>;
}

def RegSR : RegisterClass<"E2K", [i32], 32, (sequence "R%u", 0, 63)>;
def RegDR : RegisterClass<"E2K", [i64], 64, (sequence "DR%u", 0, 63)>;
def RegQR : RegisterClass<"E2K", [i128], 128, (sequence "QR%u", 0, 63)>;
def RegQPR : RegisterClass<"E2K", [i128], 128, (sequence "QPR%u", 0, 63)>;

// based registers, 0-127

foreach Index = 0-127 in {
  // based registers, SR (single register)
  def B#Index : E2KReg<Index, "b["#Index#"]">, DwarfRegNum<[Index]>;
  // based registers, DR (double register)
  let SubRegIndices = [sub_32] in {
    def DB#Index : E2KReg<Index, "db["#Index#"]", [!cast<E2KReg>("B"#Index)]>, DwarfRegNum<[Index]>;
  }
  // based registers, QR (quad register)
  let SubRegIndices = [sub_64] in {
    def QB#Index : E2KReg<Index, "qb["#Index#"]", [!cast<E2KReg>("DB"#Index)]>, DwarfRegNum<[Index]>;
  }
  def QPB#Index : E2KReg<Index, "qpb["#Index#"]">, DwarfRegNum<[Index]>;
}

def RegSB : RegisterClass<"E2K", [i32], 32, (sequence "B%u", 0, 127)>;
def RegDB : RegisterClass<"E2K", [i64], 64, (sequence "DB%u", 0, 127)>;
def RegQB : RegisterClass<"E2K", [i128], 128, (sequence "QB%u", 0, 127)>;
def RegQPB : RegisterClass<"E2K", [i128], 128, (sequence "QPB%u", 0, 127)>;

// global registers, 0-31

foreach Index = 0-31 in {
  // global registers, SR (single register)
  def G#Index : E2KReg<Index, "g"#Index>, DwarfRegNum<[Index]>;
  // global registers, DR (double register)
  let SubRegIndices = [sub_32] in {
    def DG#Index : E2KReg<Index, "dg"#Index, [!cast<E2KReg>("G"#Index)]>, DwarfRegNum<[Index]>;
  }
  // global registers, QR (quad register)
  let SubRegIndices = [sub_64] in {
    def QG#Index : E2KReg<Index, "qg"#Index, [!cast<E2KReg>("DG"#Index)]>, DwarfRegNum<[Index]>;
  }
  def QPG#Index : E2KReg<Index, "qpg"#Index>, DwarfRegNum<[Index]>;
}

def RegSG : RegisterClass<"E2K", [i32], 32, (sequence "G%u", 0, 31)>;
def RegDG : RegisterClass<"E2K", [i64], 64, (sequence "DG%u", 0, 31)>;
def RegQG : RegisterClass<"E2K", [i128], 128, (sequence "QG%u", 0, 31)>;
def RegQPG : RegisterClass<"E2K", [i128], 128, (sequence "QPG%u", 0, 31)>;

def RegS : RegisterClass<"E2K", [i32], 32, (add RegSR, RegSB, RegSG)>;
def RegD : RegisterClass<"E2K", [i64], 64, (add RegDR, RegDB, RegDG)>;
def RegQ : RegisterClass<"E2K", [i128], 128, (add RegQR, RegQB, RegQG)>;
def RegQP : RegisterClass<"E2K", [i128], 128, (add RegQPR, RegQPB, RegQPG)>;

def SRC1S : RegisterClass<"E2K", [i32], 32, (add RegS)>;
def SRC2S : RegisterClass<"E2K", [i32], 32, (add RegS)>;
def SRC3S : RegisterClass<"E2K", [i32], 32, (add RegS)>;
def DST1S : RegisterClass<"E2K", [i32], 32, (add RegS)>;
def SRC1D : RegisterClass<"E2K", [i64], 64, (add RegD)>;
def SRC2D : RegisterClass<"E2K", [i64], 64, (add RegD)>;
def SRC3D : RegisterClass<"E2K", [i64], 64, (add RegD)>;
def DST1D : RegisterClass<"E2K", [i64], 64, (add RegD)>;
def SRC1Q : RegisterClass<"E2K", [i128], 128, (add RegQ)>;
def SRC2Q : RegisterClass<"E2K", [i128], 128, (add RegQ)>;
def SRC3Q : RegisterClass<"E2K", [i128], 128, (add RegQ)>;
def DST1Q : RegisterClass<"E2K", [i128], 128, (add RegQ)>;
def SRC1QP : RegisterClass<"E2K", [i128], 128, (add RegQP)>;
def SRC2QP : RegisterClass<"E2K", [i128], 128, (add RegQP)>;
def SRC3QP : RegisterClass<"E2K", [i128], 128, (add RegQP)>;
def DST1QP : RegisterClass<"E2K", [i128], 128, (add RegQP)>;

// predicate registers, 0-31

foreach Index = 0-31 in {
  def PRED#Index : E2KReg<Index, "pred"#Index>, DwarfRegNum<[Index]>;
}

def PRED : RegisterClass<"E2K", [i1], 8, (sequence "PRED%u", 0, 31)>;

// control registers, 1-3

foreach Index = 1-3 in {
  def CTPR#Index : E2KReg<Index, "ctpr"#Index>, DwarfRegNum<[Index]>;
}

def CTPR : RegisterClass<"E2K", [i128], 128, (sequence "CTPR%u", 1, 3)>;

// AASTI & AAD, used by some commands like ldaa** and staa**

foreach Index = 0-31 in {
  def AASTI#Index : E2KReg<Index, "aasti"#Index>, DwarfRegNum<[Index]>;
}

def AASTI : RegisterClass<"E2K", [i128], 128, (sequence "AASTI%u", 0, 31)>;

foreach Index = 0-31 in {
  def AAD#Index : E2KReg<Index, "aad"#Index>, DwarfRegNum<[Index]>;
}

def AAD : RegisterClass<"E2K", [i128], 128, (sequence "AAD%u", 0, 31)>;

// current window description
def WD: E2KReg<0, "WD">;

// moving base
def BR: E2KReg<0, "BR">;

// current type
def TR: E2KReg<0, "TR">;

// processor state
def PSR: E2KReg<0, "PSR">;

// instruction pointer
def IP: E2KReg<0, "IP">;

// user processor state
def UPSR: E2KReg<0, "UPSR">;

// next instruction pointer
def NIP: E2KReg<0, "NIP">;

// packed floating point flags
def PFPFR: E2KReg<0, "PFPFR">;

// floating point flags
def FPFR: E2KReg<0, "FPFR">;

// loop status
def LSR: E2KReg<0, "LSR">;

// loop counter
def ILCR: E2KReg<0, "ILCR">;

// user stack pointer
def USD: E2KReg<0, "USD">;
def USD_HI: E2KReg<0, "USD.HI">;
def USD_LO: E2KReg<0, "USD.LO">;

// code segment
def CUD: E2KReg<0, "CUD">;
def CUD_HI: E2KReg<0, "CUD.HI">;
def CUD_LO: E2KReg<0, "CUD.LO">;

// data segment
def GD: E2KReg<0, "GD">;
def GD_HI: E2KReg<0, "GD_HI">;
def GD_LO: E2KReg<0, "GD_LO">;

// current module type
def TSD: E2KReg<0, "TSD">;

// current unit index
def CUIR: E2KReg<0, "CUIR">;

// procedure stack hardware top pointer
def PSHTP: E2KReg<0, "PSHTP">;

// procedure chain stack hardware top pointer
def PCSHTP: E2KReg<0, "PCSHTP">;

// procedure stack pointer
def PSP: E2KReg<0, "PSP">;
def PSP_HI: E2KReg<0, "PSP.HI">;
def PSP_LO: E2KReg<0, "PSP.LO">;

// user stack base pointer
def USBR: E2KReg<0, "USBR">;

// more registers are listed in:
// /usr/include/sys/user.h (struct user_regs_struct)
// /usr/include/bits/sigcontext.h (struct sigcontext & struct extra_ucontext)
// /usr/include/asm/kvm.h (struct kvm_regs)
// but they are barely documented

// used in DST1 encoding

def TST: E2KReg<0, "TST">;
def TC: E2KReg<0, "TC">;
def TCD: E2KReg<0, "TCD">;
def EMPTY_LO: E2KReg<0, "EMPTY_LO">;
def EMPTY_HI: E2KReg<0, "EMPTY_HI">;

def CORE_MODE: E2KReg<0, "CORE_MODE">;
def CWD: E2KReg<0, "CWD">;
def SBR: E2KReg<0, "SBR">;
def CUTD: E2KReg<0, "CUTD">;
def EIR: E2KReg<0, "EIR">;
def OSEM: E2KReg<0, "OSEM">;
def OSR0: E2KReg<0, "OSR0">;
def DIBCR: E2KReg<0, "DIBCR">;
def DIMCR: E2KReg<0, "DIMCR">;
def DIBSR: E2KReg<0, "DIBSR">;
def DTCR: E2KReg<0, "DTCR">;
def DTART: E2KReg<0, "DTART">;
def DTARF: E2KReg<0, "DTARF">;
def FPCR: E2KReg<0, "FPCR">;
def FPSR: E2KReg<0, "FPSR">;
def BGR: E2KReg<0, "BGR">;
def IDR: E2KReg<0, "IDR">;
def CLKR: E2KReg<0, "CLKR">;
def RNDPR: E2KReg<0, "RNDPR">;
def SCLKR: E2KReg<0, "SCLKR">;
def SBBP: E2KReg<0, "SBBP">;
def UPSRM: E2KReg<0, "UPSRM">;
def LSR1: E2KReg<0, "LSR1">;
def ILCR1: E2KReg<0, "ILCR1">;

def DIBAR0: E2KReg<0, "DIBAR0">;
def DIBAR1: E2KReg<0, "DIBAR1">;
def DIBAR2: E2KReg<0, "DIBAR2">;
def DIBAR3: E2KReg<0, "DIBAR3">;

def DIMAR0: E2KReg<0, "DIMAR0">;
def DIMAR1: E2KReg<0, "DIMAR1">;

def SCLKM1: E2KReg<0, "SCLKM1">;
def SCLKM2: E2KReg<0, "SCLKM2">;

def CU_HW0: E2KReg<0, "CU_HW0">;
def CU_HW1: E2KReg<0, "CU_HW1">;

def PCSP_HI: E2KReg<0, "PCSP.HI">;
def PCSP_LO: E2KReg<0, "PCSP.LO">;

def OSCUD_HI: E2KReg<0, "OSCUD.HI">;
def OSCUD_LO: E2KReg<0, "OSCUD.LO">;

def OSGD_HI: E2KReg<0, "OSGD.HI">;
def OSGD_LO: E2KReg<0, "OSGD.LO">;

def CS_HI: E2KReg<0, "CS.HI">;
def CS_LO: E2KReg<0, "CS.LO">;

def DS_HI: E2KReg<0, "DS.HI">;
def DS_LO: E2KReg<0, "DS.LO">;

def ES_HI: E2KReg<0, "ES.HI">;
def ES_LO: E2KReg<0, "ES.LO">;

def FS_HI: E2KReg<0, "FS.HI">;
def FS_LO: E2KReg<0, "FS.LO">;

def GS_HI: E2KReg<0, "GS.HI">;
def GS_LO: E2KReg<0, "GS.LO">;

def SS_HI: E2KReg<0, "SS.HI">;
def SS_LO: E2KReg<0, "SS.LO">;

def CR0_HI: E2KReg<0, "CR0.HI">;
def CR0_LO: E2KReg<0, "CR0.LO">;

def CR1_HI: E2KReg<0, "CR1.HI">;
def CR1_LO: E2KReg<0, "CR1.LO">;

def TIR_HI: E2KReg<0, "TIR_HI">;
def TIR_LO: E2KReg<0, "TIR_LO">;

def RPR_HI: E2KReg<0, "RPR_HI">;
def RPR_LO: E2KReg<0, "RPR_LO">;


// FIXME : figure out all the possible arguments for rws/rwd/rrs/rrd, also sizes of these registers
def RegSTATE : RegisterClass<"E2K", [i64], 64,
    (add PSR, WD, CORE_MODE, CWD, PSP_HI,
    PSP_LO, PSHTP, PCSP_HI, PCSP_LO,
    PCSHTP, CTPR1, CTPR2, CTPR3,
    SBR,
    CUTD, EIR,CUIR, OSCUD_HI, OSCUD_LO,
    OSGD_HI, OSGD_LO, OSEM, USD_HI, USD_LO, OSR0,
    CUD_HI, CUD_LO, GD_HI, GD_LO,CS_HI, CS_LO, DS_HI, DS_LO,
    ES_HI, ES_LO, FS_HI, FS_LO,GS_HI, GS_LO, SS_HI, SS_LO,
    DIBCR, DIMCR, DIBSR, DTCR,
    DIBAR0, DIBAR1, DIBAR2, DIBAR3, DIMAR0, DIMAR1, DTARF, DTART,
    CR0_HI, CR0_LO, CR1_HI, CR1_LO,
    SCLKM1, SCLKM2,
    CU_HW0, CU_HW1,
    UPSR, IP, NIP, LSR, PFPFR, FPCR, FPSR, ILCR,
    BR, BGR, IDR,
    CLKR, RNDPR, SCLKR,
    TIR_HI, TIR_LO,
    RPR_LO, SBBP, RPR_HI,
    UPSRM, LSR1, ILCR1)>;
