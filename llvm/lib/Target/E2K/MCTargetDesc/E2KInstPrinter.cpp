//===-- E2KInstPrinter.cpp - Convert E2K MCInst to assembly syntax -----==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This class prints an E2K MCInst to a .s file.
//
//===----------------------------------------------------------------------===//

#include "E2KInstPrinter.h"
#include "E2K.h"
#include "MCTargetDesc/E2KMCTargetDesc.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/Support/raw_ostream.h"
using namespace llvm;

#define DEBUG_TYPE "asm-printer"

//// The generated AsmMatcher E2KGenAsmWriter uses "E2K" as the target
//// namespace. But E2K backend uses "E2K" as its namespace.
//namespace llvm {
//namespace E2K {
//  using namespace SP;
//}
//}

#define GET_INSTRUCTION_NAME
#define PRINT_ALIAS_INSTR
#include "E2KGenAsmWriter.inc"


void E2KInstPrinter::printRegName(raw_ostream &OS, unsigned RegNo) const
{
  OS << '%' << StringRef(getRegisterName(RegNo)).lower();
}

void E2KInstPrinter::printInst(const MCInst *MI, uint64_t Address,
                                 StringRef Annot, const MCSubtargetInfo &STI,
                                 raw_ostream &O) {

  if (MI->getOpcode() == E2K::BUNDLE) {
    for (unsigned i = 0; i < MI->getNumOperands(); ++i) {
      auto SubInst = MI->getOperand(i).getInst();
      O << '\n';
      printInst(SubInst, Address, Annot, STI, O);
    }
    return;
  }

  if (!printAliasInstr(MI, Address, STI, O) &&
      !printE2KAliasInstr(MI, STI, O))
    printInstruction(MI, Address, STI, O);

  if ((MI->getFlags() & 0x80000000) == 0x80000000) {
    unsigned ops = MI->getNumOperands();
    assert(ops > 0);
    auto pred = MI->getOperand(ops - 1);

    O << " ? ";
    printRegName(O, pred.getReg());
  }

  printAnnotation(O, Annot);
}

bool E2KInstPrinter::printE2KAliasInstr(const MCInst *MI,
                                            const MCSubtargetInfo &STI,
                                            raw_ostream &O) {
  switch (MI->getOpcode()) {
  default: return false;
  }
}

void E2KInstPrinter::printOperand(const MCInst *MI, int opNum,
                                    const MCSubtargetInfo &STI,
                                    raw_ostream &O) {
  const MCOperand &MO = MI->getOperand (opNum);

  if (MO.isReg()) {
    printRegName(O, MO.getReg());
    return ;
  }

  if (MO.isImm()) {
    switch (MI->getOpcode()) {
      default: {
      uint64_t value = MO.getImm();
      uint64_t flags = MI->getFlags();
      flags &= ~0x80000000;

      unsigned prefix = flags & 0b111;
      unsigned opnum = flags >> 3;

      if (flags && opnum == opNum) {

        switch (prefix) {
        case E2K::F16SLO:
        case E2K::F16SHI:
          O << "_f16s ";
          break;
        case E2K::F32S:
          O << "_f32s ";
          break;
        case E2K::F64:
          O << "_f64 ";
          break;
        }
      }
      O << format("0x%llx", value);
    }
        return;
    }
  }

  assert(MO.isExpr() && "Unknown operand kind in printOperand");
  MO.getExpr()->print(O, &MAI);
}

template <unsigned N>
void E2KInstPrinter::printUImmOperand(const MCInst *MI, int OpNum,
                                          raw_ostream &O) {
  int64_t Value = MI->getOperand(OpNum).getImm();
  assert(isUInt<N>(Value) && "Invalid uimm argument");
  bool print_hex = true;
  uint32_t opcode = MI->getOpcode();
  if (opcode == E2K::IPD || opcode == E2K::NOP)
    print_hex = false;

  O << markup("<imm:");
  if (print_hex)
    O << format("0x%x", Value);
  else
    O << Value;
  O << markup(">");
}

template <unsigned N>
void E2KInstPrinter::printSImmOperand(const MCInst *MI, int OpNum,
                                          raw_ostream &O) {
  int64_t Value = MI->getOperand(OpNum).getImm();
  assert(isInt<N>(Value) && "Invalid simm argument");
  bool print_hex = true;
  uint32_t opcode = MI->getOpcode();
  if (opcode == E2K::IPD || opcode == E2K::NOP)
    print_hex = false;

  O << markup("<imm:");
  if (print_hex)
    O << format("0x%x", Value);
  else
    O << Value;
  O << markup(">");
}

void E2KInstPrinter::printU8Imm(const MCInst *MI, int opNum,
                                const MCSubtargetInfo &STI,
                                raw_ostream &O) {
  printUImmOperand<8>(MI, opNum, O);
}

void E2KInstPrinter::printS32Imm(const MCInst *MI, int opNum,
                                const MCSubtargetInfo &STI,
                                raw_ostream &O) {
  printSImmOperand<32>(MI, opNum, O);
}

void E2KInstPrinter::printCTCOND(const MCInst *MI, int opNum,
                                const MCSubtargetInfo &STI,
                                raw_ostream &O) {
  uint32_t CondType = MI->getOperand(opNum).getImm();
  uint32_t RegNo = MI->getOperand(opNum + 1).getReg();

  O << E2KCondCodeToString(static_cast<E2KCC::CondCodes>(CondType), "%" + StringRef(getRegisterName(RegNo)).lower());
}

void E2KInstPrinter::printMAS(const MCInst *MI, int opNum,
                                 const MCSubtargetInfo &STI,
                                 raw_ostream &O) {
  uint32_t mas = MI->getOperand(opNum).getImm();

  if (mas) {
    O << ", mas = " << format("0x%x", mas);
  }
}
