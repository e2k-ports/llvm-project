//===-- E2KAsmParser.cpp - Parse E2K assembly to MCInst instructions --===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/E2KMCExpr.h"
#include "MCTargetDesc/E2KMCTargetDesc.h"
#include "TargetInfo/E2KTargetInfo.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/Triple.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCObjectFileInfo.h"
#include "llvm/MC/MCParser/MCAsmLexer.h"
#include "llvm/MC/MCParser/MCAsmParser.h"
#include "llvm/MC/MCParser/MCParsedAsmOperand.h"
#include "llvm/MC/MCParser/MCTargetAsmParser.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/SMLoc.h"
#include "llvm/Support/raw_ostream.h"
#include <algorithm>
#include <cassert>
#include <cstdint>
#include <memory>

using namespace llvm;

//// The generated AsmMatcher E2KGenAsmMatcher uses "E2K" as the target
//// namespace. But E2K backend uses "E2K" as its namespace.
//namespace llvm {
//namespace E2K {
//
//    using namespace E2K;
//
//} // end namespace E2K
//} // end namespace llvm

namespace {

class E2KOperand;

class E2KAsmParser : public MCTargetAsmParser {
  MCAsmParser &Parser;

  enum class TailRelocKind { Load_GOT, Add_TLS, Load_TLS, Call_TLS };

  /// @name Auto-generated Match Functions
  /// {

#define GET_ASSEMBLER_HEADER
#include "E2KGenAsmMatcher.inc"

  /// }

  // public interface of the MCTargetAsmParser.
  bool MatchAndEmitInstruction(SMLoc IDLoc, unsigned &Opcode,
                               OperandVector &Operands, MCStreamer &Out,
                               uint64_t &ErrorInfo,
                               bool MatchingInlineAsm) override;
  bool parseRegister(MCRegister &RegNo, SMLoc &StartLoc,
                     SMLoc &EndLoc) override;
  OperandMatchResultTy tryParseRegister(MCRegister &RegNo, SMLoc &StartLoc,
                                        SMLoc &EndLoc) override;
  bool ParseInstruction(ParseInstructionInfo &Info, StringRef Name,
                        SMLoc NameLoc, OperandVector &Operands) override;
  bool ParseDirective(AsmToken DirectiveID) override;

  unsigned validateTargetOperandClass(MCParsedAsmOperand &Op,
                                      unsigned Kind) override;

  // Custom parse functions for E2K specific operands.
  OperandMatchResultTy parseMEMOperand(OperandVector &Operands);

  OperandMatchResultTy parseMembarTag(OperandVector &Operands);

  template <TailRelocKind Kind>
  OperandMatchResultTy parseTailRelocSym(OperandVector &Operands);

  template <unsigned N>
  OperandMatchResultTy parseShiftAmtImm(OperandVector &Operands);

  OperandMatchResultTy parseCallTarget(OperandVector &Operands);

  OperandMatchResultTy parseOperand(OperandVector &Operands, StringRef Name);

  OperandMatchResultTy
  parseE2KAsmOperand(std::unique_ptr<E2KOperand> &Operand,
                       bool isCall = false);

  OperandMatchResultTy parseBranchModifiers(OperandVector &Operands);

  // Helper function for dealing with %lo / %hi in PIC mode.
  const E2KMCExpr *adjustPICRelocation(E2KMCExpr::VariantKind VK,
                                         const MCExpr *subExpr);

  // returns true if Tok is matched to a register and returns register in RegNo.
  bool matchRegisterName(const AsmToken &Tok, MCRegister &RegNo,
                         unsigned &RegKind);

  bool matchE2KAsmModifiers(const MCExpr *&EVal, SMLoc &EndLoc);

  bool is64Bit() const {
    return getSTI().getTargetTriple().getArch() == Triple::e2k64;
  }

  bool expandSET(MCInst &Inst, SMLoc IDLoc,
                 SmallVectorImpl<MCInst> &Instructions);

  SMLoc getLoc() const { return getParser().getTok().getLoc(); }

public:
  E2KAsmParser(const MCSubtargetInfo &sti, MCAsmParser &parser,
                const MCInstrInfo &MII,
                const MCTargetOptions &Options)
      : MCTargetAsmParser(Options, sti, MII), Parser(parser) {
    Parser.addAliasForDirective(".half", ".2byte");
    Parser.addAliasForDirective(".uahalf", ".2byte");
    Parser.addAliasForDirective(".word", ".4byte");
    Parser.addAliasForDirective(".uaword", ".4byte");
    Parser.addAliasForDirective(".nword", is64Bit() ? ".8byte" : ".4byte");
    if (is64Bit())
      Parser.addAliasForDirective(".xword", ".8byte");

    // Initialize the set of available features.
    setAvailableFeatures(ComputeAvailableFeatures(getSTI().getFeatureBits()));
  }
};

} // end anonymous namespace


namespace {

/// E2KOperand - Instances of this class represent a parsed E2K machine
/// instruction.
class E2KOperand : public MCParsedAsmOperand {
public:
  enum RegisterKind {
    rk_None,
    rk_IntReg,
    rk_IntPairReg,
    rk_FloatReg,
    rk_DoubleReg,
    rk_QuadReg,
    rk_CoprocReg,
    rk_CoprocPairReg,
    rk_Special,
  };

private:
  enum KindTy {
    k_Token,
    k_Register,
    k_Immediate,
    k_MemoryReg,
    k_MemoryImm
  } Kind;

  SMLoc StartLoc, EndLoc;

  struct Token {
    const char *Data;
    unsigned Length;
  };

  struct RegOp {
    unsigned RegNum;
    RegisterKind Kind;
  };

  struct ImmOp {
    const MCExpr *Val;
  };

  struct MemOp {
    unsigned Base;
    unsigned OffsetReg;
    const MCExpr *Off;
  };

  union {
    struct Token Tok;
    struct RegOp Reg;
    struct ImmOp Imm;
    struct MemOp Mem;
  };

public:
  E2KOperand(KindTy K) : Kind(K) {}

  bool isToken() const override { return Kind == k_Token; }
  bool isReg() const override { return Kind == k_Register; }
  bool isImm() const override { return Kind == k_Immediate; }
  bool isMem() const override { return isMEMrr() || isMEMri(); }
  bool isMEMrr() const { return Kind == k_MemoryReg; }
  bool isMEMri() const { return Kind == k_MemoryImm; }
  bool isMembarTag() const { return Kind == k_Immediate; }
  bool isTailRelocSym() const { return Kind == k_Immediate; }

  bool isCallTarget() const {
    if (!isImm())
      return false;

    if (const MCConstantExpr *CE = dyn_cast<MCConstantExpr>(Imm.Val))
      return CE->getValue() % 4 == 0;

    return true;
  }

  static bool isImmUnsignedi8Value(uint64_t Value) {
    return isUInt<8>(Value) || isInt<8>(Value);
  }

  static bool isImmSignedi32Value(uint64_t Value) {
    return isUInt<32>(Value) || isInt<32>(Value);
  }

  bool isImmUnsignedi8() const {
    if (!isImm()) return false;
    // If this isn't a constant expr, just assume it fits and let relaxation
    // handle it.
    const MCConstantExpr *CE = dyn_cast<MCConstantExpr>(getImm());
    if (!CE) return true;
    return isImmUnsignedi8Value(CE->getValue());
  }

  bool isImmSignedi32() const {
    if (!isImm()) return false;
    // If this isn't a constant expr, just assume it fits and let relaxation
    // handle it.
    const MCConstantExpr *CE = dyn_cast<MCConstantExpr>(getImm());
    if (!CE) return true;
    return isImmSignedi32Value(CE->getValue());
  }

  bool isShiftAmtImm5() const {
    if (!isImm())
      return false;

    if (const MCConstantExpr *CE = dyn_cast<MCConstantExpr>(Imm.Val))
      return isUInt<5>(CE->getValue());

    return false;
  }

  bool isShiftAmtImm6() const {
    if (!isImm())
      return false;

    if (const MCConstantExpr *CE = dyn_cast<MCConstantExpr>(Imm.Val))
      return isUInt<6>(CE->getValue());

    return false;
  }

  bool isIntReg() const {
    return (Kind == k_Register && Reg.Kind == rk_IntReg);
  }

  bool isFloatReg() const {
    return (Kind == k_Register && Reg.Kind == rk_FloatReg);
  }

  bool isFloatOrDoubleReg() const {
    return (Kind == k_Register && (Reg.Kind == rk_FloatReg
                                   || Reg.Kind == rk_DoubleReg));
  }

  bool isCoprocReg() const {
    return (Kind == k_Register && Reg.Kind == rk_CoprocReg);
  }

  StringRef getToken() const {
    assert(Kind == k_Token && "Invalid access!");
    return StringRef(Tok.Data, Tok.Length);
  }

  unsigned getReg() const override {
    assert((Kind == k_Register) && "Invalid access!");
    return Reg.RegNum;
  }

  const MCExpr *getImm() const {
    assert((Kind == k_Immediate) && "Invalid access!");
    return Imm.Val;
  }

  unsigned getMemBase() const {
    assert((Kind == k_MemoryReg || Kind == k_MemoryImm) && "Invalid access!");
    return Mem.Base;
  }

  unsigned getMemOffsetReg() const {
    assert((Kind == k_MemoryReg) && "Invalid access!");
    return Mem.OffsetReg;
  }

  const MCExpr *getMemOff() const {
    assert((Kind == k_MemoryImm) && "Invalid access!");
    return Mem.Off;
  }

  /// getStartLoc - Get the location of the first token of this operand.
  SMLoc getStartLoc() const override {
    return StartLoc;
  }
  /// getEndLoc - Get the location of the last token of this operand.
  SMLoc getEndLoc() const override {
    return EndLoc;
  }

  void print(raw_ostream &OS) const override {
    switch (Kind) {
    case k_Token:     OS << "Token: " << getToken() << "\n"; break;
    case k_Register:  OS << "Reg: #" << getReg() << "\n"; break;
    case k_Immediate: OS << "Imm: " << getImm() << "\n"; break;
    case k_MemoryReg: OS << "Mem: " << getMemBase() << "+"
                         << getMemOffsetReg() << "\n"; break;
    case k_MemoryImm: assert(getMemOff() != nullptr);
      OS << "Mem: " << getMemBase()
         << "+" << *getMemOff()
         << "\n"; break;
    }
  }

  void addRegOperands(MCInst &Inst, unsigned N) const {
    assert(N == 1 && "Invalid number of operands!");
    Inst.addOperand(MCOperand::createReg(getReg()));
  }

  void addImmOperands(MCInst &Inst, unsigned N) const {
    assert(N == 1 && "Invalid number of operands!");
    const MCExpr *Expr = getImm();
    addExpr(Inst, Expr);
  }

  void addShiftAmtImm5Operands(MCInst &Inst, unsigned N) const {
    assert(N == 1 && "Invalid number of operands!");
    addExpr(Inst, getImm());
  }
  void addShiftAmtImm6Operands(MCInst &Inst, unsigned N) const {
    assert(N == 1 && "Invalid number of operands!");
    addExpr(Inst, getImm());
  }

  void addExpr(MCInst &Inst, const MCExpr *Expr) const{
    // Add as immediate when possible.  Null MCExpr = 0.
    if (!Expr)
      Inst.addOperand(MCOperand::createImm(0));
    else if (const MCConstantExpr *CE = dyn_cast<MCConstantExpr>(Expr))
      Inst.addOperand(MCOperand::createImm(CE->getValue()));
    else
      Inst.addOperand(MCOperand::createExpr(Expr));
  }

  void addMEMrrOperands(MCInst &Inst, unsigned N) const {
    assert(N == 2 && "Invalid number of operands!");

    Inst.addOperand(MCOperand::createReg(getMemBase()));

    assert(getMemOffsetReg() != 0 && "Invalid offset");
    Inst.addOperand(MCOperand::createReg(getMemOffsetReg()));
  }

  void addMEMriOperands(MCInst &Inst, unsigned N) const {
    assert(N == 2 && "Invalid number of operands!");

    Inst.addOperand(MCOperand::createReg(getMemBase()));

    const MCExpr *Expr = getMemOff();
    addExpr(Inst, Expr);
  }

  void addMembarTagOperands(MCInst &Inst, unsigned N) const {
    assert(N == 1 && "Invalid number of operands!");
    const MCExpr *Expr = getImm();
    addExpr(Inst, Expr);
  }

  void addCallTargetOperands(MCInst &Inst, unsigned N) const {
    assert(N == 1 && "Invalid number of operands!");
    addExpr(Inst, getImm());
  }

  void addTailRelocSymOperands(MCInst &Inst, unsigned N) const {
    assert(N == 1 && "Invalid number of operands!");
    addExpr(Inst, getImm());
  }

  static std::unique_ptr<E2KOperand> CreateToken(StringRef Str, SMLoc S) {
    auto Op = std::make_unique<E2KOperand>(k_Token);
    Op->Tok.Data = Str.data();
    Op->Tok.Length = Str.size();
    Op->StartLoc = S;
    Op->EndLoc = S;
    return Op;
  }

  static std::unique_ptr<E2KOperand> CreateReg(unsigned RegNum, unsigned Kind,
                                                 SMLoc S, SMLoc E) {
    auto Op = std::make_unique<E2KOperand>(k_Register);
    Op->Reg.RegNum = RegNum;
    Op->Reg.Kind   = (E2KOperand::RegisterKind)Kind;
    Op->StartLoc = S;
    Op->EndLoc = E;
    return Op;
  }

  static std::unique_ptr<E2KOperand> CreateImm(const MCExpr *Val, SMLoc S,
                                                 SMLoc E) {
    auto Op = std::make_unique<E2KOperand>(k_Immediate);
    Op->Imm.Val = Val;
    Op->StartLoc = S;
    Op->EndLoc = E;
    return Op;
  }

  static bool MorphToDoubleReg(E2KOperand &Op) {
    unsigned Reg = Op.getReg();
    assert(Op.Reg.Kind == rk_FloatReg);
    unsigned regIdx = Reg - E2K::R0;
    if (regIdx % 2 || regIdx > 31)
      return false;
    Op.Reg.RegNum = E2K::R0 + regIdx / 2;
    Op.Reg.Kind = rk_DoubleReg;
    return true;
  }

  static bool MorphToQuadReg(E2KOperand &Op) {
    unsigned Reg = Op.getReg();
    unsigned regIdx = 0;
    switch (Op.Reg.Kind) {
    default: llvm_unreachable("Unexpected register kind!");
    case rk_FloatReg:
      regIdx = Reg - E2K::R0;
      if (regIdx % 4 || regIdx > 31)
        return false;
      Reg = E2K::QR0 + regIdx / 4;
      break;
    case rk_DoubleReg:
      regIdx =  Reg - E2K::DR0;
      if (regIdx % 2 || regIdx > 31)
        return false;
      Reg = E2K::QR0 + regIdx / 2;
      break;
    }
    Op.Reg.RegNum = Reg;
    Op.Reg.Kind = rk_QuadReg;
    return true;
  }


  static std::unique_ptr<E2KOperand>
  MorphToMEMrr(unsigned Base, std::unique_ptr<E2KOperand> Op) {
    unsigned offsetReg = Op->getReg();
    Op->Kind = k_MemoryReg;
    Op->Mem.Base = Base;
    Op->Mem.OffsetReg = offsetReg;
    Op->Mem.Off = nullptr;
    return Op;
  }

  static std::unique_ptr<E2KOperand>
  CreateMEMr(unsigned Base, SMLoc S, SMLoc E) {
    auto Op = std::make_unique<E2KOperand>(k_MemoryReg);
    Op->Mem.Base = Base;
    Op->Mem.OffsetReg = E2K::G0;  // always 0
    Op->Mem.Off = nullptr;
    Op->StartLoc = S;
    Op->EndLoc = E;
    return Op;
  }

  static std::unique_ptr<E2KOperand>
  MorphToMEMri(unsigned Base, std::unique_ptr<E2KOperand> Op) {
    const MCExpr *Imm  = Op->getImm();
    Op->Kind = k_MemoryImm;
    Op->Mem.Base = Base;
    Op->Mem.OffsetReg = 0;
    Op->Mem.Off = Imm;
    return Op;
  }
};

} // end anonymous namespace

bool E2KAsmParser::MatchAndEmitInstruction(SMLoc IDLoc, unsigned &Opcode,
                                             OperandVector &Operands,
                                             MCStreamer &Out,
                                             uint64_t &ErrorInfo,
                                             bool MatchingInlineAsm) {
  llvm_unreachable("Implement any new match types added!");
}

bool E2KAsmParser::parseRegister(MCRegister &RegNo, SMLoc &StartLoc,
                                   SMLoc &EndLoc) {
  if (tryParseRegister(RegNo, StartLoc, EndLoc) != MatchOperand_Success)
    return Error(StartLoc, "invalid register name");
  return false;
}

OperandMatchResultTy E2KAsmParser::tryParseRegister(MCRegister &RegNo,
                                                      SMLoc &StartLoc,
                                                      SMLoc &EndLoc) {
  const AsmToken &Tok = Parser.getTok();
  StartLoc = Tok.getLoc();
  EndLoc = Tok.getEndLoc();
  RegNo = 0;
  if (getLexer().getKind() != AsmToken::Percent)
    return MatchOperand_NoMatch;
  Parser.Lex();
  unsigned regKind = E2KOperand::rk_None;
  if (matchRegisterName(Tok, RegNo, regKind)) {
    Parser.Lex();
    return MatchOperand_Success;
  }

  getLexer().UnLex(Tok);
  return MatchOperand_NoMatch;
}

static void applyMnemonicAliases(StringRef &Mnemonic,
                                 const FeatureBitset &Features,
                                 unsigned VariantID);

bool E2KAsmParser::ParseInstruction(ParseInstructionInfo &Info,
                                      StringRef Name, SMLoc NameLoc,
                                      OperandVector &Operands) {

  // First operand in MCInst is instruction mnemonic.
  Operands.push_back(E2KOperand::CreateToken(Name, NameLoc));

  // apply mnemonic aliases, if any, so that we can parse operands correctly.
  applyMnemonicAliases(Name, getAvailableFeatures(), 0);

  if (getLexer().isNot(AsmToken::EndOfStatement)) {
    // Read the first operand.
    if (getLexer().is(AsmToken::Comma)) {
      if (parseBranchModifiers(Operands) != MatchOperand_Success) {
        SMLoc Loc = getLexer().getLoc();
        return Error(Loc, "unexpected token");
      }
    }
    if (parseOperand(Operands, Name) != MatchOperand_Success) {
      SMLoc Loc = getLexer().getLoc();
      return Error(Loc, "unexpected token");
    }

    while (getLexer().is(AsmToken::Comma) || getLexer().is(AsmToken::Plus)) {
      if (getLexer().is(AsmToken::Plus)) {
      // Plus tokens are significant in software_traps (p83, e2kv8.pdf). We must capture them.
        Operands.push_back(E2KOperand::CreateToken("+", Parser.getTok().getLoc()));
      }
      Parser.Lex(); // Eat the comma or plus.
      // Parse and remember the operand.
      if (parseOperand(Operands, Name) != MatchOperand_Success) {
        SMLoc Loc = getLexer().getLoc();
        return Error(Loc, "unexpected token");
      }
    }
  }
  if (getLexer().isNot(AsmToken::EndOfStatement)) {
    SMLoc Loc = getLexer().getLoc();
    return Error(Loc, "unexpected token");
  }
  Parser.Lex(); // Consume the EndOfStatement.
  return false;
}

bool E2KAsmParser::
ParseDirective(AsmToken DirectiveID)
{
  StringRef IDVal = DirectiveID.getString();

  if (IDVal == ".register") {
    // For now, ignore .register directive.
    Parser.eatToEndOfStatement();
    return false;
  }
  if (IDVal == ".proc") {
    // For compatibility, ignore this directive.
    // (It's supposed to be an "optimization" in the Sun assembler)
    Parser.eatToEndOfStatement();
    return false;
  }

  // Let the MC layer to handle other directives.
  return true;
}

OperandMatchResultTy
E2KAsmParser::parseMEMOperand(OperandVector &Operands) {
  SMLoc S, E;

  std::unique_ptr<E2KOperand> LHS;
  if (parseE2KAsmOperand(LHS) != MatchOperand_Success)
    return MatchOperand_NoMatch;

  // Single immediate operand
  if (LHS->isImm()) {
    Operands.push_back(E2KOperand::MorphToMEMri(E2K::G0, std::move(LHS)));
    return MatchOperand_Success;
  }

  if (!LHS->isIntReg()) {
    Error(LHS->getStartLoc(), "invalid register kind for this operand");
    return MatchOperand_ParseFail;
  }

  AsmToken Tok = getLexer().getTok();
  // The plus token may be followed by a register or an immediate value, the
  // minus one is always interpreted as sign for the immediate value
  if (Tok.is(AsmToken::Plus) || Tok.is(AsmToken::Minus)) {
    (void)Parser.parseOptionalToken(AsmToken::Plus);

    std::unique_ptr<E2KOperand> RHS;
    if (parseE2KAsmOperand(RHS) != MatchOperand_Success)
      return MatchOperand_NoMatch;

    if (RHS->isReg() && !RHS->isIntReg()) {
      Error(RHS->getStartLoc(), "invalid register kind for this operand");
      return MatchOperand_ParseFail;
    }

    Operands.push_back(
        RHS->isImm()
            ? E2KOperand::MorphToMEMri(LHS->getReg(), std::move(RHS))
            : E2KOperand::MorphToMEMrr(LHS->getReg(), std::move(RHS)));

    return MatchOperand_Success;
  }

  Operands.push_back(E2KOperand::CreateMEMr(LHS->getReg(), S, E));
  return MatchOperand_Success;
}

template <unsigned N>
OperandMatchResultTy E2KAsmParser::parseShiftAmtImm(OperandVector &Operands) {
  SMLoc S = Parser.getTok().getLoc();
  SMLoc E = SMLoc::getFromPointer(S.getPointer() - 1);

  // This is a register, not an immediate
  if (getLexer().getKind() == AsmToken::Percent)
    return MatchOperand_NoMatch;

  const MCExpr *Expr;
  if (getParser().parseExpression(Expr))
    return MatchOperand_ParseFail;

  const MCConstantExpr *CE = dyn_cast<MCConstantExpr>(Expr);
  if (!CE) {
    Error(S, "constant expression expected");
    return MatchOperand_ParseFail;
  }

  if (!isUInt<N>(CE->getValue())) {
    Error(S, "immediate shift value out of range");
    return MatchOperand_ParseFail;
  }

  Operands.push_back(E2KOperand::CreateImm(Expr, S, E));
  return MatchOperand_Success;
}

template <E2KAsmParser::TailRelocKind Kind>
OperandMatchResultTy
E2KAsmParser::parseTailRelocSym(OperandVector &Operands) {
  SMLoc S = getLoc();
  SMLoc E = SMLoc::getFromPointer(S.getPointer() - 1);

  auto MatchesKind = [](E2KMCExpr::VariantKind VK) -> bool {
    switch (Kind) {
    case TailRelocKind::Load_GOT:
      // Non-TLS relocations on ld (or ldx).
      // ld [%rr + %rr], %rr, %rel(sym)
      return VK == E2KMCExpr::VK_E2K_GOTDATA_OP;
    case TailRelocKind::Add_TLS:
      // TLS relocations on add.
      // add %rr, %rr, %rr, %rel(sym)
      switch (VK) {
      case E2KMCExpr::VK_E2K_TLS_GD_ADD:
      case E2KMCExpr::VK_E2K_TLS_IE_ADD:
      case E2KMCExpr::VK_E2K_TLS_LDM_ADD:
      case E2KMCExpr::VK_E2K_TLS_LDO_ADD:
        return true;
      default:
        return false;
      }
    case TailRelocKind::Load_TLS:
      // TLS relocations on ld (or ldx).
      // ld[x] %addr, %rr, %rel(sym)
      switch (VK) {
      case E2KMCExpr::VK_E2K_TLS_IE_LD:
      case E2KMCExpr::VK_E2K_TLS_IE_LDX:
        return true;
      default:
        return false;
      }
    case TailRelocKind::Call_TLS:
      // TLS relocations on call.
      // call sym, %rel(sym)
      switch (VK) {
      case E2KMCExpr::VK_E2K_TLS_GD_CALL:
      case E2KMCExpr::VK_E2K_TLS_LDM_CALL:
        return true;
      default:
        return false;
      }
    }
    llvm_unreachable("Unhandled E2KAsmParser::TailRelocKind enum");
  };

  if (getLexer().getKind() != AsmToken::Percent) {
    Error(getLoc(), "expected '%' for operand modifier");
    return MatchOperand_ParseFail;
  }

  const AsmToken Tok = Parser.getTok();
  getParser().Lex(); // Eat '%'

  if (getLexer().getKind() != AsmToken::Identifier) {
    Error(getLoc(), "expected valid identifier for operand modifier");
    return MatchOperand_ParseFail;
  }

  StringRef Name = getParser().getTok().getIdentifier();
  E2KMCExpr::VariantKind VK = E2KMCExpr::parseVariantKind(Name);
  if (VK == E2KMCExpr::VK_E2K_None) {
    Error(getLoc(), "invalid operand modifier");
    return MatchOperand_ParseFail;
  }

  if (!MatchesKind(VK)) {
    // Did not match the specified set of relocation types, put '%' back.
    getLexer().UnLex(Tok);
    return MatchOperand_NoMatch;
  }

  Parser.Lex(); // Eat the identifier.
  if (getLexer().getKind() != AsmToken::LParen) {
    Error(getLoc(), "expected '('");
    return MatchOperand_ParseFail;
  }

  getParser().Lex(); // Eat '('
  const MCExpr *SubExpr;
  if (getParser().parseParenExpression(SubExpr, E)) {
    return MatchOperand_ParseFail;
  }

  const MCExpr *Val = adjustPICRelocation(VK, SubExpr);
  Operands.push_back(E2KOperand::CreateImm(Val, S, E));
  return MatchOperand_Success;
}


OperandMatchResultTy
E2KAsmParser::parseOperand(OperandVector &Operands, StringRef Mnemonic) {

  OperandMatchResultTy ResTy;

  if (getLexer().is(AsmToken::LBrac)) {
    // Memory operand
    Operands.push_back(E2KOperand::CreateToken("[",
                                                 Parser.getTok().getLoc()));
    Parser.Lex(); // Eat the [

    if (Mnemonic == "cas" || Mnemonic == "casx" || Mnemonic == "casa") {
      SMLoc S = Parser.getTok().getLoc();
      if (getLexer().getKind() != AsmToken::Percent)
        return MatchOperand_NoMatch;
      Parser.Lex(); // eat %

      MCRegister RegNo;
      unsigned RegKind;
      if (!matchRegisterName(Parser.getTok(), RegNo, RegKind))
        return MatchOperand_NoMatch;

      Parser.Lex(); // Eat the identifier token.
      SMLoc E = SMLoc::getFromPointer(Parser.getTok().getLoc().getPointer()-1);
      Operands.push_back(E2KOperand::CreateReg(RegNo, RegKind, S, E));
      ResTy = MatchOperand_Success;
    } else {
      ResTy = parseMEMOperand(Operands);
    }

    if (ResTy != MatchOperand_Success)
      return ResTy;

    if (!getLexer().is(AsmToken::RBrac))
      return MatchOperand_ParseFail;

    Operands.push_back(E2KOperand::CreateToken("]",
                                                 Parser.getTok().getLoc()));
    Parser.Lex(); // Eat the ]

    // Parse an optional address-space identifier after the address.
    if (getLexer().is(AsmToken::Integer)) {
      std::unique_ptr<E2KOperand> Op;
      ResTy = parseE2KAsmOperand(Op, false);
      if (ResTy != MatchOperand_Success || !Op)
        return MatchOperand_ParseFail;
      Operands.push_back(std::move(Op));
    }
    return MatchOperand_Success;
  }

  std::unique_ptr<E2KOperand> Op;

  ResTy = parseE2KAsmOperand(Op, (Mnemonic == "call"));
  if (ResTy != MatchOperand_Success || !Op)
    return MatchOperand_ParseFail;

  // Push the parsed operand into the list of operands
  Operands.push_back(std::move(Op));

  return MatchOperand_Success;
}

OperandMatchResultTy
E2KAsmParser::parseE2KAsmOperand(std::unique_ptr<E2KOperand> &Op,
                                     bool isCall) {
  SMLoc S = Parser.getTok().getLoc();
  SMLoc E = SMLoc::getFromPointer(Parser.getTok().getLoc().getPointer() - 1);
  const MCExpr *EVal;

  Op = nullptr;
  switch (getLexer().getKind()) {
  default:  break;

  case AsmToken::Percent: {
    Parser.Lex(); // Eat the '%'.
    MCRegister RegNo;
    unsigned RegKind;
    if (matchRegisterName(Parser.getTok(), RegNo, RegKind)) {
      Parser.Lex(); // Eat the identifier token.
      E = SMLoc::getFromPointer(Parser.getTok().getLoc().getPointer() - 1);
      switch (RegNo) {
      default:
        Op = E2KOperand::CreateReg(RegNo, RegKind, S, E);
        break;
      case E2K::PSR:
        Op = E2KOperand::CreateToken("%psr", S);
        break;
      }
      break;
    }
    if (matchE2KAsmModifiers(EVal, E)) {
      E = SMLoc::getFromPointer(Parser.getTok().getLoc().getPointer() - 1);
      Op = E2KOperand::CreateImm(EVal, S, E);
    }
    break;
  }
  case AsmToken::Plus:
  case AsmToken::Minus:
  case AsmToken::Integer:
  case AsmToken::LParen:
  case AsmToken::Dot:
  case AsmToken::Identifier:
    if (getParser().parseExpression(EVal, E))
      break;

    int64_t Res;
    if (!EVal->evaluateAsAbsolute(Res)) {
      E2KMCExpr::VariantKind Kind = E2KMCExpr::VK_E2K_13;

      if (getContext().getObjectFileInfo()->isPositionIndependent()) {
        if (isCall)
          Kind = E2KMCExpr::VK_E2K_WPLT30;
        else
          Kind = E2KMCExpr::VK_E2K_GOT13;
      }
      EVal = E2KMCExpr::create(Kind, EVal, getContext());
    }
    Op = E2KOperand::CreateImm(EVal, S, E);
    break;
  }
  return (Op) ? MatchOperand_Success : MatchOperand_ParseFail;
}

OperandMatchResultTy
E2KAsmParser::parseBranchModifiers(OperandVector &Operands) {
  // parse (,a|,pn|,pt)+

  while (getLexer().is(AsmToken::Comma)) {
    Parser.Lex(); // Eat the comma

    if (!getLexer().is(AsmToken::Identifier))
      return MatchOperand_ParseFail;
    StringRef modName = Parser.getTok().getString();
    if (modName == "a" || modName == "pn" || modName == "pt") {
      Operands.push_back(E2KOperand::CreateToken(modName,
                                                   Parser.getTok().getLoc()));
      Parser.Lex(); // eat the identifier.
    }
  }
  return MatchOperand_Success;
}

bool E2KAsmParser::matchRegisterName(const AsmToken &Tok, MCRegister &RegNo,
                                     unsigned &RegKind) {
  RegNo = 0;
  RegKind = E2KOperand::rk_None;
  if (Tok.is(AsmToken::Identifier)) {
    StringRef name = Tok.getString();

    // %fp
    if (name.equals("fp")) {
      RegNo = E2K::R6;
      RegKind = E2KOperand::rk_IntReg;
      return true;
    }
    // %sp
    if (name.equals("sp")) {
      RegNo = E2K::B6;
      RegKind = E2KOperand::rk_IntReg;
      return true;
    }

    if (name.equals("y")) {
      RegNo = E2K::R0;
      RegKind = E2KOperand::rk_Special;
      return true;
    }
  }
  return false;
}

// Determine if an expression contains a reference to the symbol
// "_GLOBAL_OFFSET_TABLE_".
static bool hasGOTReference(const MCExpr *Expr) {
  switch (Expr->getKind()) {
  case MCExpr::Target:
    if (const E2KMCExpr *SE = dyn_cast<E2KMCExpr>(Expr))
      return hasGOTReference(SE->getSubExpr());
    break;

  case MCExpr::Constant:
    break;

  case MCExpr::Binary: {
    const MCBinaryExpr *BE = cast<MCBinaryExpr>(Expr);
    return hasGOTReference(BE->getLHS()) || hasGOTReference(BE->getRHS());
  }

  case MCExpr::SymbolRef: {
    const MCSymbolRefExpr &SymRef = *cast<MCSymbolRefExpr>(Expr);
    return (SymRef.getSymbol().getName() == "_GLOBAL_OFFSET_TABLE_");
  }

  case MCExpr::Unary:
    return hasGOTReference(cast<MCUnaryExpr>(Expr)->getSubExpr());
  }
  return false;
}

const E2KMCExpr *
E2KAsmParser::adjustPICRelocation(E2KMCExpr::VariantKind VK,
                                    const MCExpr *subExpr) {
  // When in PIC mode, "%lo(...)" and "%hi(...)" behave differently.
  // If the expression refers contains _GLOBAL_OFFSET_TABLE, it is
  // actually a %pc10 or %pc22 relocation. Otherwise, they are interpreted
  // as %got10 or %got22 relocation.

  if (getContext().getObjectFileInfo()->isPositionIndependent()) {
    switch(VK) {
    default: break;
    case E2KMCExpr::VK_E2K_LO:
      VK = (hasGOTReference(subExpr) ? E2KMCExpr::VK_E2K_PC10
                                     : E2KMCExpr::VK_E2K_GOT10);
      break;
    case E2KMCExpr::VK_E2K_HI:
      VK = (hasGOTReference(subExpr) ? E2KMCExpr::VK_E2K_PC22
                                     : E2KMCExpr::VK_E2K_GOT22);
      break;
    }
  }

  return E2KMCExpr::create(VK, subExpr, getContext());
}

bool E2KAsmParser::matchE2KAsmModifiers(const MCExpr *&EVal,
                                            SMLoc &EndLoc) {
  AsmToken Tok = Parser.getTok();
  if (!Tok.is(AsmToken::Identifier))
    return false;

  StringRef name = Tok.getString();

  E2KMCExpr::VariantKind VK = E2KMCExpr::parseVariantKind(name);
  switch (VK) {
  case E2KMCExpr::VK_E2K_None:
    Error(getLoc(), "invalid operand modifier");
    return false;

  case E2KMCExpr::VK_E2K_GOTDATA_OP:
  case E2KMCExpr::VK_E2K_TLS_GD_ADD:
  case E2KMCExpr::VK_E2K_TLS_GD_CALL:
  case E2KMCExpr::VK_E2K_TLS_IE_ADD:
  case E2KMCExpr::VK_E2K_TLS_IE_LD:
  case E2KMCExpr::VK_E2K_TLS_IE_LDX:
  case E2KMCExpr::VK_E2K_TLS_LDM_ADD:
  case E2KMCExpr::VK_E2K_TLS_LDM_CALL:
  case E2KMCExpr::VK_E2K_TLS_LDO_ADD:
    // These are special-cased at tablegen level.
    return false;

  default:
    break;
  }

  Parser.Lex(); // Eat the identifier.
  if (Parser.getTok().getKind() != AsmToken::LParen)
    return false;

  Parser.Lex(); // Eat the LParen token.
  const MCExpr *subExpr;
  if (Parser.parseParenExpression(subExpr, EndLoc))
    return false;

  EVal = adjustPICRelocation(VK, subExpr);
  return true;
}

extern "C" LLVM_EXTERNAL_VISIBILITY void LLVMInitializeE2KAsmParser() {
  RegisterMCAsmParser<E2KAsmParser> A(getTheE2K32Target());
  RegisterMCAsmParser<E2KAsmParser> B(getTheE2K64Target());
  RegisterMCAsmParser<E2KAsmParser> C(getTheE2K128Target());
  RegisterMCAsmParser<E2KAsmParser> D(getTheE2K12864Target());
}

#define GET_REGISTER_MATCHER
#define GET_MATCHER_IMPLEMENTATION
#include "E2KGenAsmMatcher.inc"

unsigned E2KAsmParser::validateTargetOperandClass(MCParsedAsmOperand &GOp,
                                                    unsigned Kind) {
  E2KOperand &Op = (E2KOperand &)GOp;
  if (Op.isFloatOrDoubleReg()) {
    switch (Kind) {
    default: break;
    case MCK_RegSR:
      if (!Op.isFloatReg() || E2KOperand::MorphToDoubleReg(Op))
        return MCTargetAsmParser::Match_Success;
      break;
    case MCK_RegDR:
      if (E2KOperand::MorphToQuadReg(Op))
        return MCTargetAsmParser::Match_Success;
      break;
    }
  }
  return Match_InvalidOperand;
}
