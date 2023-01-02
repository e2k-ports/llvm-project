add_llvm_component_group(E2K)

set(LLVM_TARGET_DEFINITIONS E2K.td)

tablegen(LLVM E2KGenAsmMatcher.inc -gen-asm-matcher)
tablegen(LLVM E2KGenAsmWriter.inc -gen-asm-writer)
tablegen(LLVM E2KGenCallingConv.inc -gen-callingconv)
tablegen(LLVM E2KGenDAGISel.inc -gen-dag-isel)
tablegen(LLVM E2KGenDisassemblerTables.inc -gen-disassembler)
tablegen(LLVM E2KGenInstrInfo.inc -gen-instr-info)
tablegen(LLVM E2KGenMCCodeEmitter.inc -gen-emitter)
tablegen(LLVM E2KGenRegisterInfo.inc -gen-register-info)
tablegen(LLVM E2KGenSubtargetInfo.inc -gen-subtarget)

add_public_tablegen_target(E2KCommonTableGen)

add_llvm_target(E2KCodeGen
  DelaySlotFiller.cpp
  E2KAsmPrinter.cpp
  E2KInstrInfo.cpp
  E2KISelDAGToDAG.cpp
  E2KISelLowering.cpp
  E2KFrameLowering.cpp
  E2KMachineFunctionInfo.cpp
  E2KRegisterInfo.cpp
  E2KSubtarget.cpp
  E2KTargetMachine.cpp
  E2KMCInstLower.cpp
  E2KTargetObjectFile.cpp
  E2KExpandPseudo.cpp

  LINK_COMPONENTS
  AsmPrinter
  CodeGen
  Core
  MC
  SelectionDAG
  E2KDesc
  E2KInfo
  Support
  Target

  ADD_TO_COMPONENT
  E2K
  )

add_subdirectory(AsmParser)
add_subdirectory(Disassembler)
add_subdirectory(MCTargetDesc)
add_subdirectory(TargetInfo)
