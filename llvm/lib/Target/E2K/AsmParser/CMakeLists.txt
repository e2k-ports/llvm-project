add_llvm_component_library(LLVME2KAsmParser
  E2KAsmParser.cpp

  LINK_COMPONENTS
  MC
  MCParser
  E2KDesc
  E2KInfo
  Support

  ADD_TO_COMPONENT
  E2K
  )
