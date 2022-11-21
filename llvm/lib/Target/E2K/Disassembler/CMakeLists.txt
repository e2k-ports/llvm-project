add_llvm_component_library(LLVME2KDisassembler
  E2KDisassembler.cpp

  LINK_COMPONENTS
  MCDisassembler
  E2KInfo
  Support

  ADD_TO_COMPONENT
  E2K
  )
