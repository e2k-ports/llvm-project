add_llvm_component_library(LLVME2KInfo
  E2KTargetInfo.cpp

  LINK_COMPONENTS
  MC
  Support

  ADD_TO_COMPONENT
  E2K
  )
