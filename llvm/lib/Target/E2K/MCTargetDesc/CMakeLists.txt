add_llvm_component_library(LLVME2KDesc
  E2KAsmBackend.cpp
  E2KELFObjectWriter.cpp
  E2KInstPrinter.cpp
  E2KMCAsmInfo.cpp
  E2KMCCodeEmitter.cpp
  E2KMCTargetDesc.cpp
  E2KMCExpr.cpp
  E2KTargetStreamer.cpp

  LINK_COMPONENTS
  MC
  E2KInfo
  Support

  ADD_TO_COMPONENT
  E2K
  )
