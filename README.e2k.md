LLVM port to Elbrus 2000
========================

about
-----

the project is an attempt to port LLVM to Elbrus 2000 (E2K) architecture.
this project is **WIP** (work in progress), on a **very early** development stages.
of course, it's not intended to be used in production, installed on end-users machines and so on.
this project is developed voluntary, only in spare time and when I am in good mood.
in other words, it's a hobby side-project I develop for fun to get familiar with both LLVM and E2K.
therefore, it may take months, if not years, to get some satisfactory state.
don't expect it to compile even **hello-world** for now, or produce working executables.

the source code is periodically synchronized with upstream and rebased on top of the main branch.
therefore, the history is frequently rewritten, and changes are force-pushed from time to time.

source code
-----------

there are several good places to start getting familiar with the source code changes.

in **LLVM**:
- [llvm/lib/Target/E2K](llvm/lib/Target/E2K) - the main subdirectory to define low-level E2K target information
- [llvm/lib/Target/E2K/E2K.td](llvm/lib/Target/E2K/E2K.td) - defines E2K processors and their features (TableGen)
- [llvm/lib/Target/E2K/E2KRegisterInfo.td](llvm/lib/Target/E2K/E2KRegisterInfo.td) - defines E2K registers (TableGen)
- [llvm/lib/Target/E2K/E2KInstrFormats.td](llvm/lib/Target/E2K/E2KInstrFormats.td) - defines E2K instruction formats (TableGen)
- [llvm/lib/Target/E2K/E2KInstrInfo.td](llvm/lib/Target/E2K/E2KInstrInfo.td) - defines E2K instructions (TableGen)
- [llvm/lib/Target/E2K/E2KTargetMachine.cpp](llvm/lib/Target/E2K/E2KTargetMachine.cpp) - main definitions of E2K targets (llvm)
- [llvm/lib/Target/E2K/E2KSubtarget.cpp](llvm/lib/Target/E2K/E2KSubtarget.cpp) - main definitions of E2K sub-targets, or flavours (llvm)

in **clang**:
- [clang/lib/Basic/Targets/E2K.cpp](clang/lib/Basic/Targets/E2K.cpp) - main definitions of E2K targets (clang)
- [clang/lib/Driver/ToolChains/Gnu.cpp](clang/lib/Driver/ToolChains/Gnu.cpp) - E2K mappings for GNU/Linux (clang)
- [clang/lib/Driver/ToolChains/Linux.cpp](clang/lib/Driver/ToolChains/Linux.cpp) - E2K mappings for Linux (clang)
- [clang/lib/Basic/Targets.cpp](clang/lib/Basic/Targets.cpp) - major major E2K mappings (clang)

in **lld**:
- [lld/ELF/Arch/E2K.cpp](lld/ELF/Arch/E2K.cpp) - main definitions of E2K targets (lld)
- [lld/ELF/Driver.cpp](lld/ELF/Driver.cpp) - E2K mappings for ELF (lld)
- [llvm/lib/Support/Triple.cpp](llvm/lib/Support/Triple.cpp) - major E2K mappings (lld)

generic:
- [llvm/include/llvm/TargetParser/Triple.h](llvm/include/llvm/TargetParser/Triple.h) - defines common E2K target identifiers
- [llvm/include/llvm/BinaryFormat/ELFRelocs/E2K.def](llvm/include/llvm/BinaryFormat/ELFRelocs/E2K.def) - defines E2K relocations (from **/usr/include/elf.h**)
- [llvm/lib/Support/Triple.cpp](llvm/lib/Support/Triple.cpp) - defines mappings for E2K triples

compiling the project
---------------------

recommended options for CMake build:
- **-G Ninja** - recommended (faster) generator is *ninja-build*
- **-DCMAKE_BUILD_TYPE=Debug** - on an early development stage, it's easier to have all the debug checks
- **-DLLVM_ENABLE_PROJECTS=clang;lld** - projects being ported first are *clang* and *lld*
- **-DLLVM_EXPERIMENTAL_TARGETS_TO_BUILD=E2K** - to activate an actual *E2K* target
- **-DCMAKE_VERBOSE_MAKEFILE=ON** - to display full compiler/linker command lines
- **-DLLVM_USE_LINKER=lld** - *lld* is usually faster compare to regular *ld* or *gold* linkers

for E2K, to avoid "relocation truncated to fit" error:

- **CMAKE_C_FLAGS="--dwarf2-64bit"**
- **CMAKE_CXX_FLAGS="--dwarf2-64bit"**

compiling for EK2 via project
-----------------------------

there are several **must-have** (mandatory) options to use compiled *clang* executable:

- **fuse-ld=lld** - as your system linker probably knows nothing about E2K, it's needed to use compiled one
- **fintegrated-as** - same for your assembler and binutils, it's needed to use compiled one
- **target e2k64** - actually instructs compiler to generate code for E2K

helpful resources and tools
---------------------------

microprocessor commands:

- [**(ru) microprocessor commands**](http://ftp.altlinux.org/pub/people/mike/elbrus/docs/elbrus_prog/html/chapter10.html) - an official reference
- [**elbrus-docs**](https://github.com/nrdmn/elbrus-docs) - describes instruction encoding
- [**e2k-stuff**](https://github.com/OpenE2K/binutils-gdb/tree/binutils-mcst/opcodes/e2k-stuff) - opcode tables from the bintuils port
- [**alops.inc**](https://github.com/OpenE2K/qemu-e2k/blob/e2k/target/e2k/alops.inc) - opcode tables from the QEMU-E2K port
- **ldis** - MCST disassembler
- **objdump** - disassembler, port to E2K
