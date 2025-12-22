<h1 style="text-align: center; color: blue; font-size: 28pt;">Sprint #21: FESOM-C: Profiling, Analysis, and Optimization Roadmap (Phase 1) </h1>

#Link C/Fortran modules implementing control and CPU core affinity for MPI processes and OpenMP threads

When integrating C/Fortran modules that implement functionality for checking and setting the affinity of MPI processes and OpenMP threads, the module sources (directory `libs-SRC/threadAffinityLib/`) are compiled with the `-qopenmp` flag using the Intel oneAPI Compiler. During the build of the main executable, the OpenMP runtime library is linked only at the linking stage by enabling the same `-qopenmp` flag. At the stage of compiling the `fesom-c` source files into object files (directory `fesom-c-SRC/`), the `-qopenmp` flag is not enabled.

#Generating the executable for serial runs

To build the executable, the `-DUSE_MPI` flag in `fesom-c-SRC/Makefile.PLATFORM` and the macro for including MPI routines, `_TALCC_USE_MPI_PLUGINS_` (in `libs-SRC/threadAffinityLib/src/threadAffinityLibCoreC.h`), should be disabled.
