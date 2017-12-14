## CMAKE Script for setting up the fortran compiling and linking flags for different operating systems and compilers

# Check if linux
if(UNIX AND NOT APPLE)
    set(LINUX TRUE)
endif()

# Make sure the build type is uppercase
STRING(TOUPPER "${CMAKE_BUILD_TYPE}" BT)

IF(BT STREQUAL "RELEASE")
    SET(CMAKE_BUILD_TYPE RELEASE CACHE STRING
      "Choose the type of build, options are DEBUG, or RELEASE."
      FORCE)
ELSEIF(BT STREQUAL "DEBUG")
    SET (CMAKE_BUILD_TYPE DEBUG CACHE STRING
      "Choose the type of build, options are DEBUG, or RELEASE."
      FORCE)
ELSEIF(NOT BT)
    SET(CMAKE_BUILD_TYPE RELEASE CACHE STRING
      "Choose the type of build, options are DEBUG, or RELEASE."
      FORCE)
    MESSAGE(STATUS "CMAKE_BUILD_TYPE not given, defaulting to RELEASE.")
ELSE()
    MESSAGE(FATAL_ERROR "CMAKE_BUILD_TYPE not valid, choices are DEBUG, or RELEASE.")
ENDIF(BT STREQUAL "RELEASE")

## Determine which fortran compiler we are using
# Get the Fortran Compiler used with cmake
get_filename_component (F90 ${CMAKE_Fortran_COMPILER} NAME)

# Get a tag corresponding to the compiler
string(REGEX MATCH "gfortran" F90tag ${F90})
if (NOT F90tag)
  string(REGEX MATCH "ifort" F90tag ${F90})
endif()


# Set gfortran compile flags
if (${F90tag} MATCHES "gfortran")
  MESSAGE(STATUS "Getting gfortran flags")
  
  set (CMAKE_Fortran_FLAGS_RELEASE "-O3 -fopenmp -std=f2008ts -funroll-all-loops  -finline-functions -static-libgfortran -static-libgcc -ffree-line-length-none -fall-intrinsics")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-O0 -fopenmp -std=f2008ts -fbacktrace -fbounds-check -Waliasing -Wampersand -Wconversion -Wsurprising -Wc-binding-type -Wintrinsics-std -Wtabs -Wintrinsic-shadow -Wline-truncation -Wtarget-lifetime -Wreal-q-constant -static-libgfortran -static-libgcc -ffree-line-length-none -fall-intrinsics")

elseif (${F90tag} MATCHES "ifort") 
  MESSAGE(STATUS "Getting ifort flags")

  if (WIN32)
    set (CMAKE_Fortran_FLAGS_RELEASE "-nologo -fpp -O3 -openmp -heap-arrays1024 -QaxCORE-AVX2,CORE-AVX-I,AVX,SSE4.2,SSSE3 -Qipo -fp:fast=2 -Qdiag-disable:remark -Qmkl")
    set (CMAKE_Fortran_FLAGS_DEBUG "-nologo -fpp -Od -openmp -heap-arrays1024 -traceback -CB -Qfp-stack-check -Qmkl -warn:all -warn:nounused")
  endif ()
  
  if (${LINIX})
  
  endif ()
  
  if (APPLE)
    set (CMAKE_Fortran_FLAGS_RELEASE "-O3 -qopenmp -axCORE-AVX2,CORE-AVX-I,AVX,SSE4.2,SSSE3 -no-prec-div -fp-model fast=2")
    set (CMAKE_Fortran_FLAGS_DEBUG "-O0 -qopenmp -traceback -CB -fp-stack-check -gen-interfaces -warn interfaces")
  endif ()
endif()


##
## Display information to the user
##
MESSAGE(STATUS "Build type is ${CMAKE_BUILD_TYPE} use option -DCMAKE_BUILD_TYPE=[DEBUG RELEASE] to switch")

if(BT STREQUAL "RELEASE")
    MESSAGE(STATUS "Using the following compile flags ${CMAKE_Fortran_FLAGS_RELEASE}")
elseif(BT STREQUAL "DEBUG")
	MESSAGE(STATUS "Using the following compile flags ${CMAKE_Fortran_FLAGS_DEBUG}")
endif()


###
### Set the output directories for compiled libraries and module files.
### Paths are relative to the build folder where you call cmake
###
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/../lib) # Static library location
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/../lib) # Shared library location
# Add a folder for module placement
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/../include)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/../bin)

### 
### Enforce the library types from experience!
### I never got static libraries to work on a Mac, so make them shared
### I never got shared libraries to work on windows! so make them static
###
if (APPLE)
  set(libType SHARED) # Shared library on OSX
endif()
if(WIN32)
  set(libType STATIC) # Static library on Windows
endif()
if (${LINUX})
  set(libType STATIC) # Shared library on Linux
endif()





# Optional blas compile if not given?
#if(TARGET shared_lib)
#message("shared_lib is already defined")
#else()
#include_directories(${SHARED_LIB_INCLUDE_DIR})
#set(LIB_SRCS ./src/foo.c)
#add_library(shared_lib STATIC ${LIB_SRCS})
#endif()

