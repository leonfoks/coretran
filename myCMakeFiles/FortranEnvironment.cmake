## CMAKE Script for setting up the fortran compiling and linking flags for different operating systems and compilers

# Check if linux
if(UNIX AND NOT APPLE)
    set(LINUX TRUE)
endif()


# determine, whether we want a static binary
SET(STATIC FALSE CACHE BOOL "Build a static library or use static linking?")

# do we want static libraries?
# When STATIC is TRUE, than cmake looks for libraries ending
# with .a. This is for linux only!
IF(STATIC)
    if (WIN32)
      SET(CMAKE_FIND_LIBRARY_SUFFIXES ".lib")
    elseif(LINUX)
      SET(CMAKE_FIND_LIBRARY_SUFFIXES ".a")
    elseif(APPLE)
      SET(CMAKE_FIND_LIBRARY_SUFFIXES ".a")
    endif()
ENDIF(STATIC)

# set -static, when STATIC_LINKING is TRUE and set LINK_SEARCH_END_STATIC
# to remove the additional -bdynamic from the linker line.
#IF(STATIC)
#    SET(CMAKE_EXE_LINKER_FLAGS "-static")
#    SET_TARGET_PROPERTIES(${programName} PROPERTIES 
#        LINK_SEARCH_END_STATIC 1)
#ENDIF(STATIC)


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

  set (COMMONFLAGS "-std=f2008ts -fopenmp -cpp -ffree-line-length-none -ffixed-line-length-none -fall-intrinsics")

  set (STATIC_FLAGS "")
  if (STATIC)
    set (STATIC_FLAGS "-static-libgfortran -static-libgcc")
  endif()

  set (GFORTRAN_RELEASE "${COMMONFLAGS} -funroll-all-loops -finline-functions ${STATIC_FLAGS}")
  set (GFORTRAN_DEBUG   "${COMMONFLAGS} -O0 -fbacktrace -fbounds-check -Waliasing -Wampersand -Wconversion -Wsurprising -Wc-binding-type -Wintrinsics-std -Wtabs -Wintrinsic-shadow -Wline-truncation -Wtarget-lifetime -Wreal-q-constant ${STATIC_FLAGS}")

  set (CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -shared -fpic")
  
  if (WIN32)
    set (CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} ${GFORTRAN_RELEASE} -static-libgfortran -static-libgcc")
    set (CMAKE_Fortran_FLAGS_DEBUG   "${CMAKE_Fortran_FLAGS_DEBUG} ${GFORTRAN_DEBUG}")
  endif ()

  if (${LINUX})
    set (CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} ${GFORTRAN_RELEASE}")
    set (CMAKE_Fortran_FLAGS_DEBUG   "${CMAKE_Fortran_FLAGS_DEBUG} ${GFORTRAN_DEBUG}")
  endif ()

  if (APPLE)
    set (CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} ${GFORTRAN_RELEASE}")
    set (CMAKE_Fortran_FLAGS_DEBUG   "${CMAKE_Fortran_FLAGS_DEBUG} ${GFORTRAN_DEBUG}")
  endif ()

elseif (${F90tag} MATCHES "ifort") 

  MESSAGE(STATUS "Getting ifort flags")

  if (WIN32)
    set (CMAKE_Fortran_FLAGS_RELEASE "-nologo -fpp -O3 -openmp -heap-arrays1024 -QaxCORE-AVX2,CORE-AVX-I,AVX,SSE4.2,SSSE3 -Qipo -fp:fast=2 -Qdiag-disable:remark -Qmkl")
    set (CMAKE_Fortran_FLAGS_DEBUG "-nologo -fpp -g -Od -openmp -heap-arrays1024 -traceback -CB -Qfp-stack-check -Qmkl -warn:all -warn:nounused")
  endif ()
  
  if (${LINIX})
  
  endif ()
  
  if (APPLE)
    set (CMAKE_Fortran_FLAGS_RELEASE "-O3 -qopenmp -axCORE-AVX2,CORE-AVX-I,AVX,SSE4.2,SSSE3 -no-prec-div -fp-model fast=2")
    set (CMAKE_Fortran_FLAGS_DEBUG "-O0 -g -qopenmp -traceback -CB -fp-stack-check -gen-interfaces -warn interfaces")
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
### Set the library type
###
if (STATIC)
  set(libType STATIC)
else()
  set(libType SHARED)
  MESSAGE(STATUS "Using linker flags ${CMAKE_SHARED_LINKER_FLAGS}")
endif()



###
### Function to link a static or shared library
###
function(LINK_LIBRARY target relativePathToBuildDir lib)
  if (STATIC)
    if (WIN32)
      target_link_libraries(${target} ${lib})
    else()
      get_filename_component(absPath "${relativePathToBuildDir}/lib${lib}.a"
                        REALPATH BASE_DIR "${CMAKE_BINARY_DIR}")

      MESSAGE(STATUS "real: ${absPath}")

      target_link_libraries(${target} "${absPath}")
    endif(WIN32)
  else()
    target_link_libraries(${target} ${lib})
  endif(STATIC)
endfunction(LINK_LIBRARY)