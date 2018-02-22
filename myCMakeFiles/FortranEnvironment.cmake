## CMAKE Script for setting up the fortran compiling and linking flags for different operating systems and compilers
enable_language(Fortran)

# Check if linux
if(UNIX AND NOT APPLE)
  set(LINUX TRUE)
endif()

# Make sure the build type is uppercase
string(TOUPPER "${CMAKE_BUILD_TYPE}" BT)

if(BT STREQUAL "RELEASE")
  set(CMAKE_BUILD_TYPE RELEASE CACHE STRING
    "Choose the type of build, options are DEBUG, or RELEASE."
    FORCE)
elseif(BT STREQUAL "DEBUG")
  set(CMAKE_BUILD_TYPE DEBUG CACHE STRING
    "Choose the type of build, options are DEBUG, or RELEASE."
    FORCE)
elseif(NOT BT)
  set(CMAKE_BUILD_TYPE RELEASE CACHE STRING
    "Choose the type of build, options are DEBUG, or RELEASE."
    FORCE)
  message(STATUS "CMAKE_BUILD_TYPE not given, defaulting to RELEASE.")
else()
  message(FATAL_ERROR "CMAKE_BUILD_TYPE not valid, choices are DEBUG, or RELEASE.")
endif(BT STREQUAL "RELEASE")

# Find the openmp package and add compiler-specific flags
find_package(OpenMP)
if(OPENMP_FOUND)
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${OpenMP_Fortran_FLAGS}")
  set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ${OpenMP_EXE_LINKER_FLAGS}")
endif()

# ================================
# Set gfortran compile flags
if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  message(STATUS "Getting gfortran flags")

  # Set flags for all build types
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -std=f2008ts -cpp -ffree-line-length-none -fall-intrinsics")

  if(BUILD_SHARED_LIBS)
    # Add any shared library related stuff here
    set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -shared -fpic")

    if(NOT WIN32)
      # Taken from: https://cmake.org/Wiki/CMake_RPATH_handling#Mac_OS_X_and_the_RPATH
      # use, i.e. don't skip the full RPATH for the build tree
      set(CMAKE_SKIP_BUILD_RPATH FALSE)

      # when building, don't use the install RPATH already
      # (but later on when installing)
      set(CMAKE_BUILD_WITH_INSTALL_RPATH FALSE)

      set(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")

      # add the automatically determined parts of the RPATH
      # which point to directories outside the build tree to the install RPATH
      set(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)

      # the RPATH to be used when installing, but only if it's not a system directory
      list(FIND CMAKE_PLATFORM_IMPLICIT_LINK_DIRECTORIES "${CMAKE_INSTALL_PREFIX}/lib" isSystemDir)
      if("${isSystemDir}" STREQUAL "-1")
         set(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")
      endif("${isSystemDir}" STREQUAL "-1")
    endif()
  else()
    # Static build options
    if(APPLE)
      # gcc on OS X defaults to the dynamic quadmath library instead of the static
      # NOTE: LIBRARY_PATH environment variable must be properly defined for the
      #   current compiler or find_library(quadmath) will return the static
      #   system version of libquadmath.a
      set(CMAKE_FIND_LIBRARY_SUFFIXES ".a")
      find_library(LIB_QUADMATH quadmath)
      message(STATUS "LIB_QUADMATH: ${LIB_QUADMATH}")
      # TODO: The addition of '-lgcc_s.1' is a messy fix for libgcc_s.1.dylib
      #       not being properly included as an indirect dependency of
      #       libquadmath.a. This is a brittle hack that needs to be fixed.
      set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -static-libgfortran -static-libgcc -lgfortran -lgcc -lgcc_s.1 -lSystem -nodefaultlibs ${LIB_QUADMATH}")

      # Apple's ar and ranlib commands toss out 'no symbols' warnings
      # The following two lines quiets those warnings
      set(CMAKE_Fortran_ARCHIVE_CREATE "<CMAKE_AR> Scr <TARGET> <LINK_FLAGS> <OBJECTS>")
      set(CMAKE_Fortran_ARCHIVE_FINISH "<CMAKE_RANLIB> -no_warning_for_no_symbols -c <TARGET>")
    elseif(WIN32)
      set(CMAKE_FIND_LIBRARY_SUFFIXES ".lib")
      set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -static-libgfortran -static-libgcc")
    elseif(${LINUX})
      set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -static -static-libgfortran -static-libgcc -lgfortran -lgcc")
    endif()
  endif()

  set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -O3 -funroll-all-loops -finline-functions")
  set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -O0 -fbacktrace -fbounds-check -Waliasing -Wampersand -Wconversion -Wsurprising -Wc-binding-type -Wintrinsics-std -Wtabs -Wintrinsic-shadow -Wline-truncation -Wtarget-lifetime -Wreal-q-constant")

  if(APPLE)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fno-underscoring")
  endif()

# ================================
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  message(STATUS "Getting ifort flags")

  if(WIN32)
    set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -nologo -fpp -O3 -heap-arrays1024 -QaxCORE-AVX2,CORE-AVX-I,AVX,SSE4.2,SSSE3 -Qipo -fp:fast=2 -Qdiag-disable:remark -Qmkl")
    set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -nologo -fpp -g -Od -heap-arrays1024 -traceback -CB -Qfp-stack-check -Qmkl -warn:all -warn:nounused")
  endif()

  if(${LINUX})
    if(BUILD_SHARED_LIBS)
      # Add any shared library related stuff here
    else()
      # see: https://software.intel.com/en-us/forums/intel-fortran-compiler-for-linux-and-mac-os-x/topic/753635
      # a better alternative is to use "-static-intel" which allows for dynamic system libraries
      set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -static")
    endif()

    set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -O3 -axCORE-AVX2,CORE-AVX-I,AVX,SSE4.2,SSSE3 -no-prec-div -fp-model fast=2")
    set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -O0 -g -traceback -CB -fp-stack-check -gen-interfaces -warn interfaces")
  endif()

  if(APPLE)
      set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -O3 -axCORE-AVX2,CORE-AVX-I,AVX,SSE4.2,SSSE3 -no-prec-div -fp-model fast=2")
      set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -O0 -g -traceback -CB -fp-stack-check -gen-interfaces -warn interfaces")
  endif()
endif()


##
## Display information to the user
##
message(STATUS "Build type is ${CMAKE_BUILD_TYPE} use option -DCMAKE_BUILD_TYPE=[DEBUG RELEASE] to switch")

if(BT STREQUAL "RELEASE")
  message(STATUS "Using the following compile flags ${CMAKE_Fortran_FLAGS_RELEASE}")
elseif(BT STREQUAL "DEBUG")
  message(STATUS "Using the following compile flags ${CMAKE_Fortran_FLAGS_DEBUG}")
endif()


###
### Set the output directories for compiled libraries and module files.
### Paths are relative to the build folder where you call cmake
###
#set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/../lib) # Static library location
#set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/../lib) # Shared library location
# Add a folder for module placement
#set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/../include)
#set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/../bin)

###
### Function to link a static or shared library
###
#function(LINK_LIBRARY target relativePathToBuildDir lib)
#  if(STATIC)
#    if(WIN32)
#      target_link_libraries(${target} ${lib})
#    else()
#      get_filename_component(absPath "${relativePathToBuildDir}/lib${lib}.a"
#        REALPATH BASE_DIR "${CMAKE_BINARY_DIR}")
#
#      message(STATUS "real: ${absPath}")
#
#      target_link_libraries(${target} "${absPath}")
#    endif(WIN32)
#  else()
#    target_link_libraries(${target} ${lib})
#  endif(STATIC)
#endfunction(LINK_LIBRARY)
