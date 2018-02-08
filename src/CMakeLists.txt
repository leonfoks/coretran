cmake_minimum_required (VERSION 3.8)
project (coretran)
enable_language (Fortran)

# Optional parameters to use
# -DCMAKE_Fortran_COMPILER=/path/to/fortran/compiler
# -DCMAKE_BUILD_TYPE=[DEBUG, RELEASE]  - Build with debug flags or not.
# -DSTATIC=[ON, OFF]  - Build and shared or static library, or link a program using -static

# Setup the Fortran cmake environment
# Sets up the flags for compiling shared libraries or static libraries, and returns the libType parameter.
include(../myCMakeFiles/FortranEnvironment.cmake)

# Initialize the library with no files added yet
set(libName coretran)

# Create the library
add_library(${libName} ${libType} "")

# Add files to the library as sources
# Each CMakeLists.txt simply uses target_sources to add the source files
# Core
include(core/CMakeLists.txt)
# Time
include(time/CMakeLists.txt)
# Random Number Generators
include(random/CMakeLists.txt)
# Searching
include(searching/CMakeLists.txt)
# Arrays1D
include(arrays/CMakeLists.txt)
# Sorting
include(sorting/CMakeLists.txt)
# Spatial
include(spatial/CMakeLists.txt)
# Maths
include(maths/CMakeLists.txt)
# Dynamic Arrays
include(dynamicArrays/CMakeLists.txt)

# Thats it! Simple!
