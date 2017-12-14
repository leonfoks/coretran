cmake_minimum_required (VERSION 3.8)
project (libFortran)
enable_language (Fortran)

# Setup the Fortran cmake environment
include(../myCMakeFiles/FortranEnvironment.cmake)

# Initialize the library with no files added yet
set(libName Fortran)

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
