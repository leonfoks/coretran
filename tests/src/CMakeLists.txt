cmake_minimum_required (VERSION 3.6)
project (coretran)
enable_language (Fortran)

# Setup the Fortran cmake environment
include(../../myCMakeFiles/FortranEnvironment.cmake)

# Add the linking directory to libFortran
link_directories(../../lib)

# Initialize the program
set(programName coretranTest)
add_executable(${programName} test_coretran.f90)

LINK_LIBRARY(${programName} ../../lib coretran)

# Add the include folder that contains the library modules
target_include_directories(${programName} PUBLIC ../../include)
target_sources(${programName}
    PUBLIC
    "${CMAKE_CURRENT_LIST_DIR}/m_tests.f90"
)



# Create the scalability test
set(programName coretranScale)
add_executable(${programName} scale_coretran.f90)
# Link the Library
LINK_LIBRARY(${programName} ../../lib coretran)
# Add the library include
target_include_directories(${programName} PUBLIC ../../include)

