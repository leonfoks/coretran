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
# Link in the library
target_link_libraries(${programName} coretran)
# Add the include folder that contains the library modules
target_include_directories(${programName} PUBLIC ../../include)


# Create the scalability test
set(programName coretranScale)
add_executable(${programName} scale_coretran.f90)
# Link the Library
target_link_libraries(${programName} coretran)
# Add the library include
target_include_directories(${programName} PUBLIC ../../include)
