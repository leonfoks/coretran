cmake_minimum_required(VERSION 3.8)
project(coretranTest LANGUAGES Fortran)

# Initialize the program
# ${PROJECT_NAME} is the name given for the most recent project() call
add_executable(${PROJECT_NAME} "${CMAKE_CURRENT_LIST_DIR}/test_coretran.f90"
                               "${CMAKE_CURRENT_LIST_DIR}/m_tests.f90")
target_link_libraries(${PROJECT_NAME} coretran)

# Create the scalability test
project(coretranScale LANGUAGES Fortran)
add_executable(${PROJECT_NAME} "${CMAKE_CURRENT_LIST_DIR}/scale_coretran.f90")
target_link_libraries(${PROJECT_NAME} coretran)
