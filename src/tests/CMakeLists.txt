cmake_minimum_required(VERSION 3.8)
# Create the testing suite
project(coretranTest LANGUAGES Fortran)
# ${PROJECT_NAME} is the name given for the most recent project() call
add_executable(${PROJECT_NAME} "${CMAKE_CURRENT_LIST_DIR}/test_coretran.f90"
                               "${CMAKE_CURRENT_LIST_DIR}/m_tests.f90")
# Link coretran to the test suite
target_link_libraries(${PROJECT_NAME} coretran)

# Create the scalability test
project(coretranScale LANGUAGES Fortran)
# ${PROJECT_NAME} is the name given for the most recent project() call
add_executable(${PROJECT_NAME} "${CMAKE_CURRENT_LIST_DIR}/scale_coretran.f90")
# Link coretran to the scalability test
target_link_libraries(${PROJECT_NAME} coretran)
