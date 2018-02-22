target_sources(${PROJECT_NAME}
    PRIVATE
        "${CMAKE_CURRENT_LIST_DIR}/m_searching.f90"
        "${CMAKE_CURRENT_LIST_DIR}/sm_binarySearch.f90"
        "${CMAKE_CURRENT_LIST_DIR}/sm_intervalSearch.f90"
        "${CMAKE_CURRENT_LIST_DIR}/sm_simpleSearch.f90"
)
