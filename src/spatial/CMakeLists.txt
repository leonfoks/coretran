target_sources(${libName}
    PUBLIC
    "${CMAKE_CURRENT_LIST_DIR}/m_KdTree.f90"
    "${CMAKE_CURRENT_LIST_DIR}/sm_KdTree_class.f90"
    "${CMAKE_CURRENT_LIST_DIR}/sm_KdTreeBranch_class.f90"
    "${CMAKE_CURRENT_LIST_DIR}/sm_KdTreeSearch_class.f90"
)
