target_sources(${libName}
	PUBLIC
	"${CMAKE_CURRENT_LIST_DIR}/m_array1D.f90"
        "${CMAKE_CURRENT_LIST_DIR}/sm_array_i1D.f90"
	"${CMAKE_CURRENT_LIST_DIR}/sm_array_id1D.f90"
	"${CMAKE_CURRENT_LIST_DIR}/sm_array_r1D.f90"
	"${CMAKE_CURRENT_LIST_DIR}/sm_array_d1D.f90"
)
