target_sources(${libName}
	PUBLIC
	"${CMAKE_CURRENT_LIST_DIR}/m_maths.f90"
	"${CMAKE_CURRENT_LIST_DIR}/sm_maths_i1D.f95"
	"${CMAKE_CURRENT_LIST_DIR}/sm_maths_id1D.f95"
	"${CMAKE_CURRENT_LIST_DIR}/sm_maths_r1D.f95"
	"${CMAKE_CURRENT_LIST_DIR}/sm_maths_d1D.f95"
)
