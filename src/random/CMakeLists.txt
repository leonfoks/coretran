target_sources(${libName}
    PUBLIC       
        "${CMAKE_CURRENT_LIST_DIR}/Prng_Class.f90"
        "${CMAKE_CURRENT_LIST_DIR}/sm_xorshift128plus.f90"
        "${CMAKE_CURRENT_LIST_DIR}/sm_xorshift1024star.f90"
        "${CMAKE_CURRENT_LIST_DIR}/m_random.f90"
        "${CMAKE_CURRENT_LIST_DIR}/sm_rngExponential.f90"
        "${CMAKE_CURRENT_LIST_DIR}/sm_rngInteger.f90"
        "${CMAKE_CURRENT_LIST_DIR}/sm_rngNormal.f90"
        "${CMAKE_CURRENT_LIST_DIR}/sm_rngUniform.f90"
        "${CMAKE_CURRENT_LIST_DIR}/sm_rngWeibull.f90"
#        "${CMAKE_CURRENT_LIST_DIR}/sm_rngGamma.f90"
#        "${CMAKE_CURRENT_LIST_DIR}/sm_rngChiSq.f90"
)
