module m_parameters
  !! Module contains user defined parameters for consistent use
use variableKind
implicit none
real(r64), parameter :: inf = huge(0.d0)
real(r64),parameter :: NaN = transfer((/ Z'00000000', Z'7FF80000' /),1.0_8)
real(r64),parameter :: pi = dacos(-1.d0)
end module
