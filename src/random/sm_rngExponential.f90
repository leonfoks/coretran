submodule (m_random) sm_rngExponential
!! Generate random number from an exponential distribution.

  ! Original Version
  ! Author: Alan Miller

  ! Updated version:
  ! Version 2.00, 11 November 2016
  ! Increased precision to double
  !
  ! Updated again
  ! Added overloaded operations for single number, nD arrays
  !     Author: Leon Foks
implicit none

contains

  !====================================================================!
  module procedure rngExponential_d1D!(this)
    !! Interfaced with [[rngExponential]]
  !====================================================================!
!  real(r64) :: this(:)
  integer :: i,N
  N=size(this)
  do i=1,N
    call rngExponential(this(i))
  end do
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure rngExponential_d1!(this)
    !! Interfaced with [rngExponential]]
  !====================================================================!
  ! Adapted from Fortran 77 code from the book:
  !     Dagpunar, J. 'Principles of random variate generation'
  !     Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9

  ! FUNCTION GENERATES A RANDOM VARIATE IN [0,INFINITY) FROM
  ! A NEGATIVE EXPONENTIAL DlSTRIBUTION WlTH DENSITY PROPORTIONAL
  ! TO EXP(-random_exponential), USING INVERSION.
!  real(r64)  :: this
  real(r64)  :: r
  CALL rngUniform(r)

  do while (r == zero)
    CALL rngUniform(r)
  end do
  this = -dLOG(r)
  end procedure
  !====================================================================!
end submodule
