submodule (m_random) sm_rngWeibull
!! Generate random number from a weibull distribution.

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
  module procedure rngWeibull_d1!(this,den)
    !! Interfaced with [[rngWeibull]]
  !====================================================================!
  !     Generates a random variate from the Weibull distribution with
  !     probability density:
  !                      a
  !               a-1  -x
  !     f(x) = a.x    e
!  real(r64) :: this
!  real(r64), intent(in) :: den
  if (den == 0.d0 .or. dabs(den) < 1.d-16) call Emsg('rngWeibull : density is v.small')
  call rngExponential(this)
  this =  this**(one/den)
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure rngWeibull_d1D!(this,den)
    !! Interfaced with [[rngWeibull]]
  !====================================================================!
!  real(r64) :: this(:)
!  real(r64), intent(in) :: den
  integer :: i,N
  N=size(this)
  do i=1,N
    call rngWeibull(this(i),den)
  end do
  end procedure
  !====================================================================!

end submodule


