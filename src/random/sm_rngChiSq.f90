submodule (m_random) sm_rngChiSq
  !! Generate random numbers from a Chi squared distribution.

  ! Original Version
  ! Author: Alan Miller

  ! Updated version:
  ! Version 2.00, 11 November 2016
  ! Increased precision to double
  !
  ! Updated again
  ! Added overloaded operations for single number, nD arrays
  !     Author: Leon Foks
use variableKind

implicit none

real(r64), parameter :: half = 0.5_r64
real(r64), parameter :: two = 2.0_r64

contains

  !====================================================================!
  module procedure rngChisq_d1!(this,ndf,first)
  !====================================================================!
  ! Generates a random variate from the chi-squared distribution with
  ! ndf degrees of freedom
!    INTEGER, INTENT(IN) :: ndf
!    LOGICAL, INTENT(IN) :: first
!    real(r64) :: this
  call rngGamma(this,half*dble(ndf), first)
  this = two * this
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure rngChisq_d1D!(this,ndf,first)
  !====================================================================!
!    real(r64) :: this(:)
!    INTEGER, INTENT(IN) :: ndf
!    LOGICAL, INTENT(IN) :: first
  integer :: i,N
  N=size(this)
  call rngChisq(this(1),ndf,first)
  do i=2,N
    call rngChisq(this(i),ndf,.false.)
  end do
  end procedure
  !====================================================================!
end submodule
