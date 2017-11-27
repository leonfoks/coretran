submodule (m_random) sm_rngUniform
  !! Generate random numbers from a uniform distribution.

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
  module procedure rngUniform_d1!(this,rmin,rmax)
  !====================================================================!
!  real(r64), intent(inout) :: this
!  real(r64), intent(in), optional :: rmin,rmax
  call RANDOM_NUMBER(this)
  if (present(rmax)) this=rmin+(rmax-rmin)*this
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure rngUniform_d1D!(this,rmin,rmax)
  !====================================================================!
!  real(r64) :: this(:)
!  real(r64),optional :: rmin,rmax
  call RANDOM_NUMBER(this)
  if (present(rmax)) this=rmin+(rmax-rmin)*this
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure rngUniform_d2D!(this,rmin,rmax)
  !====================================================================!
!  real(r64) :: this(:,:)
!  real(r64),optional :: rmin,rmax
  call RANDOM_NUMBER(this)
  if (present(rmax)) this=rmin+(rmax-rmin)*this
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure rngUniform_d3D!(this,rmin,rmax)
  !====================================================================!
!  real(r64) :: this(:,:,:)
!  real(r64),optional :: rmin,rmax
  call RANDOM_NUMBER(this)
  if (present(rmax)) this=rmin+(rmax-rmin)*this
  end procedure
  !====================================================================!
end submodule
