submodule (Prng_Class) sm_rngNormal
  !! Generate random number from a normal distribution.

  ! Original Version
  ! Author: Alan Miller
  ! The base functions were originally inside http://www.netlib.org/random/random.f90.

  ! Updated version:
  ! Version 2.00, 11 November 2016
  ! Increased precision to double
  !
  ! Updated again
  ! Added overloaded operations for single number, nD arrays
  !     Author: Leon Foks

use m_indexing, only: ind2sub
use m_parameters

implicit none

contains

  !====================================================================!
  module procedure rngNormal_d1_Prng!(this, res, mean, std)
    !! Overloaded Type bound procedure Prng%rngNormal()
  !====================================================================!
  ! Adapted from the following Fortran 77 code
  !      ALGORITHM 712, COLLECTED ALGORITHMS FROM ACM.
  !      THIS WORK PUBLISHED IN TRANSACTIONS ON MATHEMATICAL SOFTWARE,
  !      VOL. 18, NO. 4, DECEMBER, 1992, PP. 434-435.

  !  The function random_normal() returns a normally distributed pseudo-random
  !  number with zero mean and unit variance.

  !  The algorithm uses the ratio of uniforms method of A.J. Kinderman
  !  and J.F. Monahan augmented with quadratic bounding curves.
  !     Generate P = (u,v) uniform in rectangle enclosing acceptance region
  !====================================================================!
!  class(Prng), intent(inout) :: this
!  real(r64), intent(inout) :: res
!  real(r64), intent(in), optional :: mean,std
  ! Local variables
  real(r64), parameter :: s = 0.449871d0
  real(r64), parameter :: t = -0.386595d0
  real(r64), parameter :: a = 0.196d0
  real(r64), parameter :: b = 0.25472d0
  real(r64) :: q
  real(r64), parameter :: r1 = 0.27597d0
  real(r64), parameter :: r2 = 0.27846d0
  real(r64) :: u
  real(r64) :: v
  real(r64) :: x
  real(r64) :: y

  do
    call this%rngUniform(u)
    call this%rngUniform(v)
    v = 1.7156d0 * (v - 0.5d0)

    ! Evaluate the quadratic form
    x = u - s
    y = dabs(v) - t
    q = x**2d0 + y*(a*y - b*x)

    ! Accept P if inside inner ellipse
    if (q < r1) exit
    ! Reject P if outside outer ellipse
    if (q > r2) cycle
    ! Reject P if outside acceptance region
    if (v**2.d0 < -4.d0*log(u)*u**2.d0) exit
  enddo

  ! Return ratio of P's coordinates as the normal deviate
  res = v/u
  if (present(std)) then
    if (.not. present(mean)) call eMsg('rngNormal : Need both mean and standard deviation')
    res = (std*res) + mean
  end if
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure rngNormal_d1D_Prng!(this, res, mean,std)
    !! Overloaded Type bound procedure Prng%rngNormal()
  !====================================================================!
!  class(Prng), intent(inout) :: this
!  real(r64), intent(inout) :: res
!  real(r64),optional :: mean,std
  integer(i32) :: i,N
  N=size(res)
  do i=1,N
    call rngNormal_d1_Prng(this, res(i), mean, std)
  end do
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure rngNormal_d2D_Prng!(this, res, mean,std)
    !! Overloaded Type bound procedure Prng%rngNormal()
  !====================================================================!
!  class(Prng), intent(inout) :: this
!  real(r64), intent(inout) :: res
!  real(r64),optional :: mean,std
  integer(i32) :: i,iSub(2)
  integer(i32) :: n,nSub(2)
  nSub = shape(res)
  n=size(res)
  do i=1,n
    iSub = ind2sub(i,nSub)
    call rngNormal_d1_Prng(this, res(iSub(1),iSub(2)), mean, std)
  enddo
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure rngNormal_d3D_Prng!(this, res, mean,std)
    !! Overloaded Type bound procedure Prng%rngNormal()
  !====================================================================!
!  class(Prng), intent(inout) :: this
!  real(r64), intent(inout) :: res
!  real(r64),optional :: mean,std
  integer(i32) :: i,iSub(3)
  integer(i32) :: n,nSub(3)
  nSub = shape(res)
  n=size(res)
  do i=1,n
    iSub = ind2sub(i,nSub)
    call rngNormal_d1_Prng(this, res(iSub(1),iSub(2),iSub(3)), mean, std)
  enddo
  end procedure
  !====================================================================!

end submodule
