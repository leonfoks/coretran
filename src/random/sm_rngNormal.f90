submodule (m_random) sm_rngNormal
  !! Generate random number from a normal distribution.

  ! Original Version
  ! Author: Alan Miller

  ! Updated version:
  ! Version 2.00, 11 November 2016
  ! Increased precision to double
  !
  ! Updated again
  ! Added overloaded operations for single number, nD arrays
  !     Author: Leon Foks

use m_indexing, only: ind2sub

implicit none

contains

  !====================================================================!
  module procedure rngNormal_d1!(this,mean,std)
    !! Interfaced with [[rngNormal]]
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
!  real(r64), intent(inout) :: this
!  real(r64), intent(in), optional :: mean,std
  ! Local variables
  real(r64) :: s
  real(r64) :: t
  real(r64) :: a
  real(r64) :: b
  real(r64) :: q
  real(r64) :: r1
  real(r64) :: r2
  real(r64) :: u
  real(r64) :: v
  real(r64) :: x
  real(r64) :: y

  s = 0.449871d0
  t = -0.386595d0
  a = 0.196d0
  b = 0.25472d0
  r1 = 0.27597d0
  r2 = 0.27846d0

  DO
    CALL rngUniform(u)
    CALL rngUniform(v)
    v = 1.7156d0 * (v - half)

    ! Evaluate the quadratic form
    x = u - s
    y = dABS(v) - t
    q = x**2d0 + y*(a*y - b*x)

    ! Accept P if inside inner ellipse
    IF (q < r1) EXIT
    ! Reject P if outside outer ellipse
    IF (q > r2) CYCLE
    ! Reject P if outside acceptance region
    IF (v**2.d0 < -4.d0*dLOG(u)*u**2.d0) EXIT
  END DO

  ! Return ratio of P's coordinates as the normal deviate
  this = v/u
  if (present(std)) then
    if (.not. present(mean)) call eMsg('rngNormal : Need both mean and standard deviation')
    this=(std*this)+mean
  end if
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure rngNormal_d1D!(this,mean,std)
    !! Interfaced with [[rngNormal]]
  !====================================================================!
!  real(r64) :: this(:)
!  real(r64),optional :: mean,std
  integer(i32) :: i,N
  N=size(this)
  do i=1,N
    call rngNormal(this(i),mean,std)
  end do
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure rngNormal_d2D!(this,mean,std)
    !! Interfaced with [[rngNormal]]
  !====================================================================!
!  real(r64) :: this(:,:)
!  real(r64),optional :: mean,std
  integer(i32) :: i,iSub(2)
  integer(i32) :: n,nSub(2)
  nSub = shape(this)
  n=size(this)
  do i=1,n
    iSub = ind2sub(i,nSub)
    call rngNormal_d1(this(iSub(1),iSub(2)),mean,std)
  enddo
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure rngNormal_d3D!(this,mean,std)
    !! Interfaced with [[rngNormal]]
  !====================================================================!
!  real(r64) :: this(:,:,:)
!  real(r64),optional :: mean,std
  integer(i32) :: i,iSub(3)
  integer(i32) :: n,nSub(3)
  nSub = shape(this)
  n=size(this)
  do i=1,n
    iSub = ind2sub(i,nSub)
    call rngNormal_d1(this(iSub(1),iSub(2),iSub(3)),mean,std)
  enddo
  end procedure
  !====================================================================!

end submodule
