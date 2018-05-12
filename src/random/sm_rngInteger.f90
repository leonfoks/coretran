submodule (Prng_Class) sm_rngInteger
  !! Generate random integers.

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
  module procedure rngInteger_i1_Prng!(this, res, imin, imax)
    !! Overloaded Type bound procedure Prng%rngInteger()
  !====================================================================!
  !class(Prng), intent(inout) :: this
  !  !! Prng Class
  !integer(i32), intent(out) :: res
  !  !! Random integer
  !integer(i32), intent(in) :: imin
  !  !! Draw >= imin
  !integer(i32), intent(in) :: imax
  !  !! Draw <= imax
  
  real(r64) :: wk
  
  call this%rngUniform(wk, 0.d0, 1.d0)
  
  res = imin + idnint(wk * dble(imax - imin))
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure rngInteger_i1D_Prng!(this, res, imin, imax)
    !! Overloaded Type bound procedure Prng%rngInteger()
  !====================================================================!
  !class(Prng), intent(inout) :: this
  !  !! Prng Class
  !integer(i32), intent(out) :: res
  !  !! Random integer
  !integer(i32), intent(in) :: imin
  !  !! Draw >= imin
  !integer(i32), intent(in) :: imax
  !  !! Draw <= imax
  integer(i32) :: i, N
  N = size(res)
  do i = 1, N
    call rngInteger_i1_Prng(this, res(i), imin, imax)
  end do
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure rngInteger_i2D_Prng!(this, res, imin, imax)
    !! Overloaded Type bound procedure Prng%rngInteger()
  !====================================================================!
  !class(Prng), intent(inout) :: this
  !  !! Prng Class
  !integer(i32), intent(out) :: res
  !  !! Random integer
  !integer(i32), intent(in) :: imin
  !  !! Draw >= imin
  !integer(i32), intent(in) :: imax
    !! Draw <= imax
  integer(i32) :: i, iSub(2)
  integer(i32) :: n, nSub(2)
  nSub = shape(res)
  n=size(res)
  do i = 1, n
    iSub = ind2sub(i, nSub)
    call rngInteger_i1_Prng(this, res(iSub(1), iSub(2)), imin, imax)
  enddo
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure rngInteger_i3D_Prng!(this, res, imin, imax)
    !! Overloaded Type bound procedure Prng%rngInteger()
  !====================================================================!
  !class(Prng), intent(inout) :: this
  !  !! Prng Class
  !integer(i32), intent(out) :: res
  !  !! Random integer
  !integer(i32), intent(in) :: imin
  !  !! Draw >= imin
  !integer(i32), intent(in) :: imax
    !! Draw <= imax
  integer(i32) :: i, iSub(3)
  integer(i32) :: n, nSub(3)
  nSub = shape(res)
  n = size(res)
  do i = 1, n
    iSub = ind2sub(i, nSub)
    call rngInteger_i1_Prng(this, res(iSub(1) ,iSub(2) ,iSub(3)), imin, imax)
  enddo
  end procedure
  !====================================================================!
end submodule
