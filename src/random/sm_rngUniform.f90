submodule (Prng_Class) sm_rngUniform
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
  module procedure rngUniform_d1_Prng!(this, res, rmin, rmax)
    !! Overloaded Type bound procedure Prng%rngUniform()
  !====================================================================!
  !class(Prng), intent(inout) :: this
  !  !! Prng Class
  !real(r64), intent(out) :: res
  !  !! Random uniform
  !real(r64), intent(in), optional :: rmin
  !  !! Minimum value to draw between. Requires rmax be used as well.
  !real(r64), intent(in), optional :: rmax
  !  !! Maximum value to draw between. Requires rmin be used as well.
  
  call rngUniform_xorshift(this, res)
  if (present(rmin)) then
    if (present(rmax)) res=rmin+(rmax-rmin)*res
  endif
  end procedure
  !====================================================================!

  !====================================================================!
  module procedure rngUniform_d1D_Prng!(this, res, rmin, rmax)
    !! Overloaded Type bound procedure Prng%rngUniform()
  !====================================================================!
  !class(Prng), intent(inout) :: this
  !  !! Prng Class
  !real(r64), intent(out) :: res
  !  !! Random uniform
  !real(r64), intent(in), optional :: rmin
  !  !! Minimum value to draw between. Requires rmax be used as well.
  !real(r64), intent(in), optional :: rmax
  !  !! Maximum value to draw between. Requires rmin be used as well.
  integer(i32) :: i, N
  N = size(res)
  do i = 1, N
    call rngUniform_d1_Prng(this, res(i), rmin, rmax)
  end do
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure rngUniform_d2D_Prng!(this, res, rmin, rmax)
    !! Overloaded Type bound procedure Prng%rngUniform()
  !====================================================================!
  !class(Prng), intent(inout) :: this
  !  !! Prng Class
  !real(r64), intent(out) :: res
  !  !! Random uniform
  !real(r64), intent(in), optional :: rmin
  !  !! Minimum value to draw between. Requires rmax be used as well.
  !real(r64), intent(in), optional :: rmax
  !  !! Maximum value to draw between. Requires rmin be used as well.
  integer(i32) :: i, iSub(2)
  integer(i32) :: n, nSub(2)
  nSub = shape(res)
  n=size(res)
  do i = 1, n
    iSub = ind2sub(i, nSub)
    call rngUniform_d1_Prng(this, res(iSub(1), iSub(2)), rmin, rmax)
  enddo
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure rngUniform_d3D_Prng!(this, res, rmin, rmax)
    !! Overloaded Type bound procedure Prng%rngUniform()
  !====================================================================!
  !class(Prng), intent(inout) :: this
  !  !! Prng Class
  !real(r64), intent(out) :: res
  !  !! Random uniform
  !real(r64), intent(in), optional :: rmin
  !  !! Minimum value to draw between. Requires rmax be used as well.
  !real(r64), intent(in), optional :: rmax
  !  !! Maximum value to draw between. Requires rmin be used as well.
  integer(i32) :: i, iSub(3)
  integer(i32) :: n, nSub(3)
  nSub = shape(res)
  n = size(res)
  do i = 1, n
    iSub = ind2sub(i, nSub)
    call rngUniform_d1_Prng(this, res(iSub(1) ,iSub(2) ,iSub(3)), rmin, rmax)
  enddo
  end procedure
  !====================================================================!
end submodule
