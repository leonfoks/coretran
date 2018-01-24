submodule (Prng_Class) sm_rngWeibull
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
  module procedure rngWeibull_d1_Prng!(this, res, lambda, k)
    !! Overloaded Type bound procedure Prng%rngWeibull()
  !====================================================================!
  ! Class(Prng), intent(inout) :: this
    !! Prnge Class
  ! real(r64), intent(inout) :: res(:)
    !! Draw from Weibull distribution
  ! real(r64), intent(in) :: lambda
    !! Scale of the distribution
  ! real(r64), intent(in) :: k
    !! Shape of the distribution
  if (lambda <= 0.d0) call eMsg('Prng%rngExponential: lambda must be > 0')
  if (dabs(k) < 1.d-16) call Emsg('Prng%rngWeibull : scale is very small, increase it.')
  call rngExponential_unscaled_d1(this, res)
  res =  (res / lambda)**(1.d0 / k)
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure rngWeibull_d1D_Prng!(this, res, lambda, k)
    !! Overloaded Type bound procedure Prng%rngWeibull()
  !====================================================================!
  ! Class(Prng), intent(inout) :: this
    !! Prnge Class
  ! real(r64), intent(inout) :: res(:)
    !! Draw from Weibull distribution
  ! real(r64), intent(in) :: lambda
    !! Scale of the distribution
  ! real(r64), intent(in) :: k
    !! Shape of the distribution
  integer(i32) :: i, N
  real(r64) :: wk, wk2, wk3
  if (lambda <= 0.d0) call eMsg('Prng%rngWeibull: lambda must be > 0')
  if (dabs(k) < 1.d-16) call Emsg('Prng%rngWeibull : scale is very small, increase it.')

  wk2 = 1.d0 / lambda
  wk3 = 1.d0 / k
  N = size(res)
  do i = 1, N
    call rngExponential_unscaled_d1(this, wk)
    res(i) =  (wk * wk2)**(wk3)
  end do
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure rngWeibull_d2D_Prng!(this, res, lambda, k)
    !! Overloaded Type bound procedure Prng%rngWeibull()
  !====================================================================!
  ! class(Prng) :: this
  ! real(r64) :: res(:, :)
  ! real(r64) :: lambda
  integer(i32) :: i, iSub(2)
  integer(i32) :: n, nSub(2)
  real(r64) :: wk, wk2, wk3

  if (lambda <= 0.d0) call eMsg('Prng%rngWeibull: lambda must be > 0')
  if (dabs(k) < 1.d-16) call Emsg('Prng%rngWeibull : scale is very small, increase it.')
  
  wk2 = 1.d0 / lambda
  wk3 = 1.d0 / k

  nSub = shape(res)
  n = size(res)
  do i = 1, n
    iSub = ind2sub(i, nSub)
    call rngExponential_unscaled_d1(this, wk)
    res(iSub(1), iSub(2)) =  (wk * wk2)**(wk3)
  enddo
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure rngWeibull_d3D_Prng!(this, res, lambda, k)
    !! Overloaded Type bound procedure Prng%rngWeibull()
  !====================================================================!
  ! class(Prng) :: this
  ! real(r64) :: res(:,:,:)
  ! real(r64) :: lambda
  integer(i32) :: i, iSub(3)
  integer(i32) :: n, nSub(3)
  real(r64) :: wk, wk2, wk3

  if (lambda <= 0.d0) call eMsg('Prng%rngWeibull: lambda must be > 0')
  if (dabs(k) < 1.d-16) call Emsg('Prng%rngWeibull : scale is very small, increase it.')

  wk2 = 1.d0 / lambda
  wk3 = 1.d0 / k

  nSub = shape(res)
  n = size(res)
  do i = 1, n
    iSub = ind2sub(i, nSub)
    call rngExponential_unscaled_d1(this, wk)
    res(iSub(1), iSub(2), iSub(3)) =  (wk * wk2)**(wk3)
  enddo
  end procedure
  !====================================================================!
end submodule


