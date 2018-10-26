submodule (Prng_Class) sm_rngExponential
!! Generate random number from an exponential distribution.

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
implicit none

contains
  !====================================================================!
  module procedure rngExponential_d1_Prng!(this, res, lambda)
    !! Overloaded Type bound procedure Prng%rngExponential()
  !====================================================================!
  ! Adapted from Fortran 77 code from the book:
  !     Dagpunar, J. 'Principles of random variate generation'
  !     Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9

  ! FUNCTION GENERATES A RANDOM VARIATE IN [0,INFINITY) FROM
  ! A NEGATIVE EXPONENTIAL DlSTRIBUTION WlTH DENSITY PROPORTIONAL
  ! TO EXP(-random_exponential), USING INVERSION.
!  class(Prng) :: this
!  real(r64)  :: res
!  real(r64) :: lambda
  if (lambda <= 0.d0) call eMsg('Prng%rngExponential: lambda must be > 0')
  call rngExponential_unscaled_d1(this, res)
  res = res / lambda
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure rngExponential_d1D_Prng!(this, res, lambda)
    !! Overloaded Type bound procedure Prng%rngExponential()
  !====================================================================!
  ! class(Prng) :: this
  ! real(r64) :: res(:)
  ! real(r64) :: lambda
  integer :: i, N
  real(r64) :: wk

  N = size(res)

  if (lambda <= 0.d0) call eMsg('Prng%rngExponential: lambda must be > 0')
  wk = 1.d0 / lambda
  do i = 1, N
    call rngExponential_unscaled_d1(this, res(i))
    res(i) = wk * res(i)
  end do
  
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure rngExponential_d2D_Prng!(this, res, lambda)
    !! Overloaded Type bound procedure Prng%rngExponential()
  !====================================================================!
  ! class(Prng) :: this
  ! real(r64) :: res(:, :)
  ! real(r64) :: lambda
  integer(i32) :: i, iSub(2)
  integer(i32) :: n, nSub(2)
  real(r64) :: wk, wk2

  if (lambda <= 0.d0) call eMsg('Prng%rngExponential: lambda must be > 0')
  wk2 = 1.d0 / lambda

  nSub = shape(res)
  n = size(res)
  do i = 1, n
    iSub = ind2sub(i, nSub)
    call rngExponential_unscaled_d1(this, wk)
    res(iSub(1), iSub(2)) = wk2 * wk
  enddo
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure rngExponential_d3D_Prng!(this, res, lambda)
    !! Overloaded Type bound procedure Prng%rngExponential()
  !====================================================================!
  ! class(Prng) :: this
  ! real(r64) :: res(:,:,:)
  ! real(r64) :: lambda
  integer(i32) :: i, iSub(3)
  integer(i32) :: n, nSub(3)
  real(r64) :: wk, wk2

  if (lambda <= 0.d0) call eMsg('Prng%rngExponential: lambda must be > 0')
  wk2 = 1.d0 / lambda

  nSub = shape(res)
  n = size(res)
  do i = 1, n
    iSub = ind2sub(i, nSub)
    call rngExponential_unscaled_d1(this, wk)
    res(iSub(1), iSub(2), iSub(3)) = wk2 * wk
  enddo
  end procedure
  !====================================================================!

  !====================================================================!
  module procedure rngExponential_unscaled_d1!(this, res)
  !====================================================================!
  ! Adapted from Fortran 77 code from the book:
  !     Dagpunar, J. 'Principles of random variate generation'
  !     Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9

  ! FUNCTION GENERATES A RANDOM VARIATE IN [0,INFINITY) FROM
  ! A NEGATIVE EXPONENTIAL DlSTRIBUTION WlTH DENSITY PROPORTIONAL
  ! TO EXP(-random_exponential), USING INVERSION.
  !class(Prng)  :: this
  !real(r64)  :: res
  call this%rngUniform(res, 0.d0, 1.d0)

  do while (res == 0.d0)
    call this%rngUniform(res, 0.d0, 1.d0)
  end do
  res = -log(res)
  end procedure
  !====================================================================!
end submodule
