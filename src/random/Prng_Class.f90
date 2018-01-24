module Prng_Class
!!Class providing a pseudo-random number generator with an xorshift128+ or xorshift1024* generator that is suitable for parallel applications

use variableKind, only: r64, i32, i64
use m_allocate, only: allocate
use m_errors, only: eMsg
use m_time, only: timeToInteger
use m_indexing, only: ind2sub

implicit none

private

public :: Prng

type Prng
  !! Class that generates pseudo random numbers. See [[Prng_Class]] for more information on how to use this class.
  integer(i64), allocatable :: seed(:)
  integer(i32) :: ptr
  logical :: big

contains

  generic, public :: rngExponential => rngExponential_d1_Prng_, rngExponential_d1D_Prng_, rngExponential_d2D_Prng_, rngExponential_d3D_Prng_
    !! Prng%rngExponential() - Draw from an exponential distribution
    !!
    !! $y = \frac{-ln(\tilde{u})}{\lambda}$
    !! where $\tilde{u}$ is a sample from a uniform distribution
  procedure, private :: rngExponential_d1_Prng_ =>  rngExponential_d1_Prng
  procedure, private :: rngExponential_d1D_Prng_ => rngExponential_d1D_Prng 
  procedure, private :: rngExponential_d2D_Prng_ => rngExponential_d2D_Prng 
  procedure, private :: rngExponential_d3D_Prng_ => rngExponential_d3D_Prng

  generic, public :: rngInteger => rngInteger_i1_Prng_, rngInteger_i1D_Prng_, rngInteger_i2D_Prng_, rngInteger_i3D_Prng_
    !! Prng%rngInteger() - Draw a random integer in the interval imin <= i <= imax
    !!
    !! $y = x_{0} + (\tilde{u} * x_{1})$
    !! where $\tilde{u}$ is a sample from a uniform distribution, and integers are generated such that $x_{0} <= \tilde{u} <= x_{1}$.
  procedure, private :: rngInteger_i1_Prng_ =>  rngInteger_i1_Prng
  procedure, private :: rngInteger_i1D_Prng_ => rngInteger_i1D_Prng 
  procedure, private :: rngInteger_i2D_Prng_ => rngInteger_i2D_Prng 
  procedure, private :: rngInteger_i3D_Prng_ => rngInteger_i3D_Prng

  generic, public :: rngNormal => rngNormal_d1_Prng_, rngNormal_d1D_Prng_, rngNormal_d2D_Prng_, rngNormal_d3D_Prng_
    !! Prng%rngNormal() - Draw from a normal distribution.
  procedure, private :: rngNormal_d1_Prng_ => rngNormal_d1_Prng
  procedure, private :: rngNormal_d1D_Prng_ => rngNormal_d1D_Prng
  procedure, private :: rngNormal_d2D_Prng_ => rngNormal_d2D_Prng
  procedure, private :: rngNormal_d3D_Prng_ => rngNormal_d3D_Prng

  generic, public :: rngUniform => rngUniform_d1_Prng_,rngUniform_d1D_Prng_,rngUniform_d2D_Prng_,rngUniform_d3D_Prng_
    !! Prng%rngUniform() - Draw from a uniform distribution
    !!
    !! Draws uniform random numbers on (0, 1) using either the xorshift1024* or xorshift128+ algorithms.
  procedure, private :: rngUniform_d1_Prng_ => rngUniform_d1_Prng
  procedure, private :: rngUniform_d1D_Prng_ => rngUniform_d1D_Prng
  procedure, private :: rngUniform_d2D_Prng_ => rngUniform_d2D_Prng
  procedure, private :: rngUniform_d3D_Prng_ => rngUniform_d3D_Prng

  generic, public :: rngWeibull => rngWeibull_d1_Prng_,rngWeibull_d1D_Prng_,rngWeibull_d2D_Prng_,rngWeibull_d3D_Prng_
  !! Prng%rngWeibull() - Draw from a Weibull distribution
  !!
  !! $y = \left[ \frac{-1}{\lambda} ln(\tilde{u}) \right]^{\frac{1}{k}}$
  !! where $\tilde{u}$ is a sample from a uniform distribution and
  !! $\frac{-1}{\lambda} ln(\tilde{u})$ is a draw from an exponential distribution.
procedure, private :: rngWeibull_d1_Prng_ => rngWeibull_d1_Prng
procedure, private :: rngWeibull_d1D_Prng_ => rngWeibull_d1D_Prng
procedure, private :: rngWeibull_d2D_Prng_ => rngWeibull_d2D_Prng
procedure, private :: rngWeibull_d3D_Prng_ => rngWeibull_d3D_Prng
end type


interface Prng
  !!Overloaded Initializer for a Prng - Pseudo random number generator.
  !!
  !!Thread safe, xorshift1024* or xorshift128+ generator than can draw from different distributions.
  !!
  !!See [[Prng_Class]] for more information on how to use this class
  module procedure :: initWithSetseed_Prng, initWithRandomSeed_Prng
end interface

public :: rngExponential_unscaled_d1

interface
  !====================================================================!
  module subroutine rngInteger_1024star(this, val)
  !====================================================================!
  class(prng), intent(inout) :: this
  integer(i64), intent(out) :: val
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine rngUniform_1024star(this, val)
  !====================================================================!
  class(prng), intent(inout) :: this
  real(r64), intent(out) :: val
  end subroutine
  !====================================================================!

  !====================================================================!
  module subroutine rngExponential_d1_Prng(this, res, lambda)
    !! Overloaded Type bound procedure Prng%rngExponential()
  !====================================================================!
  class(Prng), intent(inout) :: this
    !! Prng Class
  real(r64), intent(out) :: res
    !! Draw from exponential distribution
  real(r64), intent(in) :: lambda
    !! Inverse scale > 0
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine rngExponential_d1D_Prng(this, res, lambda)
    !! Overloaded Type bound procedure Prng%rngExponential()
  !====================================================================!
  class(Prng), intent(inout) :: this
    !! Prng Class
  real(r64), intent(out) :: res(:)
    !! Draw from exponential distribution
  real(r64), intent(in) :: lambda
    !! Inverse scale > 0
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine rngExponential_d2D_Prng(this, res, lambda)
    !! Overloaded Type bound procedure Prng%rngExponential()
  !====================================================================!
  class(Prng), intent(inout) :: this
    !! Prng Class
  real(r64), intent(out) :: res(:,:)
    !! Draw from exponential distribution
  real(r64), intent(in) :: lambda
    !! Inverse scale > 0
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine rngExponential_d3D_Prng(this, res, lambda)
    !! Overloaded Type bound procedure Prng%rngExponential()
  !====================================================================!
  class(Prng), intent(inout) :: this
    !! Prng Class
  real(r64), intent(out) :: res(:,:,:)
    !! Draw from exponential distribution
  real(r64), intent(in) :: lambda
    !! Inverse scale > 0
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine rngExponential_unscaled_d1(this, res)
  !====================================================================!
  class(Prng)  :: this
    !! Prng Class
  real(r64)  :: res
    !! Draw from exponential distribution
  end subroutine
  !====================================================================!

  !====================================================================!
  module subroutine rngInteger_i1_Prng(this, res, imin, imax)
    !! Overloaded Type bound procedure Prng%rngInteger()
  !====================================================================!
  class(Prng), intent(inout) :: this
    !! Prng Class
  integer(i32), intent(out) :: res
    !! Random integer
  integer(i32), intent(in) :: imin
    !! Draw >= imin
  integer(i32), intent(in) :: imax
    !! Draw <= imax
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine rngInteger_i1D_Prng(this, res, imin, imax)
    !! Overloaded Type bound procedure Prng%rngInteger()
  !====================================================================!
  class(Prng), intent(inout) :: this
    !! Prng Class
  integer(i32), intent(out) :: res(:)
    !! Random integer
  integer(i32), intent(in) :: imin
    !! Draw >= imin
  integer(i32), intent(in) :: imax
    !! Draw <= imax
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine rngInteger_i2D_Prng(this, res, imin, imax)
    !! Overloaded Type bound procedure Prng%rngInteger()
  !====================================================================!
  class(Prng), intent(inout) :: this
    !! Prng Class
  integer(i32), intent(out) :: res(:,:)
    !! Random integer
  integer(i32), intent(in) :: imin
    !! Draw >= imin
  integer(i32), intent(in) :: imax
    !! Draw <= imax
    end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine rngInteger_i3D_Prng(this, res, imin, imax)
    !! Overloaded Type bound procedure Prng%rngInteger()
  !====================================================================!
  class(Prng), intent(inout) :: this
    !! Prng Class
  integer(i32), intent(out) :: res(:,:,:)
    !! Random integer
  integer(i32), intent(in) :: imin
    !! Draw >= imin
  integer(i32), intent(in) :: imax
    !! Draw <= imax
  end subroutine
  !====================================================================!

  !====================================================================!
  module subroutine rngNormal_d1_Prng(this, res, mean, std)
    !! Overloaded Type bound procedure Prng%rngNormal()
  !====================================================================!
  class(prng), intent(inout) :: this
    !! Prng Class
  real(r64), intent(inout) :: res
    !! Draw from random Normal
  real(r64), intent(in), optional :: mean
    !! Mean of the normal distribution
  real(r64), intent(in), optional :: std
    !! Standard deviation of the normal distribution
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine rngNormal_d1D_Prng(this, res, mean, std)
    !! Overloaded Type bound procedure Prng%rngNormal()
  !====================================================================!
  class(prng), intent(inout) :: this
    !! Prng Class
  real(r64), intent(inout) :: res(:)
    !! Draw from random Normal
  real(r64), intent(in), optional :: mean
    !! Mean of the normal distribution
  real(r64), intent(in), optional :: std
    !! Standard deviation of the normal distribution
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine rngNormal_d2D_Prng(this, res, mean, std)
    !! Overloaded Type bound procedure Prng%rngNormal()
  !====================================================================!
  class(prng), intent(inout) :: this
    !! Prng Class
  real(r64), intent(inout) :: res(:,:)
    !! Draw from random Normal
  real(r64), intent(in), optional :: mean
    !! Mean of the normal distribution
  real(r64), intent(in), optional :: std
    !! Standard deviation of the normal distribution
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine rngNormal_d3D_Prng(this, res, mean, std)
    !! Overloaded Type bound procedure Prng%rngNormal()
  !====================================================================!
  class(prng), intent(inout) :: this
    !! Prng Class
  real(r64), intent(inout) :: res(:,:,:)
    !! Draw from random Normal
  real(r64), intent(in), optional :: mean
    !! Mean of the normal distribution
  real(r64), intent(in), optional :: std
    !! Standard deviation of the normal distribution
  end subroutine
  !====================================================================!

  !====================================================================!
  module subroutine rngUniform_d1_Prng(this, res, rmin, rmax)
      !! Overloaded Type bound procedure Prng%rngUniform()
  !====================================================================!
  class(Prng), intent(inout) :: this
    !! Prng Class
  real(r64), intent(inout) :: res
    !! Random uniform
  real(r64), intent(in), optional :: rmin
    !! Minimum value to draw between. Requires rmax be used as well.
  real(r64), intent(in), optional :: rmax
    !! Maximum value to draw between. Requires rmin be used as well.
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine rngUniform_d1D_Prng(this, res, rmin, rmax)
    !! Overloaded Type bound procedure Prng%rngUniform()
  !====================================================================!
  class(Prng), intent(inout) :: this
    !! Prng Class
  real(r64), intent(inout) :: res(:)
    !! Random uniform
  real(r64), intent(in), optional :: rmin
    !! Minimum value to draw between. Requires rmax be used as well.
  real(r64), intent(in), optional :: rmax
    !! Maximum value to draw between. Requires rmin be used as well.
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine rngUniform_d2D_Prng(this, res, rmin, rmax)
    !! Overloaded Type bound procedure Prng%rngUniform()
  !====================================================================!
  class(Prng), intent(inout) :: this
    !! Prng Class
  real(r64), intent(inout) :: res(:,:)
    !! Random uniform
  real(r64), intent(in), optional :: rmin
    !! Minimum value to draw between. Requires rmax be used as well.
  real(r64), intent(in), optional :: rmax
    !! Maximum value to draw between. Requires rmin be used as well.
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine rngUniform_d3D_Prng(this, res, rmin, rmax)
    !! Overloaded Type bound procedure Prng%rngUniform()
  !====================================================================!
  class(Prng), intent(inout) :: this
    !! Prng Class
  real(r64), intent(inout) :: res(:,:,:)
    !! Random uniform
  real(r64), intent(in), optional :: rmin
    !! Minimum value to draw between. Requires rmax be used as well.
  real(r64), intent(in), optional :: rmax
    !! Maximum value to draw between. Requires rmin be used as well.
  end subroutine
  !====================================================================!

  !====================================================================!
  module subroutine rngWeibull_d1_Prng(this, res, lambda, k)
    !! Overloaded Type bound procedure Prng%rngWeibull()
  !====================================================================!
  Class(Prng), intent(inout) :: this
    !! Prnge Class
  real(r64), intent(inout) :: res
    !! Draw from Weibull distribution
  real(r64), intent(in) :: lambda
    !! Scale of the distribution
  real(r64), intent(in) :: k
    !! Shape of the distribution
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine rngWeibull_d1D_Prng(this, res, lambda, k)
    !! Overloaded Type bound procedure Prng%rngWeibull()
  !====================================================================!
  Class(Prng), intent(inout) :: this
    !! Prnge Class
  real(r64), intent(inout) :: res(:)
    !! Draw from Weibull distribution
  real(r64), intent(in) :: lambda
    !! Scale of the distribution
  real(r64), intent(in) :: k
    !! Shape of the distribution
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine rngWeibull_d2D_Prng(this, res, lambda, k)
    !! Overloaded Type bound procedure Prng%rngWeibull()
  !====================================================================!
  Class(Prng), intent(inout) :: this
    !! Prnge Class
  real(r64), intent(inout) :: res(:,:)
    !! Draw from Weibull distribution
  real(r64), intent(in) :: lambda
    !! Scale of the distribution
  real(r64), intent(in) :: k
    !! Shape of the distribution   
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine rngWeibull_d3D_Prng(this, res, lambda, k)
    !! Overloaded Type bound procedure Prng%rngWeibull()
  !====================================================================!
  Class(Prng), intent(inout) :: this
    !! Prnge Class
  real(r64), intent(inout) :: res(:,:,:)
    !! Draw from Weibull distribution
  real(r64), intent(in) :: lambda
    !! Scale of the distribution
  real(r64), intent(in) :: k
    !! Shape of the distribution
  end subroutine
  !====================================================================!
end interface

contains

!====================================================================!
subroutine getRandomSeed(seed, big)
!====================================================================!
integer(i64), intent(inout) :: seed(:)
  !! Random seeds of 64 bit integers. If big == true, must be size 16 else must be size 2.
logical, intent(in), optional :: big
  !! Use the high period xorshift1024* (true) or xorshift128+ (false). Default is false.

logical :: useXorshift1024star
integer(i32) :: dt(8)
integer(i32) :: i
integer(i32) :: nSeeds
integer(i64) :: t

useXorshift1024star = .false.
if (present(big)) useXorshift1024star = big

nSeeds = size(seed)

if (useXorshift1024star) then
  if (nSeeds /= 16) call eMsg('getRandomSeed: Seed must have size 16')
else
  if (nSeeds /= 2) call eMsg('getRandomSeed: Seed must have size 2')
endif

call date_and_time(VALUES=dt)

t = timeToInteger(dt)

! use splimix64 for initialization
do i = 1, nSeeds
  seed(i) = splitmix64(t)
end do

end subroutine
!====================================================================!
!====================================================================!
module function initWithSetseed_Prng(seed, big) result(this)
!====================================================================!
type(Prng) :: this
  !! Prng Class
integer(i64), intent(in) :: seed(:)
  !! Fixed seeds of 64 bit integers. If big == true, must be length 16 else must be length 2.
logical, intent(in), optional :: big
  !! Use the high period xorshift1024* (true) or xorshift128+ (false). Default is false.

logical :: useXorshift1024star
integer(i32) :: i
integer(i32) :: nSeeds
integer(i64) :: tmp

useXorshift1024star = .false.
if (present(big)) useXorshift1024star = big

if (useXorshift1024star) then
  if (size(seed) /= 16) call eMsg('Prng: Seed must have size 16')
  nSeeds = 16
else
  if (size(seed) /= 2) call eMsg('Prng: Seed must have size 2')
  nSeeds = 2
endif

call allocate(this%seed, 0, nSeeds)

if(all(seed(:) == 0_i64)) then
  ! initialize with some non-zero values
  tmp = 0
  do i = 0, nSeeds-1
    this%seed(i) = splitmix64(tmp)
  end do
else
  this%seed(0:nSeeds-1) = seed(1:16)
endif
this%ptr = 0

end function
!====================================================================!
!====================================================================!
function initWithRandomSeed_Prng(big) result(this)
!====================================================================!
type(Prng) :: this
  !! Prng Class
logical, intent(in), optional :: big
  !! Use the high period xorshift1024* (true) or xorshift128+ (false). Default is false.

integer(i32) :: dt(8)
integer(i64) :: t
integer(i64) :: s(16)

call getRandomSeed(s, big)

call setSeed_Prng(this, s, big)
end function
!====================================================================!
!====================================================================!
subroutine setSeed_Prng(this, seed, big)
!====================================================================!
type(Prng), intent(inout) :: this
  !! Prng Class
integer(i64), intent(in) :: seed(:)
  !! Fixed seeds of 64 bit integers. If big == true, must be length 16 else must be length 2.
logical, intent(in), optional :: big
  !! Use the high period xorshift1024* (true) or xorshift128+ (false). Default is false.

logical :: useXorshift1024star
integer(i32) :: i
integer(i32) :: nSeeds
integer(i64) :: tmp

useXorshift1024star = .false.
if (present(big)) useXorshift1024star = big

if (useXorshift1024star) then
  if (size(seed) /= 16) call eMsg('Prng: Seed must have size 16')
  nSeeds = 16
else
  if (size(seed) /= 2) call eMsg('Prng: Seed must have size 2')
  nSeeds = 2
endif

call allocate(this%seed, 0, nSeeds)

if(all(seed(:) == 0_i64)) then
  ! initialize with some non-zero values
  tmp = 0
  do i = 0, nSeeds-1
    this%seed(i) = splitmix64(tmp)
  end do
else
  this%seed(0:nSeeds-1) = seed(1:16)
endif
this%ptr = 0

end subroutine
!====================================================================!

!====================================================================!
function splitmix64(seed) result(res)
  !! SplitMix64 implementation for random number seed initialization
!====================================================================!
integer(i64), intent(inout) :: seed
integer(i64) :: res

! As of Fortran95 BOZ literals are available only in DATA stmts.
! Also, gfortran does not allow integer(8) greater than 2^63...
integer(i64), parameter :: step = -7046029254386353131_i64 ! 0x9E3779B97F4A7C15
integer(i64), parameter :: mix1 = -4658895280553007687_i64 ! 0xBF58476D1CE4E5B9
integer(i64), parameter :: mix2 = -7723592293110705685_i64 ! 0x94D049BB133111EB

! gfortran may issue warning at the following lines.
! This is because splitmix64 assumes uint64 wrap-around, which is undefined in F90/95 std.
! Even though there are warnings, AFAIK, generated assembler codes are ones as expected.
seed = seed + step
res = seed
res = ieor(res, ishft(res, -30)) * mix1
res = ieor(res, ishft(res, -27)) * mix2
res = ieor(res, ishft(res, -31))
end function
!====================================================================!
end module 