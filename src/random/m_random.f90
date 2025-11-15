module m_random
  !!# Random number generation - Not parallel safe
  !!This module has been comprised using different sources. The first was a public domain pseudo-random number
  !!generator using the xorshift1024* and xorshift128+ methods.  See [[Prng_Class]]
  !!for more details on those methods.
  !!
  !!The secound source are the functions inside http://www.netlib.org/random/random.f90.  These are used to generate 
  !!random numbers from distributions other than a uniform distribution.
  !!
  !!This module contains subroutines to generate numbers randomly from different distributions. 
  !!They do so by instantiating a globally available [[Prng_Class]] and making calls into its type bound procedures.
  !!Since the Prng for these routines is globally available, you should *not* use it for any random number generation
  !!in parallel, whether using OpenMP or MPI!  Instead, refer to [[Prng_Class]] to see how to use the Prng in a thread safe manner.
  !!
  !!
  !!Example Usage:
  !!```fortran
  !!program randomTest
  !!
  !!use m_random, only: setPRNG, rngInteger, rngUniform, rngNormal, rngExponential, rngrngWeibull
  !!
  !!implicit none
  !!
  !!
  !!
  !!
  !!end program
  !!```
  !!Original Netlib random.f90
  !!Author: Alan Miller
  !!
  !!Updated coretran version:
  !!Version 2.00, 11 November 2016
  !!Increased precision to double
  !!Added seed setters
  !!Added overloaded operations for single number, nD arrays

  use variableKind
  use m_allocate, only: allocate
  use m_deallocate, only: deallocate
  use m_errors, only: eMsg, mErr, msg
  use Prng_Class, only: Prng
  use m_strings, only: printOptions, str
  use iso_fortran_env, only: output_unit
  use m_unitTester, only: tester

  implicit none


  ! Global Prng Class but private to this module!  I hate global variables, but here it is necessary to have a global Prng for serial code.
  type(Prng) :: globalPrng

  public :: setPrng

  interface setPrng
    module procedure :: setPrng_withSeed, setPrng_WOseed

    ! !====================================================================!
    ! module subroutine setRNG_withSeed(seed, big)
    !   !! Interfaced to setRNG()
    !   !! Sets the seed of the random number generator with a specified seed
    ! !====================================================================!
    ! integer(i64), intent(in) :: seed(:)
    ! logical, intent(in), optional :: big
    ! end subroutine
    ! !====================================================================!
    ! !====================================================================!
    ! module subroutine setRNG_WOseed(big, display)
    !   !! Interfaced to setRNG()
    !   !! 'Randomly' sets the seed of the random number generator
    ! !====================================================================!
    ! logical, intent(in), optional :: big
    ! logical, intent(in), optional :: display
    ! end subroutine
    ! !====================================================================!
  end interface

  public :: rngChisq

  interface rngChisq
    !! Pull from a Chi Ssquared distribution
    !!
    !! Example
    !!```fortran
    !!program Chisq_test
    !!use variableKind, only: r64
    !!use m_allocate, only: allocate
    !!use m_random, only: rngChiSq
    !!implicit none
    !!real(r64), allocatable :: a
    !!call allocate(a, 10000)
    !!call rngChiSq(a, )
    !!end program
    !!```
    !====================================================================!
    module subroutine rngChisq_d1(this,ndf,first)
      !! Interfaced with [[rngChiSq]]
    !====================================================================!
    ! Generates a random variate from the chi-squared distribution with
    ! ndf degrees of freedom
    real(r64) :: this
    INTEGER, INTENT(IN) :: ndf
    LOGICAL, INTENT(IN) :: first
    END subroutine
    !====================================================================!
    !====================================================================!
    module subroutine rngChisq_d1D(this,ndf,first)
      !! Interfaced with [[rngChiSq]]
    !====================================================================!
    real(r64) :: this(:)
    INTEGER, INTENT(IN) :: ndf
    LOGICAL, INTENT(IN) :: first
    end subroutine
    !====================================================================!
  end interface

  public :: rngExponential

  interface rngExponential
    module procedure :: rngExponential_d1, rngExponential_d1D, rngExponential_d2D, rngExponential_d3D
  end interface

  public :: rngGamma

  interface rngGamma
    !====================================================================!
    module subroutine rngGamma_d1D(this,s,first)
      !! Interfaced with [[rngGamma]]
    !====================================================================!
    real(r64) :: this(:)
    real(r64), INTENT(IN) :: s
    LOGICAL, INTENT(IN) :: first
    end subroutine
    !====================================================================!
    !====================================================================!
    module subroutine rngGamma_d1(this,s,first)
      !! Interfaced with [[rngGamma]]
    !====================================================================!
    real(r64) :: this
    real(r64), INTENT(IN)    :: s
    LOGICAL, INTENT(IN) :: first
    END subroutine
    !====================================================================!
  end interface

  public :: rngInteger

  interface rngInteger
    !! Generate size(this) random integers starting from imin
    module procedure :: rngInteger_i1, rngInteger_i1D, rngInteger_i2D, rngInteger_i3D
  end interface


  public :: rngNormal

  interface rngNormal
    module procedure :: rngNormal_d1, rngNormal_d1D, rngNormal_d2D, rngNormal_d3D
  end interface

  public :: rngUniform

  interface rngUniform
    module procedure :: rngUniform_d1, rngUniform_d1D, rngUniform_d2D, rngUniform_d3D
  end interface

  public :: rngWeibull

  interface rngWeibull
    module procedure :: rngWeibull_d1, rngWeibull_d1D, rngWeibull_d2D, rngWeibull_d3D
  end interface


  contains


  !====================================================================!
  subroutine setPrng_withSeed(seed, big)
    !! Interfaced to setPrng()
    !! Sets the seed of the random number generator with a specified seed
    !! 
  !====================================================================!
  integer(i64), intent(in) :: seed(:)
  logical, intent(in), optional :: big
  
  globalPrng = Prng(seed, big)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine setPrng_WOseed(big, display)
    !! Interfaced to setPrng()
    !! 'Randomly' sets the seed of the random number generator
  !====================================================================!
  !integer, allocatable :: seed(:)

  logical, intent(in), optional :: big
!  integer :: n,istat
  logical, intent(in), optional :: display

  globalPrng = Prng(big, display)

  end subroutine
  !====================================================================!

  !====================================================================!
  subroutine rngExponential_d1(this, lambda)
    !! Interfaced with [[rngExponential]]
  !====================================================================!
  real(r64)  :: this
    !! Random number
  real(r64) :: lambda
    !! Inverse scale > 0
  call globalPrng%rngExponential(this, lambda)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine rngExponential_d1D(this, lambda)
    !! Interfaced with [[rngExponential]]
  !====================================================================!
  real(r64) :: this(:)
    !! Random numbers
  real(r64) :: lambda
    !! Inverse scale > 0
  call globalPrng%rngExponential(this, lambda)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine rngExponential_d2D(this, lambda)
    !! Interfaced with [[rngExponential]]
  !====================================================================!
  real(r64) :: this(:,:)
    !! Random numbers
  real(r64) :: lambda
    !! Inverse scale > 0
  call globalPrng%rngExponential(this, lambda)    
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine rngExponential_d3D(this, lambda)
    !! Interfaced with [[rngExponential]]
  !====================================================================!
  real(r64) :: this(:,:,:)
    !! Random numbers
  real(r64) :: lambda
    !! Inverse scale > 0
  call globalPrng%rngExponential(this, lambda)
  end subroutine
  !====================================================================!

  !====================================================================!
  subroutine rngInteger_i1(this, imin, imax)
    !! Interfaced with [[rngInteger]]
  !====================================================================!
  integer(i32), intent(inout) :: this
  integer(i32), intent(in) :: imin
  integer(i32), intent(in) :: imax
  call globalPrng%rngInteger(this, imin, imax)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine rngInteger_i1D(this, imin, imax)
    !! Interfaced with [[rngInteger]]
  !====================================================================!
  integer(i32), intent(inout) :: this(:)
  integer(i32), intent(in) :: imin
  integer(i32), intent(in) :: imax
  call globalPrng%rngInteger(this, imin, imax)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine rngInteger_i2D(this, imin, imax)
    !! Interfaced with [[rngInteger]]
  !====================================================================!
  integer(i32), intent(inout) :: this(:,:)
  integer(i32), intent(in) :: imin
  integer(i32), intent(in) :: imax
  call globalPrng%rngInteger(this, imin, imax)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine rngInteger_i3D(this, imin, imax)
    !! Interfaced with [[rngInteger]]
  !====================================================================!
  integer(i32), intent(inout) :: this(:,:,:)
  integer(i32), intent(in) :: imin
  integer(i32), intent(in) :: imax
  call globalPrng%rngInteger(this, imin, imax)
  end subroutine
  !====================================================================!

  !====================================================================!
  subroutine rngNormal_d1(this, mean, std)
    !! Interfaced with [[rngNormal]]
  !====================================================================!
  real(r64), intent(inout) :: this
  real(r64), intent(in), optional :: mean, std
  call globalPrng%rngNormal(this, mean, std)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine rngNormal_d1D(this, mean, std)
    !! Interfaced with [[rngNormal]]
  !====================================================================!
  real(r64), intent(inout) :: this(:)
  real(r64),optional, intent(in) :: mean, std
  call globalPrng%rngNormal(this, mean, std)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine rngNormal_d2D(this, mean, std)
    !! Interfaced with [[rngNormal]]
  !====================================================================!
  real(r64), intent(inout) :: this(:,:)
  real(r64),optional, intent(in) :: mean, std
  call globalPrng%rngNormal(this, mean, std)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine rngNormal_d3D(this, mean, std)
    !! Interfaced with [[rngNormal]]
  !====================================================================!
  real(r64), intent(inout) :: this(:,:,:)
  real(r64),optional, intent(in) :: mean, std
  call globalPrng%rngNormal(this, mean, std)
  end subroutine
  !====================================================================!

  !====================================================================!
  subroutine rngUniform_d1(this,rmin,rmax)
    !! Interfaced with [[rngUniform]]
  !====================================================================!
  real(r64), intent(inout) :: this
  real(r64), intent(in), optional :: rmin
  real(r64), intent(in), optional :: rmax
  call globalPrng%rngUniform(this, rmin, rmax)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine rngUniform_d1D(this,rmin,rmax)
    !! Interfaced with [[rngUniform]]
  !====================================================================!
  real(r64), intent(inout) :: this(:)
  real(r64), intent(in), optional :: rmin
  real(r64), intent(in), optional :: rmax
  call globalPrng%rngUniform(this, rmin, rmax)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine rngUniform_d2D(this,rmin,rmax)
    !! Interfaced with [[rngUniform]]
  !====================================================================!
  real(r64), intent(inout) :: this(:,:)
  real(r64), intent(in), optional :: rmin
  real(r64), intent(in), optional :: rmax
  call globalPrng%rngUniform(this, rmin, rmax)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine rngUniform_d3D(this,rmin,rmax)
    !! Interfaced with [[rngUniform]]
  !====================================================================!
  real(r64), intent(inout) :: this(:,:,:)
  real(r64), intent(in), optional :: rmin
  real(r64), intent(in), optional :: rmax
  call globalPrng%rngUniform(this, rmin, rmax)
  end subroutine
  !====================================================================!

  !====================================================================!
  subroutine rngWeibull_d1(this, lambda, k)
    !! Interfaced with [[rngWeibull]]
  !====================================================================!
  real(r64), intent(inout) :: this
    !! Draw from Weibull distribution
  real(r64), intent(in) :: lambda
    !! Scale of the distribution
  real(r64), intent(in) :: k
    !! Shape of the distribution
  call globalPrng%rngWeibull(this, lambda, k)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine rngWeibull_d1D(this, lambda, k)
    !! Interfaced with [[rngWeibull]]
  !====================================================================!
  real(r64), intent(inout) :: this(:)
    !! Draw from Weibull distribution
  real(r64), intent(in) :: lambda
    !! Scale of the distribution
  real(r64), intent(in) :: k
    !! Shape of the distribution
  call globalPrng%rngWeibull(this, lambda, k)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine rngWeibull_d2D(this, lambda, k)
    !! Interfaced with [[rngWeibull]]
  !====================================================================!
  real(r64), intent(inout) :: this(:,:)
    !! Draw from Weibull distribution
  real(r64), intent(in) :: lambda
    !! Scale of the distribution
  real(r64), intent(in) :: k
    !! Shape of the distribution
  call globalPrng%rngWeibull(this, lambda, k)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine rngWeibull_d3D(this, lambda, k)
    !! Interfaced with [[rngWeibull]]
  !====================================================================!
  real(r64), intent(inout) :: this(:,:,:)
    !! Draw from Weibull distribution
  real(r64), intent(in) :: lambda
    !! Scale of the distribution
  real(r64), intent(in) :: k
    !! Shape of the distribution
  call globalPrng%rngWeibull(this, lambda, k)
  end subroutine
  !====================================================================!

end module

  !====================================================================!
  ! EXTRA CODES TO BE TRANSLATED LATER!!!
  !====================================================================!
  !
  !
  !  FUNCTION random_beta(aa, bb, first) RESULT(fn_val)
  !
  !! Adapted from Fortran 77 code from the book:
  !!     Dagpunar, J. 'Principles of random variate generation'
  !!     Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9
  !
  !! FUNCTION GENERATES A RANDOM VARIATE IN [0,1]
  !! FROM A BETA DISTRIBUTION WITH DENSITY
  !! PROPORTIONAL TO BETA**(AA-1) * (1-BETA)**(BB-1).
  !! USING CHENG'S LOG LOGISTIC METHOD.
  !
  !!     AA = SHAPE PARAMETER FROM DISTRIBUTION (0 < REAL)
  !!     BB = SHAPE PARAMETER FROM DISTRIBUTION (0 < REAL)
  !
  !REAL, INTENT(IN)    :: aa, bb
  !LOGICAL, INTENT(IN) :: first
  !REAL                :: fn_val
  !
  !!     Local variables
  !REAL, PARAMETER  :: aln4 = 1.3862944
  !REAL             :: a, b, g, r, s, x, y, z
  !REAL, SAVE       :: d, f, h, t, c
  !LOGICAL, SAVE    :: swap
  !
  !IF (aa <= zero .OR. bb <= zero) THEN
  !  WRITE(*, *) 'IMPERMISSIBLE SHAPE PARAMETER VALUE(S)'
  !  STOP
  !END IF
  !
  !IF (first) THEN                        ! Initialization, if necessary
  !  a = aa
  !  b = bb
  !  swap = b > a
  !  IF (swap) THEN
  !    g = b
  !    b = a
  !    a = g
  !  END IF
  !  d = a/b
  !  f = a+b
  !  IF (b > one) THEN
  !    h = SQRT((two*a*b - f)/(f - two))
  !    t = one
  !  ELSE
  !    h = b
  !    t = one/(one + (a/(vlarge*b))**b)
  !  END IF
  !  c = a+h
  !END IF
  !
  !DO
  !  CALL RANDOM_NUMBER(r)
  !  CALL RANDOM_NUMBER(x)
  !  s = r*r*x
  !  IF (r < vsmall .OR. s <= zero) CYCLE
  !  IF (r < t) THEN
  !    x = LOG(r/(one - r))/h
  !    y = d*EXP(x)
  !    z = c*x + f*LOG((one + d)/(one + y)) - aln4
  !    IF (s - one > z) THEN
  !      IF (s - s*z > one) CYCLE
  !      IF (LOG(s) > z) CYCLE
  !    END IF
  !    fn_val = y/(one + y)
  !  ELSE
  !    IF (4.0*s > (one + one/d)**f) CYCLE
  !    fn_val = one
  !  END IF
  !  EXIT
  !END DO
  !
  !IF (swap) fn_val = one - fn_val
  !RETURN
  !END FUNCTION random_beta
  !
  !
  !
  !FUNCTION random_t(m) RESULT(fn_val)
  !
  !! Adapted from Fortran 77 code from the book:
  !!     Dagpunar, J. 'Principles of random variate generation'
  !!     Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9
  !
  !! FUNCTION GENERATES A RANDOM VARIATE FROM A
  !! T DISTRIBUTION USING KINDERMAN AND MONAHAN'S RATIO METHOD.
  !
  !!     M = DEGREES OF FREEDOM OF DISTRIBUTION
  !!           (1 <= 1NTEGER)
  !
  !INTEGER, INTENT(IN) :: m
  !REAL                :: fn_val
  !
  !!     Local variables
  !REAL, SAVE      :: s, c, a, f, g
  !REAL            :: r, x, v
  !
  !REAL, PARAMETER :: three = 3.0, four = 4.0, quart = 0.25,   &
    !                   five = 5.0, sixteen = 16.0
    !INTEGER         :: mm = 0
    !
    !IF (m < 1) THEN
    !  WRITE(*, *) 'IMPERMISSIBLE DEGREES OF FREEDOM'
    !  STOP
    !END IF
    !
    !IF (m /= mm) THEN                    ! Initialization, if necessary
    !  s = m
    !  c = -quart*(s + one)
    !  a = four/(one + one/s)**c
    !  f = sixteen/a
    !  IF (m > 1) THEN
    !    g = s - one
    !    g = ((s + one)/g)**c*SQRT((s+s)/g)
    !  ELSE
    !    g = one
    !  END IF
    !  mm = m
    !END IF
    !
    !DO
    !  CALL RANDOM_NUMBER(r)
    !  IF (r <= zero) CYCLE
    !  CALL RANDOM_NUMBER(v)
    !  x = (two*v - one)*g/r
    !  v = x*x
    !  IF (v > five - a*r) THEN
    !    IF (m >= 1 .AND. r*(v + three) > f) CYCLE
    !    IF (r > (one + v/s)**c) CYCLE
    !  END IF
    !  EXIT
    !END DO
    !
    !fn_val = x
    !RETURN
    !END FUNCTION random_t
    !
    !
    !
    !SUBROUTINE random_mvnorm(n, h, d, f, first, x, ier)
    !
    !! Adapted from Fortran 77 code from the book:
    !!     Dagpunar, J. 'Principles of random variate generation'
    !!     Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9
    !
    !! N.B. An extra argument, ier, has been added to Dagpunar's routine
    !
    !!     SUBROUTINE GENERATES AN N VARIATE RANDOM NORMAL
    !!     VECTOR USING A CHOLESKY DECOMPOSITION.
    !
    !! ARGUMENTS:
    !!        N = NUMBER OF VARIATES IN VECTOR
    !!           (INPUT,INTEGER >= 1)
    !!     H(J) = J'TH ELEMENT OF VECTOR OF MEANS
    !!           (INPUT,REAL)
    !!     X(J) = J'TH ELEMENT OF DELIVERED VECTOR
    !!           (OUTPUT,REAL)
    !!
    !!    D(J*(J-1)/2+I) = (I,J)'TH ELEMENT OF VARIANCE MATRIX (J> = I)
    !!            (INPUT,REAL)
    !!    F((J-1)*(2*N-J)/2+I) = (I,J)'TH ELEMENT OF LOWER TRIANGULAR
    !!           DECOMPOSITION OF VARIANCE MATRIX (J <= I)
    !!            (OUTPUT,REAL)
    !
    !!    FIRST = .TRUE. IF THIS IS THE FIRST CALL OF THE ROUTINE
    !!    OR IF THE DISTRIBUTION HAS CHANGED SINCE THE LAST CALL OF THE ROUTINE.
    !!    OTHERWISE SET TO .FALSE.
    !!            (INPUT,LOGICAL)
    !
    !!    ier = 1 if the input covariance matrix is not +ve definite
    !!        = 0 otherwise
    !
    !INTEGER, INTENT(IN)   :: n
    !REAL, INTENT(IN)      :: h(:), d(:)   ! d(n*(n+1)/2)
    !REAL, INTENT(IN OUT)  :: f(:)         ! f(n*(n+1)/2)
    !REAL, INTENT(OUT)     :: x(:)
    !LOGICAL, INTENT(IN)   :: first
    !INTEGER, INTENT(OUT)  :: ier
    !
    !!     Local variables
    !INTEGER       :: j, i, m
    !REAL          :: y, v
    !INTEGER, SAVE :: n2
    !
    !IF (n < 1) THEN
    !  WRITE(*, *) 'SIZE OF VECTOR IS NON POSITIVE'
    !  STOP
    !END IF
    !
    !ier = 0
    !IF (first) THEN                        ! Initialization, if necessary
    !  n2 = 2*n
    !  IF (d(1) < zero) THEN
    !    ier = 1
    !    RETURN
    !  END IF
    !
    !  f(1) = SQRT(d(1))
    !  y = one/f(1)
    !  DO j = 2,n
    !    f(j) = d(1+j*(j-1)/2) * y
    !  END DO
    !
    !  DO i = 2,n
    !    v = d(i*(i-1)/2+i)
    !    DO m = 1,i-1
    !      v = v - f((m-1)*(n2-m)/2+i)**2
    !    END DO
    !
    !    IF (v < zero) THEN
    !      ier = 1
    !      RETURN
    !    END IF
    !
    !    v = SQRT(v)
    !    y = one/v
    !    f((i-1)*(n2-i)/2+i) = v
    !    DO j = i+1,n
    !      v = d(j*(j-1)/2+i)
    !      DO m = 1,i-1
    !        v = v - f((m-1)*(n2-m)/2+i)*f((m-1)*(n2-m)/2 + j)
    !      END DO ! m = 1,i-1
    !      f((i-1)*(n2-i)/2 + j) = v*y
    !    END DO ! j = i+1,n
    !  END DO ! i = 2,n
    !END IF
    !
    !x(1:n) = h(1:n)
    !DO j = 1,n
    !  y = random_normal()
    !  DO i = j,n
    !    x(i) = x(i) + f((j-1)*(n2-j)/2 + i) * y
    !  END DO ! i = j,n
    !END DO ! j = 1,n
    !
    !RETURN
    !END SUBROUTINE random_mvnorm
    !
    !
    !
    !FUNCTION random_inv_gauss(h, b, first) RESULT(fn_val)
    !
    !! Adapted from Fortran 77 code from the book:
    !!     Dagpunar, J. 'Principles of random variate generation'
    !!     Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9
    !
    !! FUNCTION GENERATES A RANDOM VARIATE IN [0,INFINITY] FROM
    !! A REPARAMETERISED GENERALISED INVERSE GAUSSIAN (GIG) DISTRIBUTION
    !! WITH DENSITY PROPORTIONAL TO  GIG**(H-1) * EXP(-0.5*B*(GIG+1/GIG))
    !! USING A RATIO METHOD.
    !
    !!     H = PARAMETER OF DISTRIBUTION (0 <= REAL)
    !!     B = PARAMETER OF DISTRIBUTION (0 < REAL)
    !
    !REAL, INTENT(IN)    :: h, b
    !LOGICAL, INTENT(IN) :: first
    !REAL                :: fn_val
    !
    !!     Local variables
    !REAL            :: ym, xm, r, w, r1, r2, x
    !REAL, SAVE      :: a, c, d, e
    !REAL, PARAMETER :: quart = 0.25
    !
    !IF (h < zero .OR. b <= zero) THEN
    !  WRITE(*, *) 'IMPERMISSIBLE DISTRIBUTION PARAMETER VALUES'
    !  STOP
    !END IF
    !
    !IF (first) THEN                        ! Initialization, if necessary
    !  IF (h > quart*b*SQRT(vlarge)) THEN
    !    WRITE(*, *) 'THE RATIO H:B IS TOO SMALL'
    !    STOP
    !  END IF
    !  e = b*b
    !  d = h + one
    !  ym = (-d + SQRT(d*d + e))/b
    !  IF (ym < vsmall) THEN
    !    WRITE(*, *) 'THE VALUE OF B IS TOO SMALL'
    !    STOP
    !  END IF
    !
    !  d = h - one
    !  xm = (d + SQRT(d*d + e))/b
    !  d = half*d
    !  e = -quart*b
    !  r = xm + one/xm
    !  w = xm*ym
    !  a = w**(-half*h) * SQRT(xm/ym) * EXP(-e*(r - ym - one/ym))
    !  IF (a < vsmall) THEN
    !    WRITE(*, *) 'THE VALUE OF H IS TOO LARGE'
    !    STOP
    !  END IF
    !  c = -d*LOG(xm) - e*r
    !END IF
    !
    !DO
    !  CALL RANDOM_NUMBER(r1)
    !  IF (r1 <= zero) CYCLE
    !  CALL RANDOM_NUMBER(r2)
    !  x = a*r2/r1
    !  IF (x <= zero) CYCLE
    !  IF (LOG(r1) < d*LOG(x) + e*(x + one/x) + c) EXIT
    !END DO
    !
    !fn_val = x
    !
    !RETURN
    !END FUNCTION random_inv_gauss
    !
    !
    !
    !FUNCTION random_Poisson(mu, first) RESULT(ival)
    !!**********************************************************************
    !!     Translated to Fortran 90 by Alan Miller from:
    !!                           RANLIB
    !!
    !!     Library of Fortran Routines for Random Number Generation
    !!
    !!                    Compiled and Written by:
    !!
    !!                         Barry W. Brown
    !!                          James Lovato
    !!
    !!             Department of Biomathematics, Box 237
    !!             The University of Texas, M.D. Anderson Cancer Center
    !!             1515 Holcombe Boulevard
    !!             Houston, TX      77030
    !!
    !! This work was supported by grant CA-16672 from the National Cancer Institute.
    !
    !!                    GENerate POIsson random deviate
    !
    !!                            Function
    !
    !! Generates a single random deviate from a Poisson distribution with mean mu.
    !
    !!                            Arguments
    !
    !!     mu --> The mean of the Poisson distribution from which
    !!            a random deviate is to be generated.
    !!                              REAL mu
    !
    !!                              Method
    !
    !!     For details see:
    !
    !!               Ahrens, J.H. and Dieter, U.
    !!               Computer Generation of Poisson Deviates
    !!               From Modified Normal Distributions.
    !!               ACM Trans. Math. Software, 8, 2
    !!               (June 1982),163-179
    !
    !!     TABLES: COEFFICIENTS A0-A7 FOR STEP F. FACTORIALS FACT
    !!     COEFFICIENTS A(K) - FOR PX = FK*V*V*SUM(A(K)*V**K)-DEL
    !
    !!     SEPARATION OF CASES A AND B
    !
    !!     .. Scalar Arguments ..
    !REAL, INTENT(IN)    :: mu
    !LOGICAL, INTENT(IN) :: first
    !INTEGER             :: ival
    !!     ..
    !!     .. Local Scalars ..
    !REAL          :: b1, b2, c, c0, c1, c2, c3, del, difmuk, e, fk, fx, fy, g,  &
    !                 omega, px, py, t, u, v, x, xx
    !REAL, SAVE    :: s, d, p, q, p0
    !INTEGER       :: j, k, kflag
    !LOGICAL, SAVE :: full_init
    !INTEGER, SAVE :: l, m
    !!     ..
    !!     .. Local Arrays ..
    !REAL, SAVE    :: pp(35)
    !!     ..
    !!     .. Data statements ..
    !REAL, PARAMETER :: a0 = -.5, a1 = .3333333, a2 = -.2500068, a3 = .2000118,  &
    !                   a4 = -.1661269, a5 = .1421878, a6 = -.1384794,   &
    !                   a7 = .1250060
    !
    !REAL, PARAMETER :: fact(10) = (/ 1., 1., 2., 6., 24., 120., 720., 5040.,  &
    !                                 40320., 362880. /)
    !
    !!     ..
    !!     .. Executable Statements ..
    !IF (mu > 10.0) THEN
    !!     C A S E  A. (RECALCULATION OF S, D, L IF MU HAS CHANGED)
    !
    !  IF (first) THEN
    !    s = SQRT(mu)
    !    d = 6.0*mu*mu
    !
    !!             THE POISSON PROBABILITIES PK EXCEED THE DISCRETE NORMAL
    !!             PROBABILITIES FK WHENEVER K >= M(MU). L=IFIX(MU-1.1484)
    !!             IS AN UPPER BOUND TO M(MU) FOR ALL MU >= 10 .
    !
    !    l = mu - 1.1484
    !    full_init = .false.
    !  END IF
    !
    !
    !!     STEP N. NORMAL SAMPLE - random_normal() FOR STANDARD NORMAL DEVIATE
    !
    !  g = mu + s*random_normal()
    !  IF (g > 0.0) THEN
    !    ival = g
    !
    !!     STEP I. IMMEDIATE ACCEPTANCE IF ival IS LARGE ENOUGH
    !
    !    IF (ival>=l) RETURN
    !
    !!     STEP S. SQUEEZE ACCEPTANCE - SAMPLE U
    !
    !    fk = ival
    !    difmuk = mu - fk
    !    CALL RANDOM_NUMBER(u)
    !    IF (d*u >= difmuk*difmuk*difmuk) RETURN
    !  END IF
    !
    !!     STEP P. PREPARATIONS FOR STEPS Q AND H.
    !!             (RECALCULATIONS OF PARAMETERS IF NECESSARY)
    !!             .3989423=(2*PI)**(-.5)  .416667E-1=1./24.  .1428571=1./7.
    !!             THE QUANTITIES B1, B2, C3, C2, C1, C0 ARE FOR THE HERMITE
    !!             APPROXIMATIONS TO THE DISCRETE NORMAL PROBABILITIES FK.
    !!             C=.1069/MU GUARANTEES MAJORIZATION BY THE 'HAT'-FUNCTION.
    !
    !  IF (.NOT. full_init) THEN
    !    omega = .3989423/s
    !    b1 = .4166667E-1/mu
    !    b2 = .3*b1*b1
    !    c3 = .1428571*b1*b2
    !    c2 = b2 - 15.*c3
    !    c1 = b1 - 6.*b2 + 45.*c3
    !    c0 = 1. - b1 + 3.*b2 - 15.*c3
    !    c = .1069/mu
    !    full_init = .true.
    !  END IF
    !
    !  IF (g < 0.0) GO TO 50
    !
    !!             'SUBROUTINE' F IS CALLED (KFLAG=0 FOR CORRECT RETURN)
    !
    !  kflag = 0
    !  GO TO 70
    !
    !!     STEP Q. QUOTIENT ACCEPTANCE (RARE CASE)
    !
    !  40 IF (fy-u*fy <= py*EXP(px-fx)) RETURN
    !
    !!     STEP E. EXPONENTIAL SAMPLE - random_exponential() FOR STANDARD EXPONENTIAL
    !!             DEVIATE E AND SAMPLE T FROM THE LAPLACE 'HAT'
    !!             (IF T <= -.6744 THEN PK < FK FOR ALL MU >= 10.)
    !
    !  50 e = random_exponential()
    !  CALL RANDOM_NUMBER(u)
    !  u = u + u - one
    !  t = 1.8 + SIGN(e, u)
    !  IF (t <= (-.6744)) GO TO 50
    !  ival = mu + s*t
    !  fk = ival
    !  difmuk = mu - fk
    !
    !!             'SUBROUTINE' F IS CALLED (KFLAG=1 FOR CORRECT RETURN)
    !
    !  kflag = 1
    !  GO TO 70
    !
    !!     STEP H. HAT ACCEPTANCE (E IS REPEATED ON REJECTION)
    !
    !  60 IF (c*ABS(u) > py*EXP(px+e) - fy*EXP(fx+e)) GO TO 50
    !  RETURN
    !
    !!     STEP F. 'SUBROUTINE' F. CALCULATION OF PX, PY, FX, FY.
    !!             CASE ival < 10 USES FACTORIALS FROM TABLE FACT
    !
    !  70 IF (ival>=10) GO TO 80
    !  px = -mu
    !  py = mu**ival/fact(ival+1)
    !  GO TO 110
    !
    !!             CASE ival >= 10 USES POLYNOMIAL APPROXIMATION
    !!             A0-A7 FOR ACCURACY WHEN ADVISABLE
    !!             .8333333E-1=1./12.  .3989423=(2*PI)**(-.5)
    !
    !  80 del = .8333333E-1/fk
    !  del = del - 4.8*del*del*del
    !  v = difmuk/fk
    !  IF (ABS(v)>0.25) THEN
    !    px = fk*LOG(one + v) - difmuk - del
    !  ELSE
    !    px = fk*v*v* (((((((a7*v+a6)*v+a5)*v+a4)*v+a3)*v+a2)*v+a1)*v+a0) - del
    !  END IF
    !  py = .3989423/SQRT(fk)
    !  110 x = (half - difmuk)/s
    !  xx = x*x
    !  fx = -half*xx
    !  fy = omega* (((c3*xx + c2)*xx + c1)*xx + c0)
    !  IF (kflag <= 0) GO TO 40
    !  GO TO 60
    !
    !!---------------------------------------------------------------------------
    !!     C A S E  B.    mu < 10
    !!     START NEW TABLE AND CALCULATE P0 IF NECESSARY
    !
    !ELSE
    !  IF (first) THEN
    !    m = MAX(1, INT(mu))
    !    l = 0
    !    p = EXP(-mu)
    !    q = p
    !    p0 = p
    !  END IF
    !
    !!     STEP U. UNIFORM SAMPLE FOR INVERSION METHOD
    !
    !  DO
    !    CALL RANDOM_NUMBER(u)
    !    ival = 0
    !    IF (u <= p0) RETURN
    !
    !!     STEP T. TABLE COMPARISON UNTIL THE END PP(L) OF THE
    !!             PP-TABLE OF CUMULATIVE POISSON PROBABILITIES
    !!             (0.458=PP(9) FOR MU=10)
    !
    !    IF (l == 0) GO TO 150
    !    j = 1
    !    IF (u > 0.458) j = MIN(l, m)
    !    DO k = j, l
    !      IF (u <= pp(k)) GO TO 180
    !    END DO
    !    IF (l == 35) CYCLE
    !
    !!     STEP C. CREATION OF NEW POISSON PROBABILITIES P
    !!             AND THEIR CUMULATIVES Q=PP(K)
    !
    !    150 l = l + 1
    !    DO k = l, 35
    !      p = p*mu / k
    !      q = q + p
    !      pp(k) = q
    !      IF (u <= q) GO TO 170
    !    END DO
    !    l = 35
    !  END DO
    !
    !  170 l = k
    !  180 ival = k
    !  RETURN
    !END IF
    !
    !RETURN
    !END FUNCTION random_Poisson
    !
    !
    !
    !FUNCTION random_binomial1(n, p, first) RESULT(ival)
    !
    !! FUNCTION GENERATES A RANDOM BINOMIAL VARIATE USING C.D.Kemp's method.
    !! This algorithm is suitable when many random variates are required
    !! with the SAME parameter values for n & p.
    !
    !!    P = BERNOULLI SUCCESS PROBABILITY
    !!           (0 <= REAL <= 1)
    !!    N = NUMBER OF BERNOULLI TRIALS
    !!           (1 <= INTEGER)
    !!    FIRST = .TRUE. for the first call using the current parameter values
    !!          = .FALSE. if the values of (n,p) are unchanged from last call
    !
    !! Reference: Kemp, C.D. (1986). `A modal method for generating binomial
    !!            variables', Commun. Statist. - Theor. Meth. 15(3), 805-813.
    !
    !INTEGER, INTENT(IN) :: n
    !REAL, INTENT(IN)    :: p
    !LOGICAL, INTENT(IN) :: first
    !INTEGER             :: ival
    !
    !!     Local variables
    !
    !INTEGER         :: ru, rd
    !INTEGER, SAVE   :: r0
    !REAL            :: u, pd, pu
    !REAL, SAVE      :: odds_ratio, p_r
    !REAL, PARAMETER :: zero = 0.0, one = 1.0
    !
    !IF (first) THEN
    !  r0 = (n+1)*p
    !  p_r = bin_prob(n, p, r0)
    !  odds_ratio = p / (one - p)
    !END IF
    !
    !CALL RANDOM_NUMBER(u)
    !u = u - p_r
    !IF (u < zero) THEN
    !  ival = r0
    !  RETURN
    !END IF
    !
    !pu = p_r
    !ru = r0
    !pd = p_r
    !rd = r0
    !DO
    !  rd = rd - 1
    !  IF (rd >= 0) THEN
    !    pd = pd * (rd+1) / (odds_ratio * (n-rd))
    !    u = u - pd
    !    IF (u < zero) THEN
    !      ival = rd
    !      RETURN
    !    END IF
    !  END IF
    !
    !  ru = ru + 1
    !  IF (ru <= n) THEN
    !    pu = pu * (n-ru+1) * odds_ratio / ru
    !    u = u - pu
    !    IF (u < zero) THEN
    !      ival = ru
    !      RETURN
    !    END IF
    !  END IF
    !END DO
    !
    !!     This point should not be reached, but just in case:
    !
    !ival = r0
    !RETURN
    !
    !END FUNCTION random_binomial1
    !
    !
    !
    !FUNCTION bin_prob(n, p, r) RESULT(fn_val)
    !!     Calculate a binomial probability
    !
    !INTEGER, INTENT(IN) :: n, r
    !REAL, INTENT(IN)    :: p
    !REAL                :: fn_val
    !
    !!     Local variable
    !REAL                :: one = 1.0
    !
    !fn_val = EXP( lngamma(DBLE(n+1)) - lngamma(DBLE(r+1)) - lngamma(DBLE(n-r+1)) &
    !              + r*LOG(p) + (n-r)*LOG(one - p) )
    !RETURN
    !
    !END FUNCTION bin_prob
    !
    !
    !
    !FUNCTION lngamma(x) RESULT(fn_val)
    !
    !! Logarithm to base e of the gamma function.
    !!
    !! Accurate to about 1.e-14.
    !! Programmer: Alan Miller
    !
    !! Latest revision of Fortran 77 version - 28 February 1988
    !
    !REAL (dp), INTENT(IN) :: x
    !REAL (dp)             :: fn_val
    !
    !!       Local variables
    !
    !REAL (dp) :: a1 = -4.166666666554424D-02, a2 = 2.430554511376954D-03,  &
    !             a3 = -7.685928044064347D-04, a4 = 5.660478426014386D-04,  &
    !             temp, arg, product, lnrt2pi = 9.189385332046727D-1,       &
    !             pi = 3.141592653589793D0
    !LOGICAL   :: reflect
    !
    !!       lngamma is not defined if x = 0 or a negative integer.
    !
    !IF (x > 0.d0) GO TO 10
    !IF (x /= INT(x)) GO TO 10
    !fn_val = 0.d0
    !RETURN
    !
    !!       If x < 0, use the reflection formula:
    !!               gamma(x) * gamma(1-x) = pi * cosec(pi.x)
    !
    !10 reflect = (x < 0.d0)
    !IF (reflect) THEN
    !  arg = 1.d0 - x
    !ELSE
    !  arg = x
    !END IF
    !
    !!       Increase the argument, if necessary, to make it > 10.
    !
    !product = 1.d0
    !20 IF (arg <= 10.d0) THEN
    !  product = product * arg
    !  arg = arg + 1.d0
    !  GO TO 20
    !END IF
    !
    !!  Use a polynomial approximation to Stirling's formula.
    !!  N.B. The real Stirling's formula is used here, not the simpler, but less
    !!       accurate formula given by De Moivre in a letter to Stirling, which
    !!       is the one usually quoted.
    !
    !arg = arg - 0.5D0
    !temp = 1.d0/arg**2
    !fn_val = lnrt2pi + arg * (LOG(arg) - 1.d0 + &
    !                  (((a4*temp + a3)*temp + a2)*temp + a1)*temp) - LOG(product)
    !IF (reflect) THEN
    !  temp = SIN(pi * x)
    !  fn_val = LOG(pi/temp) - fn_val
    !END IF
    !RETURN
    !END FUNCTION lngamma
    !
    !
    !
    !FUNCTION random_binomial2(n, pp, first) RESULT(ival)
    !!**********************************************************************
    !!     Translated to Fortran 90 by Alan Miller from:
    !!                              RANLIB
    !!
    !!     Library of Fortran Routines for Random Number Generation
    !!
    !!                      Compiled and Written by:
    !!
    !!                           Barry W. Brown
    !!                            James Lovato
    !!
    !!               Department of Biomathematics, Box 237
    !!               The University of Texas, M.D. Anderson Cancer Center
    !!               1515 Holcombe Boulevard
    !!               Houston, TX      77030
    !!
    !! This work was supported by grant CA-16672 from the National Cancer Institute.
    !
    !!                    GENerate BINomial random deviate
    !
    !!                              Function
    !
    !!     Generates a single random deviate from a binomial
    !!     distribution whose number of trials is N and whose
    !!     probability of an event in each trial is P.
    !
    !!                              Arguments
    !
    !!     N  --> The number of trials in the binomial distribution
    !!            from which a random deviate is to be generated.
    !!                              INTEGER N
    !
    !!     P  --> The probability of an event in each trial of the
    !!            binomial distribution from which a random deviate
    !!            is to be generated.
    !!                              REAL P
    !
    !!     FIRST --> Set FIRST = .TRUE. for the first call to perform initialization
    !!               the set FIRST = .FALSE. for further calls using the same pair
    !!               of parameter values (N, P).
    !!                              LOGICAL FIRST
    !
    !!     random_binomial2 <-- A random deviate yielding the number of events
    !!                from N independent trials, each of which has
    !!                a probability of event P.
    !!                              INTEGER random_binomial
    !
    !!                              Method
    !
    !!     This is algorithm BTPE from:
    !
    !!         Kachitvichyanukul, V. and Schmeiser, B. W.
    !!         Binomial Random Variate Generation.
    !!         Communications of the ACM, 31, 2 (February, 1988) 216.
    !
    !!**********************************************************************
    !
    !!*****DETERMINE APPROPRIATE ALGORITHM AND WHETHER SETUP IS NECESSARY
    !
    !!     ..
    !!     .. Scalar Arguments ..
    !REAL, INTENT(IN)    :: pp
    !INTEGER, INTENT(IN) :: n
    !LOGICAL, INTENT(IN) :: first
    !INTEGER             :: ival
    !!     ..
    !!     .. Local Scalars ..
    !REAL            :: alv, amaxp, f, f1, f2, u, v, w, w2, x, x1, x2, ynorm, z, z2
    !REAL, PARAMETER :: zero = 0.0, half = 0.5, one = 1.0
    !INTEGER         :: i, ix, ix1, k, mp
    !INTEGER, SAVE   :: m
    !REAL, SAVE      :: p, q, xnp, ffm, fm, xnpq, p1, xm, xl, xr, c, al, xll,  &
    !                   xlr, p2, p3, p4, qn, r, g
    !
    !!     ..
    !!     .. Executable Statements ..
    !
    !!*****SETUP, PERFORM ONLY WHEN PARAMETERS CHANGE
    !
    !IF (first) THEN
    !  p = MIN(pp, one-pp)
    !  q = one - p
    !  xnp = n * p
    !END IF
    !
    !IF (xnp > 30.) THEN
    !  IF (first) THEN
    !    ffm = xnp + p
    !    m = ffm
    !    fm = m
    !    xnpq = xnp * q
    !    p1 = INT(2.195*SQRT(xnpq) - 4.6*q) + half
    !    xm = fm + half
    !    xl = xm - p1
    !    xr = xm + p1
    !    c = 0.134 + 20.5 / (15.3 + fm)
    !    al = (ffm-xl) / (ffm - xl*p)
    !    xll = al * (one + half*al)
    !    al = (xr - ffm) / (xr*q)
    !    xlr = al * (one + half*al)
    !    p2 = p1 * (one + c + c)
    !    p3 = p2 + c / xll
    !    p4 = p3 + c / xlr
    !  END IF
    !
    !!*****GENERATE VARIATE, Binomial mean at least 30.
    !
    !  20 CALL RANDOM_NUMBER(u)
    !  u = u * p4
    !  CALL RANDOM_NUMBER(v)
    !
    !!     TRIANGULAR REGION
    !
    !  IF (u <= p1) THEN
    !    ix = xm - p1 * v + u
    !    GO TO 110
    !  END IF
    !
    !!     PARALLELOGRAM REGION
    !
    !  IF (u <= p2) THEN
    !    x = xl + (u-p1) / c
    !    v = v * c + one - ABS(xm-x) / p1
    !    IF (v > one .OR. v <= zero) GO TO 20
    !    ix = x
    !  ELSE
    !
    !!     LEFT TAIL
    !
    !    IF (u <= p3) THEN
    !      ix = xl + LOG(v) / xll
    !      IF (ix < 0) GO TO 20
    !      v = v * (u-p2) * xll
    !    ELSE
    !
    !!     RIGHT TAIL
    !
    !      ix = xr - LOG(v) / xlr
    !      IF (ix > n) GO TO 20
    !      v = v * (u-p3) * xlr
    !    END IF
    !  END IF
    !
    !!*****DETERMINE APPROPRIATE WAY TO PERFORM ACCEPT/REJECT TEST
    !
    !  k = ABS(ix-m)
    !  IF (k <= 20 .OR. k >= xnpq/2-1) THEN
    !
    !!     EXPLICIT EVALUATION
    !
    !    f = one
    !    r = p / q
    !    g = (n+1) * r
    !    IF (m < ix) THEN
    !      mp = m + 1
    !      DO i = mp, ix
    !        f = f * (g/i-r)
    !      END DO
    !
    !    ELSE IF (m > ix) THEN
    !      ix1 = ix + 1
    !      DO i = ix1, m
    !        f = f / (g/i-r)
    !      END DO
    !    END IF
    !
    !    IF (v > f) THEN
    !      GO TO 20
    !    ELSE
    !      GO TO 110
    !    END IF
    !  END IF
    !
    !!     SQUEEZING USING UPPER AND LOWER BOUNDS ON LOG(F(X))
    !
    !  amaxp = (k/xnpq) * ((k*(k/3. + .625) + .1666666666666)/xnpq + half)
    !  ynorm = -k * k / (2.*xnpq)
    !  alv = LOG(v)
    !  IF (alv<ynorm - amaxp) GO TO 110
    !  IF (alv>ynorm + amaxp) GO TO 20
    !
    !!     STIRLING'S (actually de Moivre's) FORMULA TO MACHINE ACCURACY FOR
    !!     THE FINAL ACCEPTANCE/REJECTION TEST
    !
    !  x1 = ix + 1
    !  f1 = fm + one
    !  z = n + 1 - fm
    !  w = n - ix + one
    !  z2 = z * z
    !  x2 = x1 * x1
    !  f2 = f1 * f1
    !  w2 = w * w
    !  IF (alv - (xm*LOG(f1/x1) + (n-m+half)*LOG(z/w) + (ix-m)*LOG(w*p/(x1*q)) +    &
    !      (13860.-(462.-(132.-(99.-140./f2)/f2)/f2)/f2)/f1/166320. +               &
    !      (13860.-(462.-(132.-(99.-140./z2)/z2)/z2)/z2)/z/166320. +                &
    !      (13860.-(462.-(132.-(99.-140./x2)/x2)/x2)/x2)/x1/166320. +               &
    !      (13860.-(462.-(132.-(99.-140./w2)/w2)/w2)/w2)/w/166320.) > zero) THEN
    !    GO TO 20
    !  ELSE
    !    GO TO 110
    !  END IF
    !
    !ELSE
    !!     INVERSE CDF LOGIC FOR MEAN LESS THAN 30
    !  IF (first) THEN
    !    qn = q ** n
    !    r = p / q
    !    g = r * (n+1)
    !  END IF
    !
    !  90 ix = 0
    !  f = qn
    !  CALL RANDOM_NUMBER(u)
    !  100 IF (u >= f) THEN
    !    IF (ix > 110) GO TO 90
    !    u = u - f
    !    ix = ix + 1
    !    f = f * (g/ix - r)
    !    GO TO 100
    !  END IF
    !END IF
    !
    !110 IF (pp > half) ix = n - ix
    !ival = ix
    !RETURN
    !
    !END FUNCTION random_binomial2
    !
    !
    !
    !
    !FUNCTION random_neg_binomial(sk, p) RESULT(ival)
    !
    !! Adapted from Fortran 77 code from the book:
    !!     Dagpunar, J. 'Principles of random variate generation'
    !!     Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9
    !
    !! FUNCTION GENERATES A RANDOM NEGATIVE BINOMIAL VARIATE USING UNSTORED
    !! INVERSION AND/OR THE REPRODUCTIVE PROPERTY.
    !
    !!    SK = NUMBER OF FAILURES REQUIRED (Dagpunar's words!)
    !!       = the `power' parameter of the negative binomial
    !!           (0 < REAL)
    !!    P = BERNOULLI SUCCESS PROBABILITY
    !!           (0 < REAL < 1)
    !
    !! THE PARAMETER H IS SET SO THAT UNSTORED INVERSION ONLY IS USED WHEN P <= H,
    !! OTHERWISE A COMBINATION OF UNSTORED INVERSION AND
    !! THE REPRODUCTIVE PROPERTY IS USED.
    !
    !REAL, INTENT(IN)   :: sk, p
    !INTEGER            :: ival
    !
    !!     Local variables
    !! THE PARAMETER ULN = -LOG(MACHINE'S SMALLEST REAL NUMBER).
    !
    !REAL, PARAMETER    :: h = 0.7
    !REAL               :: q, x, st, uln, v, r, s, y, g
    !INTEGER            :: k, i, n
    !
    !IF (sk <= zero .OR. p <= zero .OR. p >= one) THEN
    !  WRITE(*, *) 'IMPERMISSIBLE DISTRIBUTION PARAMETER VALUES'
    !  STOP
    !END IF
    !
    !q = one - p
    !x = zero
    !st = sk
    !IF (p > h) THEN
    !  v = one/LOG(p)
    !  k = st
    !  DO i = 1,k
    !    DO
    !      CALL RANDOM_NUMBER(r)
    !      IF (r > zero) EXIT
    !    END DO
    !    n = v*LOG(r)
    !    x = x + n
    !  END DO
    !  st = st - k
    !END IF
    !
    !s = zero
    !uln = -LOG(vsmall)
    !IF (st > -uln/LOG(q)) THEN
    !  WRITE(*, *) ' P IS TOO LARGE FOR THIS VALUE OF SK'
    !  STOP
    !END IF
    !
    !y = q**st
    !g = st
    !CALL RANDOM_NUMBER(r)
    !DO
    !  IF (y > r) EXIT
    !  r = r - y
    !  s = s + one
    !  y = y*p*g/s
    !  g = g + one
    !END DO
    !
    !ival = x + s + half
    !RETURN
    !END FUNCTION random_neg_binomial
    !
    !
    !
    !FUNCTION random_von_Mises(k, first) RESULT(fn_val)
    !
    !!     Algorithm VMD from:
    !!     Dagpunar, J.S. (1990) `Sampling from the von Mises distribution via a
    !!     comparison of random numbers', J. of Appl. Statist., 17, 165-168.
    !
    !!     Fortran 90 code by Alan Miller
    !!     CSIRO Division of Mathematical & Information Sciences
    !
    !!     Arguments:
    !!     k (real)        parameter of the von Mises distribution.
    !!     first (logical) set to .TRUE. the first time that the function
    !!                     is called, or the first time with a new value
    !!                     for k.   When first = .TRUE., the function sets
    !!                     up starting values and may be very much slower.
    !
    !REAL, INTENT(IN)     :: k
    !LOGICAL, INTENT(IN)  :: first
    !REAL                 :: fn_val
    !
    !!     Local variables
    !
    !INTEGER          :: j, n
    !INTEGER, SAVE    :: nk
    !REAL, PARAMETER  :: pi = 3.14159265
    !REAL, SAVE       :: p(20), theta(0:20)
    !REAL             :: sump, r, th, lambda, rlast
    !REAL (dp)        :: dk
    !
    !IF (first) THEN                        ! Initialization, if necessary
    !  IF (k < zero) THEN
    !    WRITE(*, *) '** Error: argument k for random_von_Mises = ', k
    !    RETURN
    !  END IF
    !
    !  nk = k + k + one
    !  IF (nk > 20) THEN
    !    WRITE(*, *) '** Error: argument k for random_von_Mises = ', k
    !    RETURN
    !  END IF
    !
    !  dk = k
    !  theta(0) = zero
    !  IF (k > half) THEN
    !
    !!     Set up array p of probabilities.
    !
    !    sump = zero
    !    DO j = 1, nk
    !      IF (j < nk) THEN
    !        theta(j) = ACOS(one - j/k)
    !      ELSE
    !        theta(nk) = pi
    !      END IF
    !
    !!     Numerical integration of e^[k.cos(x)] from theta(j-1) to theta(j)
    !
    !      CALL integral(theta(j-1), theta(j), p(j), dk)
    !      sump = sump + p(j)
    !    END DO
    !    p(1:nk) = p(1:nk) / sump
    !  ELSE
    !    p(1) = one
    !    theta(1) = pi
    !  END IF                         ! if k > 0.5
    !END IF                           ! if first
    !
    !CALL RANDOM_NUMBER(r)
    !DO j = 1, nk
    !  r = r - p(j)
    !  IF (r < zero) EXIT
    !END DO
    !r = -r/p(j)
    !
    !DO
    !  th = theta(j-1) + r*(theta(j) - theta(j-1))
    !  lambda = k - j + one - k*COS(th)
    !  n = 1
    !  rlast = lambda
    !
    !  DO
    !    CALL RANDOM_NUMBER(r)
    !    IF (r > rlast) EXIT
    !    n = n + 1
    !    rlast = r
    !  END DO
    !
    !  IF (n .NE. 2*(n/2)) EXIT         ! is n even?
    !  CALL RANDOM_NUMBER(r)
    !END DO
    !
    !fn_val = SIGN(th, (r - rlast)/(one - rlast) - half)
    !RETURN
    !END FUNCTION random_von_Mises
    !
    !
    !
    !SUBROUTINE integral(a, b, result, dk)
    !
    !!     Gaussian integration of exp(k.cosx) from a to b.
    !
    !REAL (dp), INTENT(IN) :: dk
    !REAL, INTENT(IN)      :: a, b
    !REAL, INTENT(OUT)     :: result
    !
    !!     Local variables
    !
    !REAL (dp)  :: xmid, range, x1, x2,                                    &
    !  x(3) = (/0.238619186083197_dp, 0.661209386466265_dp, 0.932469514203152_dp/), &
    !  w(3) = (/0.467913934572691_dp, 0.360761573048139_dp, 0.171324492379170_dp/)
    !INTEGER    :: i
    !
    !xmid = (a + b)/2._dp
    !range = (b - a)/2._dp
    !
    !result = 0._dp
    !DO i = 1, 3
    !  x1 = xmid + x(i)*range
    !  x2 = xmid - x(i)*range
    !  result = result + w(i)*(EXP(dk*COS(x1)) + EXP(dk*COS(x2)))
    !END DO
    !
    !result = result * range
    !RETURN
    !END SUBROUTINE integral
    !
    !
    !
    !FUNCTION random_Cauchy() RESULT(fn_val)
    !
    !!     Generate a random deviate from the standard Cauchy distribution
    !
    !REAL     :: fn_val
    !
    !!     Local variables
    !REAL     :: v(2)
    !
    !DO
    !  CALL RANDOM_NUMBER(v)
    !  v = two*(v - half)
    !  IF (ABS(v(2)) < vsmall) CYCLE               ! Test for zero
    !  IF (v(1)**2 + v(2)**2 < one) EXIT
    !END DO
    !fn_val = v(1) / v(2)
    !
    !RETURN
    !END FUNCTION random_Cauchy


