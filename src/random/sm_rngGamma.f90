submodule (m_random) sm_rngGamma
  !! Generate random numbers from a gamma distribution.

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

real(r64), parameter :: zero = 0.0_r64
real(r64), parameter :: one = 1.0_r64
real(r64), parameter :: half = 0.5_r64
real(r64), parameter :: vsmall = tiny(1.0_r64)

contains

  !====================================================================!
  module procedure rngGamma_d1!(this,s,first)
    !! Interfaced with [[rngGamma]]
  !====================================================================!
  !====================================================================!
  ! Adapted from Fortran 77 code from the book:
  !     Dagpunar, J. 'Principles of random variate generation'
  !     Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9

  !     FUNCTION GENERATES A RANDOM GAMMA VARIATE.
  !     CALLS EITHER random_gamma1 (S > 1.0)
  !     OR random_exponential (S = 1.0)
  !     OR random_gamma2 (S < 1.0).

  !     S = SHAPE PARAMETER OF DISTRIBUTION (0 < real(r64)).
  !====================================================================!
!  real(r64) :: this
!  real(r64), INTENT(IN)    :: s
!  LOGICAL, INTENT(IN) :: first

  IF (s <= zero) call Emsg('rngGamma : shape parameter must be positive')

  IF (s > one) THEN
    this = rngGamma1(s,first)
  ELSE IF (s < one) THEN
    this = rngGamma2(s,first)
  ELSE
    call rngExponential(this, one)
  END IF
  END procedure
  !====================================================================!
  !====================================================================!
  module procedure rngGamma_d1D!(this,s,first)
    !! Interfaced with [[rngGamma]]
  !====================================================================!
!  real(r64) :: this(:)
!  real(r64), INTENT(IN) :: s
!  LOGICAL, INTENT(IN) :: first
  integer :: i,N
  N=size(this)
  call rngGamma(this(1),s,first)
  do i=2,N
    call rngGamma(this(i),s,.false.)
  enddo
  end procedure
  !====================================================================!
  !====================================================================!
  function rngGamma1(s,first) result(this)
  !====================================================================!
  ! Uses the algorithm in
  ! Marsaglia, G. and Tsang, W.W. (2000) `A simple method for generating
  ! gamma variables', Trans. om Math. Software (TOMS), vol.26(3), pp.363-372.

  ! Generates a random gamma deviate for shape parameter s >= 1.
  real(r64) :: this
  real(r64), INTENT(IN)    :: s
  LOGICAL, INTENT(IN) :: first

  ! Local variables
  real(r64), SAVE  :: c, d
  real(r64)        :: u, v, x

  IF (first) THEN
    d = s - one/3.d0
    c = one/dSQRT(9.d0*d)
  END IF

  ! Start of main loop
  DO
    ! Generate v = (1+cx)^3 where x is random normal; repeat if v <= 0.
    DO
      call rngNormal(x)
      v = (one + c*x)**3.d0
      IF (v > zero) EXIT
    END DO

    ! Generate uniform variable U
    call rngUniform(u)
    IF (u < one - 0.0331d0*x**4.d0) THEN
      this = d*v
      EXIT
    ELSE IF (dLOG(u) < half*x**2.d0 + d*(one - v + dLOG(v))) THEN
      this = d*v
      EXIT
    END IF
  END DO
  END FUNCTION
  !====================================================================!
  !====================================================================!
  function rngGamma2(s,first) RESULT(this)
  !====================================================================!
  ! Adapted from Fortran 77 code from the book:
  !     Dagpunar, J. 'Principles of random variate generation'
  !     Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9

  ! FUNCTION GENERATES A RANDOM VARIATE IN [0,INFINITY) FROM
  ! A GAMMA DISTRIBUTION WITH DENSITY PROPORTIONAL TO
  ! GAMMA2**(S-1) * EXP(-GAMMA2),
  ! USING A SWITCHING METHOD.

  !    S = SHAPE PARAMETER OF DISTRIBUTION
  !          (real(r64) < 1.0)
  real(r64) :: this
  real(r64), INTENT(IN) :: s
  LOGICAL, INTENT(IN) :: first

  !     Local variables
  real(r64)       :: r, x, w
  real(r64), SAVE :: a, p, c, uf, vr, d

  IF (s <= zero .OR. s >= one) call Emsg('rngGamma2 : shape parameter outside (0 1)')

  IF (first) THEN                        ! Initialization, if necessary
    a = one - s
    p = a/(a + s*dEXP(-a))
    IF (s < vsmall) call Emsg('rngGamma2 : shape parameter too small')
    c = one/s
    uf = p*(vsmall/a)**s
    vr = one - vsmall
    d = a*dLOG(a)
  END IF

  DO
    CALL rngUniform(r)
    IF (r >= vr) THEN
      CYCLE
    ELSE IF (r > p) THEN
      x = a - dLOG((one - r)/(one - p))
      w = a*dLOG(x)-d
    ELSE IF (r > uf) THEN
      x = a*(r/p)**c
      w = x
    ELSE
      this = zero
      RETURN
    END IF

    CALL rngUniform(r)
    IF (one-r <= w .AND. r > zero) THEN
      IF (r*(w + one) >= one) CYCLE
      IF (-dLOG(r) <= w) CYCLE
    END IF
    EXIT
  END DO

  this = x
  END FUNCTION
  !====================================================================!

end submodule
