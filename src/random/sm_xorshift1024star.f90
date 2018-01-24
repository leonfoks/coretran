submodule (Prng_Class) sm_xorshift1024star
!!# xorshift1024*
!!This module contains routines to generate random numbers using the xorshift1024* method by Vigna's extensions to 
!!Marsaglia, G., 2003. Xorshift RNGs. Journal of Statistical Software 8, 1–6.
!!
!!This module is a modified version of the public domain code written by Shun Sakuraba in 2015-2016.
!!The original code can be found [here](https://bitbucket.org/shun.sakuraba/xorshiftf90).
!!The modified code in coretran is distributed under the coretran license, see the repository for that information.
!!
!!Vigna's xorshift1024* pseudorandom generator.
!!Sebastiano Vigna. An experimental exploration of Marsaglia's xorshift generators, scrambled. CoRR, abs/1402.6246, 2014.
!!xorshift1024* is a pseudorandom generator with reasonable speed and a good size state space.
!!
!!
!!## Why xorshift1024* ?
!!The seed of the xorshift1024* can be jumped by k cycles, where each cycle jumps ahead by $2^{512}$ random numbers, but quickly.
!!This ability to jump is an important aspect for using random number generators in general but especially in parallel
!!in either OpenMP or MPI paradigms.
!!The time taken to generate a 64 bit integer was 1.36ns on an IntelR CoreTM i7-4770 CPU @3.40GHz (Haswell) as shown in Vigna (2014).
!!xorshift1024* is the better for massively parallel applications that draw many realizations.
!!
!!If the size of the random number generator is too much for your application, consider using the [[m_xorshift128plus]]
!!generator which has a period of $2^{64}$ numbers. The time to generate a 64 bit integer is faster at 1.06
!!
!!This class allows you to instantiate multiple instances of the pseudo-random number generator (prng).
!!You can jump the prng, to cycle skip, or generate values from different distributions.
!!
!!### Important note: This next aspect needs to be tested.
!!Because the Fortran standard does not specify an integer model, int64 overflow has an undefined behaviour.
!!Thus, there might be warnings issued while compiling this program, or worse, it may not work.
!!A workaround uses the int(int(a, kind=16) "op" b, kind=8) trick, which works well with gfortran,
!!but commercial compilers do not accept 128-bit arithmetics (incl. Intel, Cray, and Fujitsu).

implicit none

contains

  !====================================================================!
  module procedure rngInteger_1024star!(this, val)
  !====================================================================!
  !class(prng), intent(inout) :: this
  !integer(i64), intent(out) :: val

  integer(i64) :: s0, s1, r
  ! New value to eliminate linear dependencies from lowest bit
  integer(i64), parameter :: spreader = -7046029254386353133_i64
  ! Old parameter values: 1181783497276652981_i64
  
  ! get state variables
  s0 = this%seed(this%ptr)
  this%ptr = iand(this%ptr + 1, 15)
  s1 = this%seed(this%ptr)

  ! xorshift
  s1 = ieor(s1, ishft(s1, 31))
  s1 = ieor(s1, ishft(s1, -11))
  s0 = ieor(s0, ishft(s1, -30))

  r = ieor(s0, s1)
  this%seed(this%ptr) = r

  val = r * spreader
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure rngUniform_1024star!(this, val)
  !====================================================================!
  !class(prng), intent(inout) :: this
  !real(r64), intent(out) :: val

  integer(i64) :: rnd
  ! 1.0 / (1 << 53)
  real(r64), parameter :: multiplier = 1.0_r64 / 9007199254740992_r64

  call rngInteger_1024star(this, rnd)
  
  ! 53-bit, divided by 2^53
  val = real(ishft(rnd, -11), kind=r64) * multiplier
  end procedure
  !====================================================================!
end submodule