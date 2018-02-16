submodule (Prng_Class) sm_xorshift128plus
!!# xorshift128+
!!This module contains routines to generate random numbers using the xorshift128+ method by Vigna's extensions to 
!!Marsaglia, G., 2003. Xorshift RNGs. Journal of Statistical Software 8, 1-6.
!!
!!This module is a modified version of the public domain code written by Shun Sakuraba in 2015-2016.
!!The original code can be found [here](https://bitbucket.org/shun.sakuraba/xorshiftf90).
!!The modified code in coretran is distributed under the coretran license, see the repository for that information.
!!
!!Vigna's xorshift128+ pseudorandom generator.
!!Sebastiano Vigna. 2014. Further scramblings of Marsaglia's xorshift generators. CoRR, abs/1402.6246.
!! xorshift128+ is known as a fast pseudorandom generator with reasonably good resilience to randomness tests.
!! Since its primary imporance is its speed, do not forget to add inlining directives depending your compiler.
!!
!!
!!## Why xorshift128+ ?
!!The seed of the xorshift128+ can be jumped by k cycles, where each cycle jumps ahead by $2^{64}$ random numbers, but quickly.
!!This ability to jump is an important aspect for using random number generators in general but especially in parallel
!!in either OpenMP or MPI paradigms.
!!The time taken to generate a 64 bit integer was 1.06 ns on an IntelR CoreTM i7-4770 CPU @3.40GHz (Haswell) as shown in Vigna (2014).
!!xorshift128+ is the better for less massively parallel applications.
!!
!!If the period of the random number generator is too small for your application, consider using the xorshift1024*
!!generator which has a period of $2^{512}$ numbers. The time to generate a 64 bit integer is slightly slower at 1.36 ns
!!
!!### Important note: This next aspect needs to be tested.
!!@todo: test this
!!Because the Fortran standard does not specify an integer model, int64 overflow has an undefined behaviour.
!!Thus, there might be warnings issued while compiling this program, or worse, it may not work.
!!A workaround uses the int(int(a, kind=16) "op" b, kind=8) trick, which works well with gfortran,
!!but commercial compilers do not accept 128-bit arithmetics (incl. Intel, Cray, and Fujitsu).

implicit none

contains

  !====================================================================!
  module procedure rngInteger_128plus!(this, val)
  !====================================================================!
  !class(prng), intent(inout) :: this
  !integer(i64), intent(out) :: val

  integer(i64) :: s0, s1
    
  ! swap state variables
  s0 = this%seed(2)
  s1 = this%seed(1)
  
  this%seed(1) = s0

  ! xorshift
  s1 = ieor(ishft(s1, 23), s1)

  ! scramble
  s1 = ieor(ieor(s1, s0), ieor(ishft(s1, -18), ishft(s0, -5)))
  this%seed(2) = s1

  val = s1 + s0
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure jumpState_128plus!(this)
  !====================================================================!
  !! Jumps the current prng state by $2^{512}$ pulls from the generator without drawing $2^{512}$ numbers.
  !class(Prng), intent(inout) :: this
  integer(i64) :: jump_table(2)
  integer(i64) :: t(0:1)
  integer(i64) :: tmp
  integer :: i, j, b
  data jump_table / -8476663413540573697_i64,& !0x8a5cd789635d2dff
                     1305993406145048470_i64/  !0x121fd2155c472f96
  
  t(:) = 0

  do i = 1, 2
      do b = 0, 63
        if(btest(jump_table(i), b)) then
            do j = 0, 1
              t(j) = ieor(t(j), this%seed(j))
            end do
        end if
        call rngInteger_128plus(this, tmp)
      end do
  end do
  this%seed(0:1) = t
  end procedure
  !====================================================================!
end submodule