submodule (m_random) sm_rngInteger
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

use m_array1D!, only: arange

implicit none

contains

  !====================================================================!
   module procedure rngInteger_i1!(this,imin,imax)
    !! Interfaced with [[rngInteger]]
  !====================================================================!
  !integer(i32) :: this
  !integer(i32) :: imin,imax
  real(r64) :: wk
  call rngUniform(wk)
  this=imin+idnint(wk*dble(imax-imin))
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure rngInteger_i1D!(this,imin)
    !! Interfaced with [[rngInteger]]
  !====================================================================!
!  integer(i32) :: this(:)
!  integer(i32) :: imin
  integer(i32) :: i,j,k,N
  N=size(this)
  this = [(imin + i, i = 0, N-1)]
  do i=1,N
    call rngInteger(j,1,i)
    k=this(i)
    this(i)=this(j)
    this(j)=k
  end do
  end procedure
  !====================================================================!
end submodule
