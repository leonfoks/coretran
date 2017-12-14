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

use m_allocate, only: allocate
use m_array1D, only: arange
use m_deallocate, only: deallocate

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
  module procedure rngInteger_i1D!(this,imin,imax)
    !! Interfaced with [[rngInteger]]
  !====================================================================!
  !integer(i32) :: this(:)
  !integer(i32) :: imin,imax
  real(r64), allocatable :: wk(:)
  call allocate(wk,size(this))
  call rngUniform(wk)
  this=imin+idnint(wk*dble(imax-imin))
  call deallocate(wk)
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure rngInteger_i2D!(this,imin,imax)
    !! Interfaced with [[rngInteger]]
  !====================================================================!
  !integer(i32) :: this(:,:)
  !integer(i32) :: imin,imax
  real(r64), allocatable :: wk(:,:)
  call allocate(wk, shape(this))
  call rngUniform(wk)
  this=imin+idnint(wk*dble(imax-imin))
  call deallocate(wk)
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure rngInteger_i3D!(this,imin,imax)
    !! Interfaced with [[rngInteger]]
  !====================================================================!
  !integer(i32) :: this(:,:,:)
  !integer(i32) :: imin,imax
  real(r64), allocatable :: wk(:,:,:)
  call allocate(wk, shape(this))
  call rngUniform(wk)
  this=imin+idnint(wk*dble(imax-imin))
  call deallocate(wk)
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure rngInteger_i1D_i1!(this,imin)
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
