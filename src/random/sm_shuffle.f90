submodule (m_random) sm_shuffle
  !! Perform Knuth shuffling on an array
use variableKind
use m_swap, only: swap
implicit none

contains
!====================================================================!
module procedure shuffle_r1D
  !! Interfaced with shuffle()
!====================================================================!
!module subroutine shuffle_r1D(this)
!real(r32) :: this(:)
integer(i32) :: i
integer(i32) :: N
integer(i32) :: r
N=size(this)
do i = 2, N
  call rngInteger(r, 1, i)
  call swap(this(i), this(r))
end do
end procedure
!====================================================================!
!====================================================================!
module procedure shuffle_d1D
  !! Interfaced with shuffle()
!====================================================================!
!module subroutine shuffle_d1D(this)
!real(r64) :: this(:)
integer(i32) :: i
integer(i32) :: N
integer(i32) :: r
N=size(this)
do i = 2, N
  call rngInteger(r, 1, i)
  call swap(this(i), this(r))
end do
end procedure
!====================================================================!
!====================================================================!
module procedure shuffle_i1D
  !! Interfaced with shuffle()
!====================================================================!
!module subroutine shuffle_i1D(this)
!integer(i32) :: this(:)
integer(i32) :: i
integer(i32) :: N
integer(i32) :: r
N=size(this)
do i = 2, N
  call rngInteger(r, 1, i)
  call swap(this(i), this(r))
end do
end procedure
!====================================================================!
!====================================================================!
module procedure shuffle_id1D
  !! Interfaced with shuffle()
!====================================================================!
!module subroutine shuffle_id1D(this)
!integer(i64) :: this(:)
integer(i32) :: i
integer(i32) :: N
integer(i32) :: r
N=size(this)
do i = 2, N
  call rngInteger(r, 1, i)
  call swap(this(i), this(r))
end do
end procedure
!====================================================================!
end submodule
