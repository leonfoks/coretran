module m_partition3way
  !! Contains Hoare's style partitioning algorithms used for quicksorting routines
  use variableKind
  use m_swap, only: swap
  use m_compare, only: compare
  implicit none

  interface partition3way
    !! Partitioning used for quicksort routines
!    module procedure :: partition_r1D,
    module procedure :: partition3way_d1D!, partition_i1D, partition_id1D

  end interface

!  interface argPartition
!    !! Partitioning used for argQuicksort routines
!    module procedure :: argPartition_r1D, argPartition_d1D, argPartition_i1D, argPartition_id1D
!  end interface
  contains
!  !====================================================================!
!  subroutine partition_r1D(this,left,right,iPivot)
!    !! Interfaced with partition()
!  !====================================================================!
!  real(r32), intent(inout) :: this(:) !! 1D array
!  integer(i32), intent(inout) :: left !! Left index
!  integer(i32), intent(inout) :: right !! Right index
!  integer(i32), intent(inout) :: iPivot !! Pivoting index
!  integer(i32) :: n
!  integer(i32) :: lo,hi
!  real(r32) :: pivot
!  n=(right+left)/2
!  call swap(this(left),this(n))
!  pivot=this(left)
!  lo=left;hi=right
!  do while (lo <= hi)
!    do while (this(hi) > pivot)
!      hi=hi-1
!    end do
!
!    do while (lo <= hi .and. this(lo) <= pivot)
!      lo=lo+1
!    end do
!    if (lo <= hi) then
!      call swap(this(lo),this(hi))
!      lo=lo+1;hi=hi-1
!    end if
!  end do
!  call swap(this(left),this(hi))
!  iPivot=hi
!  end subroutine
!  !====================================================================!
  !====================================================================!
  subroutine partition3way_d1D(this,left,right,iPivot1, iPivot2)
    !! Interfaced with partition()
  !====================================================================!
  real(r64), intent(inout) :: this(:) !! 1D array
  integer(i32), intent(inout) :: left !! Left index
  integer(i32), intent(inout) :: right !! Right index
  integer(i32), intent(inout) :: iPivot1 !! Pivoting index
  integer(i32), intent(inout) :: iPivot2 !! Pivoting index
  integer(i32) :: i,j
  integer(i32) :: c
  real(r64) :: pivot

  iPivot1 = left
  iPivot2 = right
  pivot = this(left)
  i = left
  do while (i <= iPivot2)
    c = compare(this(i), pivot)
    select case(c)
      case(-1)
        call swap(this(iPivot1), this(i))
        iPivot1 = iPivot1 + 1
        i = i + 1
      case(1)
        call swap(this(i), this(iPivot2))
        iPivot2 = iPivot2 - 1
      case(0)
        i = i + 1
    end select
  end do
  end subroutine
  !====================================================================!
!  !====================================================================!
!  subroutine partition_i1D(this,left,right,iPivot)
!    !! Interfaced with partition()
!  !====================================================================!
!  integer(i32), intent(inout) :: this(:)
!  integer(i32), intent(inout) :: left
!  integer(i32), intent(inout) :: right
!  integer(i32), intent(inout) :: iPivot
!  integer(i32) :: n
!  integer(i32) :: lo,hi
!  integer(i32) :: pivot
!  n=(right+left)/2
!  call swap(this(left),this(n))
!  pivot=this(left)
!  lo=left;hi=right
!  do while (lo <= hi)
!    do while (this(hi) > pivot)
!      hi=hi-1
!    end do
!
!    do while (lo <= hi .and. this(lo) <= pivot)
!      lo=lo+1
!    end do
!    if (lo <= hi) then
!      call swap(this(lo),this(hi))
!      lo=lo+1;hi=hi-1
!    end if
!  end do
!  call swap(this(left),this(hi))
!  iPivot=hi
!  end subroutine
!  !====================================================================!
!  !====================================================================!
!  subroutine partition_id1D(this,left,right,iPivot)
!    !! Interfaced with partition()
!  !====================================================================!
!  integer(i64), intent(inout) :: this(:)
!  integer(i32), intent(inout) :: left
!  integer(i32), intent(inout) :: right
!  integer(i32), intent(inout) :: iPivot
!  integer(i32) :: n
!  integer(i32) :: lo,hi
!  integer(i64) :: pivot
!  n=(right+left)/2
!  call swap(this(left),this(n))
!  pivot=this(left)
!  lo=left;hi=right
!  do while (lo <= hi)
!    do while (this(hi) > pivot)
!      hi=hi-1
!    end do
!
!    do while (lo <= hi .and. this(lo) <= pivot)
!      lo=lo+1
!    end do
!    if (lo <= hi) then
!      call swap(this(lo),this(hi))
!      lo=lo+1;hi=hi-1
!    end if
!  end do
!  call swap(this(left),this(hi))
!  iPivot=hi
!  end subroutine
!  !====================================================================!
!  !====================================================================!
!  subroutine argPartition_r1D(this,idx,left,right,i)
!    !! Interfaced with argPartition()
!  !====================================================================!
!  real(r32) :: this(:)
!  integer(i32) :: idx(:)
!  integer(i32) :: left,right,i
!  integer(i32) :: n
!  integer(i32) :: lo,hi
!  real(r32) :: pivot
!  n=(right+left)/2
!  call swap(idx(left),idx(n))
!  pivot=this(idx(left))
!  lo=left+1;hi=right
!  do while (lo <= hi)
!    do while (this(idx(hi)) > pivot)
!      hi=hi-1
!    end do
!    do while (lo <= hi .and. this(idx(lo)) <= pivot)
!      lo=lo+1
!    end do
!    if (lo <= hi) then
!      call swap(idx(lo),idx(hi))
!      lo=lo+1;hi=hi-1
!    end if
!  end do
!  call swap(idx(left),idx(hi))
!  i=hi
!  end subroutine
!  !====================================================================!
!  !====================================================================!
!  subroutine argPartition_d1D(this,idx,left,right,i)
!    !! Interfaced with argPartition()
!  !====================================================================!
!  real(r64) :: this(:)
!  integer(i32) :: idx(:)
!  integer(i32) :: left,right,i
!  integer(i32) :: n
!  integer(i32) :: lo,hi
!  real(r64) :: pivot
!!  n=(right+left)/2
!!  call swap(idx(left),idx(n))
!  pivot=this(idx(left))
!  lo=left+1;hi=right
!  do while (lo <= hi)
!    do while (this(idx(hi)) > pivot)
!      hi=hi-1
!    end do
!    do while (lo <= hi .and. (this(idx(lo)) - pivot) <= 1.d-12  )
!      lo=lo+1
!    end do
!    if (lo <= hi) then
!      call swap(idx(lo),idx(hi))
!      lo=lo+1;hi=hi-1
!    end if
!  end do
!  call swap(idx(left),idx(hi))
!  i=hi
!  end subroutine
!  !====================================================================!
!  !====================================================================!
!  subroutine argPartition_i1D(this,idx,left,right,i)
!    !! Interfaced with argPartition()
!  !====================================================================!
!  integer(i32) :: this(:)
!  integer(i32) :: idx(:)
!  integer(i32) :: left,right,i
!  integer(i32) :: n
!  integer(i32) :: lo,hi
!  integer(i32) :: pivot
!  n=(right+left)/2
!  call swap(idx(left),idx(n))
!  pivot=this(idx(left))
!  lo=left+1;hi=right
!  do while (lo <= hi)
!    do while (this(idx(hi)) > pivot)
!      hi=hi-1
!    end do
!    do while (lo <= hi .and. this(idx(lo)) <= pivot)
!      lo=lo+1
!    end do
!    if (lo <= hi) then
!      call swap(idx(lo),idx(hi))
!      lo=lo+1;hi=hi-1
!    end if
!  end do
!  call swap(idx(left),idx(hi))
!  i=hi
!  end subroutine
!  !====================================================================!
!  !====================================================================!
!  subroutine argPartition_id1D(this,idx,left,right,i)
!    !! Interfaced with argPartition()
!  !====================================================================!
!  integer(i64) :: this(:)
!  integer(i32) :: idx(:)
!  integer(i32) :: left,right,i
!  integer(i32) :: n
!  integer(i32) :: lo,hi
!  integer(i64) :: pivot
!  n=(right+left)/2
!  call swap(idx(left),idx(n))
!  pivot=this(idx(left))
!  lo=left+1;hi=right
!  do while (lo <= hi)
!    do while (this(idx(hi)) > pivot)
!      hi=hi-1
!    end do
!    do while (lo <= hi .and. this(idx(lo)) <= pivot)
!      lo=lo+1
!    end do
!    if (lo <= hi) then
!      call swap(idx(lo),idx(hi))
!      lo=lo+1;hi=hi-1
!    end if
!  end do
!  call swap(idx(left),idx(hi))
!  i=hi
!  end subroutine
!  !====================================================================!
end module
