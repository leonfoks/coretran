submodule (m_partition) sm_partition
!! Partition and argPartition routines
use m_swap, only: swap

implicit none

contains

  !====================================================================!
  module procedure partition_r1D!(this,left,right,iPivot)
    !! Interfaced with [[partition]]
  !====================================================================!
!  real(r32), intent(inout) :: this(:) !! 1D array
!  integer(i32), intent(in) :: left !! Left index
!  integer(i32), intent(in) :: right !! Right index
!  integer(i32), intent(inout) :: iPivot !! Pivoting index
  integer(i32) :: lo,hi
  real(r32) :: pivot
  pivot=this(left)
  lo=left;hi=right
  do while (lo <= hi)
    do while (this(hi) > pivot)
      hi=hi-1
    end do

    do while (lo <= hi .and. this(lo) <= pivot)
      lo=lo+1
    end do
    if (lo <= hi) then
      call swap(this(lo),this(hi))
      lo=lo+1;hi=hi-1
    end if
  end do
  call swap(this(left),this(hi))
  iPivot=hi
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure partition_d1D!(this,left,right,iPivot)
    !! Interfaced with [[partition]]
  !====================================================================!
  !real(r64), intent(inout) :: this(:) !! 1D array
  !integer(i32), intent(in) :: left !! Left index
  !integer(i32), intent(in) :: right !! Right index
  !integer(i32), intent(inout) :: iPivot !! Pivoting index
  integer(i32) :: lo,hi
  real(r64) :: pivot
  pivot=this(left)
  lo=left;hi=right
  do while (lo <= hi)
    do while (this(hi) > pivot)
      hi=hi-1
    end do

    do while (lo <= hi .and. this(lo) <= pivot)
      lo=lo+1
    end do
    if (lo <= hi) then
      call swap(this(lo),this(hi))
      lo=lo+1;hi=hi-1
    end if
  end do
  call swap(this(left),this(hi))
  iPivot=hi
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure partition_i1D!(this,left,right,iPivot)
    !! Interfaced with [[partition]]
  !====================================================================!
  !integer(i32), intent(inout) :: this(:)
  !integer(i32), intent(in) :: left
  !integer(i32), intent(in) :: right
  !integer(i32), intent(inout) :: iPivot
  integer(i32) :: lo,hi
  integer(i32) :: pivot
  pivot=this(left)
  lo=left;hi=right
  do while (lo <= hi)
    do while (this(hi) > pivot)
      hi=hi-1
    end do

    do while (lo <= hi .and. this(lo) <= pivot)
      lo=lo+1
    end do
    if (lo <= hi) then
      call swap(this(lo),this(hi))
      lo=lo+1;hi=hi-1
    end if
  end do
  call swap(this(left),this(hi))
  iPivot=hi
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure partition_id1D!(this,left,right,iPivot)
    !! Interfaced with [[partition]]
  !====================================================================!
  !integer(i64), intent(inout) :: this(:)
  !integer(i32), intent(in) :: left
  !integer(i32), intent(in) :: right
  !integer(i32), intent(inout) :: iPivot
  integer(i32) :: lo,hi
  integer(i64) :: pivot
  pivot=this(left)
  lo=left;hi=right
  do while (lo <= hi)
    do while (this(hi) > pivot)
      hi=hi-1
    end do

    do while (lo <= hi .and. this(lo) <= pivot)
      lo=lo+1
    end do
    if (lo <= hi) then
      call swap(this(lo),this(hi))
      lo=lo+1;hi=hi-1
    end if
  end do
  call swap(this(left),this(hi))
  iPivot=hi
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure argPartition_r1D!(this,idx,left,right,i)
    !! Interfaced with [[argPartition]]
  !====================================================================!
  !real(r32), intent(in) :: this(:)
  !integer(i32), intent(inout) :: idx(:)
  !integer(i32), intent(in) :: left
  !integer(i32), intent(in) :: right
  !integer(i32), intent(inout) :: i
  integer(i32) :: lo,hi
  real(r32) :: pivot
  pivot=this(idx(left))
  lo=left+1;hi=right
  do while (lo <= hi)
    do while (this(idx(hi)) > pivot)
      hi=hi-1
    end do
    do while (lo <= hi .and. this(idx(lo)) <= pivot)
      lo=lo+1
    end do
    if (lo <= hi) then
      call swap(idx(lo),idx(hi))
      lo=lo+1;hi=hi-1
    end if
  end do
  call swap(idx(left),idx(hi))
  i=hi
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure argPartition_d1D!(this,idx,left,right,i)
    !! Interfaced with [[argPartition]]
  !====================================================================!
  !real(r64), intent(in) :: this(:)
  !integer(i32), intent(inout) :: idx(:)
  !integer(i32), intent(in) :: left
  !integer(i32), intent(in) :: right
  !integer(i32), intent(inout) :: i
  integer(i32) :: lo,hi
  real(r64) :: pivot
  pivot=this(idx(left))
  lo=left+1;hi=right
  do while (lo <= hi)
    do while (this(idx(hi)) > pivot)
      hi=hi-1
    end do
    do while (lo <= hi .and. this(idx(lo)) <= pivot)
      lo=lo+1
    end do
    if (lo <= hi) then
      call swap(idx(lo),idx(hi))
      lo=lo+1;hi=hi-1
    end if
  end do
  call swap(idx(left),idx(hi))
  i=hi
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure argPartition_i1D!(this,idx,left,right,i)
    !! Interfaced with [[argPartition]]
  !====================================================================!
  !integer(i32), intent(in) :: this(:)
  !integer(i32), intent(inout) :: idx(:)
  !integer(i32), intent(in) :: left
  !integer(i32), intent(in) :: right
  !integer(i32), intent(inout) :: i
  integer(i32) :: lo,hi
  integer(i32) :: pivot
  pivot=this(idx(left))
  lo=left+1;hi=right
  do while (lo <= hi)
    do while (this(idx(hi)) > pivot)
      hi=hi-1
    end do
    do while (lo <= hi .and. this(idx(lo)) <= pivot)
      lo=lo+1
    end do
    if (lo <= hi) then
      call swap(idx(lo),idx(hi))
      lo=lo+1;hi=hi-1
    end if
  end do
  call swap(idx(left),idx(hi))
  i=hi
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure argPartition_id1D!(this,idx,left,right,i)
    !! Interfaced with [[argPartition]]
  !====================================================================!
  !integer(i64), intent(in) :: this(:)
  !integer(i32), intent(inout) :: idx(:)
  !integer(i32), intent(in) :: left
  !integer(i32), intent(in) :: right
  !integer(i32), intent(inout) :: i
  integer(i32) :: lo,hi
  integer(i64) :: pivot
  pivot=this(idx(left))
  lo=left+1;hi=right
  do while (lo <= hi)
    do while (this(idx(hi)) > pivot)
      hi=hi-1
    end do
    do while (lo <= hi .and. this(idx(lo)) <= pivot)
      lo=lo+1
    end do
    if (lo <= hi) then
      call swap(idx(lo),idx(hi))
      lo=lo+1;hi=hi-1
    end if
  end do
  call swap(idx(left),idx(hi))
  i=hi
  end procedure
  !====================================================================!

end submodule
