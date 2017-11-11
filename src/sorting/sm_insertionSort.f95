submodule (m_Sort) m_InsertionSort
  !! Insertion sort and argInsertion sort routines

use variableKind
use m_swap, only: swap

implicit none

contains

!====================================================================!
module procedure insertionSort_r1D
  !! Interfaced with insertionSort()
!====================================================================!
!module subroutine insertionSort_r1D(this,iLeft,iRight)
!real(r32) :: this(:)
!integer(i32) :: iLeft,iRight
integer(i32) :: i,j

do i=iLeft,iRight
  inner: do j=i,iLeft+1,-1
    if (this(j) < this(j-1))then
      call swap(this(j),this(j-1))
    else
      exit inner
    end if
  end do inner
end do
end procedure
!====================================================================!
!====================================================================!
module procedure argInsertionSort_r1D
  !! Interfaced with argInsertionSort()
!====================================================================!
!subroutine argInsertionSort_r1D(this,indx,iLeft,iRight)
!real(r32) :: this(:)
!integer(i32) :: indx(:)
!integer(i32) :: iLeft,iRight
integer(i32) :: i,j
do i=iLeft,iRight
  inner: do j=i,iLeft+1,-1
    if (this(indx(j)) < this(indx(j-1)))then
      call swap(indx(j),indx(j-1))
    else
      exit inner
    end if
  end do inner
end do
end procedure
!====================================================================!
!====================================================================!
module procedure insertionSort_d1D
  !! Interfaced with insertionSort()
!====================================================================!
!subroutine insertionSort_d1D(this,iLeft,iRight)
!real(r64) :: this(:)
!integer(i32) :: iLeft,iRight
integer(i32) :: i,j
do i=iLeft,iRight
  inner: do j=i,iLeft+1,-1
    if (this(j) < this(j-1))then
      call swap(this(j),this(j-1))
    else
      exit inner
    end if
  end do inner
end do
end procedure
!====================================================================!
!====================================================================!
module procedure argInsertionSort_d1D
  !! Interfaced with argInsertionSort()
!====================================================================!
!subroutine argInsertionSort_d1D(this,indx,iLeft,iRight)
!real(r64) :: this(:)
!integer(i32) :: indx(:)
!integer(i32) :: iLeft,iRight
integer(i32) :: i,j
do i=iLeft,iRight
  inner: do j=i,iLeft+1,-1
    if (this(indx(j)) < this(indx(j-1)))then
      call swap(indx(j),indx(j-1))
    else
      exit inner
    end if
  end do inner
end do
end procedure
!====================================================================!
!====================================================================!
module procedure insertionSort_i1D
  !! Interfaced with insertionSort()
!====================================================================!
!subroutine insertionSort_i1D(this,iLeft,iRight)
!integer(i32) :: this(:)
!integer(i32) :: iLeft,iRight
integer(i32) :: i,j
do i=iLeft,iRight
  inner: do j=i,iLeft+1,-1
    if (this(j) < this(j-1))then
      call swap(this(j),this(j-1))
    else
      exit inner
    end if
  end do inner
end do
end procedure
!====================================================================!
!====================================================================!
module procedure argInsertionSort_i1D
  !! Interfaced with argInsertionSort()
!====================================================================!
!subroutine argInsertionSort_i1D(this,indx,iLeft,iRight)
!integer(i32) :: this(:)
!integer(i32) :: indx(:)
!integer(i32) :: iLeft,iRight
integer(i32) :: i,j
do i=iLeft,iRight
  inner: do j=i,iLeft+1,-1
    if (this(indx(j)) < this(indx(j-1)))then
      call swap(indx(j),indx(j-1))
    else
      exit inner
    end if
  end do inner
end do
end procedure
!====================================================================!
!====================================================================!
module procedure insertionSort_id1D
  !! Interfaced with insertionSort()
!====================================================================!
!subroutine insertionSort_id1D(this,iLeft,iRight)
!integer(i64) :: this(:)
!integer(i32) :: iLeft,iRight
integer(i32) :: i,j
do i=iLeft,iRight
  inner: do j=i,iLeft+1,-1
    if (this(j) < this(j-1))then
      call swap(this(j),this(j-1))
    else
      exit inner
    end if
  end do inner
end do
end procedure
!====================================================================!
!====================================================================!
module procedure argInsertionSort_id1D
  !! Interfaced with argInsertionSort()
!====================================================================!
!subroutine argInsertionSort_id1D(this,indx,iLeft,iRight)
!integer(i64) :: this(:)
!integer(i32) :: indx(:)
!integer(i32) :: iLeft,iRight
integer(i32) :: i,j
do i=iLeft,iRight
  inner: do j=i,iLeft+1,-1
    if (this(indx(j)) < this(indx(j-1)))then
      call swap(indx(j),indx(j-1))
    else
      exit inner
    end if
  end do inner
end do
end procedure
!====================================================================!
end submodule
