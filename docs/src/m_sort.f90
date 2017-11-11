  module m_sort
    !! Module containing in-place and indirect routines to sort an array of numbers.
    !!
    !! Uses an introspective sort on a set of number. See this http://www.geeksforgeeks.org/know-your-sorting-algorithm-set-2-introsort-cs-sorting-weapon/ for more information
    !!
    !! To begin, a quicksort with a median of three pivot is used until the size of the array is less than 16.  At this point, an insertion sort is used to reduce cache overhead.
    !! Unfortunately, a quicksort is not ideal for sorted/almost sorted arrays or arrays with duplicate values.  Therefore if the number of iterations exceededs a threshold, a heapsort is used instead.
    !! This provides a robust sorting algorithm that is still very fast for almost sorted arrays.
    !!
    !! In this implementation, the quicksort and heapsort are unstable sorts. A stable merge sort is therefore provided as an alternative but it has an order(N) memory overhead.
    !!
    !! Often, the numbers wish to be maintained in their given order, so with an O(N) memory overhead we can sort an integer array instead by calling argsort()
    !!
    !!Click on the interfaces to see how to use these routines
  use variableKind

  implicit none

  private

  public  :: sort

  interface sort
    !!Use an in-place introspection sort on an array of numbers
    !!
    !!Example usage
    !!```fortran
    !!program sortTest
    !!use variableKind
    !!use m_strings, only: str
    !!use m_allocate, only: allocate
    !!use m_random, only: rngInteger, rngNormal
    !!use m_arrays, only: arange, isSorted
    !!use m_sort, only: sort
    !!real(r64),allocatable :: d1D(:)
    !!integer(i32),allocatable :: i1D(:)
    !!integer(i32) :: N
    !!
    !!N = 10000
    !!write(*,'(a)') 'In-place sort a 10000 length array of random double precision numbers'
    !!call allocate(d1D,N)
    !!call rngNormal(d1D)
    !!call sort(d1D)
    !!write(*,'(a)') 'Double array is sorted? '//str(isSorted(d1D))
    !!write(*,'(a)') 'In-place sort a 10000 length array of random integers''
    !!call allocate(i1D,N)
    !!call rngInteger(i1D)
    !!call sort(i1D)
    !!write(*,'(a)') 'Integer array is sorted? '//str(isSorted(i1D))
    !!end program
    !!```
    module subroutine sort_i1D(this, stable)
      !! Interfaced with sort()
      integer(i32) :: this(:)
      logical, optional :: stable
    end subroutine
    module subroutine sort_id1D(this, stable)
      !! Interfaced with sort()
      integer(i64) :: this(:)
      logical, optional :: stable
    end subroutine
    module subroutine sort_r1D(this, stable)
      !! Interfaced with sort()
      real(r32) :: this(:)
      logical, optional :: stable
    end subroutine
    module subroutine sort_d1D(this, stable)
      !! Interfaced with sort()
      real(r64) :: this(:)
      logical, optional :: stable
    end subroutine
  end interface
  private :: sort_i1D, sort_id1D, sort_r1D, sort_d1D

  public :: argSort

  interface argSort
    !!Use an indirect introspection sort on an array of numbers
    !!
    !!Example usage
    !!```fortran
    !!program argSortTest
    !!use variableKind
    !!use m_strings, only: str
    !!use m_random, only: rngInteger, rngNormal
    !!use m_arrays, only: arange, isSorted
    !!use m_Sort, only: argSort
    !!real(r64),allocatable :: d1D(:)
    !!integer(i32),allocatable :: i1D(:)
    !!integer(i32),allocatable :: indx(:)
    !!integer(i32) :: i, N
    !!
    !!N=10000
    !!call allocate(indx, N)
    !!call arange(indx, 1, N)
    !!call allocate(d1D, N)
    !!call rngNormal(d1D)
    !!call argSort(d1D, indx)
    !!write(*,'(a)') 'Double array is indirectly sorted? '//str(isSorted(d1D(indx)))
    !!
    !!call arange(indx, 1, N)
    !!call allocate(i1D,N)
    !!call rngInteger(i1D)
    !!call argSort(i1D, indx)
    !!write(*,'(a)') 'Integer array is indirectly sorted? '//str(isSorted(i1D(indx)))
    !!end program
    !!```
    module subroutine argSort_i1D(this, i, stable)
      !! Interfaced with argsort()
      integer(i32) :: this(:) !! 1D array
      integer(i32) :: i(:) !! Index to sort
      logical, optional :: stable !! Stable sort?
    end subroutine
    module subroutine argSort_id1D(this, i, stable)
      !! Interfaced with argsort()
      integer(i64) :: this(:) !! 1D array
      integer(i32) :: i(:) !! Index to sort
      logical, optional :: stable !! Stable sort?
    end subroutine
    module subroutine argSort_r1D(this, i, stable)
      !! Interfaced with argsort()
      real(r32) :: this(:) !! 1D array
      integer(i32) :: i(:) !! Index to sort
      logical, optional :: stable !! Stable sort?
    end subroutine
    module subroutine argSort_d1D(this, i, stable)
      !! Interfaced with argsort()
      real(r64) :: this(:) !! 1D array
      integer(i32) :: i(:) !! Index to sort
      logical, optional :: stable !! Stable sort?
    end subroutine
  end interface
  private :: argSort_i1D, argSort_id1D, argSort_r1D, argSort_d1D

  interface introsort
    !! Perform an in-place introspective sort on an array
    module subroutine introsort_r1D(this)
      !! Interfaced with introsort()
      real(r32) :: this(:) !! 1D array
    end subroutine
    module subroutine introsort_d1D(this)
      !! Interfaced with introsort()
      real(r64) :: this(:) !! 1D array
    end subroutine
    module subroutine introsort_i1D(this)
      !! Interfaced with introsort()
      integer(i32) :: this(:) !! 1D array
    end subroutine
    module subroutine introsort_id1D(this)
      !! Interfaced with introsort()
      integer(i64) :: this(:) !! 1D array
    end subroutine
  end interface

  interface argintrosort
    !! Perform an indirect introsort on an array
    module subroutine argintrosort_r1D(this,i)
      !! Interfaced with argintrosort()
      real(r32) :: this(:) !! 1D array
      integer(i32) :: i(:) !! Sort this integer key
    end subroutine
    module subroutine argintrosort_d1D(this,i)
      !! Interfaced with argintrosort()
      real(r64) :: this(:) !! 1D array
      integer(i32) :: i(:) !! Sort this integer key
    end subroutine
    module subroutine argintrosort_i1D(this,i)
      !! Interfaced with argintrosort()
      integer(i32) :: this(:) !! 1D array
      integer(i32) :: i(:) !! Sort this integer key
    end subroutine
    module subroutine argintrosort_id1D(this,i)
      !! Interfaced with argintrosort()
      integer(i64) :: this(:) !! 1D array
      integer(i32) :: i(:) !! Sort this integer key
    end subroutine
  end interface

  interface MergeSort
    !! Perform an in-place stable merge sort on an array
    module subroutine mergesort_r1D(this)
      !! Interf aced with mergesort()
      real(r32):: this(:) !! 1D array
    end subroutine
    module subroutine mergesort_d1D(this)
      !! Interfaced with mergesort()
      real(r64) :: this(:) !! 1D array
    end subroutine
    module subroutine mergesort_i1D(this)
      !! Interfaced with mergesort()
      integer(i32) :: this(:) !! 1D array
    end subroutine
    module subroutine mergesort_id1D(this)
      !! Interfaced with mergesort()
      integer(i64) :: this(:) !! 1D array
    end subroutine
  end interface

  interface argMergeSort
    !! Perform an indirect stable merge sort on an array
    module subroutine argmergesort_r1D(this,i)
      !! Interfaced with argmergesort()
      real(r32) :: this(:) !! 1D array
      integer(i32) :: i(:) !! Sort this integer key
    end subroutine
    module subroutine argmergesort_d1D(this,i)
      !! Interfaced with argmergesort()
      real(r64) :: this(:) !! 1D array
      integer(i32) :: i(:) !! Sort this integer key
    end subroutine
    module subroutine argmergesort_i1D(this,i)
      !! Interfaced with argmergesort()
      integer(i32) :: this(:) !! 1D array
      integer(i32) :: i(:) !! Sort this integer key
    end subroutine
    module subroutine argmergesort_id1D(this,i)
      !! Interfaced with argmergesort()
      integer(i64) :: this(:) !! 1D array
      integer(i32) :: i(:) !! Sort this integer key
    end subroutine
  end interface

  public :: insertionSort

  interface insertionsort
    !! Perform an in-place insertion sort on an array
    module subroutine insertionsort_r1D(this,iLeft,iRight)
      !! Interfaced with insertionsort()
      real(r32) :: this(:) !! 1D array
      integer(i32) :: iLeft !! Left index
      integer(i32) :: iRight !! Right index
    end subroutine
    module subroutine insertionsort_d1D(this,iLeft,iRight)
      !! Interfaced with insertionsort()
      real(r64) :: this(:) !! 1D array
      integer(i32) :: iLeft !! Left index
      integer(i32) :: iRight !! Right index
    end subroutine
    module subroutine insertionsort_i1D(this,iLeft,iRight)
      !! Interfaced with insertionsort()
      integer(i32) :: this(:) !! 1D array
      integer(i32) :: iLeft !! Left index
      integer(i32) :: iRight !! Right index
    end subroutine
    module subroutine insertionsort_id1D(this,iLeft,iRight)
      !! Interfaced with insertionsort()
      integer(i64) :: this(:) !! 1D array
      integer(i32) :: iLeft !! Left index
      integer(i32) :: iRight !! Right index
    end subroutine
  end interface

  public :: argInsertionsort

  interface argInsertionsort
    !! Perform an indirect insertion sort on an array
    module subroutine argInsertionsort_r1D(this,indx,iLeft,iRight)
      !! Interfaced with arginsertionsort()
      real(r32) :: this(:) !! 1D array
      integer(i32) :: indx(:) !! Sort this integer key
      integer(i32) :: iLeft !! Left index
      integer(i32) :: iRight !! Right index
    end subroutine
    module subroutine argInsertionsort_d1D(this,indx,iLeft,iRight)
      !! Interfaced with arginsertionsort()
      real(r64) :: this(:) !! 1D array
      integer(i32) :: indx(:) !! Sort this integer key
      integer(i32) :: iLeft !! Left index
      integer(i32) :: iRight !! Right index
    end subroutine
    module subroutine argInsertionsort_i1D(this,indx,iLeft,iRight)
      !! Interfaced with arginsertionsort()
      integer(i32) :: this(:) !! 1D array
      integer(i32) :: indx(:) !! Sort this integer key
      integer(i32) :: iLeft !! Left index
      integer(i32) :: iRight !! Right index
    end subroutine
    module subroutine argInsertionsort_id1D(this,indx,iLeft,iRight)
      !! Interfaced with arginsertionsort()
      integer(i64) :: this(:) !! 1D array
      integer(i32) :: indx(:) !! Sort this integer key
      integer(i32) :: iLeft !! Left index
      integer(i32) :: iRight !! Right index
    end subroutine
  end interface

  interface heapsort
    !! Perform an in-place heapsort on an array
    module subroutine heapsort_r1D(this)
      !! Interfaced with heapsort()
      real(r32) :: this(0:) !! 1D array
    end subroutine
    module subroutine heapsort_d1D(this)
      !! Interfaced with heapsort()
      real(r64):: this(0:) !! 1D array
    end subroutine
    module subroutine heapsort_i1D(this)
      !! Interfaced with heapsort()
      integer(i32):: this(0:) !! 1D array
    end subroutine
    module subroutine heapsort_id1D(this)
      !! Interfaced with heapsort()
      integer(i64):: this(0:) !! 1D array
    end subroutine
  end interface

  interface argHeapsort
    !! Perform an indirect heapsort on an array
    module subroutine argHeapsort_r1D(this,indx)
      !! Interfaced with argHeapsort()
      real(r32) :: this(:) !! 1D array
      integer(i32) :: indx(0:) !! Sort this integer key
    end subroutine
    module subroutine argHeapsort_d1D(this,indx)
      !! Interfaced with argHeapsort()
      real(r64):: this(:) !! 1D array
      integer(i32) :: indx(0:) !! Sort this integer key
    end subroutine
    module subroutine argHeapsort_i1D(this,indx)
      !! Interfaced with argHeapsort()
      integer(i32):: this(:) !! 1D array
      integer(i32) :: indx(0:) !! Sort this integer key
    end subroutine
    module subroutine argHeapsort_id1D(this,indx)
      !! Interfaced with argHeapsort()
      integer(i64):: this(:) !! 1D array
      integer(i32) :: indx(0:) !! Sort this integer key
    end subroutine
  end interface

  end module
