  module m_sort
    !! Module containing in-place and indirect routines to sort an array of numbers.
    !!
    !! Uses an introspective sort on a set of number. See this http://www.geeksforgeeks.org/know-your-sorting-algorithm-set-2-introsort-cs-sorting-weapon/ for more information
    !!
    !! To begin, a quicksort with a median of three pivot is used until the size of the array is less than 16.  At this point, an insertion sort is used to reduce cache overhead and tail recursion.
    !! Unfortunately, a quicksort is not ideal for sorted/almost sorted arrays or arrays with duplicate values.  Therefore if the number of iterations exceededs a threshold, a heapsort is used instead.
    !! This provides a robust sorting algorithm that is still very fast for almost sorted arrays.
    !!
    !! In this implementation, the quicksort and heapsort are unstable sorts. A stable merge sort is therefore provided as an alternative but it has an order(N) memory overhead.
    !!
    !! Often, the numbers wish to be maintained in their given order, so with an O(N) memory overhead we can sort an integer array instead by calling argsort()
    !!
    !! See [[sort]] and [[argSort]] for more information.
  use variableKind
  use m_errors, only: msg
  use m_allocate, only: allocate
  use m_array1D, only: arange, isSorted
  use m_random, only: rngNormal, rngInteger
  use m_unitTester, only: tester

  implicit none

  private

  public :: sorting_test

  public  :: sort

  interface sort
    !!Use an in-place introspection sort on an array of numbers
    !!
    !!Example usage
    !!```fortran
    !!program sortTest
    !!use variableKind, only: i32, r64
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
      !! Interfaced with [[sort]]
      integer(i32) :: this(:)
      logical, optional :: stable
    end subroutine
    module subroutine sort_id1D(this, stable)
      !! Interfaced with [[sort]]
      integer(i64) :: this(:)
      logical, optional :: stable
    end subroutine
    module subroutine sort_r1D(this, stable)
      !! Interfaced with [[sort]]
      real(r32) :: this(:)
      logical, optional :: stable
    end subroutine
    module subroutine sort_d1D(this, stable)
      !! Interfaced with [[sort]]
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
      !! Interfaced with [[argSort]]
      integer(i32) :: this(:) !! 1D array
      integer(i32) :: i(:) !! Index to sort
      logical, optional :: stable !! Stable sort?
    end subroutine
    module subroutine argSort_id1D(this, i, stable)
      !! Interfaced with [[argSort]]
      integer(i64) :: this(:) !! 1D array
      integer(i32) :: i(:) !! Index to sort
      logical, optional :: stable !! Stable sort?
    end subroutine
    module subroutine argSort_r1D(this, i, stable)
      !! Interfaced with [[argSort]]
      real(r32) :: this(:) !! 1D array
      integer(i32) :: i(:) !! Index to sort
      logical, optional :: stable !! Stable sort?
    end subroutine
    module subroutine argSort_d1D(this, i, stable)
      !! Interfaced with [[argSort]]
      real(r64) :: this(:) !! 1D array
      integer(i32) :: i(:) !! Index to sort
      logical, optional :: stable !! Stable sort?
    end subroutine
  end interface
  private :: argSort_i1D, argSort_id1D, argSort_r1D, argSort_d1D

  private :: introsort
  interface introsort
    !! Perform an in-place introspective sort on an array
    module subroutine introsort_r1D(this)
      !! Interfaced with [[introsort]]
      real(r32) :: this(:) !! 1D array
    end subroutine
    module subroutine introsort_d1D(this)
      !! Interfaced with [[introsort]]
      real(r64) :: this(:) !! 1D array
    end subroutine
    module subroutine introsort_i1D(this)
      !! Interfaced with [[introsort]]
      integer(i32) :: this(:) !! 1D array
    end subroutine
    module subroutine introsort_id1D(this)
      !! Interfaced with [[introsort]]
      integer(i64) :: this(:) !! 1D array
    end subroutine
  end interface

  private :: argIntrosort
  interface argintrosort
    !! Perform an indirect introsort on an array
    module subroutine argintrosort_r1D(this,i)
      !! Interfaced with [[argIntrosort]]
      real(r32) :: this(:) !! 1D array
      integer(i32) :: i(:) !! Sort this integer key
    end subroutine
    module subroutine argintrosort_d1D(this,i)
      !! Interfaced with [[argIntrosort]]
      real(r64) :: this(:) !! 1D array
      integer(i32) :: i(:) !! Sort this integer key
    end subroutine
    module subroutine argintrosort_i1D(this,i)
      !! Interfaced with [[argIntrosort]]
      integer(i32) :: this(:) !! 1D array
      integer(i32) :: i(:) !! Sort this integer key
    end subroutine
    module subroutine argintrosort_id1D(this,i)
      !! Interfaced with [[argIntrosort]]
      integer(i64) :: this(:) !! 1D array
      integer(i32) :: i(:) !! Sort this integer key
    end subroutine
  end interface

  interface MergeSort
    !! Perform an in-place stable merge sort on an array
    module subroutine mergesort_r1D(this)
      !! Interf aced with [[mergesort]]
      real(r32):: this(:) !! 1D array
    end subroutine
    module subroutine mergesort_d1D(this)
      !! Interfaced with [[mergesort]]
      real(r64) :: this(:) !! 1D array
    end subroutine
    module subroutine mergesort_i1D(this)
      !! Interfaced with [[mergesort]]
      integer(i32) :: this(:) !! 1D array
    end subroutine
    module subroutine mergesort_id1D(this)
      !! Interfaced with [[mergesort]]
      integer(i64) :: this(:) !! 1D array
    end subroutine
  end interface

  interface argMergeSort
    !! Perform an indirect stable merge sort on an array
    module subroutine argmergesort_r1D(this,i)
      !! Interfaced with [[argmergesort]]
      real(r32) :: this(:) !! 1D array
      integer(i32) :: i(:) !! Sort this integer key
    end subroutine
    module subroutine argmergesort_d1D(this,i)
      !! Interfaced with [[argmergesort]]
      real(r64) :: this(:) !! 1D array
      integer(i32) :: i(:) !! Sort this integer key
    end subroutine
    module subroutine argmergesort_i1D(this,i)
      !! Interfaced with [[argmergesort]]
      integer(i32) :: this(:) !! 1D array
      integer(i32) :: i(:) !! Sort this integer key
    end subroutine
    module subroutine argmergesort_id1D(this,i)
      !! Interfaced with [[argmergesort]]
      integer(i64) :: this(:) !! 1D array
      integer(i32) :: i(:) !! Sort this integer key
    end subroutine
  end interface

  public :: insertionSort

  interface insertionsort
    !! Perform an in-place insertion sort on an array
    module subroutine insertionsort_r1D(this,iLeft,iRight)
      !! Interfaced with [[insertionsort]]
      real(r32) :: this(:) !! 1D array
      integer(i32) :: iLeft !! Left index
      integer(i32) :: iRight !! Right index
    end subroutine
    module subroutine insertionsort_d1D(this,iLeft,iRight)
      !! Interfaced with [[insertionsort]]
      real(r64) :: this(:) !! 1D array
      integer(i32) :: iLeft !! Left index
      integer(i32) :: iRight !! Right index
    end subroutine
    module subroutine insertionsort_i1D(this,iLeft,iRight)
      !! Interfaced with [[insertionsort]]
      integer(i32) :: this(:) !! 1D array
      integer(i32) :: iLeft !! Left index
      integer(i32) :: iRight !! Right index
    end subroutine
    module subroutine insertionsort_id1D(this,iLeft,iRight)
      !! Interfaced with [[insertionsort]]
      integer(i64) :: this(:) !! 1D array
      integer(i32) :: iLeft !! Left index
      integer(i32) :: iRight !! Right index
    end subroutine
  end interface
  private :: insertionsort_r1D, insertionsort_d1D, insertionsort_i1D, insertionsort_id1D

  public :: argInsertionsort

  interface argInsertionsort
    !! Perform an indirect insertion sort on an array
    module subroutine argInsertionsort_r1D(this,indx,iLeft,iRight)
      !! Interfaced with [[arginsertionsort]]
      real(r32) :: this(:) !! 1D array
      integer(i32) :: indx(:) !! Sort this integer key
      integer(i32) :: iLeft !! Left index
      integer(i32) :: iRight !! Right index
    end subroutine
    module subroutine argInsertionsort_d1D(this,indx,iLeft,iRight)
      !! Interfaced with [[arginsertionsort]]
      real(r64) :: this(:) !! 1D array
      integer(i32) :: indx(:) !! Sort this integer key
      integer(i32) :: iLeft !! Left index
      integer(i32) :: iRight !! Right index
    end subroutine
    module subroutine argInsertionsort_i1D(this,indx,iLeft,iRight)
      !! Interfaced with [[arginsertionsort]]
      integer(i32) :: this(:) !! 1D array
      integer(i32) :: indx(:) !! Sort this integer key
      integer(i32) :: iLeft !! Left index
      integer(i32) :: iRight !! Right index
    end subroutine
    module subroutine argInsertionsort_id1D(this,indx,iLeft,iRight)
      !! Interfaced with [[arginsertionsort]]
      integer(i64) :: this(:) !! 1D array
      integer(i32) :: indx(:) !! Sort this integer key
      integer(i32) :: iLeft !! Left index
      integer(i32) :: iRight !! Right index
    end subroutine
  end interface
  private :: argInsertionsort_r1D, argInsertionsort_d1D, argInsertionsort_i1D, argInsertionsort_id1D

  private :: heapsort
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

  private :: argHeapsort
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

  contains

  !====================================================================!
  subroutine sorting_test(test, N)
    !! graph: false
  !====================================================================!
  class(tester) :: test
  integer(i32) :: N

  real(r32), allocatable :: ar1D(:), br1D(:)
  real(r64), allocatable :: a1D(:), b1D(:)
  integer(i32), allocatable :: ia1D(:), ib1D(:), ic1D(:)
  integer(i64), allocatable :: iad1D(:), ibd1D(:)

  call Msg('==========================')
  call Msg('Testing : Sorting')
  call Msg('==========================')

!   Initial setup for testing
  call allocate(ar1D, N)
  call allocate(br1D, N)

  call allocate(a1D, N)
  call allocate(b1D, N)

  call allocate(ia1D, N)
  call allocate(ib1D, N)
  call allocate(ic1D, N)

  call allocate(iad1D, N)
  call allocate(ibd1D, N)

  call rngNormal(a1D)
  ar1D = real(a1D)

  br1D = ar1D
  call sort(br1D)
  call test%test(isSorted(br1D),'Introsort_r1D')

  br1D = ar1D
  call arange(ia1D, 1, N)
  call argsort(br1D, ia1D)
  call test%test(isSorted(br1D,ia1D),'argIntrosort_r1D')

  br1D = ar1D
  call Sort(br1D,.true.)
  call test%test(isSorted(br1D),'Mergesort_r1D')

  br1D = ar1D
  call arange(ia1D, 1, N)
  call argsort(br1D, ia1D,.true.)
  call test%test(isSorted(br1D,ia1D),'argMergesort_r1D')

  b1D = a1D
  call sort(b1D)
  call test%test(isSorted(b1D),'Introsort_d1D on Sorted array')

  b1D = a1D
  call arange(ia1D, 1, N)
  call argsort(b1D, ia1D)
  call test%test(isSorted(b1D,ia1D),'argIntrosort_d1D')

  b1D = a1D
  call Sort(b1D,.true.)
  call test%test(isSorted(b1D),'Mergesort_d1D')

  b1D = a1D
  call arange(ia1D, 1, N)
  call argsort(b1D, ia1D,.true.)
  call test%test(isSorted(b1D,ia1D),'argMergesort_d1D')

  call rngInteger(ia1D,1, N)
  ib1D = ia1D
  call sort(ib1D)
  call test%test(isSorted(ib1D),'Introsort_i1D')

  ib1D = ia1D
  call arange(ic1D, 1, N)
  call argsort(ib1D,ic1D)
  call test%test(isSorted(ib1D,ic1D),'argIntrosort_i1D')

  ib1D = ia1D
  call sort(ib1D,.true.)
  call test%test(isSorted(ib1D),'Mergesort_i1D')

  ib1D = ia1D
  call arange(ic1D, 1, N)
  call argsort(ib1D,ic1D,.true.)
  call test%test(isSorted(ib1D,ic1D),'argMergesort_i1D')

  end subroutine
  !====================================================================!

  end module
