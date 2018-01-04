  module m_select
    !! Perform a quickselect on an array. Quick select finds the kth smallest number in an array. It also puts values lower than the kth on the left, and those higher on the right
    !! This makes it perfect for finding the median.
  use variableKind
  use m_errors, only: msg
  use m_allocate, only: allocate
  use m_array1D, only: arange
  use m_deallocate, only: deallocate
  use m_random, only: rngNormal, rngInteger
  use m_sort, only: sort
  use m_unitTester, only: tester

  implicit none

  private

  public :: select_test

  public  :: select

  interface select
    !!Use an in-place quick select on an array of numbers
    !!
    !!Example usage
    !!```fortran
    !!program selectTest
    !!use variableKind
    !!use m_strings, only: str
    !!use m_allocate, only: allocate
    !!use m_random, only: rngInteger,rngNormal
    !!use m_select, only: select
    !!real(r64),allocatable :: d1D(:)
    !!integer(i32),allocatable :: i1D(:)
    !!integer(i32) :: k
    !!real(r64) :: dv
    !!integer(i32) :: iv
    !!
    !!write(*,'(a)') 'Select the kth smallest element from a 10000 length array of random double precision numbers'
    !!call allocate(d1D, 10000)
    !!call rngNormal(d1D)
    !!k = (1+size(d1D))/2
    !!dv = select(d1D,k)
    !!write(*,'(a)') 'kth element? '//str(dv)
    !!write(*,'(a)') 'Select the kth smallest element from a 10000 length array of random integers''
    !!call allocate(i1D, 10000)
    !!call rngInteger(i1D)
    !!iv = select(i1D, k)
    !!write(*,'(a)') 'kth element? '//str(iv)
    !!end program
    !!```
    module subroutine quickSelect_i1D(this, k, res)
      !! Interfaced with select()
      integer(i32), intent(inout) :: this(:) !! Array to choose kth smallest from
      integer(i32), intent(in) :: k !! kth smallest element
      integer(i32) :: res
    end subroutine
    module subroutine quickSelect_id1D(this, k, res)
      !! Interfaced with select()
      integer(i64), intent(inout) :: this(:) !! Array to choose kth smallest from
      integer(i32), intent(in) :: k !! kth smallest element
      integer(i64) :: res
    end subroutine
    module subroutine quickSelect_r1D(this, k, res)
      !! Interfaced with select()
      real(r32), intent(inout) :: this(:) !! Array to choose kth smallest from
      integer(i32), intent(in) :: k !! kth smallest element
      real(r32) :: res
    end subroutine
    module subroutine quickSelect_d1D(this, k, res)
      !! Interfaced with select()
      real(r64), intent(inout) :: this(:) !! Array to choose kth smallest from
      integer(i32), intent(in) :: k !! kth smallest element
      real(r64) :: res
    end subroutine
  end interface

  public :: argSelect

  interface argSelect
    !!Use an indirect introspection sort on an array of numbers
    !!
    !!Example usage
    !!```fortran
    !!program argSortTest
    !!use variableKind
    !!use m_strings, only: str
    !!use m_random, only: rngInteger,rngNormal
    !!use m_arrays, only: isSorted
    !!use m_Sort, only: argSort
    !!real(r64),allocatable :: d1D(:)
    !!integer(i32),allocatable :: i1D(:)
    !!integer(i32),allocatable :: indx(:)
    !!integer(i32) :: i, k, N
    !!N = 10000
    !!call allocate(indx,N)
    !!call arange(indx, 1, N)
    !!call allocate(d1D,N)
    !!call rngNormal(d1D)
    !!k = (size(d1D)+1)/2
    !!call argSelect(d1D, indx, k)
    !!write(*,'(a)') 'Double array is indirectly sorted? '//str(isSorted(d1D(indx)))
    !!
    !!call arange(indx, 1, N)
    !!call allocate(i1D,N)
    !!call rngInteger(i1D)
    !!call argSelectt(i1D, indx, k)
    !!write(*,'(a)') 'Integer array is indirectly sorted? '//str(isSorted(i1D(indx)))
    !!end program
    !!```
    module subroutine argQuickSelect_i1D(this, indx, k, res, left, right)
      !! Interfaced with argSelect()
      integer(i32), intent(in) :: this(:) !! 1D array
      integer(i32), intent(inout) :: indx(:) !! Index to choose kth smallest from
      integer(i32), intent(in) :: k !! kth smallest element
      integer(i32) :: res !! Index of the kth smallest element
      integer(i32), intent(in), optional :: left !! Select over the region left:right
      integer(i32), intent(in), optional :: right !! Select over the region left:right
    end subroutine
    module subroutine argQuickSelect_id1D(this, indx, k, res, left, right)
      !! Interfaced with argSelect()
      integer(i64), intent(in) :: this(:) !! 1D array
      integer(i32), intent(inout) :: indx(:) !! Index to choose kth smallest from
      integer(i32), intent(in) :: k  !! kth smallest element
      integer(i32) :: res !! Index of the kth smallest element
      integer(i32), intent(in), optional :: left !! Select over the region left:right
      integer(i32), intent(in), optional :: right !! Select over the region left:right
    end subroutine
    module subroutine argQuickSelect_r1D(this, indx, k, res, left, right)
      !! Interfaced with argSelect()
      real(r32), intent(in) :: this(:) !! 1D array
      integer(i32), intent(inout) :: indx(:) !! Index to choose kth smallest from
      integer(i32), intent(in) :: k !! kth smallest element
      integer(i32) :: res !! Index of the kth smallest element
      integer(i32), intent(in), optional :: left !! Select over the region left:right
      integer(i32), intent(in), optional :: right !! Select over the region left:right
    end subroutine
    module subroutine argQuickSelect_d1D(this, indx, k, res, left, right)
      !! Interfaced with argSelect()
      real(r64), intent(in) :: this(:) !! 1D array
      integer(i32), intent(inout) :: indx(:) !! Index to choose kth smallest from
      integer(i32), intent(in) :: k!! kth smallest element
      integer(i32) :: res !! Index of the kth smallest element
      integer(i32), intent(in), optional :: left !! Select over the region left:right
      integer(i32), intent(in), optional :: right !! Select over the region left:right
    end subroutine
  end interface

  contains

  !====================================================================!
  subroutine select_test(test, N)
  !====================================================================!
  class(tester) :: test
  integer(i32) :: N

  real(r32) :: ar
  real(r32), allocatable :: ar1D(:), br1D(:), cr1D(:)
  real(r64) :: a
  real(r64), allocatable :: a1D(:), b1D(:), c1D(:)
  integer(i32) :: i,ia, ib, ic
  integer(i32), allocatable :: ia1D(:), ib1D(:), ic1D(:), id1D(:)
  integer(i64) :: iad
  integer(i64), allocatable :: iad1D(:), ibd1D(:), icd1D(:)
  logical :: la, lb

  call Msg('==========================')
  call Msg('Testing : Select')
  call Msg('==========================')

  call allocate(ar1D, N)
  call allocate(br1D, N)
  call allocate(cr1D, N)

  call allocate(a1D, N)

  call allocate(ia1D, N)
  call allocate(ic1D, N)

  call rngNormal(a1D)
  ar1D = real(a1D)
  call rngInteger(ia1D, 1, N)

  br1D = ar1D
  ic = (size(br1D)+1)/2 ! Get the median
  call select(br1D, ic, ar)

  la = all(br1D(1:ic-1) <= br1D(ic)) .and. all(br1D(ic+1:N) >= br1D(ic))
  call sort(br1D)
  call test%test(ar == br1D(ic) .and. la, 'quickselect_r1D')

  br1D = ar1D
  ic = 3
  call select(br1D, ic, ar)

  la = all(br1D(1:ic-1) <= br1D(ic)) .and. all(br1D(ic+1:N) >= br1D(ic))
  call sort(br1D)
  call test%test(ar == br1D(ic) .and. la, 'quickselect_r1D')

  br1D = ar1D
  call arange(ic1D, 1, N)
  ic = (size(br1D)+1)/2 ! Get the median
  call argSelect(br1D,ic1D, ic, ia)
  do i = 1, N
    cr1D(i) = br1D(ic1D(i))
  enddo
  
  la = all(cr1D(1:ic-1) <= cr1D(ic)) .and. all(cr1D(ic+1:N) >= cr1D(ic))
  call test%test(la,'argQuickSelect_r1D')

  call deallocate(ar1D)
  call deallocate(br1D)
  call deallocate(cr1D)

  call allocate(b1D, N)
  call allocate(c1D, N)

  b1D = a1D
  ic = (size(b1D)+1)/2 ! Get the median
  call select(b1D, ic, a)

  la = all(b1D(1:ic-1) <= b1D(ic)) .and. all(b1D(ic+1:N) >= b1D(ic))
  call sort(b1D)
  call test%test(a == b1D(ic) .and. la, 'quickselect_d1D')

  b1D = a1D
  ic = (size(b1D)+1)/2 ! Get the median
  call arange(ic1D, 1, N)
  call argSelect(b1D, ic1D, ic, ia)
  do i = 1, N
    c1D(i) = b1D(ic1D(i))
  enddo
  lb = all(c1D(1:ic-1) < c1D(ic)) .and. all(c1D(ic+1:N) > c1D(ic))
  call test%test(la, 'argQuickselect_d1D')

  call deallocate(a1D)
  call deallocate(b1D)
  call deallocate(c1D)

  call allocate(ib1D, N)
  call allocate(id1D, N)

  ib1D = ia1D
  ic = (size(ib1D)+1)/2 ! Get the median
  call select(ib1D, ic, ia)

  la = all(ib1D(1:ic-1) <= ib1D(ic)) .and. all(ib1D(ic+1:N) >= ib1D(ic))
  call sort(ib1D)
  call test%test(ia == ib1D(ic) .and. la, 'quickselect_i1D')

  ib1D = ia1D
  ic = (size(ib1D) + 1)/2 ! Get the median
  call arange(ic1D, 1, N)
  call argSelect(ib1D, ic1D, ic, ia)
  do i = 1, N
    id1D(i) = ib1D(ic1D(i))
  enddo
  lb = all(id1D(1:ic-1) < id1D(ic)) .and. all(id1D(ic+1:N) > id1D(ic))
  call test%test(la, 'argQuickselect_i1D')

  call allocate(iad1D, N)
  call allocate(ibd1D, N)
  call allocate(icd1D, N)

  ibd1D = ia1D
  ic = (size(ibd1D+1))/2 ! Get the median
  call select(ibd1D, ic, iad)

  la = all(ibd1D(1:ic-1) <= ibd1D(ic)) .and. all(ibd1D(ic+1:N) >= ibd1D(ic))
  call sort(ibd1D)
  call test%test(iad == ibd1D(ic) .and. la, 'quickselect_id1D')

  ibd1D = ia1D
  ic = (size(ibd1D+1))/2 ! Get the median
  call arange(ic1D, 1, N)
  call argSelect(ibd1D, ic1D, ic, ia)
  do i = 1, N
    icd1D(i) = ibd1D(ic1D(i))
  enddo
  lb = all(icd1D(1:ic-1) < icd1D(ic)) .and. all(icd1D(ic+1:N) > icd1D(ic))
  call test%test(la, 'argQuickselect_id1D')

  end subroutine
  !====================================================================!
  end module
