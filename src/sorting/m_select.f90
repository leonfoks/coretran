  module m_select
    !! Perform a quickselect on an array. Quick select finds the kth smallest number in an array. It also puts values lower than the kth on the left, and those higher on the right
    !! This makes it perfect for finding the median.
  use variableKind

  implicit none

  private

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

  end module
