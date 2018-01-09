module m_iArgDynamicArray
  !! Class that act as stacks, queues, and priority queues like [[m_iDynamicArray]] but with an added
  !! integer index so that 'lists' of both a key and value can be maintained.
  !! These classes use dynamically allocated contiguous blocks of memory to store a list of numbers.
  !! The queues can be sorted to become priority queues and use binary searches to quickly insert new numbers.
  !! If the allocated memory is filled, the available space is doubled.
  !! Memory is only reallocated to a smaller size, if the utilization is a quarter of that allocated.
  !! The array can be specified as fixed, so that no reallocation occurs.  This is useful for heaps of given 
  !! like k nearest neighbours, or k smallest.
  !!
  !! Example usage
  !!```fortran
  !!program dynamicArray_test
  !!use variableKind, only: i32
  !!use m_iArgDynamicArray, only: iArgDynamicArray
  !!
  !!implicit none
  !!
  !!type(iArgDynamicArray) :: da, da2
  !!integer(i32) :: ia
  !!
  !!da = iArgDynamicArray(10)
  !!call da%insertAt(1, 10, 10)
  !!call da%insertAt(1, 20, 20)
  !!call da%prepend(30, 30)
  !!call da%append(40, 40)
  !!call da%remove(2)
  !!call da%tighten()
  !!da2 = da
  !!da2%v%values(2) = 50
  !!call da%deallocate()
  !!call da2%deallocate()
  !!
  !!da = iArgDynamicArray(3, sorted=.true.)
  !!call da%insertSorted(1, 20)
  !!call da%insertSorted(2, 30)
  !!call da%insertSorted(3, 10)
  !!ia = da%locationOf(20)
  !!ia = da%argOf(20)
  !!call da%insertSortedUnique(4, 10)
  !!call da%insertSortedUnique(4, 15)
  !!call da%deallocate()
  !!
  !!da = iArgDynamicArray(3, sorted=.true., fixed=.true.)
  !!call da%insertSorted(1, 20)
  !!call da%insertSorted(2, 30)
  !!call da%insertSorted(3, 10)
  !!ia = da%locationOf(20)
  !!ia = da%argOf(20)
  !!call da%insertSortedUnique(4, 10)
  !!call da%insertSortedUnique(4, 15)
  !!call da%deallocate()
  !!end program
  !!```

use variableKind, only: i32
use m_errors, only: msg, eMsg
use m_iDynamicArray, only: iDynamicArray, insertAt__iDynamicArray
use m_searching, only: intervalSearch
use m_strings, only: str
use m_unitTester, only: tester

implicit none

private

public :: iArgDynamicArray_test

public :: iArgDynamicArray

type :: iArgDynamicArray
  !! Class that act as stacks, queues, and priority queues. See [[m_iArgDynamicArray]] for more information on how to use this class.
  type(iDynamicArray) :: i
    !! Argument of the values.
  type(iDynamicArray) :: v
    !! Values.
contains
  procedure, public :: append => append_iArgDynamicArray
    !! iArgDynamicArray%append() - Append a value to the end of the dynamic array.  Will change a sorted dynamic array to unsorted.
  procedure, public :: argOf => argOf_iArgDynamicArray
    !! iArgDynamicArray%argOf() - Get the argument of a value in a sorted dynamic array
  procedure, public :: deallocate => deallocate_iArgDynamicArray
    !! iArgDynamicArray%%deallocate() - Deallocate a dynamic array.
  procedure, public :: insertAt => insertAt_iArgDynamicArray
    !! iArgDynamicArray%insertAt() - Insert a value at a given index.
  procedure, public :: insertSorted => insertSorted_iArgDynamicArray
    !! iArgDynamicArray%insertSorted() - Insert a value into a sorted dynamic array.
  procedure, public :: insertSortedUnique => insertSortedUnique_iArgDynamicArray
    !! iArgDynamicArray%insertSortedUnique() - Inserts only unique numbers into a dynamic array.
  procedure, public :: isEmpty => isEmpty_iArgDynamicArray
    !! iArgDynamicArray%isEmpty() - True if the array is empty.
  procedure, public :: isFilled => isFilled_iArgDynamicArray
    !! iArgDynamicArray%isFilled() - True if the allocated memory has been filled.
  procedure, public :: locationOf => locationOf_iArgDynamicArray
    !! iArgDynamicArray%locationOf() - Get the location of a value in a sorted dynamic array.
  procedure, public :: prepend => prepend_iArgDynamicArray
    !! iArgDynamicArray%prepend() - Prepend a value to the start of the dynamic array. Only for unsorted dynamic arrays
  procedure, public :: remove => remove_iArgDynamicArray
    !! iArgDynamicArray%remove() - Remove an element from the array.
  procedure, public :: tighten => tighten_iArgDynamicArray
    !! iArgDynamicArray%tighten() - Removes excess buffer memory and trims it to the current length.
end type

interface iArgDynamicArray
  procedure :: init_iArgDynamicArray_i1, init_iArgDynamicArray_i1D
end interface

interface assignment(=)
  procedure :: copy_iArgDynamicArray
end interface

contains

  !====================================================================!
  subroutine append_iArgDynamicArray(this, i, val)
    !! Overloaded type bound procedure iArgDynamicArray%append().
  !====================================================================!
  class(iArgDynamicArray) :: this
  integer(i32) :: i
    !! Index of value
  integer(i32) :: val
    !! Value to append
  call this%i%append(i)
  call this%v%append(val)
  end subroutine
  !====================================================================!

  !====================================================================!
  function argOf_iArgDynamicArray(this, val) result(i)
    !! Overloaded type bound procedure iArgDynamicArray%locationOf().
  !====================================================================!
  class(iArgDynamicArray) :: this
  integer(i32) :: val
    !! Value to get the argument of.
  integer(i32) :: i
    !! Argument of the value.

  integer(i32) :: iSearch

  iSearch = this%v%locationOf(val)
  i = -1
  if (iSearch /= -1) i = this%i%values(iSearch)

  end function
  !====================================================================!

  !====================================================================!
  subroutine copy_iArgDynamicArray(new,this)
    !! Overloaded assignment of equals.  new = this
  !====================================================================!
  class(iArgDynamicArray), intent(in) :: this
    !! Class to copy.
  type(iArgDynamicArray), intent(out) :: new
    !! Copy of this.
  new%i = this%i
  new%v = this%v
  end subroutine
  !====================================================================!

  !====================================================================!
  subroutine deallocate_iArgDynamicArray(this)
    !! Overloaded type bound procedure iArgDynamicArray%deallocate().
  !====================================================================!
  class(iArgDynamicArray) :: this
  call this%i%deallocate()
  call this%v%deallocate()
  end subroutine
  !====================================================================!

  !====================================================================!
  function init_iArgDynamicArray_i1(M, sorted, fixed) result(this)
    !! Overloaded by interface [[iArgDynamicArray(type)]]
  !====================================================================!
    integer(i32), intent(in), optional :: M
    logical, intent(in), optional :: sorted
    logical, intent(in), optional :: fixed
    type(iArgDynamicArray) :: this
    this%i = iDynamicArray(M, .false., fixed)
    this%v = iDynamicArray(M, sorted,  fixed)
  end function
  !====================================================================!

  !====================================================================!
  function init_iArgDynamicArray_i1D(i, values, M, sorted, fixed) result(this)
    !! Overloaded by interface [[iArgDynamicArray(type)]]
  !====================================================================!
    integer(i32), intent(in) :: i(:)
    integer(i32), intent(in) :: values(:)
    integer(i32), intent(in), optional :: M
    logical, intent(in), optional :: sorted
    logical, intent(in), optional :: fixed
    type(iArgDynamicArray) :: this

    integer(i32) :: ii, iv

    ii = size(i)
    iv = size(values)

    if (ii /= iv) call eMsg("iArgDynamicArray: Size of i "//str(ii)//"must equal size of values "//str(iv))

    this%i = iDynamicArray(i, M, .false., fixed)
    this%v = iDynamicArray(values, M, sorted, fixed)
  end function
  !====================================================================!

  !====================================================================!
  subroutine insertAt_iArgDynamicArray(this, loc, i, val)
    !! Overloaded type bound procedure iArgDynamicArray%insertAt().
  !====================================================================!
  class(iArgDynamicArray) :: this
  integer(i32) :: loc
    !! Insert index and value at this location.
  integer(i32) :: i
    !! index.
  integer(i32) :: val
    !! Value.
  call this%i%insertAt(loc, i)
  call this%v%insertAt(loc, val)
  end subroutine
  !====================================================================!

  !====================================================================!
  subroutine insertSorted_iArgDynamicArray(this, i, val)
    !! Overloaded type bound procedure iArgDynamicArray%insertSorted().
  !====================================================================!
  class(iArgDynamicArray) :: this
  integer(i32) :: i
    !! Index.
  integer(i32) :: val
    !! Value.

  integer(i32) :: iSearch(3)

  iSearch = intervalSearch(this%v%values, val, 1, this%v%N)
  call this%i%insertAt(iSearch(3), i)
  call insertAt__iDynamicArray(this%v, iSearch(3), val)
  end subroutine
  !====================================================================!

  !====================================================================!
  subroutine insertSortedUnique_iArgDynamicArray(this, i, val)
    !! Overloaded type bound procedure iArgDynamicArray%insertSortedUnique().
  !====================================================================!
  class(iArgDynamicArray) :: this
  integer(i32) :: i
    !! Index.
  integer(i32) :: val
    !! Value.

  integer(i32) :: iSearch(3)

  iSearch = intervalSearch(this%v%values, val, 1, this%v%N)
  if (iSearch(1) == -1) then
    call this%i%insertAt(iSearch(3), i)
    call insertAt__iDynamicArray(this%v, iSearch(3), val)
  endif

  end subroutine
  !====================================================================!

  !====================================================================!
  function isEmpty_iArgDynamicArray(this) result(yes)
    !! Overloaded type bound procedure iArgDynamicArray%isEmpty()
  !====================================================================!
  class(iArgDynamicArray) :: this
  logical :: yes
    !! Array is empty
  yes = this%v%isEmpty()
  end function
  !====================================================================!

  !====================================================================!
  function isFilled_iArgDynamicArray(this) result(yes)
    !! Overloaded type bound procedure iArgDynamicArray%isFilled()
  !====================================================================!
  class(iArgDynamicArray) :: this
  logical :: yes
    !! Array is filled
  yes = this%v%isFilled()
  end function
  !====================================================================!  

  !====================================================================!
  function locationOf_iArgDynamicArray(this, val) result(i)
    !! Overloaded type bound procedure iArgDynamicArray%locationOf().
  !====================================================================!
  class(iArgDynamicArray) :: this
  integer(i32) :: val
    !! Value to get the argument of.
  integer(i32) :: i
    !! Index of the value.
  i = this%v%locationOf(val)
  end function
  !====================================================================!

  !====================================================================!
  subroutine prepend_iArgDynamicArray(this, i, val)
    !! Overloaded type bound procedure iArgDynamicArray%prepend().
  !====================================================================!
  class(iArgDynamicArray) :: this
  integer(i32) :: i
    !! Prepend indices with this index.
  integer(i32) :: val
    !! Prepend values with this value.

  call this%i%prepend(i)
  call this%v%prepend(val)

  end subroutine
  !====================================================================!

  !====================================================================!
  subroutine remove_iArgDynamicArray(this, i)
    !! Overloaded type bound procedure iArgDynamicArray%remove().
  !====================================================================!
  class(iArgDynamicArray) :: this
  integer(i32) :: i
    !! Remove the elements at this location.
  call this%i%remove(i)
  call this%v%remove(i)
  end subroutine
  !====================================================================!

  !====================================================================!
  subroutine tighten_iArgDynamicArray(this)
    !! Overloaded type bound procedure iArgDynamicArray%tighten().
  !====================================================================!
  class(iArgDynamicArray) :: this

  call this%i%tighten()
  call this%v%tighten()
  end subroutine
  !====================================================================!

  !====================================================================!
  subroutine iArgDynamicArray_test(test)
    !! graph: false
  !====================================================================!
  class(tester) :: test

  type(iArgDynamicArray) :: ida, ida2

  integer(i32) :: ia

  call Msg('==========================')
  call Msg('Testing : iArgDynamic Arrays')
  call Msg('==========================')

  ida = iArgDynamicArray(10)
  call test%test(size(ida%v%values)==10 .and. size(ida%i%values)==10, 'iArgDynamicArray')
  call test%test(ida%v%N == 0 .and. ida%i%N == 0, 'iArgDynamicArray')
  call ida%insertAt(1, 10, 10)
  call test%test(ida%i%values(1) == 10 .and. ida%v%values(1) == 10, 'iArgDynamicArray%insert')
  call ida%insertAt(1, 20, 20)
  call test%test(all(ida%i%values(1:2) == [20, 10]) .and. all(ida%v%values(1:2) == [20, 10]), 'iArgDynamicArray%insert')
  call ida%prepend(30, 30)
  call test%test(all(ida%i%values(1:3) == [30, 20, 10]) .and. all(ida%v%values(1:3) == [30, 20, 10]), 'iArgDynamicArray%prepend')
  call ida%append(40, 40)
  call test%test(all(ida%i%values(1:4) == [30, 20, 10, 40]) .and. all(ida%v%values(1:4) == [30, 20, 10, 40]), 'iArgDynamicArray%append')
  call ida%remove(2)
  call test%test(all(ida%i%values(1:3) == [30, 10, 40]) .and. all(ida%v%values(1:3) == [30, 10, 40]), 'iArgDynamicArray%remove')
  call ida%tighten()
  call test%test(size(ida%i%values) == 3 .and. size(ida%v%values) == 3, 'iArgDynamicArray%tighten')
  ida2 = ida
  call test%test(all(ida2%i%values == ida%i%values) .and. all(ida2%v%values == ida%v%values), 'iArgDynamicArray%copy')
  ida2%v%values(2) = 50
  call test%test(ida2%v%values(2) /= ida%v%values(2), 'iArgDynamicArray%copy')
  call ida%deallocate()
  call test%test(.not. allocated(ida%i%values) .and. .not. allocated(ida%v%values), 'iArgDynamicArray%deallocate')
  call ida2%deallocate()

  ida = iArgDynamicArray(3, sorted=.true.)
  call ida%insertSorted(1, 20)
  call ida%insertSorted(2, 30)
  call ida%insertSorted(3, 10)
  call test%test(all(ida%i%values(1:3)==[3, 1, 2]) .and. all(ida%v%values(1:3)==[10, 20, 30]), 'iArgDynamicArray%insertSorted')
  ia = ida%locationOf(20)
  call test%test(ia == 2, 'iArgDynamicArray%locationOf')
  ia = ida%argOf(20)
  call test%test(ia == 1, 'iArgDynamicArray%argOf')
  call ida%insertSortedUnique(4, 10)
  call test%test(all(ida%i%values(1:3)==[3, 1, 2]) .and. all(ida%v%values(1:3)==[10, 20, 30]), 'iArgDynamicArray%insertSortedUnique')
  call ida%insertSortedUnique(4, 15)
  call test%test(all(ida%i%values(1:4)==[3, 4, 1, 2]) .and. all(ida%v%values(1:4)==[10, 15, 20, 30]), 'iArgDynamicArray%insertSortedUnique')
  call test%test(size(ida%i%values) == 6 .and. size(ida%v%values) == 6, 'iArgDynamicArray%insert')
  call ida%deallocate()

  ida = iArgDynamicArray(3, sorted=.true., fixed=.true.)
  call ida%insertSorted(1, 20)
  call ida%insertSorted(2, 30)
  call ida%insertSorted(3, 10)
  call test%test(all(ida%i%values(1:3)==[3, 1, 2]) .and. all(ida%v%values(1:3)==[10, 20, 30]), 'iArgDynamicArray%insertSorted')
  ia = ida%locationOf(20)
  call test%test(ia == 2, 'iArgDynamicArray%locationOf')
  ia = ida%argOf(20)
  call test%test(ia == 1, 'iArgDynamicArray%argOf')
  call ida%insertSortedUnique(4, 10)
  call test%test(all(ida%i%values(1:3)==[3, 1, 2]) .and. all(ida%v%values(1:3)==[10, 20, 30]), 'iArgDynamicArray%insertSortedUnique')
  call ida%insertSortedUnique(4, 15)
  call test%test(all(ida%i%values(1:3)==[3, 4, 1]) .and. all(ida%v%values(1:3)==[10, 15, 20]), 'iArgDynamicArray%insertSortedUnique')
  call test%test(size(ida%i%values) == 3 .and. size(ida%v%values) == 3, 'iArgDynamicArray%insert')
  call ida%deallocate()
end subroutine
end module
