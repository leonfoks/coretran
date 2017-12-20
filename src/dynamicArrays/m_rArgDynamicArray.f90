module m_rArgDynamicArray

use variableKind, only: r32, i32
use m_errors, only: msg, eMsg
use m_iDynamicArray, only: iDynamicArray
use m_rDynamicArray, only: rDynamicArray
use m_searching, only: intervalSearch
use m_strings, only: str
use m_unitTester, only: tester

implicit none

private

!public :: argDynamicArray_test


public :: rArgDynamicArray

type :: rArgDynamicArray
  !! Class that act as stacks, queues, and priority queues. See [[m_rArgDynamicArray]] for more information on how to use this class.
  type(iDynamicArray) :: i
    !! Argument of the values.
  type(rDynamicArray) :: v
    !! Values.
contains
  procedure, public :: append => append_rArgDynamicArray
    !! dDynamicArray%append() - Append a value to the end of the dynamic array.  Will change a sorted dynamic array to unsorted.
  procedure, public :: deallocate => deallocate_rArgDynamicArray
    !! dDynamicArray%%deallocate() - Deallocate a dynamic array.
  procedure, public :: insertAt => insertAt_rArgDynamicArray
    !! dDynamicArray%insertAt() - Insert a value at a given index.
  procedure, public :: insertSorted => insertSorted_rArgDynamicArray
    !! dDynamicArray%insertSorted() - Insert a value into a sorted dynamic array.
  procedure, public :: insertSortedUnique => insertSortedUnique_rArgDynamicArray
    !! dDynamicArray%insertSortedUnique() - Inserts only unique numbers into a dynamic array.
  procedure, public :: locationOf => locationOf_rArgDynamicArray
    !! dDynamicArray%locationOf() - Get the argument of a value in a sorted dynamic array.
  procedure, public :: prepend => prepend_rArgDynamicArray
    !! dDynamicArray%prepend() - Prepend a value to the start of the dynamic array. Only for unsorted dynamic arrays
  procedure, public :: remove => remove_rArgDynamicArray
    !! dDynamicArray%remove() - Remove an element from the array.
  procedure, public :: tighten => tighten_rArgDynamicArray
    !! dDynamicArray%tighten() - Removes excess buffer memory and trims it to the current length.
end type

interface rArgDynamicArray
  procedure :: init_rArgDynamicArray_i1, init_rArgDynamicArray_r1D
end interface

interface assignment(=)
  procedure :: copy_rArgDynamicArray
end interface

contains

  !====================================================================!
  subroutine append_rArgDynamicArray(this, i, val)
    !! Overloaded type bound procedure rArgDynamicArray%append().
  !====================================================================!
  class(rArgDynamicArray) :: this
  integer(i32) :: i
    !! Index of value
  real(r32) :: val
    !! Value to append
  call this%i%append(i)
  call this%v%append(val)
  end subroutine
  !====================================================================!

  !====================================================================!
  subroutine copy_rArgDynamicArray(new,this)
    !! Overloaded assignment of equals.  new = this
  !====================================================================!
  class(rArgDynamicArray), intent(in) :: this
    !! Class to copy.
  type(rArgDynamicArray), intent(out) :: new
    !! Copy of this.
  new%i = this%i
  new%v = this%v
  end subroutine
  !====================================================================!

  !====================================================================!
  subroutine deallocate_rArgDynamicArray(this)
    !! Overloaded type bound procedure rArgDynamicArray%deallocate().
  !====================================================================!
  class(rArgDynamicArray) :: this
  call this%i%deallocate()
  call this%v%deallocate()
  end subroutine
  !====================================================================!

  !====================================================================!
  function init_rArgDynamicArray_i1(M, sorted, fixed) result(this)
    !! Overloaded by interface [[rArgDynamicArray(type)]]
  !====================================================================!
    integer(i32), intent(in), optional :: M
    logical, intent(in), optional :: sorted
    logical, intent(in), optional :: fixed
    type(rArgDynamicArray) :: this
    this%i = iDynamicArray(M, .false., fixed)
    this%v = rDynamicArray(M, sorted,  fixed)
  end function
  !====================================================================!

  !====================================================================!
  function init_rArgDynamicArray_r1D(i, values, M, sorted, fixed) result(this)
    !! Overloaded by interface [[rArgDynamicArray(type)]]
  !====================================================================!
    integer(i32), intent(in) :: i(:)
    real(r32), intent(in) :: values(:)
    integer(i32), intent(in), optional :: M
    logical, intent(in), optional :: sorted
    logical, intent(in), optional :: fixed
    type(rArgDynamicArray) :: this

    integer(i32) :: ii, iv

    ii = size(i)
    iv = size(values)

    if (ii /= iv) call eMsg("rArgDynamicArray: Size of i "//str(ii)//"must equal size of values "//str(iv))

    this%i = iDynamicArray(i, M, .false., fixed)
    this%v = rDynamicArray(values, M, sorted, fixed)
  end function
  !====================================================================!

  !====================================================================!
  subroutine insertAt_rArgDynamicArray(this, loc, i, val)
    !! Overloaded type bound procedure rArgDynamicArray%insertAt().
  !====================================================================!
  class(rArgDynamicArray) :: this
  integer(i32) :: loc
    !! Insert index and value at this location.
  integer(i32) :: i
    !! index/
  real(r32) :: val
    !! Value/
  call this%i%insertAt(loc, i)
  call this%v%insertAt(loc, val)
  end subroutine
  !====================================================================!

  !====================================================================!
  subroutine insertSorted_rArgDynamicArray(this, i, val)
    !! Overloaded type bound procedure rArgDynamicArray%insertSorted().
  !====================================================================!
  class(rArgDynamicArray) :: this
  integer(i32) :: i
    !! Index.
  real(r32) :: val
    !! Value.

  integer(i32) :: iSearch(3)

  iSearch = intervalSearch(this%v%values, val, 1, this%v%N)
  call this%i%insertAt(iSearch(3), i)
  call this%v%insertAt(iSearch(3), val)
  end subroutine
  !====================================================================!

  !====================================================================!
  subroutine insertSortedUnique_rArgDynamicArray(this, i, val)
    !! Overloaded type bound procedure rArgDynamicArray%insertSortedUnique().
  !====================================================================!
  class(rArgDynamicArray) :: this
  integer(i32) :: i
    !! Index.
  real(r32) :: val
    !! Value.

  integer(i32) :: iSearch(3)

  iSearch = intervalSearch(this%v%values, val, 1, this%v%N)
  if (iSearch(1) == -1) then
    call this%i%insertAt(iSearch(3), i)
    call this%v%insertAt(iSearch(3), val)
  endif

  end subroutine
  !====================================================================!

  !====================================================================!
  function locationOf_rArgDynamicArray(this, val) result(i)
    !! Overloaded type bound procedure rArgDynamicArray%locationOf().
  !====================================================================!
  class(rArgDynamicArray) :: this
  real(r32) :: val
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
  subroutine prepend_rArgDynamicArray(this, i, val)
    !! Overloaded type bound procedure rArgDynamicArray%prepend().
  !====================================================================!
  class(rArgDynamicArray) :: this
  integer(i32) :: i
    !! Prepend indices with this index.
  real(r32) :: val
    !! Prepend values with this value.

  call this%i%prepend(i)
  call this%v%prepend(val)

  end subroutine
  !====================================================================!

  !====================================================================!
  subroutine remove_rArgDynamicArray(this, i)
    !! Overloaded type bound procedure rArgDynamicArray%remove().
  !====================================================================!
  class(rArgDynamicArray) :: this
  integer(i32) :: i
    !! Remove the elements at this location.
  call this%i%remove(i)
  call this%v%remove(i)
  end subroutine
  !====================================================================!

  !====================================================================!
  subroutine tighten_rArgDynamicArray(this)
    !! Overloaded type bound procedure rArgDynamicArray%tighten().
  !====================================================================!
  class(rArgDynamicArray) :: this

  call this%i%tighten()
  call this%v%tighten()
  end subroutine
  !====================================================================!
end module
