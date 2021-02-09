module rArgDynamicArray_Class
  !! Class that act as stacks, queues, and priority queues like [[rDynamicArray_Class]] but with an added
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
  !!type(rArgDynamicArray) :: da, da2
  !!integer(i32) :: ia
  !!
  !!da = rArgDynamicArray(10)
  !!call da%insertAt(1, 10, 10.0)
  !!call da%insertAt(1, 20, 20.0)
  !!call da%prepend(30, 30.0)
  !!call da%append(40, 40.0)
  !!call da%remove(2)
  !!call da%tighten()
  !!da2 = da
  !!da2%v%values(2) = 50.0
  !!call da%deallocate()
  !!call da2%deallocate()
  !!
  !!da = rArgDynamicArray(3, sorted=.true.)
  !!call da%insertSorted(1, 20.0)
  !!call da%insertSorted(2, 30.0)
  !!call da%insertSorted(3, 10.0)
  !!ia = da%locationOf(20.0)
  !!ia = da%argOf(20.0)
  !!call da%insertSortedUnique(4, 10.0)
  !!call da%insertSortedUnique(4, 15.0)
  !!call da%deallocate()
  !!
  !!da = rArgDynamicArray(3, sorted=.true., fixed=.true.)
  !!call da%insertSorted(1, 20.0)
  !!call da%insertSorted(2, 30.0)
  !!call da%insertSorted(3, 10.0)
  !!ia = da%locationOf(20.0)
  !!ia = da%argOf(20.0)
  !!call da%insertSortedUnique(4, 10.0)
  !!call da%insertSortedUnique(4, 15.0)
  !!call da%deallocate()
  !!end program
  !!```

use variableKind, only: r32, i32
use m_errors, only: msg, eMsg
use iDynamicArray_Class, only: iDynamicArray
use rDynamicArray_Class, only: rDynamicArray
use m_searching, only: intervalSearch
use m_strings, only: str

implicit none

private

public :: rArgDynamicArray

type :: rArgDynamicArray
  !! Class that act as stacks, queues, and priority queues. See [[rArgDynamicArray_Class]] for more information on how to use this class.
  type(iDynamicArray) :: i
    !! Argument of the values.
  type(rDynamicArray) :: v
    !! Values.
contains
  procedure, public :: append => append_rArgDynamicArray
    !! rArgDynamicArray%append() - Append a value to the end of the dynamic array.  Will change a sorted dynamic array to unsorted.
  procedure, public :: argOf => argOf_rArgDynamicArray
    !! rArgDynamicArray%argOf() - Get the argument of a value in a sorted dynamic array
  procedure, public :: deallocate => deallocate_rArgDynamicArray
    !! rArgDynamicArray%%deallocate() - Deallocate a dynamic array.
  procedure, public :: insertAt => insertAt_rArgDynamicArray
    !! rArgDynamicArray%insertAt() - Insert a value at a given index.
  procedure, public :: insertSorted => insertSorted_rArgDynamicArray
    !! rArgDynamicArray%insertSorted() - Insert a value into a sorted dynamic array.
  procedure, public :: insertSortedUnique => insertSortedUnique_rArgDynamicArray
    !! rArgDynamicArray%insertSortedUnique() - Inserts only unique numbers into a dynamic array.
  procedure, public :: isEmpty => isEmpty_rArgDynamicArray
    !! rArgDynamicArray%isEmpty() - True if the array is empty.
  procedure, public :: isFilled => isFilled_rArgDynamicArray
    !! rArgDynamicArray%isFilled() - True if the allocated memory has been filled.
  procedure, public :: locationOf => locationOf_rArgDynamicArray
    !! rArgDynamicArray%locationOf() - Get the location of a value in a sorted dynamic array.
  procedure, public :: prepend => prepend_rArgDynamicArray
    !! rArgDynamicArray%prepend() - Prepend a value to the start of the dynamic array. Only for unsorted dynamic arrays
  procedure, public :: remove => remove_rArgDynamicArray
    !! rArgDynamicArray%remove() - Remove an element from the array.
  procedure, public :: size => size_rArgDynamicArray
    !! rArgDynamicArray%size() - Get the size of the array
  procedure, public :: tighten => tighten_rArgDynamicArray
    !! rArgDynamicArray%tighten() - Removes excess buffer memory and trims it to the current length.
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
  function argOf_rArgDynamicArray(this, val) result(i)
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
  function isEmpty_rArgDynamicArray(this) result(yes)
    !! Overloaded type bound procedure rArgDynamicArray%isEmpty()
  !====================================================================!
  class(rArgDynamicArray) :: this
  logical :: yes
    !! Array is empty
  yes = this%v%isEmpty()
  end function
  !====================================================================!

  !====================================================================!
  function isFilled_rArgDynamicArray(this) result(yes)
    !! Overloaded type bound procedure rArgDynamicArray%isFilled()
  !====================================================================!
  class(rArgDynamicArray) :: this
  logical :: yes
    !! Array is filled
  yes = this%v%isFilled()
  end function
  !====================================================================!

  !====================================================================!
  function locationOf_rArgDynamicArray(this, val) result(i)
    !! Overloaded type bound procedure rArgDynamicArray%locationOf().
  !====================================================================!
  class(rArgDynamicArray) :: this
  real(r32) :: val
    !! Value to get the argument of.
  integer(i32) :: i
    !! Index of the value.
  i = this%v%locationOf(val)
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
  function size_rArgDynamicArray(this) result(res)
    !! Overloaded type bound procedure rArgDynamicArray%size()
  !====================================================================!
  class(rArgDynamicArray) :: this
  integer(i32) :: res
    !! The size of the array
  res = this%i%N
  end function
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
