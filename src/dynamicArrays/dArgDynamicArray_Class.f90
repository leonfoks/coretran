module dArgDynamicArray_Class
  !! Class that act as stacks, queues, and priority queues like [[dDynamicArray_Class]] but with an added
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
  !!use dArgDynamicArray_Class, only: dArgDynamicArray
  !!
  !!implicit none
  !!
  !!type(dArgDynamicArray) :: da, da2
  !!integer(i32) :: ia
  !!
  !!da = dArgDynamicArray(10)
  !!call da%insertAt(1, 10, 10.d0)
  !!call da%insertAt(1, 20, 20.d0)
  !!call da%prepend(30, 30.d0)
  !!call da%append(40, 40.d0)
  !!call da%remove(2)
  !!call da%tighten()
  !!da2 = da
  !!da2%v%values(2) = 50.d0
  !!call da%deallocate()
  !!call da2%deallocate()
  !!
  !!da = dArgDynamicArray(3, sorted=.true.)
  !!call da%insertSorted(1, 20.d0)
  !!call da%insertSorted(2, 30.d0)
  !!call da%insertSorted(3, 10.d0)
  !!ia = da%locationOf(20.d0)
  !!ia = da%argOf(20.d0)
  !!call da%insertSortedUnique(4, 10.d0)
  !!call da%insertSortedUnique(4, 15.d0)
  !!call da%deallocate()
  !!
  !!da = dArgDynamicArray(3, sorted=.true., fixed=.true.)
  !!call da%insertSorted(1, 20.d0)
  !!call da%insertSorted(2, 30.d0)
  !!call da%insertSorted(3, 10.d0)
  !!ia = da%locationOf(20.d0)
  !!ia = da%argOf(20.d0)
  !!call da%insertSortedUnique(4, 10.d0)
  !!call da%insertSortedUnique(4, 15.d0)
  !!call da%deallocate()
  !!end program
  !!```

use variableKind, only: i32, r64
use iso_fortran_env, only: output_unit
use m_errors, only: msg, eMsg
use iDynamicArray_Class, only: iDynamicArray
use dDynamicArray_Class, only: dDynamicArray
use m_searching, only: intervalSearch
use m_strings, only: str, printOptions

implicit none

private

public :: dArgDynamicArray

type :: dArgDynamicArray
  !! Class that act as stacks, queues, and priority queues. See [[dArgDynamicArray_Class]] for more information on how to use this class.
  type(iDynamicArray) :: i
    !! Argument of the values.
  type(dDynamicArray) :: v
    !! Values
contains
  procedure, public :: append => append_dArgDynamicArray
    !! dArgDynamicArray%append() - Append a value to the end of the dynamic array.  Will change a sorted dynamic array to unsorted.
  procedure, public :: argOf => argOf_dArgDynamicArray
    !! dArgDynamicArray%argOf() - Get the argument of a value in a sorted dynamic array
  procedure, public :: deallocate => deallocate_dArgDynamicArray
    !! dArgDynamicArray%%deallocate() - Deallocate a dynamic array.
  procedure, public :: insertAt => insertAt_dArgDynamicArray
    !! dArgDynamicArray%insertAt() - Insert a value at a given index.
  procedure, public :: insertSorted => insertSorted_dArgDynamicArray
    !! dArgDynamicArray%insertSorted() - Insert a value into a sorted dynamic array.
  procedure, public :: insertSortedUnique => insertSortedUnique_dArgDynamicArray
    !! dArgDynamicArray%insertSortedUnique() - Inserts only unique numbers into a dynamic array.
  procedure, public :: isEmpty => isEmpty_dArgDynamicArray
    !! dArgDynamicArray%isEmpty() - True if the array is empty.
  procedure, public :: isFilled => isFilled_dArgDynamicArray
    !! dArgDynamicArray%isFilled() - True if the allocated memory has been filled.
  procedure, public :: locationOf => locationOf_dArgDynamicArray
    !! dArgDynamicArray%locationOf() - Get the location of a value in a sorted dynamic array.
  procedure, public :: prepend => prepend_dArgDynamicArray
    !! dArgDynamicArray%prepend() - Prepend a value to the start of the dynamic array. Only for unsorted dynamic arrays
  procedure, public :: print => print_dArgDynamicArray
    !! dArgDynamicArray%print() - Print to the screen
  procedure, public :: remove => remove_dArgDynamicArray
    !! dArgDynamicArray%remove() - Remove an element from the array.
  procedure, public :: size => size_dArgDynamicArray
    !! dArgDynamicArray%size() - Get the size of the array
  procedure, public :: tighten => tighten_dArgDynamicArray
    !! dArgDynamicArray%tighten() - Removes excess buffer memory and trims it to the current length.
end type

interface dArgDynamicArray
  procedure :: init_dArgDynamicArray_i1, init_dArgDynamicArray_d1D
end interface

interface assignment(=)
  procedure :: copy_dArgDynamicArray
end interface

contains

  !====================================================================!
  subroutine append_dArgDynamicArray(this, i, val)
    !! Overloaded type bound procedure dArgDynamicArray%append().
  !====================================================================!
  class(dArgDynamicArray) :: this
  integer(i32) :: i
    !! Index of value
  real(r64) :: val
    !! Value to append
  call this%i%append(i)
  call this%v%append(val)
  end subroutine
  !====================================================================!

  !====================================================================!
  function argOf_dArgDynamicArray(this, val) result(i)
    !! Overloaded type bound procedure dArgDynamicArray%locationOf().
  !====================================================================!
  class(dArgDynamicArray) :: this
  real(r64) :: val
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
  subroutine copy_dArgDynamicArray(new,this)
    !! Overloaded assignment of equals.  new = this
  !====================================================================!
  class(dArgDynamicArray), intent(in) :: this
    !! Class to copy.
  type(dArgDynamicArray), intent(out) :: new
    !! Copy of this.
  new%i = this%i
  new%v = this%v
  end subroutine
  !====================================================================!

  !====================================================================!
  subroutine deallocate_dArgDynamicArray(this)
    !! Overloaded type bound procedure dArgDynamicArray%deallocate().
  !====================================================================!
  class(dArgDynamicArray) :: this
  call this%i%deallocate()
  call this%v%deallocate()
  end subroutine
  !====================================================================!

  !====================================================================!
  function init_dArgDynamicArray_i1(M, sorted, fixed) result(this)
    !! Overloaded by interface [[dArgDynamicArray(type)]]
  !====================================================================!
    integer(i32), intent(in), optional :: M
      !! Amount of memory to allocate.
    logical, intent(in), optional :: sorted
      !! Maintain a sorted array.
    logical, intent(in), optional :: fixed
      !! Maintain a fixed size array.
    type(dArgDynamicArray) :: this

    this%i = iDynamicArray(M, .false., fixed)
    this%v = dDynamicArray(M, sorted,  fixed)
  end function
  !====================================================================!

  !====================================================================!
  function init_dArgDynamicArray_d1D(i, values, M, sorted, fixed) result(this)
    !! Overloaded by interface [[dArgDynamicArray(type)]]
  !====================================================================!
    integer(i32), intent(in) :: i(:)
      !! Set of value indices to initialize with.
    real(r64), intent(in) :: values(:)
      !! Set of values to initialize with.
    integer(i32), intent(in), optional :: M
      !! Amount of memory to allocate.
    logical, intent(in), optional :: sorted
      !! Maintain a sorted array.
    logical, intent(in), optional :: fixed
      !! Maintain a fixed size array.
    type(dArgDynamicArray) :: this

    integer(i32) :: ii, iv

    ii = size(i)
    iv = size(values)

    if (ii /= iv) call eMsg("dArgDynamicArray: Size of i "//str(ii)//"must equal size of values "//str(iv))

    this%i = iDynamicArray(i, M, .false., fixed)
    this%v = dDynamicArray(values, M, sorted, fixed)
  end function
  !====================================================================!

  !====================================================================!
  subroutine insertAt_dArgDynamicArray(this, loc, i, val)
    !! Overloaded type bound procedure dArgDynamicArray%insertAt().
  !====================================================================!
  class(dArgDynamicArray) :: this
  integer(i32) :: loc
    !! Insert index and value at this location.
  integer(i32) :: i
    !! index/
  real(r64) :: val
    !! Value/
  call this%i%insertAt(loc, i)
  call this%v%insertAt(loc, val)
  end subroutine
  !====================================================================!

  !====================================================================!
  subroutine insertSorted_dArgDynamicArray(this, i, val)
    !! Overloaded type bound procedure dArgDynamicArray%insertSorted().
  !====================================================================!
  class(dArgDynamicArray) :: this
  integer(i32) :: i
    !! Index.
  real(r64) :: val
    !! Value.

  integer(i32) :: iSearch(3)

  iSearch = intervalSearch(this%v%values, val, 1, this%v%N)
  call this%i%insertAt(iSearch(3), i)
  call this%v%insertAt(iSearch(3), val)
  end subroutine
  !====================================================================!

  !====================================================================!
  subroutine insertSortedUnique_dArgDynamicArray(this, i, val)
    !! Overloaded type bound procedure dArgDynamicArray%insertSortedUnique().
  !====================================================================!
  class(dArgDynamicArray) :: this
  integer(i32) :: i
    !! Index.
  real(r64) :: val
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
  function isEmpty_dArgDynamicArray(this) result(yes)
    !! Overloaded type bound procedure dArgDynamicArray%isEmpty()
  !====================================================================!
  class(dArgDynamicArray) :: this
  logical :: yes
    !! Array is empty
  yes = this%v%isEmpty()
  end function
  !====================================================================!

  !====================================================================!
  function isFilled_dArgDynamicArray(this) result(yes)
    !! Overloaded type bound procedure dArgDynamicArray%isFilled()
  !====================================================================!
  class(dArgDynamicArray) :: this
  logical :: yes
    !! Array is filled
  yes = this%v%isFilled()
  end function
  !====================================================================!

  !====================================================================!
  function locationOf_dArgDynamicArray(this, val) result(i)
    !! Overloaded type bound procedure dArgDynamicArray%locationOf().
  !====================================================================!
  class(dArgDynamicArray) :: this
  real(r64) :: val
    !! Value to get the argument of.
  integer(i32) :: i
    !! Index of the value.
  i = this%v%locationOf(val)
  end function
  !====================================================================!

  !====================================================================!
  subroutine prepend_dArgDynamicArray(this, i, val)
    !! Overloaded type bound procedure dArgDynamicArray%prepend().
  !====================================================================!
  class(dArgDynamicArray) :: this
  integer(i32) :: i
    !! Prepend indices with this index.
  real(r64) :: val
    !! Prepend values with this value.
  call this%i%prepend(i)
  call this%v%prepend(val)

  end subroutine
  !====================================================================!
  
  !====================================================================!
  subroutine print_dArgDynamicArray(this)
    !! Overloaded type bound procedure dArgDynamicArray%print()
  !====================================================================!
  class(dArgDynamicArray) :: this
  printOptions%threshold=0
  write(output_unit, '(a)') 'size:   '//str(this%v%N)
  if (this%v%N <= 0) return
  write(output_unit, '(a)') 'indx:   '//str(this%i%values(1:this%i%N))
  write(output_unit, '(a)') 'values: '//str(this%v%values(1:this%v%N))
  end subroutine
  !====================================================================!

  !====================================================================!
  subroutine remove_dArgDynamicArray(this, i)
    !! Overloaded type bound procedure dArgDynamicArray%remove().
  !====================================================================!
  class(dArgDynamicArray) :: this
  integer(i32) :: i
    !! Remove the elements at this location.
  call this%i%remove(i)
  call this%v%remove(i)
  end subroutine
  !====================================================================!

  !====================================================================!
  function size_dArgDynamicArray(this) result(res)
    !! Overloaded type bound procedure dArgDynamicArray%size()
  !====================================================================!
  class(dArgDynamicArray) :: this
  integer(i32) :: res
    !! The size of the array
  res = this%i%N
  end function
  !====================================================================!

  !====================================================================!
  subroutine tighten_dArgDynamicArray(this)
    !! Overloaded type bound procedure dArgDynamicArray%tighten().
  !====================================================================!
  class(dArgDynamicArray) :: this

  call this%i%tighten()
  call this%v%tighten()
  end subroutine
  !====================================================================!
end module
