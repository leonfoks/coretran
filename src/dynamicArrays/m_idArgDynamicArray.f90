module m_idArgDynamicArray
  !! Class that act as stacks, queues, and priority queues like [[m_idDynamicArray]] but with an added
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
  !!type(idArgDynamicArray) :: da, da2
  !!integer(i32) :: ia
  !!
  !!da = idArgDynamicArray(10)
  !!call da%insertAt(1, 10, 10_i64)
  !!call da%insertAt(1, 20, 20_i64)
  !!call da%prepend(30, 30_i64)
  !!call da%append(40, 40_i64)
  !!call da%remove(2)
  !!call da%tighten()
  !!da2 = da
  !!da2%v%values(2) = 50_i64
  !!call da%deallocate()
  !!call da2%deallocate()
  !!
  !!da = idArgDynamicArray(3, sorted=.true.)
  !!call da%insertSorted(1, 20_i64)
  !!call da%insertSorted(2, 30_i64)
  !!call da%insertSorted(3, 10_i64)
  !!ia = da%locationOf(20_i64)
  !!ia = da%argOf(20_i64)
  !!call da%insertSortedUnique(4, 10_i64)
  !!call da%insertSortedUnique(4, 15_i64)
  !!call da%deallocate()
  !!
  !!da = idArgDynamicArray(3, sorted=.true., fixed=.true.)
  !!call da%insertSorted(1, 20_i64)
  !!call da%insertSorted(2, 30_i64)
  !!call da%insertSorted(3, 10_i64)
  !!ia = da%locationOf(20_i64)
  !!ia = da%argOf(20_i64)
  !!call da%insertSortedUnique(4, 10_i64)
  !!call da%insertSortedUnique(4, 15_i64)
  !!call da%deallocate()
  !!end program
  !!```

use variableKind, only: i32, i64
use m_errors, only: msg, eMsg
use m_iDynamicArray, only: iDynamicArray
use m_idDynamicArray, only: idDynamicArray, insertAt__idDynamicArray
use m_searching, only: intervalSearch
use m_strings, only: str
use m_unitTester, only: tester

implicit none

private

public :: idArgDynamicArray_test

public :: idArgDynamicArray

type :: idArgDynamicArray
  !! Class that act as stacks, queues, and priority queues. See [[m_idArgDynamicArray]] for more information on how to use this class.
  type(iDynamicArray) :: i
    !! Argument of the values.
  type(idDynamicArray) :: v
    !! Values.
contains
  procedure, public :: append => append_idArgDynamicArray
    !! idArgDynamicArray%append() - Append a value to the end of the dynamic array.  Will change a sorted dynamic array to unsorted.
  procedure, public :: argOf => argOf_idArgDynamicArray
    !! idArgDynamicArray%argOf() - Get the argument of a value in a sorted dynamic array
  procedure, public :: deallocate => deallocate_idArgDynamicArray
    !! idArgDynamicArray%%deallocate() - Deallocate a dynamic array.
  procedure, public :: insertAt => insertAt_idArgDynamicArray
    !! idArgDynamicArray%insertAt() - Insert a value at a given index.
  procedure, public :: insertSorted => insertSorted_idArgDynamicArray
    !! idArgDynamicArray%insertSorted() - Insert a value into a sorted dynamic array.
  procedure, public :: insertSortedUnique => insertSortedUnique_idArgDynamicArray
    !! idArgDynamicArray%insertSortedUnique() - Inserts only unique numbers into a dynamic array.
  procedure, public :: isEmpty => isEmpty_idArgDynamicArray
    !! idArgDynamicArray%isEmpty() - True if the array is empty.
  procedure, public :: isFilled => isFilled_idArgDynamicArray
    !! idArgDynamicArray%isFilled() - True if the allocated memory has been filled.
  procedure, public :: locationOf => locationOf_idArgDynamicArray
    !! idArgDynamicArray%locationOf() - Get the location of a value in a sorted dynamic array.
  procedure, public :: prepend => prepend_idArgDynamicArray
    !! idArgDynamicArray%prepend() - Prepend a value to the start of the dynamic array. Only for unsorted dynamic arrays
  procedure, public :: remove => remove_idArgDynamicArray
    !! idArgDynamicArray%remove() - Remove an element from the array.
  procedure, public :: tighten => tighten_idArgDynamicArray
    !! idArgDynamicArray%tighten() - Removes excess buffer memory and trims it to the current length.
end type

interface idArgDynamicArray
  procedure :: init_idArgDynamicArray_i1, init_idArgDynamicArray_id1D
end interface

interface assignment(=)
  procedure :: copy_idArgDynamicArray
end interface

contains

  !====================================================================!
  subroutine append_idArgDynamicArray(this, i, val)
    !! Overloaded type bound procedure idArgDynamicArray%append().
  !====================================================================!
  class(idArgDynamicArray) :: this
  integer(i32) :: i
    !! Index of value
  integer(i64) :: val
    !! Value to append
  call this%i%append(i)
  call this%v%append(val)
  end subroutine
  !====================================================================!

  !====================================================================!
  function argOf_idArgDynamicArray(this, val) result(i)
    !! Overloaded type bound procedure idArgDynamicArray%locationOf().
  !====================================================================!
  class(idArgDynamicArray) :: this
  integer(i64) :: val
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
  subroutine copy_idArgDynamicArray(new,this)
    !! Overloaded assignment of equals.  new = this
  !====================================================================!
  class(idArgDynamicArray), intent(in) :: this
    !! Class to copy.
  type(idArgDynamicArray), intent(out) :: new
    !! Copy of this.
  new%i = this%i
  new%v = this%v
  end subroutine
  !====================================================================!

  !====================================================================!
  subroutine deallocate_idArgDynamicArray(this)
    !! Overloaded type bound procedure idArgDynamicArray%deallocate().
  !====================================================================!
  class(idArgDynamicArray) :: this
  call this%i%deallocate()
  call this%v%deallocate()
  end subroutine
  !====================================================================!

  !====================================================================!
  function init_idArgDynamicArray_i1(M, sorted, fixed) result(this)
    !! Overloaded by interface [[idArgDynamicArray(type)]]
  !====================================================================!
    integer(i32), intent(in), optional :: M
    logical, intent(in), optional :: sorted
    logical, intent(in), optional :: fixed
    type(idArgDynamicArray) :: this
    this%i = iDynamicArray(M, .false., fixed)
    this%v = idDynamicArray(M, sorted,  fixed)
  end function
  !====================================================================!

  !====================================================================!
  function init_idArgDynamicArray_id1D(i, values, M, sorted, fixed) result(this)
    !! Overloaded by interface [[idArgDynamicArray(type)]]
  !====================================================================!
    integer(i32), intent(in) :: i(:)
    integer(i64), intent(in) :: values(:)
    integer(i32), intent(in), optional :: M
    logical, intent(in), optional :: sorted
    logical, intent(in), optional :: fixed
    type(idArgDynamicArray) :: this

    integer(i32) :: ii, iv

    ii = size(i)
    iv = size(values)

    if (ii /= iv) call eMsg("idArgDynamicArray: Size of i "//str(ii)//"must equal size of values "//str(iv))

    this%i = iDynamicArray(i, M, .false., fixed)
    this%v = idDynamicArray(values, M, sorted, fixed)
  end function
  !====================================================================!

  !====================================================================!
  subroutine insertAt_idArgDynamicArray(this, loc, i, val)
    !! Overloaded type bound procedure idArgDynamicArray%insertAt().
  !====================================================================!
  class(idArgDynamicArray) :: this
  integer(i32) :: loc
    !! Insert index and value at this location.
  integer(i32) :: i
    !! index/
  integer(i64) :: val
    !! Value/
  call this%i%insertAt(loc, i)
  call this%v%insertAt(loc, val)
  end subroutine
  !====================================================================!

  !====================================================================!
  subroutine insertSorted_idArgDynamicArray(this, i, val)
    !! Overloaded type bound procedure idArgDynamicArray%insertSorted().
  !====================================================================!
  class(idArgDynamicArray) :: this
  integer(i32) :: i
    !! Index.
  integer(i64) :: val
    !! Value.

  integer(i32) :: iSearch(3)

  iSearch = intervalSearch(this%v%values, val, 1, this%v%N)
  call this%i%insertAt(iSearch(3), i)
  call insertAt__idDynamicArray(this%v, iSearch(3), val)
  end subroutine
  !====================================================================!

  !====================================================================!
  subroutine insertSortedUnique_idArgDynamicArray(this, i, val)
    !! Overloaded type bound procedure idArgDynamicArray%insertSortedUnique().
  !====================================================================!
  class(idArgDynamicArray) :: this
  integer(i32) :: i
    !! Index.
  integer(i64) :: val
    !! Value.

  integer(i32) :: iSearch(3)

  iSearch = intervalSearch(this%v%values, val, 1, this%v%N)
  if (iSearch(1) == -1) then
    call this%i%insertAt(iSearch(3), i)
    call insertAt__idDynamicArray(this%v, iSearch(3), val)
  endif

  end subroutine
  !====================================================================!

  !====================================================================!
  function isEmpty_idArgDynamicArray(this) result(yes)
    !! Overloaded type bound procedure idArgDynamicArray%isEmpty()
  !====================================================================!
  class(idArgDynamicArray) :: this
  logical :: yes
    !! Array is empty
  yes = this%v%isEmpty()
  end function
  !====================================================================!

  !====================================================================!
  function isFilled_idArgDynamicArray(this) result(yes)
    !! Overloaded type bound procedure idArgDynamicArray%isFilled()
  !====================================================================!
  class(idArgDynamicArray) :: this
  logical :: yes
    !! Array is filled
  yes = this%v%isFilled()
  end function
  !====================================================================!

  !====================================================================!
  function locationOf_idArgDynamicArray(this, val) result(i)
    !! Overloaded type bound procedure idArgDynamicArray%locationOf().
  !====================================================================!
  class(idArgDynamicArray) :: this
  integer(i64) :: val
    !! Value to get the argument of.
  integer(i32) :: i
    !! Index of the value.
  i = this%v%locationOf(val)
  end function
  !====================================================================!

  !====================================================================!
  subroutine prepend_idArgDynamicArray(this, i, val)
    !! Overloaded type bound procedure idArgDynamicArray%prepend().
  !====================================================================!
  class(idArgDynamicArray) :: this
  integer(i32) :: i
    !! Prepend indices with this index.
  integer(i64) :: val
    !! Prepend values with this value.

  call this%i%prepend(i)
  call this%v%prepend(val)

  end subroutine
  !====================================================================!

  !====================================================================!
  subroutine remove_idArgDynamicArray(this, i)
    !! Overloaded type bound procedure idArgDynamicArray%remove().
  !====================================================================!
  class(idArgDynamicArray) :: this
  integer(i32) :: i
    !! Remove the elements at this location.
  call this%i%remove(i)
  call this%v%remove(i)
  end subroutine
  !====================================================================!

  !====================================================================!
  subroutine tighten_idArgDynamicArray(this)
    !! Overloaded type bound procedure idArgDynamicArray%tighten().
  !====================================================================!
  class(idArgDynamicArray) :: this

  call this%i%tighten()
  call this%v%tighten()
  end subroutine
  !====================================================================!

  !====================================================================!
  subroutine idArgDynamicArray_test(test)
  !====================================================================!
  class(tester) :: test

  type(idArgDynamicArray) :: idda, idda2

  integer(i32) :: ia

  call Msg('==========================')
  call Msg('Testing : idArgDynamic Arrays')
  call Msg('==========================')

  idda = idArgDynamicArray(10)
  call test%test(size(idda%v%values)==10 .and. size(idda%i%values)==10, 'idArgDynamicArray')
  call test%test(idda%v%N == 0 .and. idda%i%N == 0, 'idArgDynamicArray')
  call idda%insertAt(1, 10, 10_i64)
  call test%test(idda%i%values(1) == 10_i64 .and. idda%v%values(1) == 10_i64, 'idArgDynamicArray%insert')
  call idda%insertAt(1, 20, 20_i64)
  call test%test(all(idda%i%values(1:2) == [20_i64, 10_i64]) .and. all(idda%v%values(1:2) == [20_i64, 10_i64]), 'idArgDynamicArray%insert')
  call idda%prepend(30, 30_i64)
  call test%test(all(idda%i%values(1:3) == [30_i64, 20_i64, 10_i64]) .and. all(idda%v%values(1:3) == [30_i64, 20_i64, 10_i64]), 'idArgDynamicArray%prepend')
  call idda%append(40, 40_i64)
  call test%test(all(idda%i%values(1:4) == [30_i64, 20_i64, 10_i64, 40_i64]) .and. all(idda%v%values(1:4) == [30_i64, 20_i64, 10_i64, 40_i64]), 'idArgDynamicArray%append')
  call idda%remove(2)
  call test%test(all(idda%i%values(1:3) == [30_i64, 10_i64, 40_i64]) .and. all(idda%v%values(1:3) == [30_i64, 10_i64, 40_i64]), 'idArgDynamicArray%remove')
  call idda%tighten()
  call test%test(size(idda%i%values) == 3 .and. size(idda%v%values) == 3, 'idArgDynamicArray%tighten')
  idda2 = idda
  call test%test(all(idda2%i%values == idda%i%values) .and. all(idda2%v%values == idda%v%values), 'idArgDynamicArray%copy')
  idda2%v%values(2) = 50_i64
  call test%test(idda2%v%values(2) /= idda%v%values(2), 'idArgDynamicArray%copy')
  call idda%deallocate()
  call test%test(.not. allocated(idda%i%values) .and. .not. allocated(idda%v%values), 'idArgDynamicArray%deallocate')
  call idda2%deallocate()

  idda = idArgDynamicArray(3, sorted=.true.)
  call idda%insertSorted(1, 20_i64)
  call idda%insertSorted(2, 30_i64)
  call idda%insertSorted(3, 10_i64)
  call test%test(all(idda%i%values(1:3)==[3, 1, 2]) .and. all(idda%v%values(1:3)==[10_i64, 20_i64, 30_i64]), 'idArgDynamicArray%insertSorted')
  ia = idda%locationOf(20_i64)
  call test%test(ia == 2, 'idArgDynamicArray%locationOf')
  ia = idda%argOf(20_i64)
  call test%test(ia == 1, 'idArgDynamicArray%argOf')
  call idda%insertSortedUnique(4, 10_i64)
  call test%test(all(idda%i%values(1:3)==[3, 1, 2]) .and. all(idda%v%values(1:3)==[10_i64, 20_i64, 30_i64]), 'idArgDynamicArray%insertSortedUnique')
  call idda%insertSortedUnique(4, 15_i64)
  call test%test(all(idda%i%values(1:4)==[3, 4, 1, 2]) .and. all(idda%v%values(1:4)==[10_i64, 15_i64, 20_i64, 30_i64]), 'idArgDynamicArray%insertSortedUnique')
  call test%test(size(idda%i%values) == 6 .and. size(idda%v%values) == 6, 'idArgDynamicArray%insert')
  call idda%deallocate()

  idda = idArgDynamicArray(3, sorted=.true., fixed=.true.)
  call idda%insertSorted(1, 20_i64)
  call idda%insertSorted(2, 30_i64)
  call idda%insertSorted(3, 10_i64)
  call test%test(all(idda%i%values(1:3)==[3, 1, 2]) .and. all(idda%v%values(1:3)==[10_i64, 20_i64, 30_i64]), 'idArgDynamicArray%insertSorted')
  ia = idda%locationOf(20_i64)
  call test%test(ia == 2, 'idArgDynamicArray%locationOf')
  ia = idda%argOf(20_i64)
  call test%test(ia == 1, 'idArgDynamicArray%argOf')
  call idda%insertSortedUnique(4, 10_i64)
  call test%test(all(idda%i%values(1:3)==[3, 1, 2]) .and. all(idda%v%values(1:3)==[10_i64, 20_i64, 30_i64]), 'idArgDynamicArray%insertSortedUnique')
  call idda%insertSortedUnique(4, 15_i64)
  call test%test(all(idda%i%values(1:3)==[3, 4, 1]) .and. all(idda%v%values(1:3)==[10_i64, 15_i64, 20_i64]), 'idArgDynamicArray%insertSortedUnique')
  call test%test(size(idda%i%values) == 3 .and. size(idda%v%values) == 3, 'idArgDynamicArray%insert')
  call idda%deallocate()
end subroutine
end module
