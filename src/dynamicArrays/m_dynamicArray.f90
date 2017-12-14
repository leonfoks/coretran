module m_dynamicArray
!! Class that act as stacks, queues, and priority queues.
!! These classes use dynamically allocated contiguous blocks of memory to store a list of numbers.
!! The queues can be sorted to become priority queues and use binary searches to quickly insert new numbers.
!! If the allocated memory is filled, the available space is doubled.
!! Memory is only reallocated to a smaller size, if the utilization is a quarter of that allocated.
!!
!!```fortran
!!program dynamicArray_test
!!use variableKind, only: i32
!!use m_dynamicArray, only: dDynamicArray
!!
!!implicit none
!!
!!type(dDynamicArray) :: dda, dda2
!!integer(i32) :: ia
!!
!!dda = dDynamicArray(10) ! array is empty but with memory allocated for 10 numbers
!!call dda%insert(1, 10.d0) ! array is [10.d0]
!!call dda%insert(1, 20.d0) ! array is [20.d0, 10.d0]
!!call dda%prepend(30.d0) ! array is [30.d0, 20.d0, 10.d0]
!!call dda%append(40.d0) ! array is [30.d0, 20.d0, 10.d0, 40.d0]
!!call dda%remove(2) ! array is [30.d0, 10.d0, 40.d0]
!!call dda%tighten() ! array memory changed to match, i.e. 3.
!!dda2 = dda ! non-pointer copy of dynamic array
!!call dda%deallocate() ! deallocate memory in the dynamic array
!!call dda2%deallocate() ! deallocate memory in the dynamic array
!!dda = dDynamicArray(3) ! Initialized 3 space dynamic array
!!call dda%insertSorted(20.d0) ! Sorted insertion
!!call dda%insertSorted(30.d0) ! Sorted insertion [20.d0, 30.d0]
!!call dda%insertSorted(10.d0) ! Sorted insertion [10.d0, 20.d0, 30.d0]
!!ia = dda%locationOf(20.d0) ! Only use locat
!!call test%test(ia == 2, 'dDynamicArray%locationOf')
!!call dda%insertSortedUnique(10.d0)
!!call test%test(all(dda%values==[10.d0, 20.d0, 30.d0]), 'dDynamicArray%insertSortedUnique')
!!call dda%insertSortedUnique(15.d0)
!!call test%test(all(dda%values==[10.d0, 15.d0, 20.d0, 30.d0]), 'dDynamicArray%insertSortedUnique')
!!call test%test(size(dda%values) == 6, 'dDynamicArray%insert')
!!end program
!!```


use variableKind, only: r32, r64, i32, i64
use m_errors, only: msg
use m_unitTester, only: tester

implicit none

private

public :: dynamicArray_test


public :: rDynamicArray

type :: rDynamicArray
  !! Class that act as stacks, queues, and priority queues. See [[m_dynamicArray]] for more information on how to use this class.
  integer(i32) :: N
    !! Current size of the array
  real(r32), allocatable :: values(:)
    !! Memory for values, can be larger than N
  logical :: sorted = .false.
    !! Keep track of whether the array is sorted
  logical :: fixed = .false.
    !! Don't allow the memory to change after initial instantiation.
contains
  procedure, public :: append => append_rDynamicArray
    !! dDynamicArray%append() - Append a value to the end of the dynamic array.  Will change a sorted dynamic array to unsorted.
  procedure, public :: deallocate => deallocate_rDynamicArray
    !! dDynamicArray%%deallocate() - Deallocate a dynamic array.
  procedure, public :: insertAt => insertAt_rDynamicArray
    !! dDynamicArray%insertAt() - Insert a value at a given index.
  procedure, public :: insertSorted => insertSorted_rDynamicArray
    !! dDynamicArray%insertSorted() - Insert a value into a sorted dynamic array.
  procedure, public :: insertSortedUnique => insertSortedUnique_rDynamicArray
    !! dDynamicArray%insertSortedUnique() - Inserts only unique numbers into a dynamic array.
  procedure, public :: locationOf => locationOf_rDynamicArray
    !! dDynamicArray%locationOf() - Get the location of a value in a sorted dynamic array.
  procedure, public :: prepend => prepend_rDynamicArray
    !! dDynamicArray%prepend() - Prepend a value to the start of the dynamic array. Only for unsorted dynamic arrays
  procedure, public :: reallocate => reallocate_rDynamicArray
    !! dDynamicArray%reallocate() - Create new contiguous memory to match the needs of the expanding or shrinking array.
  procedure, public :: remove => remove_rDynamicArray
    !! dDynamicArray%remove() - Remove an element from the array.
  procedure, public :: tighten => tighten_rDynamicArray
    !! dDynamicArray%tighten() - Removes excess buffer memory and trims it to the current length.
end type



public :: dDynamicArray

type :: dDynamicArray
  !! Class that act as stacks, queues, and priority queues. See [[m_dynamicArray]] for more information on how to use this class.
  integer(i32) :: N
    !! Current size of the array
  real(r64), allocatable :: values(:)
    !! Memory for values, can be larger than N
  logical :: sorted = .false.
    !! Keep track of whether the array is sorted for potential speed increases
  logical :: fixed = .false.
    !! Don't allow the memory to change after initial instantiation.
contains
  procedure, public :: append => append_dDynamicArray
    !! dDynamicArray%append() - Append a value to the end of the dynamic array.  Will change a sorted dynamic array to unsorted.
  procedure, public :: deallocate => deallocate_dDynamicArray
    !! dDynamicArray%%deallocate() - Deallocate a dynamic array.
  procedure, public :: insertAt => insertAt_dDynamicArray
    !! dDynamicArray%insertAt() - Insert a value at a given index.
  procedure, public :: insertSorted => insertSorted_dDynamicArray
    !! dDynamicArray%insertSorted() - Insert a value into a sorted dynamic array.
  procedure, public :: insertSortedUnique => insertSortedUnique_dDynamicArray
    !! dDynamicArray%insertSortedUnique() - Inserts only unique numbers into a dynamic array.
  procedure, public :: locationOf => locationOf_dDynamicArray
    !! dDynamicArray%locationOf() - Get the location of a value in a sorted dynamic array.
  procedure, public :: prepend => prepend_dDynamicArray
    !! dDynamicArray%prepend() - Prepend a value to the start of the dynamic array. Only for unsorted dynamic arrays
  procedure, public :: reallocate => reallocate_dDynamicArray
    !! dDynamicArray%reallocate() - Create new contiguous memory to match the needs of the expanding or shrinking array.
  procedure, public :: remove => remove_dDynamicArray
    !! dDynamicArray%remove() - Remove an element from the array.
  procedure, public :: tighten => tighten_dDynamicArray
    !! dDynamicArray%tighten() - Removes excess buffer memory and trims it to the current length.
end type



public :: iDynamicArray

type :: iDynamicArray
  !! Class that act as stacks, queues, and priority queues. See [[m_dynamicArray]] for more information on how to use this class.
  integer(i32) :: N
    !! Current size of the array
  integer(i32), allocatable :: values(:)
    !! Memory for values, can be larger than N
  logical :: sorted = .false.
    !! Keep track of whether the array is sorted for potential speed increases
  logical :: fixed = .false.
    !! Don't allow the memory to change after initial instantiation.
contains
  procedure, public :: append => append_iDynamicArray
    !! dDynamicArray%append() - Append a value to the end of the dynamic array.  Will change a sorted dynamic array to unsorted.
  procedure, public :: deallocate => deallocate_iDynamicArray
    !! dDynamicArray%%deallocate() - Deallocate a dynamic array.
  procedure, public :: insertAt => insertAt_iDynamicArray
    !! dDynamicArray%insertAt() - Insert a value at a given index.
  procedure, public :: insertSorted => insertSorted_iDynamicArray
    !! dDynamicArray%insertSorted() - Insert a value into a sorted dynamic array.
  procedure, public :: insertSortedUnique => insertSortedUnique_iDynamicArray
    !! dDynamicArray%insertSortedUnique() - Inserts only unique numbers into a dynamic array.
  procedure, public :: locationOf => locationOf_iDynamicArray
    !! dDynamicArray%locationOf() - Get the location of a value in a sorted dynamic array.
  procedure, public :: prepend => prepend_iDynamicArray
    !! dDynamicArray%prepend() - Prepend a value to the start of the dynamic array. Only for unsorted dynamic arrays
  procedure, public :: reallocate => reallocate_iDynamicArray
    !! dDynamicArray%reallocate() - Create new contiguous memory to match the needs of the expanding or shrinking array.
  procedure, public :: remove => remove_iDynamicArray
    !! dDynamicArray%remove() - Remove an element from the array.
  procedure, public :: tighten => tighten_iDynamicArray
    !! dDynamicArray%tighten() - Removes excess buffer memory and trims it to the current length.
end type



public :: idDynamicArray

type :: idDynamicArray
  !! Class that act as stacks, queues, and priority queues. See [[m_dynamicArray]] for more information on how to use this class.
  integer(i32) :: N
    !! Current size of the array
  integer(i64), allocatable :: values(:)
    !! Memory for values, can be larger than N
  logical :: sorted = .false.
    !! Keep track of whether the array is sorted for potential speed increases
  logical :: fixed = .false.
    !! Don't allow the memory to change after initial instantiation.
contains
  procedure, public :: append => append_idDynamicArray
    !! dDynamicArray%append() - Append a value to the end of the dynamic array.  Will change a sorted dynamic array to unsorted.
  procedure, public :: deallocate => deallocate_idDynamicArray
    !! dDynamicArray%%deallocate() - Deallocate a dynamic array.
  procedure, public :: insertAt => insertAt_idDynamicArray
    !! dDynamicArray%insertAt() - Insert a value at a given index.
  procedure, public :: insertSorted => insertSorted_idDynamicArray
    !! dDynamicArray%insertSorted() - Insert a value into a sorted dynamic array.
  procedure, public :: insertSortedUnique => insertSortedUnique_idDynamicArray
    !! dDynamicArray%insertSortedUnique() - Inserts only unique numbers into a dynamic array.
  procedure, public :: locationOf => locationOf_idDynamicArray
    !! dDynamicArray%locationOf() - Get the location of a value in a sorted dynamic array.
  procedure, public :: prepend => prepend_idDynamicArray
    !! dDynamicArray%prepend() - Prepend a value to the start of the dynamic array. Only for unsorted dynamic arrays
  procedure, public :: reallocate => reallocate_idDynamicArray
    !! dDynamicArray%reallocate() - Create new contiguous memory to match the needs of the expanding or shrinking array.
  procedure, public :: remove => remove_idDynamicArray
    !! dDynamicArray%remove() - Remove an element from the array.
  procedure, public :: tighten => tighten_idDynamicArray
    !! dDynamicArray%tighten() - Removes excess buffer memory and trims it to the current length.
end type


!
! real(r32) Interfaces
!
interface rDynamicArray
  !====================================================================!
  module function init_rDynamicArray_i1(M, sorted, fixed) result(this)
    !! Overloaded by interface [[rDynamicArray(type)]]
  !====================================================================!
    integer(i32), intent(in), optional :: M
    logical, intent(in), optional :: sorted
    logical, intent(in), optional :: fixed
    type(rDynamicArray) :: this
  end function
  !====================================================================!
  !====================================================================!
  module function init_rDynamicArray_r1D(values, M, sorted, fixed) result(this)
    !! Overloaded by interface [[rDynamicArray(type)]]
  !====================================================================!
    real(r32), intent(in) :: values(:)
    integer(i32), intent(in), optional :: M
    logical, intent(in), optional :: sorted
    logical, intent(in), optional :: fixed
    type(rDynamicArray) :: this
  end function
  !====================================================================!
end interface

interface assignment(=)
  !====================================================================!
  module subroutine copy_rDynamicArray(new,this)
    !! Overloaded assignment of equals.  new = this
  !====================================================================!
  class(rDynamicArray), intent(in) :: this
  type(rDynamicArray), intent(out) :: new
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine copy_dDynamicArray(new,this)
    !! Overloaded assignment of equals.  new = this
  !====================================================================!
  class(dDynamicArray), intent(in) :: this
  type(dDynamicArray), intent(out) :: new
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine copy_iDynamicArray(new,this)
    !! Overloaded assignment of equals.  new = this
  !====================================================================!
  class(iDynamicArray), intent(in) :: this
  type(iDynamicArray), intent(out) :: new
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine copy_idDynamicArray(new,this)
    !! Overloaded assignment of equals.  new = this
  !====================================================================!
  class(idDynamicArray), intent(in) :: this
  type(idDynamicArray), intent(out) :: new
  end subroutine
  !====================================================================!
end interface

interface
  !====================================================================!
  module subroutine append_rDynamicArray(this,val)
    !! Overloaded type bound procedure rDynamicArray%append().
  !====================================================================!
  class(rDynamicArray) :: this
  real(r32) :: val
  end subroutine
  !====================================================================!

  !====================================================================!
  module subroutine deallocate_rDynamicArray(this)
    !! Overloaded type bound procedure rDynamicArray%deallocate().
  !====================================================================!
  class(rDynamicArray) :: this
  end subroutine
  !====================================================================!

  !====================================================================!
  module subroutine insertAt_rDynamicArray(this,i,val)
    !! Overloaded type bound procedure rDynamicArray%insertAt().
  !====================================================================!
  class(rDynamicArray) :: this
  integer(i32) :: i ! Insert at this location
  real(r32) :: val
  end subroutine
  !====================================================================!

  !====================================================================!
  module subroutine insertSorted_rDynamicArray(this,val)
    !! Overloaded type bound procedure rDynamicArray%insertSorted().
  !====================================================================!
  class(rDynamicArray) :: this
  real(r32) :: val
  end subroutine
  !====================================================================!

  !====================================================================!
  module subroutine insertSortedUnique_rDynamicArray(this,val)
    !! Overloaded type bound procedure rDynamicArray%insertSortedUnique().
  !====================================================================!
  class(rDynamicArray) :: this
  real(r32) :: val
  end subroutine
  !====================================================================!

  !====================================================================!
  module function locationOf_rDynamicArray(this, val) result(i)
    !! Overloaded type bound procedure rDynamicArray%locationOf(). Only for sorted dynamic arrays.
  !====================================================================!
    class(rDynamicArray) :: this
    real(r32) :: val
    integer(i32) :: i
  end function
  !====================================================================!

  !====================================================================!
  module subroutine prepend_rDynamicArray(this,val)
    !! Overloaded type bound procedure rDynamicArray%prepend().
  !====================================================================!
  class(rDynamicArray) :: this
  real(r32) :: val
  end subroutine
  !====================================================================!

  !====================================================================!
  module subroutine reallocate_rDynamicArray(this, M)
    !! Overloaded type bound proedure rDynamicArray%reallocate().
  !====================================================================!
  class(rDynamicArray) :: this
  integer(i32) :: M
  end subroutine
  !====================================================================!

  !====================================================================!
  module subroutine remove_rDynamicArray(this, i)
    !! Overloaded type bound procedure rDynamicArray%remove().
  !====================================================================!
  class(rDynamicArray) :: this
  integer(i32) :: i
  end subroutine
  !====================================================================!

  !====================================================================!
  module subroutine tighten_rDynamicArray(this)
    !! Overloaded type bound procedure rDynamicArray%tighten().
  !====================================================================!
  class(rDynamicArray) :: this
  end subroutine
  !====================================================================!
end interface


!
! real(r64) Interfaces
!
interface dDynamicArray
  !====================================================================!
  module function init_dDynamicArray_i1(M, sorted, fixed) result(this)
    !! Overloaded by interface [[dDynamicArray(type)]]
  !====================================================================!
    integer(i32), intent(in), optional :: M
    logical, intent(in), optional :: sorted
    logical, intent(in), optional :: fixed
    type(dDynamicArray) :: this
  end function
  !====================================================================!
  !====================================================================!
  module function init_dDynamicArray_d1D(values, M, sorted, fixed) result(this)
    !! Overloaded by interface [[dDynamicArray(type)]]
  !====================================================================!
    real(r64), intent(in) :: values(:)
    integer(i32), intent(in), optional :: M
    logical, intent(in), optional :: sorted
    logical, intent(in), optional :: fixed
    type(dDynamicArray) :: this
  end function
  !====================================================================!
end interface

interface
  !====================================================================!
  module subroutine append_dDynamicArray(this,val)
    !! Overloaded type bound procedure dDynamicArray%append().
  !====================================================================!
  class(dDynamicArray) :: this
  real(r64) :: val
  end subroutine
  !====================================================================!

  !====================================================================!
  module subroutine deallocate_dDynamicArray(this)
    !! Overloaded type bound procedure dDynamicArray%deallocate().
  !====================================================================!
  class(dDynamicArray) :: this
  end subroutine
  !====================================================================!

  !====================================================================!
  module subroutine insertAt_dDynamicArray(this,i,val)
    !! Overloaded type bound procedure dDynamicArray%insertAt().
  !====================================================================!
  class(dDynamicArray) :: this
  integer(i32) :: i ! Insert at this location
  real(r64) :: val
  end subroutine
  !====================================================================!

  !====================================================================!
  module subroutine insertSorted_dDynamicArray(this,val)
    !! Overloaded type bound procedure dDynamicArray%insertSorted().
  !====================================================================!
  class(dDynamicArray) :: this
  real(r64) :: val
  end subroutine
  !====================================================================!

  !====================================================================!
  module subroutine insertSortedUnique_dDynamicArray(this,val)
    !! Overloaded type bound procedure dDynamicArray%insertSortedUnique().
  !====================================================================!
  class(dDynamicArray) :: this
  real(r64) :: val
  end subroutine
  !====================================================================!

  !====================================================================!
  module function locationOf_dDynamicArray(this, val) result(i)
    !! Overloaded type bound procedure dDynamicArray%locationOf(). Only for sorted dynamic arrays.
  !====================================================================!
    class(dDynamicArray) :: this
    real(r64) :: val
    integer(i32) :: i
  end function
  !====================================================================!

  !====================================================================!
  module subroutine prepend_dDynamicArray(this,val)
    !! Overloaded type bound procedure dDynamicArray%prepend().
  !====================================================================!
  class(dDynamicArray) :: this
  real(r64) :: val
  end subroutine
  !====================================================================!

  !====================================================================!
  module subroutine reallocate_dDynamicArray(this, M)
    !! Overloaded type bound proedure dDynamicArray%reallocate().
  !====================================================================!
  class(dDynamicArray) :: this
  integer(i32) :: M
  end subroutine
  !====================================================================!

  !====================================================================!
  module subroutine remove_dDynamicArray(this, i)
    !! Overloaded type bound procedure dDynamicArray%remove().
  !====================================================================!
  class(dDynamicArray) :: this
  integer(i32) :: i
  end subroutine
  !====================================================================!

  !====================================================================!
  module subroutine tighten_dDynamicArray(this)
    !! Overloaded type bound procedure dDynamicArray%tighten().
  !====================================================================!
  class(dDynamicArray) :: this
  end subroutine
  !====================================================================!
end interface


!
! Integer(i32) dynamic array interfaces
!
interface iDynamicArray
  !====================================================================!
  module function init_iDynamicArray_i1(M, sorted, fixed) result(this)
    !! Overloaded by interface [[iDynamicArray(type)]]
  !====================================================================!
    integer(i32), intent(in), optional :: M
    logical, intent(in), optional :: sorted
    logical, intent(in), optional :: fixed
    type(iDynamicArray) :: this
  end function
  !====================================================================!
  !====================================================================!
  module function init_iDynamicArray_d1D(values, M, sorted, fixed) result(this)
    !! Overloaded by interface [[iDynamicArray(type)]]
  !====================================================================!
    integer(i32), intent(in) :: values(:)
    integer(i32), intent(in), optional :: M
    logical, intent(in), optional :: sorted
    logical, intent(in), optional :: fixed
    type(iDynamicArray) :: this
  end function
  !====================================================================!
end interface

interface
  !====================================================================!
  module subroutine append_iDynamicArray(this,val)
    !! Overloaded type bound procedure iDynamicArray%append().
  !====================================================================!
  class(iDynamicArray) :: this
  integer(i32) :: val
  end subroutine
  !====================================================================!

  !====================================================================!
  module subroutine deallocate_iDynamicArray(this)
    !! Overloaded type bound procedure iDynamicArray%deallocate().
  !====================================================================!
  class(iDynamicArray) :: this
  end subroutine
  !====================================================================!

  !====================================================================!
  module subroutine insertAt_iDynamicArray(this,i,val)
    !! Overloaded type bound procedure iDynamicArray%insertAt().
  !====================================================================!
  class(iDynamicArray) :: this
  integer(i32) :: i ! Insert at this location
  integer(i32) :: val
  end subroutine
  !====================================================================!

  !====================================================================!
  module subroutine insertSorted_iDynamicArray(this,val)
    !! Overloaded type bound procedure iDynamicArray%insertSorted().
  !====================================================================!
  class(iDynamicArray) :: this
  integer(i32) :: val
  end subroutine
  !====================================================================!

  !====================================================================!
  module subroutine insertSortedUnique_iDynamicArray(this,val)
    !! Overloaded type bound procedure iDynamicArray%insertSortedUnique().
  !====================================================================!
  class(iDynamicArray) :: this
  integer(i32) :: val
  end subroutine
  !====================================================================!

  !====================================================================!
  module function locationOf_iDynamicArray(this, val) result(i)
    !! Overloaded type bound procedure iDynamicArray%locationOf(). Only for sorted dynamic arrays.
  !====================================================================!
    class(iDynamicArray) :: this
    integer(i32) :: val
    integer(i32) :: i
  end function
  !====================================================================!

  !====================================================================!
  module subroutine prepend_iDynamicArray(this,val)
    !! Overloaded type bound procedure iDynamicArray%prepend().
  !====================================================================!
  class(iDynamicArray) :: this
  integer(i32) :: val
  end subroutine
  !====================================================================!

  !====================================================================!
  module subroutine reallocate_iDynamicArray(this, M)
    !! Overloaded type bound proedure iDynamicArray%reallocate().
  !====================================================================!
  class(iDynamicArray) :: this
  integer(i32) :: M
  end subroutine
  !====================================================================!

  !====================================================================!
  module subroutine remove_iDynamicArray(this, i)
    !! Overloaded type bound procedure iDynamicArray%remove().
  !====================================================================!
  class(iDynamicArray) :: this
  integer(i32) :: i
  end subroutine
  !====================================================================!

  !====================================================================!
  module subroutine tighten_iDynamicArray(this)
    !! Overloaded type bound procedure iDynamicArray%tighten().
  !====================================================================!
  class(iDynamicArray) :: this
  end subroutine
  !====================================================================!
end interface


!
! Integer(i64) dynamic array interfaces
!
interface idDynamicArray
  !====================================================================!
  module function init_idDynamicArray_i1(M, sorted, fixed) result(this)
    !! Overloaded by interface [[idDynamicArray(type)]]
  !====================================================================!
    integer(i32), intent(in), optional :: M
    logical, intent(in), optional :: sorted
    logical, intent(in), optional :: fixed
    type(idDynamicArray) :: this
  end function
  !====================================================================!
  !====================================================================!
  module function init_idDynamicArray_d1D(values, M, sorted, fixed) result(this)
    !! Overloaded by interface [[idDynamicArray(type)]]
  !====================================================================!
    integer(i64), intent(in) :: values(:)
    integer(i32), intent(in), optional :: M
    logical, intent(in), optional :: sorted
    logical, intent(in), optional :: fixed
    type(idDynamicArray) :: this
  end function
  !====================================================================!
end interface

interface
  !====================================================================!
  module subroutine append_idDynamicArray(this,val)
    !! Overloaded type bound procedure idDynamicArray%append().
  !====================================================================!
  class(idDynamicArray) :: this
  integer(i64) :: val
  end subroutine
  !====================================================================!

  !====================================================================!
  module subroutine deallocate_idDynamicArray(this)
    !! Overloaded type bound procedure idDynamicArray%deallocate().
  !====================================================================!
  class(idDynamicArray) :: this
  end subroutine
  !====================================================================!

  !====================================================================!
  module subroutine insertAt_idDynamicArray(this,i,val)
    !! Overloaded type bound procedure idDynamicArray%insertAt().
  !====================================================================!
  class(idDynamicArray) :: this
  integer(i32) :: i ! Insert at this location
  integer(i64) :: val
  end subroutine
  !====================================================================!

  !====================================================================!
  module subroutine insertSorted_idDynamicArray(this,val)
    !! Overloaded type bound procedure idDynamicArray%insertSorted().
  !====================================================================!
  class(idDynamicArray) :: this
  integer(i64) :: val
  end subroutine
  !====================================================================!

  !====================================================================!
  module subroutine insertSortedUnique_idDynamicArray(this,val)
    !! Overloaded type bound procedure idDynamicArray%insertSortedUnique().
  !====================================================================!
  class(idDynamicArray) :: this
  integer(i64) :: val
  end subroutine
  !====================================================================!

  !====================================================================!
  module function locationOf_idDynamicArray(this, val) result(i)
    !! Overloaded type bound procedure idDynamicArray%locationOf(). Only for sorted dynamic arrays.
  !====================================================================!
    class(idDynamicArray) :: this
    integer(i64) :: val
    integer(i32) :: i
  end function
  !====================================================================!

  !====================================================================!
  module subroutine prepend_idDynamicArray(this,val)
    !! Overloaded type bound procedure idDynamicArray%prepend().
  !====================================================================!
  class(idDynamicArray) :: this
  integer(i64) :: val
  end subroutine
  !====================================================================!

  !====================================================================!
  module subroutine reallocate_idDynamicArray(this, M)
    !! Overloaded type bound proedure idDynamicArray%reallocate().
  !====================================================================!
  class(idDynamicArray) :: this
  integer(i32) :: M
  end subroutine
  !====================================================================!

  !====================================================================!
  module subroutine remove_idDynamicArray(this, i)
    !! Overloaded type bound procedure idDynamicArray%remove().
  !====================================================================!
  class(idDynamicArray) :: this
  integer(i32) :: i
  end subroutine
  !====================================================================!

  !====================================================================!
  module subroutine tighten_idDynamicArray(this)
    !! Overloaded type bound procedure idDynamicArray%tighten().
  !====================================================================!
  class(idDynamicArray) :: this
  end subroutine
  !====================================================================!
end interface

contains

  !====================================================================!
  subroutine dynamicArray_test(test)
  !====================================================================!
  class(tester) :: test

  type(rDynamicArray) :: rda, rda2
  type(dDynamicArray) :: dda, dda2
  type(iDynamicArray) :: ida, ida2
  type(idDynamicArray) :: idda, idda2

  integer(i32) :: ia

  call Msg('==========================')
  call Msg('Testing : Dynamic Arrays')
  call Msg('==========================')

  rda = rDynamicArray(10)
  call test%test(size(rda%values)==10, 'rDynamicArray')
  call test%test(rda%N==0, 'rDynamicArray')
  call rda%insertAt(1, 10.0)
  call test%test(rda%values(1) == 10, 'rDynamicArray%insert')
  call rda%insertAt(1, 20.0)
  call test%test(all(rda%values(1:2) == [20.0, 10.0]), 'rDynamicArray%insert')
  call rda%prepend(30.0)
  call test%test(all(rda%values(1:3) == [30.0, 20.0, 10.0]), 'rDynamicArray%prepend')
  call rda%append(40.0)
  call test%test(all(rda%values(1:4) == [30.0, 20.0, 10.0, 40.0]), 'rDynamicArray%append')
  call rda%remove(2)
  call test%test(all(rda%values(1:3) == [30.0, 10.0, 40.0]), 'rDynamicArray%remove')
  call rda%tighten()
  call test%test(size(rda%values) == 3, 'rDynamicArray%tighten')
  rda2 = rda
  call test%test(all(rda2%values == rda%values), 'rDynamicArray%copy')
  rda2%values(2) = 50.0
  call test%test(rda2%values(2) /= rda%values(2), 'rDynamicArray%copy')
  call rda%deallocate()
  call test%test(.not. allocated(rda%values), 'rDynamicArray%deallocate')
  call rda2%deallocate()

  rda = rDynamicArray(3, sorted=.true.)
  call rda%insertSorted(20.0)
  call rda%insertSorted(30.0)
  call rda%insertSorted(10.0)
  call test%test(all(rda%values==[10.0, 20.0, 30.0]), 'rDynamicArray%insertSorted')
  ia = rda%locationOf(20.0)
  call test%test(ia == 2, 'rDynamicArray%locationOf')
  call rda%insertSortedUnique(10.0)
  call test%test(all(rda%values==[10.0, 20.0, 30.0]), 'rDynamicArray%insertSortedUnique')
  call rda%insertSortedUnique(15.0)
  call test%test(all(rda%values(1:rda%N)==[10.0, 15.0, 20.0, 30.0]), 'rDynamicArray%insertSortedUnique')
  call test%test(size(rda%values) == 6, 'rDynamicArray%insert')
  call rda%deallocate()

  rda = rDynamicArray(3, sorted=.true., fixed=.true.)
  call rda%insertSorted(20.0)
  call rda%insertSorted(30.0)
  call rda%insertSorted(10.0)
  call test%test(all(rda%values(1:rda%N)==[10.0, 20.0, 30.0]), 'rDynamicArray%insertSorted')
  ia = rda%locationOf(20.0)
  call test%test(ia == 2, 'rDynamicArray%locationOf')
  call rda%insertSortedUnique(10.0)
  call test%test(all(rda%values(1:rda%N)==[10.0, 20.0, 30.0]), 'rDynamicArray%insertSortedUnique')
  call rda%insertSortedUnique(15.0)
  call test%test(all(rda%values(1:rda%N)==[10.0, 15.0, 20.0]), 'rDynamicArray%insertSortedUnique')
  call test%test(size(rda%values) == 3, 'rDynamicArray%insert')
  call rda%deallocate()



  dda = dDynamicArray(10)
  call test%test(size(dda%values)==10, 'dDynamicArray')
  call test%test(dda%N==0, 'dDynamicArray')
  call dda%insertAt(1, 10.d0)
  call test%test(dda%values(1) == 10, 'dDynamicArray%insert')
  call dda%insertAt(1, 20.d0)
  call test%test(all(dda%values(1:2) == [20.d0, 10.d0]), 'dDynamicArray%insert')
  call dda%prepend(30.d0)
  call test%test(all(dda%values(1:3) == [30.d0, 20.d0, 10.d0]), 'dDynamicArray%prepend')
  call dda%append(40.d0)
  call test%test(all(dda%values(1:4) == [30.d0, 20.d0, 10.d0, 40.d0]), 'dDynamicArray%append')
  call dda%remove(2)
  call test%test(all(dda%values(1:3) == [30.d0, 10.d0, 40.d0]), 'dDynamicArray%remove')
  call dda%tighten()
  call test%test(size(dda%values) == 3, 'dDynamicArray%tighten')
  dda2 = dda
  call test%test(all(dda2%values == dda%values), 'dDynamicArray%copy')
  dda2%values(2) = 50.d0
  call test%test(dda2%values(2) /= dda%values(2), 'dDynamicArray%copy')
  call dda%deallocate()
  call test%test(.not. allocated(dda%values), 'dDynamicArray%deallocate')
  call dda2%deallocate()

  dda = dDynamicArray(3, sorted=.true.)
  call dda%insertSorted(20.d0)
  call dda%insertSorted(30.d0)
  call dda%insertSorted(10.d0)
  call test%test(all(dda%values==[10.d0, 20.d0, 30.d0]), 'dDynamicArray%insertSorted')
  ia = dda%locationOf(20.d0)
  call test%test(ia == 2, 'dDynamicArray%locationOf')
  call dda%insertSortedUnique(10.d0)
  call test%test(all(dda%values==[10.d0, 20.d0, 30.d0]), 'dDynamicArray%insertSortedUnique')
  call dda%insertSortedUnique(15.d0)
  call test%test(all(dda%values(1:dda%N)==[10.d0, 15.d0, 20.d0, 30.d0]), 'dDynamicArray%insertSortedUnique')
  call test%test(size(dda%values) == 6, 'dDynamicArray%insert')
  call dda%deallocate()

  dda = dDynamicArray(3, sorted=.true., fixed=.true.)
  call dda%insertSorted(20.d0)
  call dda%insertSorted(30.d0)
  call dda%insertSorted(10.d0)
  call test%test(all(dda%values(1:dda%N)==[10.d0, 20.d0, 30.d0]), 'dDynamicArray%insertSorted')
  ia = dda%locationOf(20.d0)
  call test%test(ia == 2, 'dDynamicArray%locationOf')
  call dda%insertSortedUnique(10.d0)
  call test%test(all(dda%values(1:dda%N)==[10.d0, 20.d0, 30.d0]), 'dDynamicArray%insertSortedUnique')
  call dda%insertSortedUnique(15.d0)
  call test%test(all(dda%values(1:dda%N)==[10.d0, 15.d0, 20.d0]), 'dDynamicArray%insertSortedUnique')
  call test%test(size(dda%values) == 3, 'dDynamicArray%insert')
  call dda%deallocate()


  ida = iDynamicArray(10)
  call test%test(size(ida%values)==10, 'iDynamicArray')
  call test%test(ida%N==0, 'iDynamicArray')
  call ida%insertAt(1, 10)
  call test%test(ida%values(1) == 10, 'iDynamicArray%insert')
  call ida%insertAt(1, 20)
  call test%test(all(ida%values(1:2) == [20, 10]), 'iDynamicArray%insert')
  call ida%prepend(30)
  call test%test(all(ida%values(1:3) == [30, 20, 10]), 'iDynamicArray%prepend')
  call ida%append(40)
  call test%test(all(ida%values(1:4) == [30, 20, 10, 40]), 'iDynamicArray%append')
  call ida%remove(2)
  call test%test(all(ida%values(1:3) == [30, 10, 40]), 'iDynamicArray%remove')
  call ida%tighten()
  call test%test(size(ida%values) == 3, 'iDynamicArray%tighten')
  ida2 = ida
  call test%test(all(ida2%values == ida%values), 'iDynamicArray%copy')
  ida2%values(2) = 50
  call test%test(ida2%values(2) /= ida%values(2), 'iDynamicArray%copy')
  call ida%deallocate()
  call test%test(.not. allocated(ida%values), 'iDynamicArray%deallocate')
  call ida2%deallocate()

  ida = iDynamicArray(3, sorted=.true.)
  call ida%insertSorted(20)
  call ida%insertSorted(30)
  call ida%insertSorted(10)
  call test%test(all(ida%values==[10, 20, 30]), 'iDynamicArray%insertSorted')
  ia = ida%locationOf(20)
  call test%test(ia == 2, 'iDynamicArray%locationOf')
  call ida%insertSortedUnique(10)
  call test%test(all(ida%values==[10, 20, 30]), 'iDynamicArray%insertSortedUnique')
  call ida%insertSortedUnique(15)
  call test%test(all(ida%values(1:ida%N)==[10, 15, 20, 30]), 'iDynamicArray%insertSortedUnique')
  call test%test(size(ida%values) == 6, 'iDynamicArray%insert')
  call ida%deallocate()

  ida = iDynamicArray(3, sorted=.true., fixed=.true.)
  call ida%insertSorted(20)
  call ida%insertSorted(30)
  call ida%insertSorted(10)
  call test%test(all(ida%values(1:ida%N)==[10, 20, 30]), 'iDynamicArray%insertSorted')
  ia = ida%locationOf(20)
  call test%test(ia == 2, 'iDynamicArray%locationOf')
  call ida%insertSortedUnique(10)
  call test%test(all(ida%values(1:ida%N)==[10, 20, 30]), 'iDynamicArray%insertSortedUnique')
  call ida%insertSortedUnique(15)
  call test%test(all(ida%values(1:ida%N)==[10, 15, 20]), 'iDynamicArray%insertSortedUnique')
  call test%test(size(ida%values) == 3, 'iDynamicArray%insert')
  call ida%deallocate()


  idda = idDynamicArray(10)
  call test%test(size(idda%values)==10, 'idDynamicArray')
  call test%test(idda%N==0, 'idDynamicArray')
  call idda%insertAt(1, 10_i64)
  call test%test(idda%values(1) == 10, 'idDynamicArray%insert')
  call idda%insertAt(1, 20_i64)
  call test%test(all(idda%values(1:2) == [20_i64, 10_i64]), 'idDynamicArray%insert')
  call idda%prepend(30_i64)
  call test%test(all(idda%values(1:3) == [30_i64, 20_i64, 10_i64]), 'idDynamicArray%prepend')
  call idda%append(40_i64)
  call test%test(all(idda%values(1:4) == [30_i64, 20_i64, 10_i64, 40_i64]), 'idDynamicArray%append')
  call idda%remove(2)
  call test%test(all(idda%values(1:3) == [30_i64, 10_i64, 40_i64]), 'idDynamicArray%remove')
  call idda%tighten()
  call test%test(size(idda%values) == 3, 'idDynamicArray%tighten')
  idda2 = idda
  call test%test(all(idda2%values == idda%values), 'idDynamicArray%copy')
  idda2%values(2) = 50_i64
  call test%test(idda2%values(2) /= idda%values(2), 'idDynamicArray%copy')
  call idda%deallocate()
  call test%test(.not. allocated(idda%values), 'idDynamicArray%deallocate')
  call idda2%deallocate()

  idda = idDynamicArray(3, sorted=.true.)
  call idda%insertSorted(20_i64)
  call idda%insertSorted(30_i64)
  call idda%insertSorted(10_i64)
  call test%test(all(idda%values==[10_i64, 20_i64, 30_i64]), 'idDynamicArray%insertSorted')
  ia = idda%locationOf(20_i64)
  call test%test(ia == 2, 'idDynamicArray%locationOf')
  call idda%insertSortedUnique(10_i64)
  call test%test(all(idda%values==[10_i64, 20_i64, 30_i64]), 'idDynamicArray%insertSortedUnique')
  call idda%insertSortedUnique(15_i64)
  call test%test(all(idda%values(1:idda%N)==[10_i64, 15_i64, 20_i64, 30_i64]), 'idDynamicArray%insertSortedUnique')
  call test%test(size(idda%values) == 6, 'idDynamicArray%insert')
  call idda%deallocate()

  idda = idDynamicArray(3, sorted=.true., fixed=.true.)
  call idda%insertSorted(20_i64)
  call idda%insertSorted(30_i64)
  call idda%insertSorted(10_i64)
  call test%test(all(idda%values(1:idda%N)==[10_i64, 20_i64, 30_i64]), 'idDynamicArray%insertSorted')
  ia = idda%locationOf(20_i64)
  call test%test(ia == 2, 'idDynamicArray%locationOf')
  call idda%insertSortedUnique(10_i64)
  call test%test(all(idda%values(1:idda%N)==[10_i64, 20_i64, 30_i64]), 'idDynamicArray%insertSortedUnique')
  call idda%insertSortedUnique(15_i64)
  call test%test(all(idda%values(1:idda%N)==[10_i64, 15_i64, 20_i64]), 'idDynamicArray%insertSortedUnique')
  call test%test(size(idda%values) == 3, 'idDynamicArray%insert')
  call idda%deallocate()

  end subroutine
  !====================================================================!

end module
