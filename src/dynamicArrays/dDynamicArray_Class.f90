module dDynamicArray_Class
!! Class that act as stacks, queues, and priority queues.
!! These classes use dynamically allocated contiguous blocks of memory to store a list of numbers.
!! The queues can be sorted to become priority queues and use binary searches to quickly insert new numbers.
!! If the allocated memory is filled, the available space is doubled.
!! Memory is only reallocated to a smaller size, if the utilization is a quarter of that allocated.
!! The array can be specified as fixed, so that no reallocation occurs.  This is useful for heaps of given 
!! like k nearest neighbours, or k smallest.
!!
!!```fortran
!!program dynamicArray_test
!!use variableKind, only: i32
!!use m_dDynamicArray, only: dDynamicArray
!!
!!implicit none
!!
!!type(dDynamicArray) :: da, da2
!!integer(i32) :: ia
!!
!!da = dDynamicArray(10)
!!call da%insertAt(1, 10.d0)
!!call da%insertAt(1, 20.d0)
!!call da%prepend(30.d0)
!!call da%append(40.d0)
!!call da%remove(2)
!!call da%tighten()
!!da2 = da
!!da2%values(2) = 50.d0
!!call da%deallocate()
!!call da2%deallocate()
!!
!!da = dDynamicArray(3, sorted=.true.)
!!call da%insertSorted(20.d0)
!!call da%insertSorted(30.d0)
!!call da%insertSorted(10.d0)
!!ia = da%locationOf(20.d0)
!!call da%insertSortedUnique(10.d0)
!!call da%insertSortedUnique(15.d0)
!!call da%deallocate()
!!
!!da = dDynamicArray(3, sorted=.true., fixed=.true.)
!!call da%insertSorted(20.d0)
!!call da%insertSorted(30.d0)
!!call da%insertSorted(10.d0)
!!ia = da%locationOf(20.d0)
!!call da%insertSortedUnique(10.d0)
!!call da%insertSortedUnique(15.d0)
!!call da%deallocate()
!!end program
!!```

use variableKind, only: r64, i32
use m_allocate, only: allocate
use m_searching, only: binarySearch, intervalSearch
use m_deallocate, only: deallocate
use m_errors, only: eMsg, msg
use m_reallocate, only: reallocate
use m_sort, only: sort
use m_strings, only: str

implicit none

private

public :: dDynamicArray

type :: dDynamicArray
  !! Class that act as stacks, queues, and priority queues. See [[m_dDynamicArray]] for more information on how to use this class.
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
    !! dDynamicArray%deallocate() - Deallocate a dynamic array.
  procedure, public :: insertAt => insertAt_dDynamicArray
    !! dDynamicArray%insertAt() - Insert a value at a given index.
  procedure, public :: insertSorted => insertSorted_dDynamicArray
    !! dDynamicArray%insertSorted() - Insert a value into a sorted dynamic array.
  procedure, public :: insertSortedUnique => insertSortedUnique_dDynamicArray
    !! dDynamicArray%insertSortedUnique() - Inserts only unique numbers into a dynamic array.
  procedure, public :: isEmpty => isEmpty_dDynamicArray
    !! dDynamicArray%isEmpty() - True if the array is empty.
  procedure, public :: isFilled => isFilled_dDynamicArray
    !! dDynamicArray%isFilled() - True if the array is filled.
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


interface dDynamicArray
  procedure :: init_dDynamicArray_i1, init_dDynamicArray_d1D
end interface

interface assignment(=)
  procedure :: copy_dDynamicArray
end interface

contains

  !====================================================================!
  subroutine append_dDynamicArray(this,val)
    !! Overloaded type bound procedure dDynamicArray%append()
  !====================================================================!
  class(dDynamicArray) :: this
  real(r64) :: val
    !! Value to append.
  if (this%fixed) call eMsg('dDynamicArray%append: Cannot use append with fixed array.')
  call this%insertAt(this%N + 1, val) ! Append at last location
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine copy_dDynamicArray(new,this)
    !! Overloaded assignment of equals.  new = this
  !====================================================================!
  class(dDynamicArray), intent(in) :: this
    !! Class to copy.
  type(dDynamicArray), intent(out) :: new
    !! Copy of this.
  call allocate(new%values, size(this%values))
  new%N = this%N
  new%values = this%values
  new%sorted = this%sorted
  new%fixed = this%fixed
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine deallocate_dDynamicArray(this)
    !! Overloaded type bound procedure dDynamicArray%deallocate()
  !====================================================================!
  class(dDynamicArray) :: this
  call deallocate(this%values)
  this%N = 0
  this%sorted = .false.
  end subroutine
  !====================================================================!
  !====================================================================!
  function init_dDynamicArray_i1(M, sorted, fixed) result(this)
    !! Overloaded by interface dDynamicArray()
  !====================================================================!
  integer(i32), intent(in), optional :: M
    !! Amount of memory to allocate.
  logical, intent(in), optional :: sorted
    !! Maintain a sorted array.
  logical, intent(in), optional :: fixed
    !! Maintain a fixed size array.
  type(dDynamicArray) :: this
    !! Return type.

  integer(i32) :: M_
  M_ = 1
  if (present(M)) then
    if (M < 1) call eMsg('dDynamicArray: M must be > 0')
    M_ = M
  endif
  call allocate(this%values, M_)
  this%N = 0

  this%sorted = .false.
  if (present(sorted)) this%sorted = sorted
  this%fixed = .false.
  if (present(fixed)) this%fixed = fixed
  end function
  !====================================================================!
  !====================================================================!
  function init_dDynamicArray_d1D(values, M, sorted, fixed) result(this)
    !! Overloaded by interface dDynamicArray()
  !====================================================================!
  real(r64), intent(in) :: values(:)
    !! Set of values to initialize with.
  integer(i32), intent(in), optional :: M
    !! Amount of memory to allocate.
  logical, intent(in), optional :: sorted
    !! Maintain a sorted array.
  logical, intent(in), optional :: fixed
    !! Maintain a fixed size array.
  type(dDynamicArray) :: this
    !! Return type

  if (present(M)) then
    if (M < size(values)) call eMsg('dDynamicArray: M must be >= size(values)')
    call allocate(this%values, M)
  else
    call allocate(this%values, size(values))
  endif

  this%N = size(values)

  this%sorted = .false.
  if (present(sorted)) this%sorted = sorted
  if (this%sorted) then
      this%values(1:this%N) = values
      call sort(this%values(1:this%N))
  else
      this%values(1:this%N) = values
  endif
  this%fixed = .false.
  if (present(fixed)) this%fixed = fixed
  end function
  !====================================================================!
  !====================================================================!
  subroutine insertAt_dDynamicArray(this,i,val)
    !! Overloaded type bound procedure dDynamicArray%insertAt()
  !====================================================================!
  class(dDynamicArray) :: this
  integer(i32) :: i
    !! Insert value at this location.
  real(r64) :: val
    !! Insert this value.
  integer :: j, N
  if (i < 1 .or. i > this%N + 1) call Emsg('dDynamicArray%insert: 1 <= i <= '//str(this%N + 1))

  N = size(this%values)

  if (this%fixed) then
    if (i > N) call Emsg('dDynamicArray%insert: For fixed array, 1 <= i <= '//str(N))

    if (this%N < N) this%N = this%N + 1

    do j = this%N, i+1, -1
      this%values(j) = this%values(j-1)
    enddo
  else
    ! Expand the vector if needed
    if (N < this%N + 1) call this%reallocate(2 * N)
    do j = this%N+1, i+1, -1
      this%values(j) = this%values(j-1)
    enddo
    this%N = this%N + 1
  endif

  this%values(i) = val
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine insertSorted_dDynamicArray(this,val)
    !! Overloaded type bound procedure dDynamicArray%insertSorted()
  !====================================================================!
  class(dDynamicArray) :: this
  real(r64) :: val
    !! Insert this value.
  integer(i32) :: iSearch(3) ! location and interval of new value
  if (.not. this%sorted) call eMsg('dDynamicArray%insertSorted: Cannot use insertSorted with unsorted dynamic array')
  iSearch = intervalSearch(this%values, val, 1, this%N)
  call this%insertAt(iSearch(3), val)
  this%sorted = .true.
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine insertSortedUnique_dDynamicArray(this,val)
    !! Overloaded type bound procedure dDynamicArray%insertSortedUnique()
  !====================================================================!
  class(dDynamicArray) :: this
  real(r64) :: val
    !! Insert this value.
  integer(i32) :: iSearch(3) ! location and interval of new value
  if (.not. this%sorted) call eMsg('dDynamicArray%insertSortedUnique: Cannot use insertSortedUnique with unsorted dynamic array')
  iSearch = intervalSearch(this%values, val, 1, this%N)
  if (iSearch(1) == -1) then
    call this%insertAt(iSearch(3), val)
    this%sorted = .true.
  endif
  end subroutine
  !====================================================================!
  !====================================================================!
  function isEmpty_dDynamicArray(this) result(yes)
    !! Overloaded type bound procedure dDynamicArray%isEmpty()
  !====================================================================!
  class(dDynamicArray) :: this
  logical :: yes
    !! Array is empty
  yes = (this%N == 0)
  end function
  !====================================================================!
  !====================================================================!
  function isFilled_dDynamicArray(this) result(yes)
    !! Overloaded type bound procedure dDynamicArray%isFilled()
  !====================================================================!
  class(dDynamicArray) :: this
  logical :: yes
    !! Array is filled
  yes = (this%N == size(this%values))
  end function
  !====================================================================!
  !====================================================================!
  function locationOf_dDynamicArray(this, val) result(i)
    !! Overloaded type bound procedure dDynamicArray%locationOf().
  !====================================================================!
  class(dDynamicArray) :: this
  real(r64) :: val
    !! Get the location of this value
  integer(i32) :: i
    !! Location of value
  if (.not. this%sorted) call eMsg('dDynamicArray%locationOf: Cannot use locationOf with unsorted dynamic array')
  i = binarySearch(this%values, val, 1, this%N)
  end function
  !====================================================================!
  !====================================================================!
  subroutine prepend_dDynamicArray(this,val)
    !! Overloaded type bound procedure dDynamicArray%prepend()
  !====================================================================!
  class(dDynamicArray) :: this
  real(r64) :: val
    !! Value to prepend.
  if (this%fixed) call eMsg('dDynamicArray%prepend: Cannot use prepend with fixed array.')
  call this%insertAt(1, val) ! Prepend at first location
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine reallocate_dDynamicArray(this, M)
    !! Overloaded type bound procedure dDynamicArray%reallocate().
  !====================================================================!
  class(dDynamicArray) :: this
  integer(i32) :: M
    !! Reallocate memory to this size.
  call reallocate(this%values, M)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine remove_dDynamicArray(this, i)
    !! Overloaded type bound procedure dDynamicArray%remove().
  !====================================================================!
  class(dDynamicArray) :: this
  integer(i32) :: i
    !! Remove the value at this location.
  integer(i32) :: j
  if (i < 1 .or. i > this%N) call Emsg('dDynamic%remove: 1 <= i <= '//str(this%N))
  do j = i, this%N - 1
    this%values(j) = this%values(j + 1)
  enddo
  this%N = this%N - 1
  if (.not. this%fixed) then
    if (this%N < size(this%values)/4) call this%reallocate(this%N)
  endif
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine tighten_dDynamicArray(this)
    !! Overloaded type bound procedure dDynamicArray%tighten().
  !====================================================================!
  class(dDynamicArray) :: this
  if (this%fixed) call eMsg('dDynamicArray%tighten: Cannot use tighten with fixed array.')
  call this%reallocate(this%N)
  end subroutine
  !====================================================================!
end module
