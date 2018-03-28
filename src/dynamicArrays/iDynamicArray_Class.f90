module iDynamicArray_Class
!! Class that act as stacks, queues, and priority queues.
!! These classes use dynamically allocated contiguous blocks of memory to store a list of numbers.
!! The queues can be sorted to become priority queues and use binary searches to quickly insert new numbers.
!! If the allocated memory is filled, the available space is doubled.
!! Memory is only reallocated to a smaller size, if the utilization is a quarter of that allocated.
!!
!!```fortran
!!program dynamicArray_test
!!use variableKind, only: i32
!!use iDynamicArray_Class, only: iDynamicArray
!!
!!implicit none
!!
!!type(iDynamicArray) :: da, da2
!!integer(i32) :: ia
!!
!!da = iDynamicArray(10)
!!call da%insertAt(1, 10)
!!call da%insertAt(1, 20)
!!call da%prepend(30)
!!call da%append(40)
!!call da%remove(2)
!!call da%tighten()
!!da2 = da
!!da2%values(2) = 50
!!call da%deallocate()
!!call da2%deallocate()
!!
!!da = iDynamicArray(3, sorted=.true.)
!!call da%insertSorted(20)
!!call da%insertSorted(30)
!!call da%insertSorted(10)
!!ia = da%locationOf(20)
!!call da%insertSortedUnique(10)
!!call da%insertSortedUnique(15)
!!call da%deallocate()
!!
!!da = iDynamicArray(3, sorted=.true., fixed=.true.)
!!call da%insertSorted(20)
!!call da%insertSorted(30)
!!call da%insertSorted(10)
!!ia = da%locationOf(20)
!!call da%insertSortedUnique(10)
!!call da%insertSortedUnique(15)
!!call da%deallocate()
!!end program
!!```

use variableKind, only: i32
use m_allocate, only: allocate
use m_searching, only: binarySearch, intervalSearch
use m_deallocate, only: deallocate
use m_errors, only: eMsg, msg
use m_reallocate, only: reallocate
use m_sort, only: sort
use m_strings, only: str

implicit none

private

public :: iDynamicArray

type :: iDynamicArray
  !! Class that act as stacks, queues, and priority queues. See [[iDynamicArray_Class]] for more information on how to use this class.
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
    !! iDynamicArray%append() - Append a value to the end of the dynamic array.  Will change a sorted dynamic array to unsorted.
  procedure, public :: deallocate => deallocate_iDynamicArray
    !! iDynamicArray%deallocate() - Deallocate a dynamic array.
  procedure, public :: insertAt => insertAt_iDynamicArray
    !! iDynamicArray%insertAt() - Insert a value at a given index.
  procedure, public :: insertSorted => insertSorted_iDynamicArray
    !! iDynamicArray%insertSorted() - Insert a value into a sorted dynamic array.
  procedure, public :: insertSortedUnique => insertSortedUnique_iDynamicArray
    !! iDynamicArray%insertSortedUnique() - Inserts only unique numbers into a dynamic array.
  procedure, public :: isEmpty => isEmpty_iDynamicArray
    !! iDynamicArray%isEmpty() - True if the array is empty.
  procedure, public :: isFilled => isFilled_iDynamicArray
    !! iDynamicArray%isFilled() - True if the array is filled.
  procedure, public :: locationOf => locationOf_iDynamicArray
    !! iDynamicArray%locationOf() - Get the location of a value in a sorted dynamic array.
  procedure, public :: prepend => prepend_iDynamicArray
    !! iDynamicArray%prepend() - Prepend a value to the start of the dynamic array. Only for unsorted dynamic arrays
  procedure, public :: reallocate => reallocate_iDynamicArray
    !! iDynamicArray%reallocate() - Create new contiguous memory to match the needs of the expanding or shrinking array.
  procedure, public :: remove => remove_iDynamicArray
    !! iDynamicArray%remove() - Remove an element from the array.
  procedure, public :: tighten => tighten_iDynamicArray
    !! iDynamicArray%tighten() - Removes excess buffer memory and trims it to the current length.
end type


interface iDynamicArray
  procedure :: init_iDynamicArray_i1, init_iDynamicArray_d1D
end interface

interface assignment(=)
  procedure :: copy_iDynamicArray
end interface

contains

  !====================================================================!
  subroutine append_iDynamicArray(this,val)
    !! Overloaded type bound procedure iDynamicArray%append()
  !====================================================================!
  class(iDynamicArray) :: this
  integer(i32) :: val
    !! Value to append.
  if (this%fixed) call eMsg('iDynamicArray%append: Cannot use append with fixed array.')
  call this%insertAt(this%N + 1,val) ! Append at last location
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine copy_iDynamicArray(new,this)
    !! Overloaded assignment of equals.  new = this
  !====================================================================!
  class(iDynamicArray), intent(in) :: this
    !! Class to copy.
  type(iDynamicArray), intent(out) :: new
    !! Copy of this.
  call allocate(new%values, size(this%values))
  new%N = this%N
  new%values = this%values
  new%sorted = this%sorted
  new%fixed = this%fixed
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine deallocate_iDynamicArray(this)
    !! Overloaded type bound procedure iDynamicArray%deallocate()
  !====================================================================!
  class(iDynamicArray) :: this
  call deallocate(this%values)
  this%N = 0
  this%sorted = .false.
  end subroutine
  !====================================================================!
  !====================================================================!
  function init_iDynamicArray_i1(M, sorted, fixed) result(this)
    !! Overloaded by interface iDynamicArray()
  !====================================================================!
  integer(i32), intent(in), optional :: M
    !! Amount of memory to allocate.
  logical, intent(in), optional :: sorted
    !! Maintain a sorted array.
  logical, intent(in), optional :: fixed
    !! Maintain a fixed size array.
  type(iDynamicArray) :: this
    !! Return type.

  integer(i32) :: M_
  M_ = 1
  if (present(M)) then
    if (M < 1) call eMsg('iDynamicArray: M must be > 0')
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
  function init_iDynamicArray_d1D(values, M, sorted, fixed) result(this)
    !! Overloaded by interface iDynamicArray()
  !====================================================================!
  integer(i32), intent(in) :: values(:)
      !! Set of values to initialize with.
  integer(i32), intent(in), optional :: M
    !! Amount of memory to allocate.
  logical, intent(in), optional :: sorted
    !! Maintain a sorted array.
  logical, intent(in), optional :: fixed
    !! Maintain a fixed size array.
  type(iDynamicArray) :: this
    !! Return type

  if (present(M)) then
    if (M < size(values)) call eMsg('iDynamicArray:M must be >= size(values)')
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
  subroutine insertAt_iDynamicArray(this,i,val)
    !! Private insert into array without checking for sorted flag.
  !====================================================================!
  class(iDynamicArray) :: this
  integer(i32) :: i
    !! Insert value at this location.
  integer(i32) :: val
    !! Insert this value.
  integer :: j, N
  if (i < 1 .or. i > this%N + 1) call Emsg('iDynamicArray%insertAt: 1 <= i <= '//str(this%N + 1))

  N = size(this%values)

  if (this%fixed) then
    if (i > N) call Emsg('iDynamicArray%insertAt: For fixed array, 1 <= i <= '//str(N))

    if (this%N < N) this%N = this%N + 1

    do j = this%N, i+1, -1
      this%values(j) = this%values(j-1)
    enddo
  else
    ! Expand the vector if needed
    if (N < this%N + 1) call this%reallocate(2 * N)
    do j = this%N + 1, i + 1, -1
      this%values(j) = this%values(j-1)
    enddo
    this%N = this%N + 1
  endif

  this%values(i) = val
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine insertSorted_iDynamicArray(this,val)
    !! Overloaded type bound procedure iDynamicArray%insertSorted()
  !====================================================================!
  class(iDynamicArray) :: this
  integer(i32) :: val
    !! Insert this value.
  integer(i32) :: iSearch(3) ! location and interval of new value
  if (.not. this%sorted) call eMsg('iDynamicArray%insertSorted: Cannot use insertSorted with unsorted dynamic array')
  iSearch=intervalSearch(this%values, val, 1, this%N)
  call this%insertAt(iSearch(3), val)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine insertSortedUnique_iDynamicArray(this,val)
    !! Overloaded type bound procedure iDynamicArray%insertSortedUnique()
  !====================================================================!
  class(iDynamicArray) :: this
  integer(i32) :: val
    !! Insert this value.
  integer(i32) :: iSearch(3) ! location and interval of new value
  if (.not. this%sorted) call eMsg('iDynamicArray%insertSortedUnique: Cannot use insertSortedUnique with unsorted dynamic array')
  iSearch=intervalSearch(this%values, val, 1, this%N)
  if (iSearch(1) == -1) call this%insertAt(iSearch(3), val)
  end subroutine
  !====================================================================!
  !====================================================================!
  function isEmpty_iDynamicArray(this) result(yes)
    !! Overloaded type bound procedure iDynamicArray%isEmpty()
  !====================================================================!
  class(iDynamicArray) :: this
  logical :: yes
    !! Array is empty
  yes = (this%N == 0)
  end function
  !====================================================================!
  !====================================================================!
  function isFilled_iDynamicArray(this) result(yes)
    !! Overloaded type bound procedure iDynamicArray%isFilled()
  !====================================================================!
  class(iDynamicArray) :: this
  logical :: yes
    !! Array is filled
  yes = (this%N == size(this%values))
  end function
  !====================================================================!
  !====================================================================!
  function locationOf_iDynamicArray(this, val) result(i)
    !! Overloaded type bound procedure iDynamicArray%locationOf().
  !====================================================================!
  class(iDynamicArray) :: this
  integer(i32) :: val
    !! Get the location of this value
  integer(i32) :: i
    !! Location of value
  if (.not. this%sorted) call eMsg('iDynamicArray%locationOf: Cannot use locationOf with unsorted dynamic array')
  i = binarySearch(this%values, val, 1, this%N)
  end function
  !====================================================================!
  !====================================================================!
  subroutine prepend_iDynamicArray(this,val)
    !! Overloaded type bound procedure iDynamicArray%prepend()
  !====================================================================!
  class(iDynamicArray) :: this
  integer(i32) :: val
    !! Value to prepend.
  if (this%fixed) call eMsg('iDynamicArray%prepend: Cannot use prepend with fixed array.')
  call this%insertAt(1, val) ! Prepend at first location
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine reallocate_iDynamicArray(this, M)
    !! Overloaded type bound procedure iDynamicArray%reallocate().
  !====================================================================!
  class(iDynamicArray) :: this
  integer(i32) :: M
    !! Reallocate memory to this size.
  call reallocate(this%values, M)
  end subroutine
  !====================================================================!

  !====================================================================!
  subroutine remove_iDynamicArray(this, i)
    !! Overloaded type bound procedure iDynamicArray%remove().
  !====================================================================!
  class(iDynamicArray) :: this
  integer(i32) :: i
    !! Remove the value at this location.
  integer(i32) :: j
  if (i < 1 .or. i > this%N) call Emsg('iDynamic%remove: 1 <= i <= '//str(this%N))
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
  subroutine tighten_iDynamicArray(this)
    !! Overloaded type bound procedure iDynamicArray%tighten().
  !====================================================================!
  class(iDynamicArray) :: this
  if (this%fixed) call eMsg('iDynamicArray%tighten: Cannot use tighten with fixed array.')
  call this%reallocate(this%N)
  end subroutine
  !====================================================================!
end module
