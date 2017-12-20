module m_rDynamicArray
!! Class that act as stacks, queues, and priority queues.
!! These classes use dynamically allocated contiguous blocks of memory to store a list of numbers.
!! The queues can be sorted to become priority queues and use binary searches to quickly insert new numbers.
!! If the allocated memory is filled, the available space is doubled.
!! Memory is only reallocated to a smaller size, if the utilization is a quarter of that allocated.
!!
!!```fortran
!!program dynamicArray_test
!!use variableKind, only: i32
!!use m_dynamicArray, only: rDynamicArray
!!
!!implicit none
!!
!!type(rDynamicArray) :: rda, rda2
!!integer(i32) :: ia
!!
!!rda = rDynamicArray(10) ! array is empty but with memory allocated for 10 numbers
!!call rda%insert(1, 10.d0) ! array is [10.d0]
!!call rda%insert(1, 20.d0) ! array is [20.d0, 10.d0]
!!call rda%prepend(30.d0) ! array is [30.d0, 20.d0, 10.d0]
!!call rda%append(40.d0) ! array is [30.d0, 20.d0, 10.d0, 40.d0]
!!call rda%remove(2) ! array is [30.d0, 10.d0, 40.d0]
!!call rda%tighten() ! array memory changed to match, i.e. 3.
!!rda2 = rda ! non-pointer copy of dynamic array
!!call rda%deallocate() ! deallocate memory in the dynamic array
!!call rda2%deallocate() ! deallocate memory in the dynamic array
!!rda = rDynamicArray(3) ! Initialized 3 space dynamic array
!!call rda%insertSorted(20.d0) ! Sorted insertion
!!call rda%insertSorted(30.d0) ! Sorted insertion [20.d0, 30.d0]
!!call rda%insertSorted(10.d0) ! Sorted insertion [10.d0, 20.d0, 30.d0]
!!ia = rda%locationOf(20.d0) ! Only use locat
!!call test%test(ia == 2, 'rDynamicArray%locationOf')
!!call rda%insertSortedUnique(10.d0)
!!call test%test(all(rda%values==[10.d0, 20.d0, 30.d0]), 'rDynamicArray%insertSortedUnique')
!!call rda%insertSortedUnique(15.d0)
!!call test%test(all(rda%values==[10.d0, 15.d0, 20.d0, 30.d0]), 'rDynamicArray%insertSortedUnique')
!!call test%test(size(rda%values) == 6, 'rDynamicArray%insert')
!!end program
!!```

use variableKind, only: r32, i32
use m_allocate, only: allocate
use m_searching, only: binarySearch, intervalSearch
use m_deallocate, only: deallocate
use m_errors, only: eMsg
use m_reallocate, only: reallocate
use m_sort, only: sort
use m_strings, only: str

implicit none

private

public :: rDynamicArray

type :: rDynamicArray
  !! Class that act as stacks, queues, and priority queues. See [[m_rDynamicArray]] for more information on how to use this class.
  integer(i32) :: N
    !! Current size of the array
  real(r32), allocatable :: values(:)
    !! Memory for values, can be larger than N
  logical :: sorted = .false.
    !! Keep track of whether the array is sorted for potential speed increases
  logical :: fixed = .false.
    !! Don't allow the memory to change after initial instantiation.
contains
  procedure, public :: append => append_rDynamicArray
    !! rDynamicArray%append() - Append a value to the end of the dynamic array.  Will change a sorted dynamic array to unsorted.
  procedure, public :: deallocate => deallocate_rDynamicArray
    !! rDynamicArray%deallocate() - Deallocate a dynamic array.
  procedure, public :: insertAt => insertAt_rDynamicArray
    !! rDynamicArray%insertAt() - Insert a value at a given index.
  procedure, public :: insertSorted => insertSorted_rDynamicArray
    !! rDynamicArray%insertSorted() - Insert a value into a sorted dynamic array.
  procedure, public :: insertSortedUnique => insertSortedUnique_rDynamicArray
    !! rDynamicArray%insertSortedUnique() - Inserts only unique numbers into a dynamic array.
  procedure, public :: locationOf => locationOf_rDynamicArray
    !! rDynamicArray%locationOf() - Get the location of a value in a sorted dynamic array.
  procedure, public :: prepend => prepend_rDynamicArray
    !! rDynamicArray%prepend() - Prepend a value to the start of the dynamic array. Only for unsorted dynamic arrays
  procedure, public :: reallocate => reallocate_rDynamicArray
    !! rDynamicArray%reallocate() - Create new contiguous memory to match the needs of the expanding or shrinking array.
  procedure, public :: remove => remove_rDynamicArray
    !! rDynamicArray%remove() - Remove an element from the array.
  procedure, public :: tighten => tighten_rDynamicArray
    !! rDynamicArray%tighten() - Removes excess buffer memory and trims it to the current length.
end type


interface rDynamicArray
  procedure :: init_rDynamicArray_i1, init_rDynamicArray_d1D
end interface

interface assignment(=)
  procedure :: copy_rDynamicArray
end interface

contains

  !====================================================================!
  subroutine append_rDynamicArray(this,val)
    !! Overloaded type bound procedure rDynamicArray%append()
  !====================================================================!
  class(rDynamicArray) :: this
  real(r32) :: val
    !! Value to append.
  if (this%fixed .or. this%sorted) call eMsg('rDynamicArray%append: Cannot use append with fixed/sorted array.')
  call this%insertAt(this%N + 1, val) ! Append at last location
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine copy_rDynamicArray(new,this)
    !! Overloaded assignment of equals.  new = this
  !====================================================================!
  class(rDynamicArray), intent(in) :: this
    !! Class to copy.
  type(rDynamicArray), intent(out) :: new
    !! Copy of this.
  call allocate(new%values, size(this%values))
  new%N = this%N
  new%values = this%values
  new%sorted = this%sorted
  new%fixed = this%fixed
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine deallocate_rDynamicArray(this)
    !! Overloaded type bound procedure rDynamicArray%deallocate()
  !====================================================================!
  class(rDynamicArray) :: this
  call deallocate(this%values)
  this%N = 0
  this%sorted = .false.
  end subroutine
  !====================================================================!
  !====================================================================!
  function init_rDynamicArray_i1(M, sorted, fixed) result(this)
    !! Overloaded by interface rDynamicArray()
  !====================================================================!
  integer(i32), intent(in), optional :: M
    !! Amount of memory to allocate.
  logical, intent(in), optional :: sorted
    !! Maintain a sorted array.
  logical, intent(in), optional :: fixed
    !! Maintain a fixed size array.
  type(rDynamicArray) :: this
    !! Return type.

  integer(i32) :: M_
  M_ = 1
  if (present(M)) then
    if (M < 1) call eMsg('M must be > 0')
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
  function init_rDynamicArray_d1D(values, M, sorted, fixed) result(this)
    !! Overloaded by interface rDynamicArray()
  !====================================================================!
  real(r32), intent(in) :: values(:)
      !! Set of values to initialize with.
  integer(i32), intent(in), optional :: M
    !! Amount of memory to allocate.
  logical, intent(in), optional :: sorted
    !! Maintain a sorted array.
  logical, intent(in), optional :: fixed
    !! Maintain a fixed size array.
  type(rDynamicArray) :: this
    !! Return type

  if (present(M)) then
    if (M < size(values)) call eMsg('M must be >= size(values)')
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
  subroutine insertAt_rDynamicArray(this,i,val)
    !! Overloaded type bound procedure rDynamicArray%insertAt()
  !====================================================================!
  class(rDynamicArray) :: this
  integer(i32) :: i
    !! Insert value at this location.
  real(r32) :: val
    !! Insert this value.
  integer :: j, N
  if (i < 1 .or. i > this%N + 1) call Emsg('rDynamicArray%insert: 1 <= i <= '//str(this%N + 1))

  N = size(this%values)

  if (this%fixed) then
    if (i > N) call Emsg('rDynamicArray%insert: For fixed array, 1 <= i <= '//str(N))

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
  subroutine insertSorted_rDynamicArray(this,val)
    !! Overloaded type bound procedure rDynamicArray%insertSorted()
  !====================================================================!
  class(rDynamicArray) :: this
  real(r32) :: val
    !! Insert this value.
  integer(i32) :: iSearch(3) ! location and interval of new value
  if (.not. this%sorted) call eMsg('rDynamicArray%insertSorted: Cannot use insertSorted with unsorted dynamic array')
  iSearch=intervalSearch(this%values, val, 1, this%N)
  call this%insertAt(iSearch(3), val)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine insertSortedUnique_rDynamicArray(this,val)
    !! Overloaded type bound procedure rDynamicArray%insertSortedUnique()
  !====================================================================!
  class(rDynamicArray) :: this
  real(r32) :: val
    !! Insert this value.
  integer(i32) :: iSearch(3) ! location and interval of new value
  if (.not. this%sorted) call eMsg('rDynamicArray%insertSortedUnique: Cannot use insertSortedUnique with unsorted dynamic array')
  iSearch=intervalSearch(this%values, val, 1, this%N)
  if (iSearch(1) == -1) then
    call this%insertAt(iSearch(3), val)
    this%sorted = .true.
  endif
  end subroutine
  !====================================================================!
  !====================================================================!
  function locationOf_rDynamicArray(this, val) result(i)
    !! Overloaded type bound procedure rDynamicArray%locationOf().
  !====================================================================!
  class(rDynamicArray) :: this
  real(r32) :: val
    !! Get the location of this value
  integer(i32) :: i
    !! Location of value
  if (.not. this%sorted) call eMsg('rDynamicArray%locationOf: Cannot use locationOf with unsorted dynamic array')
  i = binarySearch(this%values, val, 1, this%N)
  end function
  !====================================================================!
  !====================================================================!
  subroutine prepend_rDynamicArray(this,val)
    !! Overloaded type bound procedure rDynamicArray%prepend()
  !====================================================================!
  class(rDynamicArray) :: this
  real(r32) :: val
    !! Value to prepend.
  if (this%fixed .or. this%sorted) call eMsg('rDynamicArray%prepend: Cannot use prepend with fixed/sorted array.')
  call this%insertAt(1, val) ! Prepend at first location
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine reallocate_rDynamicArray(this, M)
    !! Overloaded type bound procedure rDynamicArray%reallocate().
  !====================================================================!
  class(rDynamicArray) :: this
  integer(i32) :: M
    !! Reallocate memory to this size.
  call reallocate(this%values, M)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine remove_rDynamicArray(this, i)
    !! Overloaded type bound procedure rDynamicArray%remove().
  !====================================================================!
  class(rDynamicArray) :: this
  integer(i32) :: i
    !! Remove the value at this location.
  integer(i32) :: j
  if (i < 1 .or. i > this%N) call Emsg('rDynamic%remove: 1 <= i <= '//str(this%N))
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
  subroutine tighten_rDynamicArray(this)
    !! Overloaded type bound procedure rDynamicArray%tighten().
  !====================================================================!
  class(rDynamicArray) :: this
  if (this%fixed) call eMsg('rDynamicArray%tighten: Cannot use tighten with fixed array.')
  call this%reallocate(this%N)
  end subroutine
  !====================================================================!
end module
