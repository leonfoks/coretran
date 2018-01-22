module m_idDynamicArray
!! Class that act as stacks, queues, and priority queues.
!! These classes use dynamically allocated contiguous blocks of memory to store a list of numbers.
!! The queues can be sorted to become priority queues and use binary searches to quickly insert new numbers.
!! If the allocated memory is filled, the available space is doubled.
!! Memory is only reallocated to a smaller size, if the utilization is a quarter of that allocated.
!!
!!```fortran
!!program dynamicArray_test
!!use variableKind, only: i32
!!use m_dynamicArray, only: idDynamicArray
!!
!!implicit none
!!
!!type(idDynamicArray) :: da, da2
!!integer(i32) :: ia
!!
!!da = idDynamicArray(10)
!!call da%insertAt(1, 10_i64)
!!call da%insertAt(1, 20_i64)
!!call da%prepend(30_i64)
!!call da%append(40_i64)
!!call da%remove(2)
!!call da%tighten()
!!da2 = da
!!da2%values(2) = 50_i64
!!call da%deallocate()
!!call da2%deallocate()
!!
!!da = idDynamicArray(3, sorted=.true.)
!!call da%insertSorted(20_i64)
!!call da%insertSorted(30_i64)
!!call da%insertSorted(10_i64)
!!ia = da%locationOf(20_i64)
!!call da%insertSortedUnique(10_i64)
!!call da%insertSortedUnique(15_i64)
!!call da%deallocate()
!!
!!da = idDynamicArray(3, sorted=.true., fixed=.true.)
!!call da%insertSorted(20_i64)
!!call da%insertSorted(30_i64)
!!call da%insertSorted(10_i64)
!!ia = da%locationOf(20_i64)
!!call da%insertSortedUnique(10_i64)
!!call da%insertSortedUnique(15_i64)
!!call da%deallocate()
!!end program
!!```

use variableKind, only: i32, i64
use m_allocate, only: allocate
use m_searching, only: binarySearch, intervalSearch
use m_deallocate, only: deallocate
use m_errors, only: eMsg, msg
use m_reallocate, only: reallocate
use m_sort, only: sort
use m_strings, only: str
use m_unitTester, only: tester

implicit none

private

public :: idDynamicArray_test

public :: insertAt__idDynamicArray

public :: idDynamicArray

type :: idDynamicArray
  !! Class that act as stacks, queues, and priority queues. See [[m_idDynamicArray]] for more information on how to use this class.
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
    !! idDynamicArray%append() - Append a value to the end of the dynamic array.  Will change a sorted dynamic array to unsorted.
  procedure, public :: deallocate => deallocate_idDynamicArray
    !! idDynamicArray%deallocate() - Deallocate a dynamic array.
  procedure, public :: insertAt => insertAt_idDynamicArray
    !! idDynamicArray%insertAt() - Insert a value at a given index.
  procedure, public :: insertSorted => insertSorted_idDynamicArray
    !! idDynamicArray%insertSorted() - Insert a value into a sorted dynamic array.
  procedure, public :: insertSortedUnique => insertSortedUnique_idDynamicArray
    !! idDynamicArray%insertSortedUnique() - Inserts only unique numbers into a dynamic array.
  procedure, public :: isEmpty => isEmpty_idDynamicArray
    !! idDynamicArray%isEmpty() - True if the array is empty.
  procedure, public :: isFilled => isFilled_idDynamicArray
    !! idDynamicArray%isFilled() - True if the array is filled.
  procedure, public :: locationOf => locationOf_idDynamicArray
    !! idDynamicArray%locationOf() - Get the location of a value in a sorted dynamic array.
  procedure, public :: prepend => prepend_idDynamicArray
    !! idDynamicArray%prepend() - Prepend a value to the start of the dynamic array. Only for unsorted dynamic arrays
  procedure, public :: reallocate => reallocate_idDynamicArray
    !! idDynamicArray%reallocate() - Create new contiguous memory to match the needs of the expanding or shrinking array.
  procedure, public :: remove => remove_idDynamicArray
    !! idDynamicArray%remove() - Remove an element from the array.
  procedure, public :: tighten => tighten_idDynamicArray
    !! idDynamicArray%tighten() - Removes excess buffer memory and trims it to the current length.
end type


interface idDynamicArray
  procedure :: init_idDynamicArray_i1, init_idDynamicArray_d1D
end interface

interface assignment(=)
  procedure :: copy_idDynamicArray
end interface

contains

  !====================================================================!
  subroutine append_idDynamicArray(this,val)
    !! Overloaded type bound procedure idDynamicArray%append()
  !====================================================================!
  class(idDynamicArray) :: this
  integer(i64) :: val
    !! Value to append.
  if (this%fixed) call eMsg('idDynamicArray%append: Cannot use append with fixed array.')
  call insertAt__idDynamicArray(this, this%N + 1, val) ! Append at last location
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine copy_idDynamicArray(new,this)
    !! Overloaded assignment of equals.  new = this
  !====================================================================!
  class(idDynamicArray), intent(in) :: this
    !! Class to copy.
  type(idDynamicArray), intent(out) :: new
    !! Copy of this.
  call allocate(new%values, size(this%values))
  new%N = this%N
  new%values = this%values
  new%sorted = this%sorted
  new%fixed = this%fixed
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine deallocate_idDynamicArray(this)
    !! Overloaded type bound procedure idDynamicArray%deallocate()
  !====================================================================!
  class(idDynamicArray) :: this
  call deallocate(this%values)
  this%N = 0
  this%sorted = .false.
  end subroutine
  !====================================================================!
  !====================================================================!
  function init_idDynamicArray_i1(M, sorted, fixed) result(this)
    !! Overloaded by interface idDynamicArray()
  !====================================================================!
  integer(i32), intent(in), optional :: M
    !! Amount of memory to allocate.
  logical, intent(in), optional :: sorted
    !! Maintain a sorted array.
  logical, intent(in), optional :: fixed
    !! Maintain a fixed size array.
  type(idDynamicArray) :: this
    !! Return type.

  integer(i32) :: M_
  M_ = 1
  if (present(M)) then
    if (M < 1) call eMsg('idDynamicArray: M must be > 0')
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
  function init_idDynamicArray_d1D(values, M, sorted, fixed) result(this)
    !! Overloaded by interface idDynamicArray()
  !====================================================================!
  integer(i64), intent(in) :: values(:)
      !! Set of values to initialize with.
  integer(i32), intent(in), optional :: M
    !! Amount of memory to allocate.
  logical, intent(in), optional :: sorted
    !! Maintain a sorted array.
  logical, intent(in), optional :: fixed
    !! Maintain a fixed size array.
  type(idDynamicArray) :: this
    !! Return type

  if (present(M)) then
    if (M < size(values)) call eMsg('idDynamicArray: M must be >= size(values)')
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
  subroutine insertAt_idDynamicArray(this,i,val)
    !! Overloaded type bound procedure rDynamicArray%insertAt()
  !====================================================================!
  class(idDynamicArray) :: this
  integer(i32) :: i
    !! Insert value at this location.
  integer(i64) :: val
    !! Insert this value.
  if (this%sorted) call eMsg('idDynamicArray%insertAt: Cannot use insertAt with sorted array')
  call insertAt__idDynamicArray(this, i, val)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine insertAt__idDynamicArray(this,i,val)
    !! Private insert into array without checking for sorted flag.
  !====================================================================!
  class(idDynamicArray) :: this
  integer(i32) :: i
    !! Insert value at this location.
  integer(i64) :: val
    !! Insert this value.
  integer :: j, N
  if (i < 1 .or. i > this%N + 1) call Emsg('idDynamicArray%insertAt: 1 <= i <= '//str(this%N + 1))

  N = size(this%values)

  if (this%fixed) then
    if (i > N) call Emsg('idDynamicArray%insertAt: For fixed array, 1 <= i <= '//str(N))

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
  subroutine insertSorted_idDynamicArray(this,val)
    !! Overloaded type bound procedure idDynamicArray%insertSorted()
  !====================================================================!
  class(idDynamicArray) :: this
  integer(i64) :: val
    !! Insert this value.
  integer(i32) :: iSearch(3) ! location and interval of new value
  if (.not. this%sorted) call eMsg('idDynamicArray%insertSorted: Cannot use insertSorted with unsorted dynamic array')
  iSearch=intervalSearch(this%values, val, 1, this%N)
  call insertAt__idDynamicArray(this, iSearch(3), val)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine insertSortedUnique_idDynamicArray(this,val)
    !! Overloaded type bound procedure idDynamicArray%insertSortedUnique()
  !====================================================================!
  class(idDynamicArray) :: this
  integer(i64) :: val
    !! Insert this value.
  integer(i32) :: iSearch(3) ! location and interval of new value
  if (.not. this%sorted) call eMsg('idDynamicArray%insertSortedUnique: Cannot use insertSortedUnique with unsorted dynamic array')
  iSearch=intervalSearch(this%values, val, 1, this%N)
  if (iSearch(1) == -1) then
    call insertAt__idDynamicArray(this, iSearch(3), val)
  endif
  end subroutine
  !====================================================================!
  !====================================================================!
  function isEmpty_idDynamicArray(this) result(yes)
    !! Overloaded type bound procedure idDynamicArray%isEmpty()
  !====================================================================!
  class(idDynamicArray) :: this
  logical :: yes
    !! Array is empty
  yes = (this%N == 0)
  end function
  !====================================================================!
  !====================================================================!
  function isFilled_idDynamicArray(this) result(yes)
    !! Overloaded type bound procedure idDynamicArray%isFilled()
  !====================================================================!
  class(idDynamicArray) :: this
  logical :: yes
    !! Array is filled
  yes = (this%N == size(this%values))
  end function
  !====================================================================!
  !====================================================================!
  function locationOf_idDynamicArray(this, val) result(i)
    !! Overloaded type bound procedure idDynamicArray%locationOf().
  !====================================================================!
  class(idDynamicArray) :: this
  integer(i64) :: val
    !! Get the location of this value
  integer(i32) :: i
    !! Location of value
  if (.not. this%sorted) call eMsg('idDynamicArray%locationOf: Cannot use locationOf with unsorted dynamic array')
  i = binarySearch(this%values, val, 1, this%N)
  end function
  !====================================================================!
  !====================================================================!
  subroutine prepend_idDynamicArray(this,val)
    !! Overloaded type bound procedure idDynamicArray%prepend()
  !====================================================================!
  class(idDynamicArray) :: this
  integer(i64) :: val
    !! Value to prepend.
  if (this%fixed) call eMsg('idDynamicArray%prepend: Cannot use prepend with fixed array.')
  call insertAt__idDynamicArray(this, 1, val) ! Prepend at first location
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine reallocate_idDynamicArray(this, M)
    !! Overloaded type bound procedure idDynamicArray%reallocate().
  !====================================================================!
  class(idDynamicArray) :: this
  integer(i32) :: M
    !! Reallocate memory to this size.
  call reallocate(this%values, M)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine remove_idDynamicArray(this, i)
    !! Overloaded type bound procedure idDynamicArray%remove().
  !====================================================================!
  class(idDynamicArray) :: this
  integer(i32) :: i
    !! Remove the value at this location.
  integer(i32) :: j
  if (i < 1 .or. i > this%N) call Emsg('idDynamic%remove: 1 <= i <= '//str(this%N))
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
  subroutine tighten_idDynamicArray(this)
    !! Overloaded type bound procedure idDynamicArray%tighten().
  !====================================================================!
  class(idDynamicArray) :: this
  if (this%fixed) call eMsg('idDynamicArray%tighten: Cannot use tighten with fixed array.')
  call this%reallocate(this%N)
  end subroutine
  !====================================================================!

  !====================================================================!
  subroutine idDynamicArray_test(test)
    !! graph: false
  !====================================================================!
  class(tester) :: test

  type(idDynamicArray) :: idda, idda2

  integer(i32) :: ia
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
end module
