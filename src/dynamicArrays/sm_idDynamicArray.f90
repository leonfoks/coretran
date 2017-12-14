submodule (m_dynamicArray) sm_idDynamicArray

use m_allocate, only: allocate
use m_searching, only: binarySearch, intervalSearch
use m_deallocate, only: deallocate
use m_errors, only: eMsg
use m_reallocate, only: reallocate
use m_sort, only: sort
use m_strings, only: str

implicit none


contains

  !====================================================================!
  module procedure append_idDynamicArray!(this,val)
    !! Overloaded type bound procedure idDynamicArray%append()
  !====================================================================!
!  class(idDynamicArray) :: this
!  real(r64) :: val
  if (this%fixed) call eMsg('idDynamicArray%append: Cannot use append with fixed array.')
  call this%insertAt(this%N + 1, val) ! Append at last location
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure copy_idDynamicArray!(new,this)
    !! Overloaded assignment of equals.  new = this
  !====================================================================!
!  class(idDynamicArray), intent(in) :: this
!  type(idDynamicArray), intent(out) :: new
  call allocate(new%values, size(this%values))
  new%N = this%N
  new%values = this%values
  new%sorted = this%sorted
  new%fixed = this%fixed
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure deallocate_idDynamicArray!(this)
    !! Overloaded type bound procedure idDynamicArray%deallocate()
  !====================================================================!
!  class(idDynamicArray) :: this
  call deallocate(this%values)
  this%N = 0
  this%sorted = .false.
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure init_idDynamicArray_i1!(M) result(this)
  !====================================================================!
  ! type(idDynamicArray) :: this
  ! integer(i32), intent(in), optional :: M
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
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure init_idDynamicArray_d1D!(values, M) result(this)
  !====================================================================!
  ! type(idDynamicArray) :: this
  ! real(r64) :: values(:)
  ! integer(i32), optional :: M
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
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure insertAt_idDynamicArray!(this,i,val)
    !! Overloaded type bound procedure idDynamicArray%insertAt()
  !====================================================================!
!  class(idDynamicArray) :: this
!  integer(i32) :: i ! Insert at this location
!  real(r64) :: val
  integer :: j, N
  if (i < 1 .or. i > this%N + 1) call Emsg('idDynamicArray%insert: 1 <= i <= '//str(this%N + 1))

  N = size(this%values)

  if (this%fixed) then
    if (i > N) call Emsg('idDynamicArray%insert: For fixed array, 1 <= i <= '//str(N))

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
  this%sorted = .false.
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure insertSorted_idDynamicArray!(this,val)
    !! Overloaded type bound procedure idDynamicArray%insertSorted()
  !====================================================================!
  !class(idDynamicArray) :: this
  !real(r64) :: val
  integer(i32) :: iSearch(3) ! location and interval of new value
  if (.not. this%sorted) call eMsg('idDynamicArray%insertSorted: Cannot use insertSorted with unsorted dynamic array')
  iSearch=intervalSearch(this%values, val, 1, this%N)
  call this%insertAt(iSearch(3), val)
  this%sorted = .true.
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure insertSortedUnique_idDynamicArray!(this,val)
    !! Overloaded type bound procedure idDynamicArray%insertSortedUnique()
  !====================================================================!
!  class(idDynamicArray) :: this
!  real(r64) :: val
  integer(i32) :: iSearch(3) ! location and interval of new value
  if (.not. this%sorted) call eMsg('idDynamicArray%insertSortedUnique: Cannot use insertSortedUnique with unsorted dynamic array')
  iSearch=intervalSearch(this%values, val, 1, this%N)
  if (iSearch(1) == -1) then
    call this%insertAt(iSearch(3), val)
    this%sorted = .true.
  endif
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure locationOf_idDynamicArray!(this, val) result(i)
    !! Overloaded type bound procedure idDynamicArray%locationOf().
  !====================================================================!
!  class(idDynamicArray) :: this
!  real(r64) :: val
!  integer(i32) :: i
  if (.not. this%sorted) call eMsg('idDynamicArray%locationOf: Cannot use locationOf with unsorted dynamic array')
  i = binarySearch(this%values, val, 1, this%N)
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure prepend_idDynamicArray!(this,val)
    !! Overloaded type bound procedure idDynamicArray%prepend()
  !====================================================================!
!  class(idDynamicArray) :: this
!  real(r64) :: val
  if (this%fixed) call eMsg('idDynamicArray%prepend: Cannot use prepend with fixed array.')
  call this%insertAt(1, val) ! Prepend at first location
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure reallocate_idDynamicArray!(this, M)
    !! Overloaded type bound procedure idDynamicArray%reallocate().
  !====================================================================!
!  class(idDynamicArray) :: this
!  integer(i32) :: M
  call reallocate(this%values, M)
  end procedure
  !====================================================================!
  !====================================================================!
  module subroutine remove_idDynamicArray(this, i)
    !! Overloaded type bound procedure idDynamicArray%remove().
  !====================================================================!
  class(idDynamicArray) :: this
  integer(i32) :: i
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
  module subroutine tighten_idDynamicArray(this)
    !! Overloaded type bound procedure idDynamicArray%tighten().
  !====================================================================!
  class(idDynamicArray) :: this
  if (this%fixed) call eMsg('idDynamicArray%tighten: Cannot use tighten with fixed array.')
  call this%reallocate(this%N)
  end subroutine
  !====================================================================!
end submodule