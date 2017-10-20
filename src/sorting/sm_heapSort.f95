  submodule (m_sort) m_heapsort
    !! Heapsort routines see https://rosettacode.org/wiki/Sorting_algorithms/Heapsort#Fortran
  use variableKind
  use m_swap, only: swap

  implicit none

  contains
  !====================================================================!
  module procedure heapsort_r1D
  !====================================================================!
  integer(i32) :: start, bottom
  integer(i32) :: N
  N=size(this)
  do start =((n - 2) / 2), 0, -1
    call siftdown_r1D(this, start, n);
  end do
  do bottom = n-1 , 1, -1
    call swap(this(0),this(bottom))
    call siftdown_r1D(this, 0, bottom)
  end do
  end procedure
  !====================================================================!
  !====================================================================!
  subroutine siftdown_r1D(this, start, bottom)
  !====================================================================!
  real(r32) :: this(0:)
  integer(i32) :: start
  integer(i32) :: bottom
  integer(i32) :: i,j
  i = start
  do while((i*2)+1 < bottom)
    j=(i*2)+1
    if (j + 1 < bottom) then
      if (this(j) < this(j+1)) j = j + 1
    end if
    if (this(i) < this(j)) then
      call swap(this(i), this(j))
      i = j
    else
      return
    end if
  end do
  end subroutine
  !====================================================================!
  !====================================================================!
  module procedure heapsort_d1D
  !====================================================================!
  integer(i32) :: start, bottom
  integer(i32) :: N
  N=size(this)
  do start =((n - 2) / 2), 0, -1
    call siftdown_d1D(this, start, n);
  end do
  do bottom = n-1 , 1, -1
    call swap(this(0),this(bottom))
    call siftdown_d1D(this, 0, bottom)
  end do
  end procedure
  !====================================================================!
  !====================================================================!
  subroutine siftdown_d1D(this, start, bottom)
  !====================================================================!
  real(r64) :: this(0:)
  integer(i32) :: start
  integer(i32) :: bottom
  integer(i32) :: i,j
  i = start
  do while((i*2)+1 < bottom)
    j=(i*2)+1
    if (j + 1 < bottom) then
      if (this(j) < this(j+1)) j = j + 1
    end if
    if (this(i) < this(j)) then
      call swap(this(i), this(j))
      i = j
    else
      return
    end if
  end do
  end subroutine
  !====================================================================!
  !====================================================================!
  module procedure heapsort_i1D
  !====================================================================!
  integer(i32) :: start, bottom
  integer(i32) :: N
  N=size(this)
  do start =((n - 2) / 2), 0, -1
    call siftdown_i1D(this, start, n);
  end do
  do bottom = n-1 , 1, -1
    call swap(this(0),this(bottom))
    call siftdown_i1D(this, 0, bottom)
  end do
  end procedure
  !====================================================================!
  !====================================================================!
  subroutine siftdown_i1D(this, start, bottom)
  !====================================================================!
  integer(i32) :: this(0:)
  integer(i32) :: start
  integer(i32) :: bottom
  integer(i32) :: i,j
  i = start
  do while((i*2)+1 < bottom)
    j=(i*2)+1
    if (j + 1 < bottom) then
      if (this(j) < this(j+1)) j = j + 1
    end if
    if (this(i) < this(j)) then
      call swap(this(i), this(j))
      i = j
    else
      return
    end if
  end do
  end subroutine
  !====================================================================!
  !====================================================================!
  module procedure heapsort_id1D
  !====================================================================!
  integer(i32) :: start, bottom
  integer(i32) :: N
  N=size(this)
  do start =((n - 2) / 2), 0, -1
    call siftdown_id1D(this, start, n);
  end do
  do bottom = n-1 , 1, -1
    call swap(this(0),this(bottom))
    call siftdown_id1D(this, 0, bottom)
  end do
  end procedure
  !====================================================================!
  !====================================================================!
  subroutine siftdown_id1D(this, start, bottom)
  !====================================================================!
  integer(i64) :: this(0:)
  integer(i32) :: start
  integer(i32) :: bottom
  integer(i32) :: i,j
  i = start
  do while((i*2)+1 < bottom)
    j=(i*2)+1
    if (j + 1 < bottom) then
      if (this(j) < this(j+1)) j = j + 1
    end if
    if (this(i) < this(j)) then
      call swap(this(i), this(j))
      i = j
    else
      return
    end if
  end do
  end subroutine
  !====================================================================!
  !====================================================================!
  module procedure argHeapsort_r1D
  !====================================================================!
  integer(i32) :: start, bottom
  integer(i32) :: N
  N=size(indx)
  do start =((n - 2) / 2), 0, -1
    call argSiftdown_r1D(this, indx, start, n);
  end do
  do bottom = n-1 , 1, -1
    call swap(indx(0),indx(bottom))
    call argSiftdown_r1D(this, indx, 0, bottom)
  end do
  end procedure
  !====================================================================!
  !====================================================================!
  subroutine argSiftdown_r1D(this, indx, start, bottom)
  !====================================================================!
  real(r32) :: this(:)
  integer(i32) :: indx(0:)
  integer(i32) :: start
  integer(i32) :: bottom
  integer(i32) :: i,j
  i = start
  do while((i*2)+1 < bottom)
    j=(i*2)+1
    if (j + 1 < bottom) then
      if (this(indx(j)) < this(indx(j+1))) j = j + 1
    end if
    if (this(indx(i)) < this(indx(j))) then
      call swap(indx(i), indx(j))
      i = j
    else
      return
    end if
  end do
  end subroutine
  !====================================================================!
  !====================================================================!
  module procedure argHeapsort_d1D
  !====================================================================!
  integer(i32) :: start, bottom
  integer(i32) :: N
  N=size(indx)
  do start =((n - 2) / 2), 0, -1
    call argSiftdown_d1D(this, indx, start, n);
  end do
  do bottom = n-1 , 1, -1
    call swap(indx(0),indx(bottom))
    call argSiftdown_d1D(this, indx, 0, bottom)
  end do
  end procedure
  !====================================================================!
  !====================================================================!
  subroutine argSiftdown_d1D(this, indx, start, bottom)
  !====================================================================!
  real(r64) :: this(:)
  integer(i32) :: indx(0:)
  integer(i32) :: start
  integer(i32) :: bottom
  integer(i32) :: i,j
  i = start
  do while((i*2)+1 < bottom)
    j=(i*2)+1
    if (j + 1 < bottom) then
      if (this(indx(j)) < this(indx(j+1))) j = j + 1
    end if
    if (this(indx(i)) < this(indx(j))) then
      call swap(indx(i), indx(j))
      i = j
    else
      return
    end if
  end do
  end subroutine
  !====================================================================!
  !====================================================================!
  module procedure argHeapsort_i1D
  !====================================================================!
  integer(i32) :: start, bottom
  integer(i32) :: N
  N=size(indx)
  do start =((n - 2) / 2), 0, -1
    call argSiftdown_i1D(this, indx, start, n);
  end do
  do bottom = n-1 , 1, -1
    call swap(indx(0),indx(bottom))
    call argSiftdown_i1D(this, indx, 0, bottom)
  end do
  end procedure
  !====================================================================!
  !====================================================================!
  subroutine argSiftdown_i1D(this, indx, start, bottom)
  !====================================================================!
  integer(i32) :: this(:)
  integer(i32) :: indx(0:)
  integer(i32) :: start
  integer(i32) :: bottom
  integer(i32) :: i,j
  i = start
  do while((i*2)+1 < bottom)
    j=(i*2)+1
    if (j + 1 < bottom) then
      if (this(indx(j)) < this(indx(j+1))) j = j + 1
    end if
    if (this(indx(i)) < this(indx(j))) then
      call swap(indx(i), indx(j))
      i = j
    else
      return
    end if
  end do
  end subroutine
  !====================================================================!
  !====================================================================!
  module procedure argHeapsort_id1D
  !====================================================================!
  integer(i32) :: start, bottom
  integer(i32) :: N
  N=size(indx)
  do start =((n - 2) / 2), 0, -1
    call argSiftdown_id1D(this, indx, start, n);
  end do
  do bottom = n-1 , 1, -1
    call swap(indx(0),indx(bottom))
    call argSiftdown_id1D(this, indx, 0, bottom)
  end do
  end procedure
  !====================================================================!
  !====================================================================!
  subroutine argSiftdown_id1D(this, indx, start, bottom)
  !====================================================================!
  integer(i64) :: this(:)
  integer(i32) :: indx(0:)
  integer(i32) :: start
  integer(i32) :: bottom
  integer(i32) :: i,j
  i = start
  do while((i*2)+1 < bottom)
    j=(i*2)+1
    if (j + 1 < bottom) then
      if (this(indx(j)) < this(indx(j+1))) j = j + 1
    end if
    if (this(indx(i)) < this(indx(j))) then
      call swap(indx(i), indx(j))
      i = j
    else
      return
    end if
  end do
  end subroutine
  !====================================================================!
end submodule
