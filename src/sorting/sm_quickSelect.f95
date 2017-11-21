submodule (m_Select) sm_quickselect
  !! quickselect and argQuickselect routines

  use variableKind
  use m_swap, only: swap
  use m_random, only: rngInteger
  use m_partition, only: partition, argPartition
  use m_medianOf3, only: medianOf3, argMedianOf3
  use m_sort, only: insertionSort, argInsertionSort
  implicit none

  contains
  !====================================================================!
  module procedure quickSelect_r1D
    !! Interfaced with quickselect()
  !====================================================================!
  !real(r32), intent(inout) :: this(:) !! 1D array
  !integer(i32), intent(in) :: k !! kth smallest element to find
  !real(r32) :: res !! kth element
  integer(i32) :: hi
  integer(i32) :: j
  integer(i32) :: lo
  integer(i32) :: mid

  lo = 1
  hi = size(this)

  do while (hi > lo)
    if (hi - lo < 16) then
      call insertionSort(this, lo, hi)
      res = this(k)
      return
    end if
    mid = (hi+lo)/2
    call medianOf3(this, lo, mid, hi)
    call swap(this(lo), this(mid))
    call partition(this, lo, hi, j)
    ! Found the kth value, exit
    if (j == k) then
      res = this(k)
      return
    end if
    if (j < k) then
      lo = j + 1
    elseif (j > k) then
      hi = j - 1
    end if
  end do
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure quickSelect_d1D !(this, k) result(res)
    !! Interfaced with quickselect()
  !====================================================================!
  !real(r64), intent(inout) :: this(:) !! 1D array
  !integer(i32), intent(in) :: k !! kth smallest element to find
  !real(r64) :: res !! kth element
  integer(i32) :: hi
  integer(i32) :: j
  integer(i32) :: lo
  integer(i32) :: mid

  lo = 1
  hi = size(this)

  do while (hi > lo)
    if (hi - lo < 16) then
      call insertionSort(this, lo, hi)
      res = this(k)
      return
    end if
    mid = (hi+lo)/2
    call medianOf3(this, lo, mid, hi)
    call swap(this(lo), this(mid))
    call partition(this, lo, hi, j)
    ! Found the kth value, exit
    if (j == k) then
      res = this(k)
      return
    end if
    if (j < k) then
      lo = j + 1
    elseif (j > k) then
      hi = j - 1
    end if
  end do
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure quickSelect_i1D !(this, k) result(res)
    !! Interfaced with quickselect()
  !====================================================================!
  !integer(i32), intent(inout) :: this(:) !! 1D array
  !integer(i32), intent(in) :: k !! kth smallest element to find
  !integer(i32) :: res !! kth element
  integer(i32) :: hi
  integer(i32) :: j
  integer(i32) :: lo
  integer(i32) :: mid

  lo = 1
  hi = size(this)

  do while (hi > lo)
    if (hi - lo < 16) then
      call insertionSort(this, lo, hi)
      res = this(k)
      return
    end if
    mid = (hi+lo)/2
    call medianOf3(this, lo, mid, hi)
    call swap(this(lo), this(mid))
    call partition(this, lo, hi, j)
    ! Found the kth value, exit
    if (j == k) then
      res = this(k)
      return
    end if
    if (j < k) then
      lo = j + 1
    elseif (j > k) then
      hi = j - 1
    end if
  end do
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure quickSelect_id1D !(this, k) result(res)
    !! Interfaced with quickselect()
  !====================================================================!
  !integer(i64), intent(inout) :: this(:) !! 1D array
  !integer(i32), intent(in) :: k !! kth smallest element to find
  !integer(i64) :: res !! kth element
  integer(i32) :: hi
  integer(i32) :: j
  integer(i32) :: lo
  integer(i32) :: mid

  lo = 1
  hi = size(this)

  do while (hi > lo)
    if (hi - lo < 16) then
      call insertionSort(this, lo, hi)
      res = this(k)
      return
    end if
    mid = (hi+lo)/2
    call medianOf3(this, lo, mid, hi)
    call swap(this(lo), this(mid))
    call partition(this, lo, hi, j)
    ! Found the kth value, exit
    if (j == k) then
      res = this(k)
      return
    end if
    if (j < k) then
      lo = j + 1
    elseif (j > k) then
      hi = j - 1
    end if
  end do
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure argQuickselect_r1D !(this, indx, k, left, right) result(res)
    !! Interfaced with argQuickselect()
  !====================================================================!
  !real(r32), intent(in) :: this(:) !! 1D array
  !integer(i32), intent(inout) :: indx(:) !! Index into array
  !integer(i32), intent(in) :: k !! kth smallest element to find
  !integer(i32) :: res !! kth index
  integer(i32) :: hi
  integer(i32) :: j
  integer(i32) :: lo
  integer(i32) :: mid

  lo = 1
  hi = size(indx)
  if (present(left)) lo = left
  if (present(right)) hi = right

  do while (hi > lo)
    if (hi - lo < 16) then
      call argInsertionSort(this, indx, lo, hi)
      res = indx(k)
      return
    end if
    mid = (hi+lo)/2
    call argMedianOf3(this, indx, lo, mid, hi)
    call swap(indx(lo), indx(mid))
    call argPartition(this, indx, lo, hi, j)
    ! Found the kth value, exit
    if (j == k) then
      res = indx(k)
      return
    end if
    if (j < k) then
      lo = j + 1
    elseif (j > k) then
      hi = j - 1
    end if
  end do
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure argQuickselect_d1D
    !! Interfaced with argQuickselect()
  !====================================================================!
  !real(r64), intent(in) :: this(:) !! 1D array
  !integer(i32), intent(inout) :: indx(:) !! Index into array
  !integer(i32), intent(in) :: k !! kth smallest element to find
  !integer(i32) :: res !! kth index
  integer(i32) :: hi
  integer(i32) :: j
  integer(i32) :: lo
  integer(i32) :: mid

  lo = 1
  hi = size(indx)
  if (present(left)) lo = left
  if (present(right)) hi = right

  do while (hi > lo)
    if (hi - lo < 16) then
      call argInsertionSort(this, indx, lo, hi)
      res = indx(k)
      return
    end if
    mid = (hi+lo)/2
    call argMedianOf3(this, indx, lo, mid, hi)
    call swap(indx(lo), indx(mid))
    call argPartition(this, indx, lo, hi, j)
    ! Found the kth value, exit
    if (j == k) then
      res = indx(k)
      return
    end if
    if (j < k) then
      lo = j + 1
    elseif (j > k) then
      hi = j - 1
    end if
  end do
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure argQuickselect_i1D !(this, indx, k, left, right) result(res)
    !! Interfaced with argQuickselect()
  !====================================================================!
  !integer(i32), intent(in) :: this(:) !! 1D array
  !integer(i32), intent(inout) :: indx(:) !! Index into array
  !integer(i32), intent(in) :: k !! kth smallest element to find
  !integer(i32) :: res !! kth index
  integer(i32) :: hi
  integer(i32) :: j
  integer(i32) :: lo
  integer(i32) :: mid

  lo = 1
  hi = size(indx)
  if (present(left)) lo = left
  if (present(right)) hi = right

  do while (hi > lo)
    if (hi - lo < 16) then
      call argInsertionSort(this, indx, lo, hi)
      res = indx(k)
      return
    end if
    mid = (hi+lo)/2
    call argMedianOf3(this, indx, lo, mid, hi)
    call swap(indx(lo), indx(mid))
    call argPartition(this, indx, lo, hi, j)
    ! Found the kth value, exit
    if (j == k) then
      res = indx(k)
      return
    end if
    if (j < k) then
      lo = j + 1
    elseif (j > k) then
      hi = j - 1
    end if
  end do
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure argQuickselect_id1D !(this, indx, k, left, right) result(res)
    !! Interfaced with argQuickselect()
  !====================================================================!
  !integer(i64), intent(in) :: this(:) !! 1D array
  !integer(i32), intent(inout) :: indx(:) !! Index into array
  !integer(i32), intent(in) :: k !! kth smallest element to find
  !integer(i32) :: res !! kth index
  integer(i32) :: hi
  integer(i32) :: j
  integer(i32) :: lo
  integer(i32) :: mid

  lo = 1
  hi = size(indx)
  if (present(left)) lo = left
  if (present(right)) hi = right

  do while (hi > lo)
    if (hi - lo < 16) then
      call argInsertionSort(this, indx, lo, hi)
      res = indx(k)
      return
    end if
    mid = (hi+lo)/2
    call argMedianOf3(this, indx, lo, mid, hi)
    call swap(indx(lo), indx(mid))
    call argPartition(this, indx, lo, hi, j)
    ! Found the kth value, exit
    if (j == k) then
      res = indx(k)
      return
    end if
    if (j < k) then
      lo = j + 1
    elseif (j > k) then
      hi = j - 1
    end if
  end do
  end procedure
  !====================================================================!
end submodule
