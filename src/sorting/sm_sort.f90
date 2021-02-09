  submodule (m_sort) sm_sort
    !! Sorting routines, by default uses a introsort. If stable_ is true, then a merge sort is used instead.

  implicit none

  contains
  !====================================================================!
  module procedure sort_i1D
    !! Interfaced with sort()
  !====================================================================!
  !module subroutine Sort_i1D(this, stable)
  !integer(i32) :: this(:)
  !logical, optional :: stable
  logical :: stable_=.false.
  if (present(stable)) stable_=stable
  if (stable_) then
    call MergeSort_i1D(this)
  else
    call introsort_i1D(this)
  end if
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure sort_id1D
    !! Interfaced with sort()
  !====================================================================!
  !module subroutine Sort_id1D(this, stable)
  !integer(i64) :: this(:)
  !logical, optional :: stable
  logical :: stable_=.false.
  if (present(stable)) stable_=stable
  if (stable_) then
    call MergeSort_id1D(this)
  else
    call introsort_id1D(this)
  end if
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure sort_r1D
    !! Interfaced with sort()
  !====================================================================!
  !module subroutine Sort_r1D(this, stable)
  !real(r32) :: this(:)
  !logical, optional :: stable
  logical :: stable_=.false.
  if (present(stable)) stable_=stable
  if (stable_) then
    call MergeSort_r1D(this)
  else
    call introsort_r1D(this)
  end if
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure sort_d1D
    !! Interfaced with sort()
  !====================================================================!
  !module subroutine Sort_r1D(this, stable)
  !real(r64) :: this(:)
  !logical, optional :: stable
  logical :: stable_=.false.
  if (present(stable)) stable_=stable
  if (stable_) then
    call MergeSort_d1D(this)
  else
    call introsort_d1D(this)
  end if
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure argSort_r1D
    !! Interfaced with argsort()
  !====================================================================!
  !real(r32) :: this(:)
  !integer(i32) :: i(:)
  !logical, optional :: stable
  logical :: stable_=.false.
  if (present(stable)) stable_=stable
  if (stable_) then
    call argMergeSort_r1D(this,i)
  else
    call argintrosort_r1D(this,i)
  end if
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure argSort_d1D
    !! Interfaced with argsort()
  !====================================================================!
  !real(r64) :: this(:)
  !integer(i32) :: i(:)
  !logical, optional :: stable
  logical :: stable_=.false.
  if (present(stable)) stable_=stable
  if (stable_) then
    call argMergeSort_d1D(this,i)
  else
    call argintrosort_d1D(this,i)
  end if
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure argSort_i1D
    !! Interfaced with argsort()
  !====================================================================!
  !integer(i32) :: this(:)
  !integer(i32) :: i(:)
  !logical, optional :: stable
  logical :: stable_=.false.
  if (present(stable)) stable_=stable
  if (stable_) then
    call argMergeSort_i1D(this,i)
  else
    call argintrosort_i1D(this,i)
  end if
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure argSort_id1D
    !! Interfaced with argsort()
  !====================================================================!
  !integer(i64) :: this(:)
  !integer(i32) :: i(:)
  !logical, optional :: stable
  logical :: stable_=.false.
  if (present(stable)) stable_=stable
  if (stable_) then
    call argMergeSort_id1D(this,i)
  else
    call argintrosort_id1D(this,i)
  end if
  end procedure
  !====================================================================!
!!  !====================================================================!
!!  subroutine Sort_d2D(this,along)
!!  !====================================================================!
!!  real(r64) :: this(:,:)
!!  integer(i32) :: along
!!  call Heapsort(this,along)
!!  end subroutine
!!  !====================================================================!
  end submodule
