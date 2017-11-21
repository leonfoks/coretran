  submodule (m_sort) sm_sort
    !! Sorting routines, by default uses a introsort. If stable_ is true, then a merge sort is used instead.
  use variableKind

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
    call MergeSort(this)
  else
    call introsort(this)
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
    call MergeSort(this)
  else
    call introsort(this)
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
    call MergeSort(this)
  else
    call introsort(this)
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
    call MergeSort(this)
  else
    call introsort(this)
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
    call argMergeSort(this,i)
  else
    call argintrosort(this,i)
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
    call argMergeSort(this,i)
  else
    call argintrosort(this,i)
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
    call argMergeSort(this,i)
  else
    call argintrosort(this,i)
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
    call argMergeSort(this,i)
  else
    call argintrosort(this,i)
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
