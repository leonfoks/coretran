  submodule (m_array1D) m_Array_id1D
    !! Routines for double precision integer arrays
  use variableKind
  use m_errors, only: mErr, eMsg
  use m_sort, only: argSort
  use m_strings, only: str
  implicit none

  contains
  !====================================================================!
  module procedure arange_id1D
    !! Interfaced with arange()
  !====================================================================!
  !module function arange_id1D(start,stp,_step) result(this)
  !integer(i64) :: start !! Start from here
  !integer(i64) :: stp !! Stop here
  !integer(i64) :: step !! Step size
  !integer(i64), allocatable :: res(:)
  integer(i64) :: i
  integer(i64) :: N
  integer(i64) :: step
  step=1
  if (present(step_)) step=step_
  N=((stp-start)/step_)+1
  if (size(res) /= N) call eMsg('arange_id1D:1D Array must be size '//str(N))
  res=[(start+(i-1)*step_,i=1,N)]
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure diff_id1D
    !! Interfaced diff()
  !====================================================================!
!  integer(i64), intent(in) :: this(:) !! 1D array
!  integer(i64) :: res(size(this)-1) !! Difference along array
  integer(i32) :: i
  integer(i32) :: N
  N=size(this)
  if (size(res) /= N-1) call eMsg('diff_id1D:Result must be size '//str(N-1))
  do i=1,N-1
    res(i) = this(i+1) - this(i)
  end do
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure isSorted_id1D
    !! Interfaced with isSorted()
  !====================================================================!
  !module function isSorted_id1D(this) result(yes)
  !integer(i64):: this(:) !! 1D array
  !logical :: yes !! isSorted
  integer :: i,N
  N=size(this)
  yes=.true.
  do i=2,N
    if (this(i) < this(i-1)) then
      yes=.false.
      return
    end if
  end do
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure repeat_id1D
    !! Interfaced with repeat()
  !====================================================================!
!  integer(i64) :: this(:) !! 1D array
!  integer(i32) :: nRepeats !! Number of times each element should be repeated
!  integer(i64) :: res(size(this)*nRepeats)
  integer(i32) :: i,k,N,nTmp
  N = size(this)
  nTmp = N*nRepeats
  if (size(res) /= nTmp) call eMsg('repeat_d1D:Result must be size '//str(nTmp))
  k=1
  do i = 1, N
    res(k:k+nRepeats-1) = this(i) ! Repeat the element
    k = k + nRepeats
  end do
  end procedure
  !====================================================================!
end submodule
