  submodule (m_array1D) m_Array_i1D
    !! Routines for integer arrays
  use variableKind
  use m_errors, only: mErr, eMsg
  use m_sort, only: argSort
  use m_strings, only: str
  implicit none

  contains
  !====================================================================!
  module procedure arange_i1D
    !! Interfaced with [[arange]]
  !====================================================================!
  !module function arange_i1D(start,stp,_step) result(this)
  !integer(i32) :: start !! Start from here
  !integer(i32) :: stp !! Stop here
  !integer(i32) :: step !! Step size
  !integer(i32), allocatable :: res(:)
  integer(i32) :: i
  integer(i32) :: N
  integer(i32) :: step_
  step_ = 1
  if (present(step)) step_ = step
  N=(stp-start)/step_ + 1
  if (size(res) /= N) call eMsg('arange_i1D:1D Array must be size '//str(N))
  if (step_ == 1) then
      do i = 1, N
          res(i) = start + i-1
      enddo
  else
      do i = 1, N
          res(i) = start + (i-1)*step_
      enddo
  endif
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure diff_i1D
    !! Interfaced [[diff]]
  !====================================================================!
!  integer(i32), intent(in) :: this(:) !! 1D array
!  integer(i32) :: res(size(this)-1) !! Difference along array
  integer(i32) :: i
  integer(i32) :: N
  N=size(this)
  if (size(res) /= N-1) call eMsg('diff_i1D:Result must be size '//str(N-1))
  do i=1,N-1
    res(i) = this(i+1) - this(i)
  end do
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure isSorted_i1D
    !! Interfaced with [[isSorted]]
  !====================================================================!
  !module function isSorted_i1D(this) result(yes)
  !integer(i32):: this(:) !! 1D array
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
  module procedure isSorted_i1Di1D
    !! Interfaced with [[isSorted]]
  !====================================================================!
  !module function isSorted_d1D(this) result(yes)
  !integer(i32) :: this(:) !! 1D array
  !integer(i32) :: indx(:)
  !logical :: yes !! isSorted
  integer :: i,N
  N=size(this)
  yes=.true.
  do i=2,N
    if (this(indx(i)) < this(indx(i-1))) then
      yes=.false.
      return
    end if
  end do
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure repeat_i1D
    !! Interfaced with [[repeat]]
  !====================================================================!
!  integer(i32) :: this(:) !! 1D array
!  integer(i32) :: nRepeats !! Number of times each element should be repeated
!  integer(i32) :: res(size(this)*nRepeats)
  integer(i32) :: i,k,N,nTmp
  N = size(this)
  nTmp = N*nRepeats
  call allocate(res, nTmp)
  !if (size(res) /= nTmp) call eMsg('repeat_d1D:Result must be size '//str(nTmp))
  k=1
  do i = 1, N
    res(k:k+nRepeats-1) = this(i) ! Repeat the element
    k = k + nRepeats
  end do
  end procedure
  !====================================================================!
end submodule
