  submodule (m_array1D) m_Array_r1D
    !! Routines for single precision arrays

  implicit none

  contains
  !====================================================================!
  module procedure arange_r1D
    !! Interfaced with [[arange]]
  !====================================================================!
  !module function arange_r1D(start,stp,_step) result(this)
  !real(r32) :: start !! Start from here
  !real(r32) :: stp !! Stop here
  !real(r32) :: step !! Step size
  !real(r32), allocatable :: res(:)
  integer(i32) :: i
  integer(i32) :: N
  real(r32) :: step_
  step_ = 1.d0
  if (present(step)) step_ = step
  N=int((stp-start)/step_)+1
  if (size(res) /= N) call eMsg('arange_r1D:1D Array must be size '//str(N))
  if (step_ == 1.0) then
      do i = 1, N
          res(i) = start + real(i-1, kind=r32)
      enddo
  else
      do i = 1, N
          res(i) = start + real(i-1, kind=r32)*step_
      enddo
  endif
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure diff_r1D
    !! Interfaced [[diff]]
  !====================================================================!
!  real(r32), intent(in) :: this(:) !! 1D array
!  real(r32) :: res(size(this)-1) !! Difference along array
  integer(i32) :: i
  integer(i32) :: N
  N=size(this)
  if (size(res) /= N-1) call eMsg('diff_r1D:Result must be size '//str(N-1))
  do i=1,N-1
    res(i) = this(i+1) - this(i)
  end do
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure isSorted_r1D
    !! Interfaced with [[isSorted]]
  !====================================================================!
  !module function isSorted_r1D(this) result(yes)
  !real(r32):: this(:) !! 1D array
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
  module procedure isSorted_r1Di1D
    !! Interfaced with [[isSorted]]
  !====================================================================!
  !module function isSorted_d1D(this) result(yes)
  !real(r64):: this(:) !! 1D array
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
  module procedure repeat_r1D
    !! Interfaced with [[repeat]]
  !====================================================================!
!  real(r32) :: this(:) !! 1D array
!  integer(i32) :: nRepeats !! Number of times each element should be repeated
!  real(r32) :: res(size(this)*nRepeats)
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
  !====================================================================!
  module procedure shuffle_r1D
    !! Interfaced with [[shuffle]]
  !====================================================================!
  !module subroutine shuffle_r1D(this)
  !real(r32) :: this(:)
  integer(i32) :: i
  integer(i32) :: N
  integer(i32) :: r
  N=size(this)
  do i = 2, N
    call rngInteger(r, 1, i)
    call swap(this(i), this(r))
  end do
  end procedure
  !====================================================================!


end submodule
