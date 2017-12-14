submodule (m_searching) sm_simpleSearch
  !! Contains simple search routines

implicit none

contains

  !====================================================================!
  module procedure simpleSearch_i1D!(this, val) result(iout)
    !! Simple search over an integer vector
  !====================================================================!
!  integer(i32) :: this(:)
    !! Search this vector
!  integer(i32) :: val
    !! Number to find in the vector
!  integer(i32) :: iout
    !! Location of i in this
  integer(i32) :: i
  integer(i32) :: N
  N = size(this)
  iout = -1
  do i = 1, N
    if (this(i) == val) then
      iout = i
      return
    endif
  enddo
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure simpleSearch_id1D!(this, val) result(iout)
    !! Simple search over an integer vector
  !====================================================================!
!  integer(i64) :: this(:)
    !! Search this vector
!  integer(i64) :: val
    !! Number to find in the vector
!  integer(i32) :: iout
    !! Location of i in this
  integer(i32) :: i
  integer(i32) :: N
  N = size(this)
  iout = -1
  do i = 1, N
    if (this(i) == val) then
      iout = i
      return
    endif
  enddo
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure simpleSearch_r1D!(this, val) result(iout)
    !! Simple search over an integer vector
  !====================================================================!
!  real(r32) :: this(:)
    !! Search this vector
!  real(r32) :: val
    !! Number to find in the vector
!  integer(i32) :: iout
    !! Location of i in this
  integer(i32) :: i
  integer(i32) :: N
  N=size(this)
  iout=-1
  do i=1,N
    if (this(i)==val) then
      iout=i
      return
    endif
  enddo
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure simpleSearch_d1D!(this, val) result(iout)
    !! Simple search over an integer vector
  !====================================================================!
!  real(r64) :: this(:)
    !! Search this vector
!  real(r64) :: val
    !! Number to find in the vector
!  integer(i32) :: iout
    !! Location of i in this
  integer(i32) :: i
  integer(i32) :: N
  N=size(this)
  iout=-1
  do i=1,N
    if (this(i)==val) then
      iout=i
      return
    endif
  enddo
  end procedure
  !====================================================================!
end submodule
