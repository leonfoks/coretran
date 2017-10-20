module m_simpleSearch
  !! Contains simple search routines

implicit none

private

public simpleSearch

interface simpleSearch
module procedure :: simpleSearch_i1D
end interface

contains
  !====================================================================!
  function simpleSearch_i1D(this,i) result(iout)
    !! Simple search over an integer vector
  !====================================================================!
  integer(i32) :: this(:)
    !! Search this vector
  integer(i32) :: i
    !! Number to find in the vector
  integer(i32) :: iout
    !! Location of i in this
  integer(i32) :: ii
  integer(i32) :: N
  N=size(this)
  iout=-1
  do ii=1,N
    if (this(ii)==i) then
      iout=ii
      return
    endif
  enddo
  end function
  !====================================================================!
end module
