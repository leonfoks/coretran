submodule (m_searching) sm_BinarySearch

use variableKind, only: r32, r64, i32, i64
implicit none

contains

  !====================================================================!
  module procedure binarySearch_i1D!(this,v,imin,imax) result(iout)
    !! Search for the value i in an integer vector
    !! Assumes this is sorted!
  !====================================================================!
!  integer(i32) :: this(:) !! Vector to search within
!  integer(i32) :: v !! Number to find in the vector
!  integer(i32) :: imin !! Left integer
!  integer(i32) :: imax !! Right integer
!  integer(i32) :: iout !! Location of i in this. Returns -1 if not present

  if (imax < imin) then
    iout=-1
    return
  else
    ! Cut the search in half
    iout=(imax+imin)/2
    ! Three-Way Comparison
    if (this(iout) > v) then
      ! Result is in lower subset
      iout=binarySearch_i1D(this,v,imin,iout-1)
    elseif (this(iout) < v) then
      ! Result is in upper subset
      iout=binarySearch_i1D(this,v,iout+1,imax)
    else
      ! Result has been found
      return
    endif
  endif
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure binarySearch_id1D!(this,v,imin,imax) result(iout)
    !! Search for the value i in an integer vector
    !! Assumes this is sorted!
  !====================================================================!
!  integer(i64) :: this(:) !! Vector to search within
!  integer(i64) :: v !! Number to find in the vector
!  integer(i32) :: imin !! Left integer
!  integer(i32) :: imax !! Right integer
!  integer(i32) :: iout !! Location of i in this. Returns -1 if not present

  if (imax < imin) then
    iout=-1
    return
  else
    ! Cut the search in half
    iout=(imax+imin)/2
    ! Three-Way Comparison
    if (this(iout) > v) then
      ! Result is in lower subset
      iout=binarySearch_id1D(this,v,imin,iout-1)
    elseif (this(iout) < v) then
      ! Result is in upper subset
      iout=binarySearch_id1D(this,v,iout+1,imax)
    else
      ! Result has been found
      return
    endif
  endif
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure binarySearch_r1D!(this,v,imin,imax) result(iout)
    !! Search for the value i in an integer vector
    !! Assumes this is sorted!
  !====================================================================!
!  real(r32) :: this(:) !! Vector to search within
!  real(r32) :: v !! Number to find in the vector
!  integer(i32) :: imin !! Left integer
!  integer(i32) :: imax !! Right integer
!  integer(i32) :: iout !! Location of i in this. Returns -1 if not present

  if (imax < imin) then
    iout=-1
    return
  else
    ! Cut the search in half
    iout=(imax+imin)/2
    ! Three-Way Comparison
    if (this(iout) > v) then
      ! Result is in lower subset
      iout=binarySearch_r1D(this,v,imin,iout-1)
    elseif (this(iout) < v) then
      ! Result is in upper subset
      iout=binarySearch_r1D(this,v,iout+1,imax)
    else
      ! Result has been found
      return
    endif
  endif
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure binarySearch_d1D!(this,v,imin,imax) result(iout)
    !! Search for the value i in an integer vector
    !! Assumes this is sorted!
  !====================================================================!
!  real(r64) :: this(:) !! Vector to search within
!  real(r64) :: v !! Number to find in the vector
!  integer(i32) :: imin !! Left integer
!  integer(i32) :: imax !! Right integer
!  integer(i32) :: iout !! Location of i in this. Returns -1 if not present

  if (imax < imin) then
    iout=-1
    return
  else
    ! Cut the search in half
    iout=(imax+imin)/2
    ! Three-Way Comparison
    if (this(iout) > v) then
      ! Result is in lower subset
      iout=binarySearch_d1D(this,v,imin,iout-1)
    elseif (this(iout) < v) then
      ! Result is in upper subset
      iout=binarySearch_d1D(this,v,iout+1,imax)
    else
      ! Result has been found
      return
    endif
  endif
  end procedure
  !====================================================================!
end submodule
