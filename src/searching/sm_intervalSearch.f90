submodule (m_searching) sm_intervalSearch

use variableKind, only: r32, r64, i32, i64
implicit none

contains

  !====================================================================!
  module procedure intervalSearch_i1D!(this,v,imin,imax) result(iout)
    !! Perform a binary search on an integer vector
    !! Returns a length 3 integer(i32) vector where the last two entries are the left and right neighbours
    !! The first entry of iout is -1 if the value is not present in the vector
    !! Assumes this is sorted!
  !====================================================================!
!  integer(i32) :: this(:) !! Vector to search within
!  integer(i32) :: v !! Number to find in the vector
!  integer(i32) :: imin !! Left integer
!  integer(i32) :: imax !! Right integer
!  integer(i32) :: iout(3) !! Location of i in this. iout(1) = -1 if not present with iout(2-3) as the interval

  if (imax < imin) then
    iout(2)=imax;iout(3)=imin
    iout(1)=-1
    return
  else
    ! Cut the search in half
    iout(1)=(imax+imin)/2
    ! Three-Way Comparison
    if (this(iout(1)) > v) then
      ! Result is in lower subset
      iout(2)=imin;iout(3)=imax
      iout=intervalSearch_i1D(this,v,imin,iout(1)-1)
    elseif (this(iout(1)) < v) then
      ! Result is in upper subset
      iout(2)=imin;iout(3)=imax
      iout=intervalSearch_i1D(this,v,iout(1)+1,imax)
    else
      ! Result has been found
      iout(2)=iout(1)-1
      iout(2)=max(1,iout(2))
      iout(3)=iout(1)+1
      iout(3)=min(size(this),iout(3))
      return
    endif
  endif
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure intervalSearch_id1D!(this,v,imin,imax) result(iout)
    !! Perform a binary search on an integer vector
    !! Returns a length 3 integer(i32) vector where the last two entries are the left and right neighbours
    !! The first entry of iout is -1 if the value is not present in the vector
    !! Assumes this is sorted!
  !====================================================================!
!  integer(i64) :: this(:) !! Vector to search within
!  integer(i64) :: v !! Number to find in the vector
!  integer(i32) :: imin !! Left integer
!  integer(i32) :: imax !! Right integer
!  integer(i32) :: iout(3) !! Location of i in this. iout(1) = -1 if not present with iout(2-3) as the interval

  if (imax < imin) then
    iout(2)=imax;iout(3)=imin
    iout(1)=-1
    return
  else
    ! Cut the search in half
    iout(1)=(imax+imin)/2
    ! Three-Way Comparison
    if (this(iout(1)) > v) then
      ! Result is in lower subset
      iout(2)=imin;iout(3)=imax
      iout=intervalSearch_id1D(this,v,imin,iout(1)-1)
    elseif (this(iout(1)) < v) then
      ! Result is in upper subset
      iout(2)=imin;iout(3)=imax
      iout=intervalSearch_id1D(this,v,iout(1)+1,imax)
    else
      ! Result has been found
      iout(2)=iout(1)-1
      iout(2)=max(1,iout(2))
      iout(3)=iout(1)+1
      iout(3)=min(size(this),iout(3))
      return
    endif
  endif

  end procedure
  !====================================================================!
  !====================================================================!
  module procedure intervalSearch_r1D!(this,v,imin,imax) result(iout)
  !! Perform a binary search on a double precision vector
  !! Returns a length 3 integer(i32) vector where the last two entries are the left and right neighbours
  !! The first entry of iout is -1 if the value is not present in the vector
  !! Assumes this is sorted!
  !====================================================================!
!  real(r32) :: this(:) !! Vector to search within
!  real(r32) :: v !! Number to find in the vector
!  integer(i32) :: imin !! Left integer
!  integer(i32) :: imax !! Right integer
!  integer(i32) :: iout(3) !! Location of i in this. iout(1) = -1 if not present with iout(2-3) as the interval

  if (imax < imin) then
    iout(2)=imax;iout(3)=imin
    iout(1)=-1
    return
  else
    ! Cut the search in half
    iout(1)=(imax+imin)/2
    ! Three-Way Comparison
    if (this(iout(1)) > v) then
      ! Result is in lower subset
      iout(2)=imin;iout(3)=imax
      iout=intervalSearch_r1D(this,v,imin,iout(1)-1)
    elseif (this(iout(1)) < v) then
      ! Result is in upper subset
      iout(2)=imin;iout(3)=imax
      iout=intervalSearch_r1D(this,v,iout(1)+1,imax)
    else
      ! Result has been found
      iout(2)=iout(1)-1
      iout(2)=max(1,iout(2))
      iout(3)=iout(1)+1
      iout(3)=min(size(this),iout(3))
      return
    endif
  endif

  end procedure
  !====================================================================!
  !====================================================================!
  module procedure intervalSearch_d1D!(this,v,imin,imax) result(iout)
  !! Perform a binary search on a double precision vector
  !! Returns a length 3 integer(i32) vector where the last two entries are the left and right neighbours
  !! The first entry of iout is -1 if the value is not present in the vector
  !! Assumes this is sorted!
  !====================================================================!
!  real(r64) :: this(:) !! Vector to search within
!  real(r64) :: v !! Number to find in the vector
!  integer(i32) :: imin !! Left integer
!  integer(i32) :: imax !! Right integer
!  integer(i32) :: iout(3) !! Location of i in this. iout(1) = -1 if not present with iout(2-3) as the interval

  if (imax < imin) then
    iout(2)=imax;iout(3)=imin
    iout(1)=-1
    return
  else
    ! Cut the search in half
    iout(1)=(imax+imin)/2
    ! Three-Way Comparison
    if (this(iout(1)) > v) then
      ! Result is in lower subset
      iout(2)=imin;iout(3)=imax
      iout=intervalSearch_d1D(this,v,imin,iout(1)-1)
    elseif (this(iout(1)) < v) then
      ! Result is in upper subset
      iout(2)=imin;iout(3)=imax
      iout=intervalSearch_d1D(this,v,iout(1)+1,imax)
    else
      ! Result has been found
      iout(2)=iout(1)-1
      iout(2)=max(1,iout(2))
      iout(3)=iout(1)+1
      iout(3)=min(size(this),iout(3))
      return
    endif
  endif

  end procedure
  !====================================================================!
end submodule
