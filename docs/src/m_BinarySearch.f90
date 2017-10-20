  module m_BinarySearch
    !! Contains routines to perform a simple binary search on a vector
  use variableKind
  implicit none

  private

  public :: binarySearch
  public :: binarySearch_wNeighbours

  interface binarySearch
    !! Perform a binary search
    !!
    !! Example usage
    !!```fortran
    !!use variableKind
    !!use m_BinarySearch, only: binarySearch
    !!real(r64) :: arr(20)
    !!integer(i32) :: i
    !!integer(i32) :: j
    !!arr=[(dble(i), i = 1, 20)]
    !!j = binarySearch(arr, 10.d0, 1, 20)
    !!write(*,*) 'Location of 10.0 in arr is 10? ',j == 10
    !!```
    module procedure :: binarySearch_r1D, binarySearch_d1D, binarySearch_i1D, binarySearch_id1D
  end interface

  interface binarySearch_wNeighbours
    !! Perform a binary search but also return the neighbours.
    !! This is useful if you need to find a number that is not contained in the array and you want the interval
    !!
    !! Example usage
    !!```fortran
    !!use variableKind
    !!use m_BinarySearch, only: binarySearch_wNeighbours
    !!real(r64) :: arr(20)
    !!integer(i32) :: i
    !!integer(i32) :: j(3)
    !!arr=[(dble(i), i = 1, 20)]
    !!j = binarySearch_wNeighbours(arr, 10.5d0, 1, 20)
    !!write(*,*) 'Location of 10.5 in arr is -1? ',j(1) == -1
    !!write(*,*) 'The interval containing 10.5 is [10,11]? ',j(2:3) == [10,11]
    !!```
    module procedure :: binarySearch_wNeighbours_r1D,binarySearch_wNeighbours_d1D,binarySearch_wNeighbours_i1D,binarySearch_wNeighbours_id1D
  end interface

  contains
  !====================================================================!
  recursive function binarySearch_i1D(this,v,imin,imax) result(iout)
    !! Search for the value i in an integer vector
    !! Assumes this is sorted!
  !====================================================================!
  integer(i32) :: this(:) !! Vector to search within
  integer(i32) :: v !! Number to find in the vector
  integer(i32) :: imin !! Left integer
  integer(i32) :: imax !! Right integer
  integer(i32) :: iout !! Location of i in this. Returns -1 if not present

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
  end function
  !====================================================================!
  !====================================================================!
  recursive function binarySearch_id1D(this,v,imin,imax) result(iout)
    !! Search for the value i in an integer vector
    !! Assumes this is sorted!
  !====================================================================!
  integer(i64) :: this(:) !! Vector to search within
  integer(i64) :: v !! Number to find in the vector
  integer(i32) :: imin !! Left integer
  integer(i32) :: imax !! Right integer
  integer(i32) :: iout !! Location of i in this. Returns -1 if not present

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
  end function
  !====================================================================!
  !====================================================================!
  recursive function binarySearch_r1D(this,v,imin,imax) result(iout)
    !! Search for the value i in an integer vector
    !! Assumes this is sorted!
  !====================================================================!
  real(r32) :: this(:) !! Vector to search within
  real(r32) :: v !! Number to find in the vector
  integer(i32) :: imin !! Left integer
  integer(i32) :: imax !! Right integer
  integer(i32) :: iout !! Location of i in this. Returns -1 if not present

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
  end function
  !====================================================================!
  !====================================================================!
  recursive function binarySearch_d1D(this,v,imin,imax) result(iout)
    !! Search for the value i in an integer vector
    !! Assumes this is sorted!
  !====================================================================!
  real(r64) :: this(:) !! Vector to search within
  real(r64) :: v !! Number to find in the vector
  integer(i32) :: imin !! Left integer
  integer(i32) :: imax !! Right integer
  integer(i32) :: iout !! Location of i in this. Returns -1 if not present

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
  end function
  !====================================================================!
  !====================================================================!
  recursive function binarySearch_wNeighbours_i1D(this,v,imin,imax) result(iout)
    !! Perform a binary search on an integer vector
    !! Returns a length 3 integer(i32) vector where the last two entries are the left and right neighbours
    !! The first entry of iout is -1 if the value is not present in the vector
    !! Assumes this is sorted!
  !====================================================================!
  integer(i32) :: this(:) !! Vector to search within
  integer(i32) :: v !! Number to find in the vector
  integer(i32) :: imin !! Left integer
  integer(i32) :: imax !! Right integer
  integer(i32) :: iout(3) !! Location of i in this. iout(1) = -1 if not present with iout(2-3) as the interval

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
      iout=binarySearch_wNeighbours_i1D(this,v,imin,iout(1)-1)
    elseif (this(iout(1)) < v) then
      ! Result is in upper subset
      iout(2)=imin;iout(3)=imax
      iout=binarySearch_wNeighbours_i1D(this,v,iout(1)+1,imax)
    else
      ! Result has been found
      iout(2)=iout(1)-1
      iout(2)=max(1,iout(2))
      iout(3)=iout(1)+1
      iout(3)=min(size(this),iout(3))
      return
    endif
  endif
  end function
  !====================================================================!
  !====================================================================!
  recursive function binarySearch_wNeighbours_id1D(this,v,imin,imax) result(iout)
    !! Perform a binary search on an integer vector
    !! Returns a length 3 integer(i32) vector where the last two entries are the left and right neighbours
    !! The first entry of iout is -1 if the value is not present in the vector
    !! Assumes this is sorted!
  !====================================================================!
  integer(i64) :: this(:) !! Vector to search within
  integer(i64) :: v !! Number to find in the vector
  integer(i32) :: imin !! Left integer
  integer(i32) :: imax !! Right integer
  integer(i32) :: iout(3) !! Location of i in this. iout(1) = -1 if not present with iout(2-3) as the interval

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
      iout=binarySearch_wNeighbours_id1D(this,v,imin,iout(1)-1)
    elseif (this(iout(1)) < v) then
      ! Result is in upper subset
      iout(2)=imin;iout(3)=imax
      iout=binarySearch_wNeighbours_id1D(this,v,iout(1)+1,imax)
    else
      ! Result has been found
      iout(2)=iout(1)-1
      iout(2)=max(1,iout(2))
      iout(3)=iout(1)+1
      iout(3)=min(size(this),iout(3))
      return
    endif
  endif

  end function
  !====================================================================!
  !====================================================================!
  recursive function binarySearch_wNeighbours_r1D(this,v,imin,imax) result(iout)
  !! Perform a binary search on a double precision vector
  !! Returns a length 3 integer(i32) vector where the last two entries are the left and right neighbours
  !! The first entry of iout is -1 if the value is not present in the vector
  !! Assumes this is sorted!
  !====================================================================!
  real(r32) :: this(:) !! Vector to search within
  real(r32) :: v !! Number to find in the vector
  integer(i32) :: imin !! Left integer
  integer(i32) :: imax !! Right integer
  integer(i32) :: iout(3) !! Location of i in this. iout(1) = -1 if not present with iout(2-3) as the interval

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
      iout=binarySearch_wNeighbours_r1D(this,v,imin,iout(1)-1)
    elseif (this(iout(1)) < v) then
      ! Result is in upper subset
      iout(2)=imin;iout(3)=imax
      iout=binarySearch_wNeighbours_r1D(this,v,iout(1)+1,imax)
    else
      ! Result has been found
      iout(2)=iout(1)-1
      iout(2)=max(1,iout(2))
      iout(3)=iout(1)+1
      iout(3)=min(size(this),iout(3))
      return
    endif
  endif

  end function
  !====================================================================!
  !====================================================================!
  recursive function binarySearch_wNeighbours_d1D(this,v,imin,imax) result(iout)
  !! Perform a binary search on a double precision vector
  !! Returns a length 3 integer(i32) vector where the last two entries are the left and right neighbours
  !! The first entry of iout is -1 if the value is not present in the vector
  !! Assumes this is sorted!
  !====================================================================!
  real(r64) :: this(:) !! Vector to search within
  real(r64) :: v !! Number to find in the vector
  integer(i32) :: imin !! Left integer
  integer(i32) :: imax !! Right integer
  integer(i32) :: iout(3) !! Location of i in this. iout(1) = -1 if not present with iout(2-3) as the interval

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
      iout=binarySearch_wNeighbours_d1D(this,v,imin,iout(1)-1)
    elseif (this(iout(1)) < v) then
      ! Result is in upper subset
      iout(2)=imin;iout(3)=imax
      iout=binarySearch_wNeighbours_d1D(this,v,iout(1)+1,imax)
    else
      ! Result has been found
      iout(2)=iout(1)-1
      iout(2)=max(1,iout(2))
      iout(3)=iout(1)+1
      iout(3)=min(size(this),iout(3))
      return
    endif
  endif

  end function
  !====================================================================!
  end module
