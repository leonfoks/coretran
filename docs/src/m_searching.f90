module m_searching
    !! Contains routines to perform a simple binary search on a vector
    !!
    !! Example usage
    !!```fortran
    !!program binarySearch_test
    !!use variableKind, only: i32, r64
    !!use m_BinarySearch, only: binarySearch
    !!
    !!implicit none
    !!
    !!real(r64) :: arr(20)
    !!integer(i32) :: i
    !!integer(i32) :: j
    !!arr=[(dble(i), i = 1, 20)]
    !!j = binarySearch(arr, 10.d0, 1, 20)
    !!write(*,*) 'Location of 10.0 in arr is 10? ',j == 10
    !!end program
    !!```

!! Perform a binary search but also return the neighbouring interval if the actual value is not found.
  !! This is useful if you need to find a number that is not contained in the array and you want the interval
  !!
  !! Example usage
  !!```fortran
  !!use variableKind
  !!use m_BinarySearch, only: intervalSearch
  !!real(r64) :: arr(20)
  !!integer(i32) :: i
  !!integer(i32) :: j(3)
  !!arr=[(dble(i), i = 1, 20)]
  !!j = intervalSearch(arr, 10.5d0, 1, 20)
  !!write(*,*) 'Location of 10.5 in arr is -1? ',j(1) == -1
  !!write(*,*) 'The interval containing 10.5 is [10,11]? ',j(2:3) == [10,11]
  !!```

use variableKind
implicit none

private

public :: binarySearch
public :: intervalSearch
public :: simpleSearch

interface binarySearch
  !! Perform a binary search.  See [[m_searching]] for more information on how to use this interface
  !====================================================================!
  module recursive function binarySearch_i1D(this,v,imin,imax) result(iout)
    !! Search for the value i in an integer vector
    !! Assumes this is sorted!
  !====================================================================!
  integer(i32) :: this(:)
    !! Vector to search within
  integer(i32) :: v
    !! Number to find in the vector
  integer(i32) :: imin
    !! Left integer
  integer(i32) :: imax
    !! Right integer
  integer(i32) :: iout
    !! Location of i in this. Returns -1 if not present
  end function
  !====================================================================!
  !====================================================================!
  module recursive function binarySearch_id1D(this,v,imin,imax) result(iout)
    !! Search for the value i in an integer vector
    !! Assumes this is sorted!
  !====================================================================!
  integer(i64) :: this(:)
    !! Vector to search within
  integer(i64) :: v
    !! Number to find in the vector
  integer(i32) :: imin
    !! Left integer
  integer(i32) :: imax
    !! Right integer
  integer(i32) :: iout
    !! Location of i in this. Returns -1 if not present
  end function
  !====================================================================!
  !====================================================================!
  module recursive function binarySearch_r1D(this,v,imin,imax) result(iout)
    !! Search for the value i in an integer vector
    !! Assumes this is sorted!
  !====================================================================!
  real(r32) :: this(:)
    !! Vector to search within
  real(r32) :: v
    !! Number to find in the vector
  integer(i32) :: imin
    !! Left integer
  integer(i32) :: imax
    !! Right integer
  integer(i32) :: iout
    !! Location of i in this. Returns -1 if not present
  end function
  !====================================================================!
  !====================================================================!
  module recursive function binarySearch_d1D(this,v,imin,imax) result(iout)
    !! Search for the value i in an integer vector
    !! Assumes this is sorted!
  !====================================================================!
  real(r64) :: this(:)
    !! Vector to search within
  real(r64) :: v
    !! Number to find in the vector
  integer(i32) :: imin
    !! Left integer
  integer(i32) :: imax
    !! Right integer
  integer(i32) :: iout
    !! Location of i in this. Returns -1 if not present
  end function
  !====================================================================!
end interface

interface intervalSearch
  !! Perform an interval search on an array
  !! Returns a length 3 integer(i32) array where the last two entries are the left and right neighbours
  !! The first entry of iout is -1 if the value is not present in the vector
  !! Assumes this is sorted!See [[m_searching]] for more information on how to use this interface
  !====================================================================!
  module recursive function intervalSearch_i1D(this,v,imin,imax) result(iout)
    !! interfaced with [[intervalSearch]]
  !====================================================================!
  integer(i32) :: this(:)
    !! Vector to search within
  integer(i32) :: v
    !! Number to find in the vector
  integer(i32) :: imin
    !! Left integer
  integer(i32) :: imax
    !! Right integer
  integer(i32) :: iout(3)
    !! Location of i in this. iout(1) = -1 if not present with iout(2-3) as the interval
  end function
  !====================================================================!
  !====================================================================!
  module recursive function intervalSearch_id1D(this,v,imin,imax) result(iout)
    !! interfaced with [[intervalSearch]]
  !====================================================================!
  integer(i64) :: this(:)
    !! Vector to search within
  integer(i64) :: v
    !! Number to find in the vector
  integer(i32) :: imin
    !! Left integer
  integer(i32) :: imax
    !! Right integer
  integer(i32) :: iout(3)
    !! Location of i in this. iout(1) = -1 if not present with iout(2-3) as the interval
  end function
  !====================================================================!
  !====================================================================!
  module recursive function intervalSearch_r1D(this,v,imin,imax) result(iout)
    !! interfaced with [[intervalSearch]]
  !====================================================================!
  real(r32) :: this(:)
    !! Vector to search within
  real(r32) :: v
    !! Number to find in the vector
  integer(i32) :: imin
    !! Left integer
  integer(i32) :: imax
    !! Right integer
  integer(i32) :: iout(3)
    !! Location of i in this. iout(1) = -1 if not present with iout(2-3) as the interval
  end function
  !====================================================================!
  !====================================================================!
  module recursive function intervalSearch_d1D(this,v,imin,imax) result(iout)
    !! interfaced with [[intervalSearch]]
  !====================================================================!
  real(r64) :: this(:)
    !! Vector to search within
  real(r64) :: v
    !! Number to find in the vector
  integer(i32) :: imin
    !! Left integer
  integer(i32) :: imax
    !! Right integer
  integer(i32) :: iout(3)
    !! Location of i in this. iout(1) = -1 if not present with iout(2-3) as the interval
  end function
  !====================================================================!
end interface

interface simpleSearch
  !! Carry out a brute force search on an array for a given number. Returns -1 if the value is not found.
  !====================================================================!
  module function simpleSearch_i1D(this, val) result(iout)
    !! Interfaced with [[simpleSearch]]
  !====================================================================!
  integer(i32) :: this(:)
    !! Search this vector
  integer(i32) :: val
    !! Number to find in the vector
  integer(i32) :: iout
    !! Location of i in this
  end function
  !====================================================================!
  !====================================================================!
  module function simpleSearch_id1D(this, val) result(iout)
    !! Interfaced with [[simpleSearch]]
  !====================================================================!
  integer(i64) :: this(:)
    !! Search this vector
  integer(i64) :: val
    !! Number to find in the vector
  integer(i32) :: iout
    !! Location of i in this
  end function
  !====================================================================!
  !====================================================================!
  module function simpleSearch_r1D(this, val) result(iout)
    !! Interfaced with [[simpleSearch]]
  !====================================================================!
  real(r32) :: this(:)
    !! Search this vector
  real(r32) :: val
    !! Number to find in the vector
  integer(i32) :: iout
    !! Location of i in this
  end function
  !====================================================================!
  !====================================================================!
  module function simpleSearch_d1D(this, val) result(iout)
    !! Interfaced with [[simpleSearch]]
  !====================================================================!
  real(r64) :: this(:)
    !! Search this vector
  real(r64) :: val
    !! Number to find in the vector
  integer(i32) :: iout
    !! Location of i in this
  end function
  !====================================================================!
end interface

end module



