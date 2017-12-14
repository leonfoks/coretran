module m_array1D
  !! 1D array routines
  !!
  !! See
  !! [[arange]], [[diff]], [[isSorted]], [[repeat]]
  !! for more information.
use variableKind, only: r32, r64, i32, i64
use m_errors, only: msg
use m_allocate, only: allocate
use m_deallocate, only: deallocate
use m_unitTester, only: tester

implicit none

private

public :: array1D_test

public :: arange
interface arange
  !! Create a 1D array from start to stop in given increments of 1 or optionally step
  !!
  !! Example Usage
  !!```fortran
  !!program arangeTest
  !!use variableKind, only: i32,i64,r32,r64
  !!use m_allocatable, only: allocatable
  !!use m_array1D, only: arange
  !!real(r32), allocatable :: a(:)
  !!integer(i32), allocatable :: b(:)
  !!real(r64), allocatable :: c(:)
  !!integer(i64), allocatable :: d(:)
  !!integer :: N
  !!N = 10000
  !!call allocate(a, N)
  !!call allocate(b, N)
  !!call allocate(c, N)
  !!call allocate(d, N)
  !!
  !!call arange(a, 1, N)
  !!call arange(b, 1, N)
  !!call arange(c, 1, N)
  !!call arange(d, 1, N)
  !!
  !!call deallocate(a)
  !!call deallocate(b)
  !!call deallocate(c)
  !!call deallocate(d)
  !!
  !!end program
  !!```
  module subroutine arange_r1D(res,start,stp,step)
    !! Interfaced with [[arange]]
    real(r32), intent(in) :: start !! Start from here
    real(r32), intent(in) :: stp !! Stop here
    real(r32), optional, intent(in) :: step !! Step size
    real(r32), allocatable, intent(inout) :: res(:)
  end subroutine
  module subroutine arange_d1D(res,start,stp,step)
    !! Interfaced with [[arange]]
    real(r64), intent(in) :: start !! Start from here
    real(r64), intent(in) :: stp !! Stop here
    real(r64), optional, intent(in) :: step !! Step size
    real(r64), allocatable, intent(inout) :: res(:)
  end subroutine
  module subroutine arange_i1D(res,start,stp,step)
    !! Interfaced with [[arange]]
    integer(i32), intent(in) :: start !! Start from here
    integer(i32), intent(in) :: stp !! Stop here
    integer(i32), optional, intent(in) :: step !! Step size
    integer(i32), allocatable, intent(inout) :: res(:)
  end subroutine
  module subroutine arange_id1D(res,start,stp,step)
    !! Interfaced with [[arange]]
    integer(i64), intent(in) :: start !! Start from here
    integer(i64), intent(in) :: stp !! Stop here
    integer(i64), optional, intent(in) :: step !! Step size
    integer(i64), allocatable, intent(inout) :: res(:)
  end subroutine
end interface

public :: diff
interface diff
  !! Compute the difference along an array
  !!
  !! Example Usage
  !!```fortran
  !!program arangeTest
  !!use variableKind, only: i32,i64,r32,r64
  !!use m_allocatable, only: allocatable
  !!use m_array1D, only: arange, diff
  !!real(r32), allocatable :: a(:)
  !!integer(i32), allocatable :: b(:)
  !!real(r64), allocatable :: c(:)
  !!integer(i64), allocatable :: d(:)
  !!integer :: N
  !!N = 10000
  !!call allocate(a, N)
  !!call allocate(b, N)
  !!call allocate(c, N)
  !!call allocate(d, N)
  !!
  !!call arange(a, 1, N)
  !!call arange(b, 1, N)
  !!call arange(c, 1, N)
  !!call arange(d, 1, N)
  !!
  !!call diff(a, 1, N)
  !!call diff(b, 1, N)
  !!call diff(c, 1, N)
  !!call diff(d, 1, N)
  !!
  !!call deallocate(a)
  !!call deallocate(b)
  !!call deallocate(c)
  !!call deallocate(d)
  !!
  !!end program
  !!```
  module subroutine diff_r1D(this, res)
      !! Interfaced with [[diff]]
      real(r32),intent(in) :: this(:) !! 1D array
      real(r32) :: res(:) !! Difference along array
  end subroutine
  module subroutine diff_d1D(this, res)
      !! Interfaced with [[diff]]
      real(r64),intent(in) :: this(:) !! 1D array
      real(r64) :: res(:) !! Difference along array
  end subroutine
  module subroutine diff_i1D(this, res)
      !! Interfaced with [[diff]]
      integer(i32),intent(in) :: this(:) !! 1D array
      integer(i32) :: res(:) !! Difference along array
  end subroutine
  module subroutine diff_id1D(this, res)
      !! Interfaced with [[diff]]
      integer(i64),intent(in) :: this(:) !! 1D array
      integer(i64) :: res(:) !! Difference along array
  end subroutine
end interface

public isSorted
interface isSorted
  !! Check that a 1D array is sorted
  !!
  !! Example Usage
  !!```fortran
  !!program arangeTest
  !!use variableKind, only: i32,i64,r32,r64
  !!use m_allocatable, only: allocatable
  !!use m_array1D, only: arange, isSorted
  !!real(r32), allocatable :: a(:)
  !!integer(i32), allocatable :: b(:)
  !!real(r64), allocatable :: c(:)
  !!integer(i64), allocatable :: d(:)
  !!integer :: N
  !!N = 10000
  !!call allocate(a, N)
  !!call allocate(b, N)
  !!call allocate(c, N)
  !!call allocate(d, N)
  !!
  !!call arange(a, 1, N)
  !!call arange(b, 1, N)
  !!call arange(c, 1, N)
  !!call arange(d, 1, N)
  !!
  !!call diff(a, 1, N)
  !!call diff(b, 1, N)
  !!call diff(c, 1, N)
  !!call diff(d, 1, N)
  !!
  !!call deallocate(a)
  !!call deallocate(b)
  !!call deallocate(c)
  !!call deallocate(d)
  !!
  !!end program
  !!```

  module function isSorted_r1D(this) result(yes)
    !! Interfaced with [[isSorted]]
    real(r32), intent(in) :: this(:) !! 1D array
    logical :: yes !! isSorted
  end function
  module function isSorted_d1D(this) result(yes)
    !! Interfaced with [[isSorted]]
    real(r64), intent(in) :: this(:) !! 1D array
    logical :: yes !! isSorted
  end function
  module function isSorted_i1D(this) result(yes)
    !! Interfaced with [[isSorted]]
    integer(i32), intent(in) :: this(:) !! 1D array
    logical :: yes !! isSorted
  end function
  module function isSorted_id1D(this) result(yes)
    !! Interfaced with [[isSorted]]
    integer(i64), intent(in) :: this(:) !! 1D array
    logical :: yes !! isSorted
  end function
    module function isSorted_r1Di1D(this, indx) result(yes)
    !! Interfaced with [[isSorted]]
    real(r32), intent(in) :: this(:) !! 1D array
    integer(i32), intent(in) :: indx(:) !! Index into 1D array
    logical :: yes !! isSorted
  end function
  module function isSorted_d1Di1D(this, indx) result(yes)
    !! Interfaced with [[isSorted]]
    real(r64), intent(in) :: this(:) !! 1D array
    integer(i32), intent(in) :: indx(:) !! Index into 1D array
    logical :: yes !! isSorted
  end function
  module function isSorted_i1Di1D(this, indx) result(yes)
    !! Interfaced with [[isSorted]]
    integer(i32), intent(in) :: this(:) !! 1D array
    integer(i32), intent(in) :: indx(:) !! Index into 1D array
    logical :: yes !! isSorted
  end function
  module function isSorted_id1Di1D(this, indx) result(yes)
    !! Interfaced with [[isSorted]]
    integer(i64), intent(in) :: this(:) !! 1D array
    integer(i32), intent(in) :: indx(:) !! Index into 1D array
    logical :: yes !! isSorted
  end function
end interface


public repeat
interface repeat
    !! Repeat each element nRepeat times
    module subroutine repeat_r1D(this, nRepeats, res)
      !! Interfaced with [[repeat]]
      real(r32) :: this(:) !! 1D array
      integer(i32) :: nRepeats !! Number of times each element should be repeated
      real(r32), allocatable :: res(:)
    end subroutine
    module subroutine repeat_d1D(this, nRepeats, res)
      !! Interfaced with [[repeat]]
      real(r64) :: this(:) !! 1D array
      integer(i32) :: nRepeats !! Number of times each element should be repeated
      real(r64), allocatable :: res(:)
    end subroutine
    module subroutine repeat_i1D(this, nRepeats, res)
      !! Interfaced with [[repeat]]
      integer(i32) :: this(:) !! 1D array
      integer(i32) :: nRepeats !! Number of times each element should be repeated
      integer(i32), allocatable :: res(:)
    end subroutine
    module subroutine repeat_id1D(this, nRepeats, res)
      !! Interfaced with [[repeat]]
      integer(i64) :: this(:) !! 1D array
      integer(i32) :: nRepeats !! Number of times each element should be repeated
      integer(i64), allocatable :: res(:)
    end subroutine
end interface

!  interface isConstant
!  module procedure :: isConstant_DV
!  end interface
!  private :: isConstant_DV
!
!  interface isConstantIncrement
!  module procedure :: isConstantIncrement_DV
!  end interface
!  private :: isConstantIncrement_DV
!
!  interface isInside
!  module procedure :: isInside1D_I1,isInside1D_D1
!  end interface
!
!  interface getBin
!  module procedure :: getBin1D_I1,getBin1D_D1
!  end interface
!
!  interface scale
!  module procedure :: scaleVector
!  end interface
!
!  interface deintegerize
!  module procedure :: deintegerizeVector
!  end interface
!  private :: deintegerizeVector
!
!  private :: isInside1D_I1,isInside1D_D1
!  private :: getBin1D_I1,getBin1D_D1
!
!  interface mapExponential
!  module procedure :: mapExponential_1D
!  end interface
!  private :: mapExponential_1D
!
!  interface unitize
!  module procedure :: unitize_1D
!  end interface
!  private :: unitize_1D

contains

  !====================================================================!
  subroutine array1D_test(test)
  !====================================================================!
  class(tester) :: test

  real(r32) :: ar
  real(r32), allocatable :: ar1D(:), br1D(:)
  real(r64) :: a
  real(r64), allocatable :: a1D(:), b1D(:)
  integer(i32) :: ia, ib, ic
  integer(i32), allocatable :: ia1D(:), ib1D(:)
  integer(i64) :: iad
  integer(i64), allocatable :: iad1D(:), ibd1D(:)

  call Msg('==========================')
  call Msg('Testing : Array 1D')
  call Msg('==========================')

  call allocate(ar1D, 3)
  call allocate(a1D, 3)
  call allocate(ia1D, 3)
  call allocate(iad1D, 3)
  call allocate(br1D, 2)
  call allocate(b1D, 2)
  call allocate(ib1D, 2)
  call allocate(ibd1D, 2)

  call arange(ar1D,1.0, 3.0, 1.0)
  call test%test(all(ar1D==[1.0,2.0,3.0]),'arange_r1D')
  call arange(a1D,1.d0, 3.d0, 1.d0)
  call test%test(all(a1D==[1.d0,2.d0,3.d0]),'arange_d1D')
  call arange(ia1D,1, 3, 1)
  call test%test(all(ia1D==[1,2,3]),'arange_i1D')
  call arange(iad1D,1_i64, 3_i64, 1_i64)
  call test%test(all(iad1D==[1,2,3]),'arange_id1D')

  call diff(ar1D, br1D)
  call test%test(all(br1D==[1.0,1.0]),'diff_r1D')
  call diff(a1D, b1D)
  call test%test(all(b1D==[1.d0,1.d0]),'diff_d1D')
  call diff(ia1d, ib1D)
  call test%test(all(ib1D==[1,1]),'diff_i1D')
  call diff(iad1d, ibd1D)
  call test%test(all(ibd1D==[1,1]),'diff_id1D')

  call allocate(br1D, size(ar1D)*3)
  call allocate(b1D, size(a1D)*3)
  call allocate(ib1D, size(ia1D)*3)
  call allocate(ibd1D, size(iad1D)*3)

  call repeat(ar1D, 3, br1D)
  call test%test(all(br1D == [1.0,1.0,1.0,2.0,2.0,2.0,3.0,3.0,3.0]), 'repeat_r1D')
  call repeat(a1D,3, b1D)
  call test%test(all(b1D == [1.d0,1.d0,1.d0,2.d0,2.d0,2.d0,3.d0,3.d0,3.d0]), 'repeat_d1D')
  call repeat(ia1D,3, ib1D)
  call test%test(all(ib1D == [1,1,1,2,2,2,3,3,3]), 'repeat_ib1D')
  call repeat(iad1D,3, ibd1D)
  call test%test(all(ibd1D == [1,1,1,2,2,2,3,3,3]), 'repeat_ibd1D')

  call deallocate(ar1D)
  call deallocate(a1D)
  call deallocate(ia1D)
  call deallocate(iad1D)
  call deallocate(br1D)
  call deallocate(b1D)
  call deallocate(ib1D)
  call deallocate(ibd1D)

  end subroutine
  !====================================================================!
end module
