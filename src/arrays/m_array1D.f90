module m_array1D
  !! 1D array routines
use variableKind
implicit none

private

public arange
interface arange
  !! Create a 1D array from start to stop in given increments
  module subroutine arange_r1D(res,start,stp,step_)
    !! Interfaced with arange()
    real(r32), intent(in) :: start !! Start from here
    real(r32), intent(in) :: stp !! Stop here
    real(r32),optional, intent(in) :: step_ !! Step size
    real(r32),allocatable, intent(inout) :: res(:)
  end subroutine
  module subroutine arange_d1D(res,start,stp,step_)
    !! Interfaced with arange()
    real(r64), intent(in) :: start !! Start from here
    real(r64), intent(in) :: stp !! Stop here
    real(r64),optional, intent(in) :: step_ !! Step size
    real(r64),allocatable, intent(inout) :: res(:)
  end subroutine
  module subroutine arange_i1D(res,start,stp,step_)
    !! Interfaced with arange()
    integer(i32), intent(in) :: start !! Start from here
    integer(i32), intent(in) :: stp !! Stop here
    integer(i32),optional, intent(in) :: step_ !! Step size
    integer(i32),allocatable, intent(inout) :: res(:)
  end subroutine
  module subroutine arange_id1D(res,start,stp,step_)
    !! Interfaced with arange()
    integer(i64), intent(in) :: start !! Start from here
    integer(i64), intent(in) :: stp !! Stop here
    integer(i64),optional, intent(in) :: step_ !! Step size
    integer(i64),allocatable, intent(inout) :: res(:)
  end subroutine
end interface

public :: diff
interface diff
!! Compute the difference along an array
  module subroutine diff_r1D(this, res)
      !! Interfaced with diff()
      real(r32),intent(in) :: this(:) !! 1D array
      real(r32) :: res(:) !! Difference along array
  end subroutine
  module subroutine diff_d1D(this, res)
      !! Interfaced with diff()
      real(r64),intent(in) :: this(:) !! 1D array
      real(r64) :: res(:) !! Difference along array
  end subroutine
  module subroutine diff_i1D(this, res)
      !! Interfaced with diff()
      integer(i32),intent(in) :: this(:) !! 1D array
      integer(i32) :: res(:) !! Difference along array
  end subroutine
  module subroutine diff_id1D(this, res)
      !! Interfaced with diff()
      integer(i64),intent(in) :: this(:) !! 1D array
      integer(i64) :: res(:) !! Difference along array
  end subroutine
end interface

public isSorted
interface isSorted
  !! Check that a 1D array is sorted
  module function isSorted_r1D(this) result(yes)
    !! Interfaced with isSorted()
    real(r32), intent(in) :: this(:) !! 1D array
    logical :: yes !! isSorted
  end function
  module function isSorted_d1D(this) result(yes)
    !! Interfaced with isSorted()
    real(r64), intent(in) :: this(:) !! 1D array
    logical :: yes !! isSorted
  end function
  module function isSorted_i1D(this) result(yes)
    !! Interfaced with isSorted()
    integer(i32), intent(in) :: this(:) !! 1D array
    logical :: yes !! isSorted
  end function
  module function isSorted_id1D(this) result(yes)
    !! Interfaced with isSorted()
    integer(i64), intent(in) :: this(:) !! 1D array
    logical :: yes !! isSorted
  end function
end interface


public repeat
interface repeat
    !! Repeat each element nRepeat times
    module subroutine repeat_r1D(this, nRepeats, res)
      !! Interfaced with repeat()
      real(r32) :: this(:) !! 1D array
      integer(i32) :: nRepeats !! Number of times each element should be repeated
      real(r32) :: res(:)
    end subroutine
    module subroutine repeat_d1D(this, nRepeats, res)
      !! Interfaced with repeat()
      real(r64) :: this(:) !! 1D array
      integer(i32) :: nRepeats !! Number of times each element should be repeated
      real(r64) :: res(:)
    end subroutine
    module subroutine repeat_i1D(this, nRepeats, res)
      !! Interfaced with repeat()
      integer(i32) :: this(:) !! 1D array
      integer(i32) :: nRepeats !! Number of times each element should be repeated
      integer(i32) :: res(:)
    end subroutine
    module subroutine repeat_id1D(this, nRepeats, res)
      !! Interfaced with repeat()
      integer(i64) :: this(:) !! 1D array
      integer(i32) :: nRepeats !! Number of times each element should be repeated
      integer(i64) :: res(:)
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









end module
