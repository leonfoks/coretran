module m_partition
  !! Contains Hoare's style partitioning algorithm used for quicksorting and quickselect routines.
  !!
  !! See [[partition]] and [[argPartition]] for more information.
  use variableKind, only: r32, r64, i32, i64

  implicit none

  private

  public :: partition

  interface partition
    !! Partitioning used for quickSort and quickSelect routines
    !====================================================================!
    module subroutine partition_r1D(this,left,right,iPivot)
      !! Interfaced with [[partition]]
    !====================================================================!
    real(r32), intent(inout) :: this(:) !! 1D array
    integer(i32), intent(in) :: left !! Left index
    integer(i32), intent(in) :: right !! Right index
    integer(i32), intent(inout) :: iPivot !! Pivoting index
    end subroutine
    !====================================================================!
    !====================================================================!
    module subroutine partition_d1D(this,left,right,iPivot)
      !! Interfaced with [[partition]]
    !====================================================================!
    real(r64), intent(inout) :: this(:) !! 1D array
    integer(i32), intent(in) :: left !! Left index
    integer(i32), intent(in) :: right !! Right index
    integer(i32), intent(inout) :: iPivot !! Pivoting index
    end subroutine
    !====================================================================!
    !====================================================================!
    module subroutine partition_i1D(this,left,right,iPivot)
      !! Interfaced with [[partition]]
    !====================================================================!
    integer(i32), intent(inout) :: this(:)
    integer(i32), intent(in) :: left
    integer(i32), intent(in) :: right
    integer(i32), intent(inout) :: iPivot
    end subroutine
    !====================================================================!
    !====================================================================!
    module subroutine partition_id1D(this,left,right,iPivot)
      !! Interfaced with [[partition]]
    !====================================================================!
    integer(i64), intent(inout) :: this(:)
    integer(i32), intent(in) :: left
    integer(i32), intent(in) :: right
    integer(i32), intent(inout) :: iPivot
    end subroutine
    !====================================================================!
  end interface

  public :: argPartition

  interface argPartition
    !! Partitioning used for argQuicksort routines
    !====================================================================!
    module subroutine argPartition_r1D(this,idx,left,right,i)
      !! Interfaced with [[argPartition]]
    !====================================================================!
    real(r32), intent(in) :: this(:)
    integer(i32), intent(inout) :: idx(:)
    integer(i32), intent(in) :: left
    integer(i32), intent(in) :: right
    integer(i32), intent(inout) :: i
    end subroutine
    !====================================================================!
    !====================================================================!
    module subroutine argPartition_d1D(this,idx,left,right,i)
      !! Interfaced with [[argPartition]]
    !====================================================================!
    real(r64), intent(in) :: this(:)
    integer(i32), intent(inout) :: idx(:)
    integer(i32), intent(in) :: left
    integer(i32), intent(in) :: right
    integer(i32), intent(inout) :: i
    end subroutine
    !====================================================================!
    !====================================================================!
    module subroutine argPartition_i1D(this,idx,left,right,i)
      !! Interfaced with [[argPartition]]
    !====================================================================!
    integer(i32), intent(in) :: this(:)
    integer(i32), intent(inout) :: idx(:)
    integer(i32), intent(in) :: left
    integer(i32), intent(in) :: right
    integer(i32), intent(inout) :: i
    end subroutine
    !====================================================================!
    !====================================================================!
    module subroutine argPartition_id1D(this,idx,left,right,i)
      !! Interfaced with [[argPartition]]
    !====================================================================!
    integer(i64), intent(in) :: this(:)
    integer(i32), intent(inout) :: idx(:)
    integer(i32), intent(in) :: left
    integer(i32), intent(in) :: right
    integer(i32), intent(inout) :: i
    end subroutine
    !====================================================================!
  end interface

  contains
end module
