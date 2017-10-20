module m_medianOf3
  !! Given three numbers, find their median and sort at the same time
  use variableKind
  use m_swap, only: swap

  implicit none

  private

  public medianOf3
  interface medianOf3
    !! Sort three numbers in an array and return the location of the median
    module procedure :: medianOf3_r1D,medianOf3_d1D,medianOf3_i1D,medianOf3_id1D
  end interface

  public argMedianOf3
  interface argMedianOf3
    !! Sort the indices of three numbers into an array and return the location of the median
    module procedure :: argMedianOf3_r1D, argMedianOf3_d1D, argMedianOf3_i1D, argMedianOf3_id1D
  end interface

  contains
  !====================================================================!
  subroutine medianOf3_r1D(this, left, mid, right)
    !! Interfaced with medianOf3()
  !====================================================================!
    real(r32) :: this(:)
    integer(i32) :: left
    integer(i32) :: mid
    integer(i32) :: right

    if (this(right) < this(left)) call swap(this(left), this(right))
    if (this(mid) < this(left)) call swap(this(mid), this(left))
    if (this(right) < this(mid)) call swap(this(right), this(mid))
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine medianOf3_d1D(this, left, mid, right)
    !! Interfaced with medianOf3()
  !====================================================================!
    real(r64) :: this(:)
    integer(i32) :: left
    integer(i32) :: mid
    integer(i32) :: right

    if (this(right) < this(left)) call swap(this(left), this(right))
    if (this(mid) < this(left)) call swap(this(mid), this(left))
    if (this(right) < this(mid)) call swap(this(right), this(mid))
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine medianOf3_i1D(this, left, mid, right)
    !! Interfaced with medianOf3()
  !====================================================================!
    integer(i32) :: this(:)
    integer(i32) :: left
    integer(i32) :: mid
    integer(i32) :: right

    if (this(right) < this(left)) call swap(this(left), this(right))
    if (this(mid) < this(left)) call swap(this(mid), this(left))
    if (this(right) < this(mid)) call swap(this(right), this(mid))
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine medianOf3_id1D(this, left, mid, right)
    !! Interfaced with medianOf3()
  !====================================================================!
    integer(i64) :: this(:)
    integer(i32) :: left
    integer(i32) :: mid
    integer(i32) :: right

    if (this(right) < this(left)) call swap(this(left), this(right))
    if (this(mid) < this(left)) call swap(this(mid), this(left))
    if (this(right) < this(mid)) call swap(this(right), this(mid))
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine argMedianOf3_r1D(this, i, left, mid, right)
    !! Interfaced with argMedianOf3()
  !====================================================================!
    real(r32) :: this(:)
    integer(i32) :: i(:)
    integer(i32) :: left
    integer(i32) :: mid
    integer(i32) :: right

    if (this(i(right)) < this(i(left))) call swap(i(left), i(right))
    if (this(i(mid)) < this(i(left))) call swap(i(mid), i(left))
    if (this(i(right)) < this(i(mid))) call swap(i(right), i(mid))
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine argMedianOf3_d1D(this, i, left, mid, right)
    !! Interfaced with argMedianOf3()
  !====================================================================!
    real(r64) :: this(:)
    integer(i32) :: i(:)
    integer(i32) :: left
    integer(i32) :: mid
    integer(i32) :: right

    if (this(i(right)) < this(i(left))) call swap(i(left), i(right))
    if (this(i(mid)) < this(i(left))) call swap(i(mid), i(left))
    if (this(i(right)) < this(i(mid))) call swap(i(right), i(mid))
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine argMedianOf3_i1D(this, i, left, mid, right)
    !! Interfaced with argMedianOf3()
  !====================================================================!
    integer(i32) :: this(:)
    integer(i32) :: i(:)
    integer(i32) :: left
    integer(i32) :: mid
    integer(i32) :: right

    if (this(i(right)) < this(i(left))) call swap(i(left), i(right))
    if (this(i(mid)) < this(i(left))) call swap(i(mid), i(left))
    if (this(i(right)) < this(i(mid))) call swap(i(right), i(mid))
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine argMedianOf3_id1D(this, i, left, mid, right)
    !! Interfaced with argMedianOf3()
  !====================================================================!
    integer(i64) :: this(:)
    integer(i32) :: i(:)
    integer(i32) :: left
    integer(i32) :: mid
    integer(i32) :: right

    if (this(i(right)) < this(i(left))) call swap(i(left), i(right))
    if (this(i(mid)) < this(i(left))) call swap(i(mid), i(left))
    if (this(i(right)) < this(i(mid))) call swap(i(right), i(mid))
  end subroutine
  !====================================================================!
end module
