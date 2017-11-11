module m_Select_FloydRivest

  !! Perform a median of medians on an array. Median of mediansfinds the kth smallest number in an array in O(n) time. It also puts values lower than the kth on the left, and those higher on the right
  !! This makes it perfect for finding the median. This is slower than the quickselect in general, however quickselect go O(n^2) for certain inputs.  This algorithm is used to switch from a quick select when recursion becomes too high.
  use variableKind
  use m_swap, only: swap
  implicit none

  private

  public select1
  interface select1
    !! Find the kth smallest element in an array
    module procedure :: floydRivest_r1D, floydRivest_d1D!, quickSelect_i1D, quickSelect_id1D
  end interface

  !public argMedianOfMedians
  !interface argMedianOfMedians
    !! Find the kth smallest element in an array without modifying the array, instead the indices into that array are modified
  !  module procedure :: argMedianOfMedians_r1D!, argQuickSelect_d1D, argQuickSelect_i1D, argQuickSelect_id1D
  !end interface

  contains

  !====================================================================!
  function floydRivest_r1D(this, k) result(res)
  !====================================================================!
  real(r32) :: this(:)
  integer(i32) :: k
  real(r32) :: res

  integer(i32) :: left
  integer(i32) :: right

  left = 1
  right = size(this)

  call floydRivest_loop_r1D(this, left, right, k, res)

  end function
  !====================================================================!
  !====================================================================!
  recursive subroutine floydRivest_loop_r1D(this, left, right, k, res)
  !====================================================================!
  real(r32) :: this(:)
  integer(i32) :: left
  integer(i32) :: right
  integer(i32) :: k
  real(r32) :: res

  integer(i32) :: i,j,n,newLeft, newRight
  integer(i32) :: s,sd,z
  real(r32) :: tmp

  do while (right > left)

    if ((right - left) > 600) then
      n = right - left + 1
      i = k - left + 1
      z = int(log(dble(n)))
      s = int(0.5d0 * exp(2.d0 * (dble(z)/3.d0)))
      sd = int(0.5d0 * sqrt(dble(z * s * (n-s)/n))) * sign(i-(n/2),1)
      newLeft = max(left, k-(i*(s/n))+sd)
      newRight = min(right, k + ((n-i)*(s/n))+sd)
      call floydRivest_loop_r1D(this, newLeft, newRight, k, res)
    endif

    tmp = this(k)
    i=left
    j=right
    call swap(this(left), this(k))
    if (this(right) > tmp) call swap(this(right), this(left))

    do while (i < j)
      call swap(this(i), this(j))
      i=i+1
      j=j-1

      do while(this(i) < tmp)
        i=i+1
      enddo
      do while (this(j) > tmp)
        j=j-1
      enddo
    enddo
    if (this(left) == tmp) then
      call swap(this(left), this(j))
    else
      j=j+1
      call swap(this(j), this(right))
    endif

    if (j <= k) left = j+1
    if (k <= j) right = j-1

  enddo

  res=this(k)
  end subroutine
  !====================================================================!
  !====================================================================!
  function floydRivest_d1D(this, k) result(res)
  !====================================================================!
  real(r64) :: this(:)
  integer(i32) :: k
  real(r64) :: res

  integer(i32) :: left
  integer(i32) :: right

  left = 1
  right = size(this)

  call floydRivest_loop_d1D(this, left, right, k, res)

  end function
  !====================================================================!
  !====================================================================!
  recursive subroutine floydRivest_loop_d1D(this, left, right, k, res)
  !====================================================================!
  real(r64) :: this(:)
  integer(i32) :: left
  integer(i32) :: right
  integer(i32) :: k
  real(r64) :: res

  integer(i32) :: i,j,n,newLeft, newRight
  integer(i32) :: s,sd,z
  real(r64) :: tmp

  do while (right > left)

    if ((right - left) > 600) then
      n = right - left + 1
      i = k - left + 1
      z = int(log(dble(n)))
      s = int(0.5d0 * exp(2.d0 * (dble(z)/3.d0)))
      sd = int(0.5d0 * sqrt(dble(z * s * (n-s)/n))) * sign(i-(n/2),1)
      newLeft = max(left, k-(i*(s/n))+sd)
      newRight = min(right, k + ((n-i)*(s/n))+sd)
      call floydRivest_loop_d1D(this, newLeft, newRight, k, res)
    endif

    tmp = this(k)
    i=left
    j=right
    call swap(this(left), this(k))
    if (this(right) > tmp) call swap(this(right), this(left))

    do while (i < j)
      call swap(this(i), this(j))
      i=i+1
      j=j-1

      do while(this(i) < tmp)
        i=i+1
      enddo
      do while (this(j) > tmp)
        j=j-1
      enddo
    enddo
    if (this(left) == tmp) then
      call swap(this(left), this(j))
    else
      j=j+1
      call swap(this(j), this(right))
    endif

    if (j <= k) left = j+1
    if (k <= j) right = j-1

  enddo

  res=this(k)
  end subroutine
  !====================================================================!



end module























