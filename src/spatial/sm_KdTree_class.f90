submodule (m_KdTree) sm_KdTree_class

implicit none

contains

  !====================================================================!
  module procedure init2D_KdTree!(this, x, y)
  !====================================================================!
  !  class(kdTree) :: this
  !    !! KdTree Class
  !  real(r64),intent(in) :: x(:)
  !    !! x-coordinates of the points
  !  real(r64),intent(in) :: y(:)
  !    !! y-coordinates of the points

  integer(i32) :: i, istat
  real(r64), allocatable :: aux(:)

  call this%deallocate()

  ! Initialize the number of entries and dimensions of the tree
  this%N = size(x)
  this%nDims = 2

  ! Set the root node
  call this%trunk%init(1, this%N)

  ! Allocated and initialize the indexer
  call allocate(this%indx, this%N)
  call arange(this%indx, 1, this%N)

  ! Allocate the auxiliary space
  call allocate(aux, this%N)

  ! Grow the KD tree
  call growKdTree_2D(this%trunk, x, y, this%indx, aux)

  call deallocate(aux)
  this%set=.true.
  end procedure
  !====================================================================!
  !====================================================================!
  recursive subroutine growKdTree_2D(trunk, x, y, indx, aux)
  !====================================================================!
  class(KdTreebranch) :: trunk
  real(r64), intent(in) :: x(:), y(:)
  integer(i32), intent(inout) :: indx(:)
  real(r64), intent(inout) :: aux(:)

  integer(i32) :: splitAlong, iMedian, iMid, istat, N
  real(r64) :: maxVariance

  N = trunk%right - trunk%left + 1

  iMid = (trunk%right+trunk%left) / 2

  ! Only recurse through the tree for array sections bigger than a fixed size to prevent tail recursion.
  ! Arrays of size 5 or less are the leaves at the end of the branch
  if (N > 5) then
    ! Assign first dimension as split along to begin with
    aux(1:N) = x(indx(trunk%left:trunk%right))
    maxVariance = variance(aux(1:N))
    splitAlong = 1
    ! Test the second dimension
    aux(1:N) = y(indx(trunk%left:trunk%right))
    if (variance(aux(1:N)) > maxVariance) then
      splitAlong = 2
    end if
    trunk%splitAlong = splitAlong

    iMedian = 0
    select case(splitAlong)
    case(1)
      ! arg select the median value
      call argSelect(x, indx, iMid, iMedian, trunk%left, trunk%right)
      trunk%median = x(iMedian)
    case(2)
      ! arg select the median value
      call argSelect(y, indx, iMid, iMedian, trunk%left, trunk%right)
      trunk%median = y(iMedian)
    end select

    ! Allocate the children nodes
    allocate(trunk%buds(2))
    ! Initialize the children nodes
    call trunk%buds(1)%init(trunk%left, iMid)
    call trunk%buds(2)%init(iMid+1, trunk%right)
    ! Grow the tree through the new branches
    call growKdTree_2D(trunk%buds(1),x,y,indx, aux)
    call growKdTree_2D(trunk%buds(2),x,y,indx, aux)
  end if
  end subroutine
  !====================================================================!
  !====================================================================!
  module procedure deallocate_KdTree
    !! Overloaded type bound procedure KdTree%deallocate()
  !====================================================================!
  call deallocate(this%indx)
  call this%trunk%deallocate()
  end procedure
  !====================================================================!
end submodule
