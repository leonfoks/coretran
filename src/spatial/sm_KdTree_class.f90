submodule (m_KdTree) sm_KdTree_class
  !! Contains the implementations of overloaded KdTree class type bound procedures
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
  aux = 0.d0

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

  integer(i32) :: i,i1,splitAlong, iMedian, iMid, istat, N
  real(r64) :: maxVariance

  N = trunk%right - trunk%left + 1

  iMid = (trunk%right+trunk%left) / 2

  ! Only recurse through the tree for array sections bigger than a fixed size to prevent tail recursion.
  ! Arrays of size 5 or less are the leaves at the end of the branch
  if (N > 5) then
    ! Assign first dimension as split along to begin with
    do i = 0, N-1
        i1 = indx(trunk%left+i)
        aux(i+1) = x(i1)
    enddo
    maxVariance = variance(aux(1:N))
    splitAlong = 1
    ! Test the second dimension
    do i = 0, N-1
        i1 = indx(trunk%left+i)
        aux(i+1) = y(i1)
    enddo
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
  module procedure init3D_KdTree!(this, x, y, z)
  !====================================================================!
  !  class(kdTree) :: this
  !    !! KdTree Class
  !  real(r64),intent(in) :: x(:)
  !    !! x-coordinates of the points
  !  real(r64),intent(in) :: y(:)
  !    !! y-coordinates of the points
  !  real(r64),intent(in) :: z(:)
  !    !! z-coordinates of the points

  integer(i32) :: i, istat
  real(r64), allocatable :: aux(:)

  call this%deallocate()

  ! Initialize the number of entries and dimensions of the tree
  this%N = size(x)
  this%nDims = 3

  ! Set the root node
  call this%trunk%init(1, this%N)

  ! Allocated and initialize the indexer
  call allocate(this%indx, this%N)
  call arange(this%indx, 1, this%N)

  ! Allocate the auxiliary space
  call allocate(aux, this%N)

  ! Grow the KD tree
  call growKdTree_3D(this%trunk, x, y, z, this%indx, aux)

  call deallocate(aux)
  this%set=.true.
  end procedure
  !====================================================================!
  !====================================================================!
  recursive subroutine growKdTree_3D(trunk, x, y, z, indx, aux)
  !====================================================================!
  class(KdTreebranch) :: trunk
  real(r64), intent(in) :: x(:), y(:), z(:)
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
    ! Test the thrid dimension
    aux(1:N) = z(indx(trunk%left:trunk%right))
    if (variance(aux(1:N)) > maxVariance) then
      splitAlong = 3
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
    case(3)
      ! arg select the median value
      call argSelect(z, indx, iMid, iMedian, trunk%left, trunk%right)
      trunk%median = z(iMedian)
    end select

    ! Allocate the children nodes
    allocate(trunk%buds(2))
    ! Initialize the children nodes
    call trunk%buds(1)%init(trunk%left, iMid)
    call trunk%buds(2)%init(iMid+1, trunk%right)
    ! Grow the tree through the new branches
    call growKdTree_3D(trunk%buds(1),x,y,z,indx, aux)
    call growKdTree_3D(trunk%buds(2),x,y,z,indx, aux)
  end if
  end subroutine
  !====================================================================!


  !====================================================================!
  module procedure initKD_KdTree!(this, D)
  !====================================================================!
  !  class(kdTree) :: this
  !    !! KdTree Class
  !  real(r64),intent(in) :: D(:,:)
  !    !! Co-ordinates of the points, the k columns contain the k dimensional values.

  integer(i32) :: i, istat
  real(r64), allocatable :: aux(:)

  call this%deallocate()

  ! Initialize the number of entries and dimensions of the tree
  this%N = size(D, 1)
  this%nDims = size(D, 2)

  ! Set the root node
  call this%trunk%init(1, this%N)

  ! Allocated and initialize the indexer
  call allocate(this%indx, this%N)
  call arange(this%indx, 1, this%N)

  ! Allocate the auxiliary space
  call allocate(aux, this%N)

  ! Grow the KD tree
  call growKdTree_KD(this%trunk, D, this%indx, aux)

  call deallocate(aux)
  this%set=.true.
  end procedure
  !====================================================================!
  !====================================================================!
  recursive subroutine growKdTree_KD(trunk, D, indx, aux)
  !====================================================================!
  class(KdTreebranch) :: trunk
  real(r64), intent(in) :: D(:,:)
  integer(i32), intent(inout) :: indx(:)
  real(r64), intent(inout) :: aux(:)

  integer(i32) :: splitAlong, iMedian, iMid, istat, k, N, nDims
  real(r64) :: maxVariance,varianceTmp

  nDims = size(D, 2)

  N = trunk%right - trunk%left + 1

  iMid = (trunk%right+trunk%left) / 2

  ! Only recurse through the tree for array sections bigger than a fixed size to prevent tail recursion.
  ! Arrays of size 5 or less are the leaves at the end of the branch
  if (N > 5) then
    ! Assign first dimension as split along to begin with
    aux(1:N) = D(indx(trunk%left:trunk%right),1)
    maxVariance = variance(aux(1:N))
    splitAlong = 1
    ! Test the other dimensions
    do k = 2,nDims
      aux(1:N) = D(indx(trunk%left:trunk%right), k)
      varianceTmp = variance(aux(1:N))
      if (varianceTmp > maxVariance) then
        maxVariance = varianceTmp
        splitAlong = k
      end if
    enddo

    iMedian = 0
    ! arg select the median value
    call argSelect(D(:, splitAlong), indx, iMid, iMedian, trunk%left, trunk%right)
    trunk%median = D(iMedian, splitAlong)

    ! Allocate the children nodes
    allocate(trunk%buds(2))
    ! Initialize the children nodes
    call trunk%buds(1)%init(trunk%left, iMid)
    call trunk%buds(2)%init(iMid+1, trunk%right)
    ! Grow the tree through the new branches
    call growKdTree_KD(trunk%buds(1), D, indx, aux)
    call growKdTree_KD(trunk%buds(2), D, indx, aux)
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
