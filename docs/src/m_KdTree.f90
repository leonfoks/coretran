module m_KdTree
  !!Module contains classes to create K-dimensional trees in 2, 3, and N dimensions.
  !!
  !!This KdTree is balanced, in that splits are made along the dimension with the largest variance. A quickselect is used to find the median in that split dimension as the splitting value.
  !!
  !!After the tree is initialized, for a given set of points, a search class can be used to perform nearest neighbour, range search, k nearest neighbours etc.
  !!The searches are thread safe and can be used in a parallel region if multiple are needed.
  !!
  !!Once a tree is generated, the point coordinates should not be changed, otherwise the tree will no longer be correct.
  !!
  !!Generating the tree does not modify the incoming point coordinates.
  !!
  !!Example
  !!```fortran
  !!program kdTree_test
  !!use variableKind, only: r64
  !!use m_allocate, only: allocate
  !!use m_deallocate, only: deallocate
  !!use m_random, only: rngNormal
  !!use m_KdTree, only: KdTree, KdTreeSearch
  !!use m_string, only: str
  !!implicit none
  !!real(r64), allocatable :: x(:), y(:)
  !!integer(i32) :: N
  !!type(KdTree) :: tree
  !!type(KdSearch) :: search
  !!
  !!N = 1d6
  !!call allocate(x, N)
  !!call allocate(y, N)
  !!call rngNormal(x)
  !!call rngNormal(y)
  !!call tree%init(x, y)
  !!ia = search%kNearest(tree, x, y, xQuery=0.d0, yQuery=0.d0)
  !!write(*,'(3a)')'Nearest point to the query location: ', str(x(ia)), str(y(ia))
  !!call tree%deallocate()
  !!call deallocate(x)
  !!call deallocate(y)
  !!end program
  !!```
  use variableKind, only: i32, r64
  use m_errors, only: mErr
  use m_allocate, only: allocate
  use m_deallocate, only: deallocate
  use m_array1D, only: arange
  use m_random, only: rngNormal
  use m_select, only: argSelect
  use m_maths, only: variance

  implicit none

  private
  !====================================================================!
  ! Type Definitions
  !====================================================================!
  !====================================================================!
  type, private :: KdTreebranch
    private
    !! A branch of the tree, could be a leaf, or branch
    integer(i32) :: splitAlong  !index of median in dat d
    real(r64) :: median  !median value in dat x
    integer(i32) :: left, right ! left and right indices
    type(KdTreeBranch), pointer :: buds(:) => null()
  contains
    procedure :: init => init_branch
    procedure :: deallocate => deallocate_branch
  end type
  !====================================================================!
  !====================================================================!
  type, public :: KdTree
    !!KdTree in 2, 3, or N dimensions.  See [[m_KdTree]] for more information on how to use this class.
    private
    type(KdTreebranch) :: trunk
    integer(i32),allocatable :: indx(:)
    integer(i32) :: nDims
    integer(i32) :: N
    logical(i32) :: set =.false.
  contains
    generic, public :: init => ikdt2
      !! KdTree%init() - Initialize the class
    procedure, private :: ikdt2 => init2D_KdTree
  !  procedure :: isSet => isSet_KdTree
    procedure, public :: deallocate => deallocate_KdTree
      !! kdTree%deallocate() - deallocate the recursive pointers
  end type
  !====================================================================!
  !====================================================================!
  type, public :: KdTreeSearch
    !!Class to search a KdTree.  See [[m_KdTree]] for more information on how to use this class.
    private
    integer(i32) :: currentNearest
    real(r64) :: distance
  contains
    procedure, public :: init => init_KdTreeSearch !! Initialize the class
    generic, public :: kNearest => kNearest2D !! Perform a k nearest neighbour search
    procedure, private :: kNearest2D => kNearest_2D
  end type
  !====================================================================!

  !====================================================================!
  ! Interface definitions
  !====================================================================!
  interface
    !====================================================================!
    module subroutine init_branch(this, left,right)
      !! Overloaded Type bound procedure KdTreeBranch%init()
    !====================================================================!
    class(KdTreebranch) :: this
      !! KdTreeBranch class
    integer(i32), intent(in) :: left
      !! Left index, first call normally = 1
    integer(i32), intent(in) :: right
      !! Right index, first call normally = size of coordinates
    end subroutine
    !====================================================================!
  end interface
  interface
    !====================================================================!
    module recursive subroutine deallocate_branch(this)
    !====================================================================!
    class(KdTreebranch) :: this
      !! KdTreeBranch class
    end subroutine
    !====================================================================!
  end interface


  interface
    !====================================================================!
    module subroutine init2D_KdTree(this, x, y)
      !! Overloaded Type bound procedure KdTree%init()
    !====================================================================!
    class(kdTree) :: this
      !! KdTree Class
    real(r64),intent(in) :: x(:)
      !! x-coordinates of the points
    real(r64),intent(in) :: y(:)
      !! y-coordinates of the points
    end subroutine
    !====================================================================!
  end interface
  interface
    !====================================================================!
    module subroutine deallocate_KdTree(this)
      !! Overloaded Type bound procedure KdTree%deallocate()
    !====================================================================!
    class(KdTree) :: this
      !! KdTree Class
    end subroutine
    !====================================================================!
  end interface


  interface
    !====================================================================!
    module subroutine init_KdTreeSearch(this)
      !! Overloaded Type bound procedure KdTreeSearch%init()
    !====================================================================!
    class(KdTreeSearch) :: this
      !! KdTreeSearch class
    end subroutine
    !====================================================================!
  end interface
  interface
    !====================================================================!
    module function kNearest_2D(search, tree, x, y, xQuery, yQuery) result(nearest)
      !! Overloaded Type bound procedure KdTreeSearch%kNearest()
    !====================================================================!
    class(KdTreeSearch), intent(inout) :: search
      !! KdTreeSearch class
    class(kdTree),intent(in) :: tree
      !! KdTree class
    real(r64),intent(in) :: x(:)
      !! x co-ordinates of the points
    real(r64),intent(in) :: y(:)
      !! y co-ordinates of the points
    real(r64),intent(in) :: xQuery
      !! x co-ordinate of the query location
    real(r64),intent(in) :: yQuery
      !! y co-ordinate of the query location
    integer(i32) :: nearest
      !! Index of the nearest point to the query location
    end function
    !====================================================================!
  end interface

!  !====================================================================!
!  !====================================================================!
!  ! 3-Dimensional KD tree using 3 1D arrays as input
!  !====================================================================!
!  !====================================================================!
!  !====================================================================!
!  function SearchKD_3D(srch,kd,x,y,z,xq,yq,zq) result(iNear)
!  !====================================================================!
!  class(kdSearch) :: srch
!  class(kdTree),intent(in) :: kd
!  real(r64),intent(in) :: x(:),y(:),z(:)
!  real(r64),intent(in) :: xq,yq,zq
!  integer :: iNear
!  call srch%set()
!  iNear=findNearestFast(kd%trunk, x,y,z, kd%indx, xq,yq,zq, srch%cn, srch%cndiff)
!  iNear=kd%indx(iNear)
!  end function
!  !====================================================================!
!  !====================================================================!
!  subroutine setTree_3D(this,x,y,z)
!  !====================================================================!
!  class(kdTree) :: this
!  real(r64),intent(in) :: x(:),y(:),z(:)
!
!  integer :: i,istat
!  ! Initialize the number of entries and dimensions of the tree
!  this%N=size(x);this%nDims=3
!  ! Set the root node
!  call this%trunk%set(1,this%N)
!  ! Allocated and initialize the indexer
!  if (allocated(this%indx)) deallocate(this%indx)
!  allocate(this%indx(this%N),stat=istat)
!  call Merr(istat,'setTree','indx')
!  this%indx=[(i,i=1,this%N)]
!  ! Grow the KD tree
!  call growTree(this%trunk,x,y,z,this%indx)
!  this%kdSet=.true.
!  end subroutine
!  !====================================================================!
!  !====================================================================!
!  recursive subroutine growTree_3D(root, x,y,z, indx)
!  !====================================================================!
!  class(node) :: root
!  real(r64),intent(in) :: x(:),y(:),z(:)
!  integer :: indx(:)
!
!  integer :: bucketMax, d, nDims, medIndex, sz
!  real(r64) :: spreadMax, var
!
!  bucketMax = 5
!  nDims = 3
!  sz = trunk%maxx-trunk%minn+1
!  medIndex = (trunk%minn+trunk%maxx)/2
!  if (sz>bucketMax) then
!    spreadMax=0.d0
!    var=variance(x(trunk%minn:trunk%maxx))
!    if (var>spreadMax) then
!      spreadMax=var
!      d=1
!    end if
!    var=variance(y(trunk%minn:trunk%maxx))
!    if (var>spreadMax) then
!      spreadMax=var
!      d=2
!    end if
!    var=variance(z(trunk%minn:trunk%maxx))
!    if (var>spreadMax) then
!      spreadMax=var
!      d=3
!    end if
!    trunk%d = d
!    select case(d)
!    case(1)
!      ! Quick sort the current chunk of the data
!      call partialQuickSort(x,indx, trunk%minn, trunk%maxx, medIndex-trunk%minn)
!      trunk%x = x(indx(medIndex))
!    case(2)
!      call partialQuickSort(y,indx, trunk%minn, trunk%maxx, medIndex-trunk%minn)
!      trunk%x = y(indx(medIndex))
!    case(3)
!      call partialQuickSort(z,indx, trunk%minn, trunk%maxx, medIndex-trunk%minn)
!      trunk%x = z(indx(medIndex))
!    end select
!
!    ! Allocate the children nodes
!    allocate(trunk%kiddos(2))
!    ! Initialize the children nodes
!    call trunk%kiddos(1)%set(trunk%minn,medIndex)
!    call trunk%kiddos(2)%set(medIndex+1,trunk%maxx)
!    ! Grow the tree through the new branches
!    call growTree(trunk%kiddos(1),x,y,z,indx)
!    call growTree(trunk%kiddos(2),x,y,z,indx)
!  end if
!  end subroutine
!  !====================================================================!
!  !====================================================================!
!  recursive function findNearestFast_3D(root,x,y,z,indx,xq,yq,zq,cn,cndiff) result(nrst)
!  !====================================================================!
!  class(node),intent(in) :: root
!  real(r64),intent(in) :: x(:),y(:),z(:)
!  integer,intent(in) :: indx(:)
!  real(r64),intent(in) :: xq,yq,zq
!  integer :: cn
!  real(r64) :: cndiff
!  integer :: nrst
!
!  real(r64) :: cnsqdiff,q
!
!  ! If the node is not associated, we are at a leaf.
!  ! Cycle through its buds
!  if (.not. associated(trunk%kiddos)) then
!    cn = findNearestSLOW(x,y,z, indx, trunk%minn, trunk%maxx,xq,yq,zq, cn)
!
!    cnsqdiff=(xq-x(indx(cn)))**2.d0+(yq-y(indx(cn)))**2.d0+(zq-z(indx(cn)))**2.d0
!    cndiff = dsqrt(cnsqdiff)
!  else
!    q=xq
!    select case(trunk%d)
!    case(1)
!      q=xq
!    case(2)
!      q=yq
!    case(3)
!      q=zq
!    end select
!    if (q<=trunk%x) then
!      cn = findNearestFAST(trunk%kiddos(1), x,y,z, indx, xq,yq,zq, cn, cndiff)
!      if ((q+cndiff)>=trunk%x) then
!        cn = findNearestFAST(trunk%kiddos(2), x,y,z, indx, xq,yq,zq, cn, cndiff)
!      end if
!    else
!      cn = findNearestFAST(trunk%kiddos(2), x,y,z, indx, xq,yq,zq, cn, cndiff)
!      if ((q-cndiff)<=trunk%x) then
!        cn = findNearestFAST(trunk%kiddos(1), x,y,z, indx, xq,yq,zq, cn, cndiff)
!      end if
!    end if
!  end if
!  nrst = cn
!  end function
!  !====================================================================!
!  !====================================================================!
!  function findNearestSLOW_3D (x,y,z, indx, minn, maxx,xq,yq,zq, cn) result(nrst)
!  !====================================================================!
!  real(r64),intent(in) :: x(:),y(:),z(:)
!  integer,intent(in) :: indx(:)
!  integer,intent(in) :: maxx,minn
!  real(r64),intent(in) :: xq,yq,zq
!  integer,intent(in) :: cn
!  integer :: nrst
!
!  integer :: i,nDims
!  real(r64) ::  sqdiff,minsqdiff
!
!  ! Get the number of dimensions in the KD tree
!  nDims=3
!
!  ! If cn is zero, we went straight to a leaf node
!  if (cn==0) then
!    i=minn
!  else
!    i=cn
!  end if
!
!  sqdiff=(xq-x(indx(i)))**2.d0+(yq-y(indx(i)))**2.d0+(zq-z(indx(i)))**2.d0
!
!  minsqdiff = sqdiff
!  nrst = i
!
!  do i=minn, maxx
!    sqdiff=(xq-x(indx(i)))**2.d0+(yq-y(indx(i)))**2.d0+(zq-z(indx(i)))**2.d0
!    if (sqdiff<minsqdiff) then
!      minsqdiff=sqdiff
!      nrst=i
!    end if
!  end do
!  end function
!  !====================================================================!
!  !====================================================================!
!  !====================================================================!
!  ! N-Dimensional KD tree using a 2D array as input
!  !====================================================================!
!  !====================================================================!
!  !====================================================================!
!  function SearchKD_nD(srch,kd,this,q) result(iNear)
!  !====================================================================!
!  class(kdSearch) :: srch
!  class(kdTree),intent(in) :: kd
!  real(r64),intent(in) :: this(:,:)
!  real(r64),intent(in) :: q(:)
!  integer :: iNear
!  call srch%set()
!  iNear=findNearestFast(kd%trunk, this, kd%indx, q, srch%cn, srch%cndiff)
!  iNear=kd%indx(iNear)
!  end function
!  !====================================================================!
!  !====================================================================!
!  subroutine setTree_nD(this,arr)
!  !====================================================================!
!  class(kdTree) :: this
!  real(r64),intent(in) :: arr(:,:)
!
!  integer :: i,istat
!  ! Initialize the number of entries and dimensions of the tree
!  this%N=size(arr,1);this%nDims=size(arr,2)
!  ! Set the root node
!  call this%trunk%set(1,this%N)
!  ! Allocated and initialize the indexer
!  if (allocated(this%indx)) deallocate(this%indx)
!  allocate(this%indx(this%N),stat=istat)
!  call Merr(istat,'setTree','indx')
!  this%indx=[(i,i=1,this%N)]
!  ! Grow the KD tree
!  call growTree(this%trunk,arr,this%indx)
!  this%kdSet=.true.
!  end subroutine
!  !====================================================================!
!  !====================================================================!
!  recursive subroutine growTree_nD(root, dat, indx)
!  !====================================================================!
!  class(node) :: root
!  real(r64),intent(in) :: dat(:,:)
!  integer :: indx(:)
!
!  integer :: bucketMax, j, d, nDims, medIndex, sz
!  real(r64) :: spreadMax, var
!
!  bucketMax = 5
!  nDims = size(dat,2)
!  sz = trunk%maxx-trunk%minn+1
!  if (sz>bucketMax) then
!    d=1
!    if(nDims/=1) then
!      spreadMax=0.d0
!      do j=1,nDims
!        var=variance(dat(trunk%minn:trunk%maxx,j))
!        if (var>spreadMax) then
!          spreadMax=var
!          d=j
!        end if
!      end do
!    endif
!    medIndex = (trunk%minn+trunk%maxx)/2
!    ! Quick sort the current chunk of the data
!    call partialQuickSort(dat(:,d),indx, trunk%minn, trunk%maxx, medIndex-trunk%minn)
!    trunk%d = d
!    trunk%x = dat(indx(medIndex),d)
!    ! Allocate the children nodes
!    allocate(trunk%kiddos(2))
!    ! Initialize the children nodes
!    call trunk%kiddos(1)%set(trunk%minn,medIndex)
!    call trunk%kiddos(2)%set(medIndex+1,trunk%maxx)
!    ! Grow the tree through the new branches
!    call growTree(trunk%kiddos(1),dat,indx)
!    call growTree(trunk%kiddos(2),dat,indx)
!  end if
!  end subroutine
!  !====================================================================!
!  !====================================================================!
!  recursive function findNearestFast_nD(root, dat, indx, q, cn, cndiff) result(nrst)
!  !====================================================================!
!  class(node),intent(in) :: root
!  real(r64),intent(in) :: dat(:,:)
!  integer,intent(in) :: indx(:)
!  real(r64),intent(in) :: q(:)
!  integer :: cn
!  real(r64) :: cndiff
!  integer :: nrst
!
!  real(r64) :: cnsqdiff
!
!  ! If the node is not associated, we are at a leaf.
!  ! Cycle through its buds
!  if (.not. associated(trunk%kiddos)) then
!    cn = findNearestSLOW(dat, indx, trunk%minn, trunk%maxx, q, cn)
!
!    cnsqdiff=sum((q-dat(indx(cn),:))**2.d0)
!    cndiff = dsqrt(cnsqdiff)
!  else
!    if (q(trunk%d)<=trunk%x) then
!      cn = findNearestFAST(trunk%kiddos(1), dat, indx, q, cn, cndiff)
!      if ((q(trunk%d)+cndiff)>=trunk%x) then
!        cn = findNearestFAST(trunk%kiddos(2), dat, indx, q, cn, cndiff)
!      end if
!    else
!      cn = findNearestFAST(trunk%kiddos(2), dat, indx, q, cn, cndiff)
!      if ((q(trunk%d)-cndiff)<=trunk%x) then
!        cn = findNearestFAST(trunk%kiddos(1), dat, indx, q, cn, cndiff)
!      end if
!    end if
!  end if
!  nrst = cn
!  end function
!  !====================================================================!
!  !====================================================================!
!  function findNearestSLOW_nD (dat, indx, minn, maxx, q, cn) result(nrst)
!  !====================================================================!
!  real(r64),intent(in) :: dat(:,:)
!  integer,intent(in) :: indx(:)
!  integer,intent(in) :: maxx,minn
!  real(r64),intent(in) :: q(:)
!  integer,intent(in) :: cn
!  integer :: nrst
!
!  integer :: i,nDims
!  real(r64) ::  sqdiff,minsqdiff
!
!  ! Get the number of dimensions in the KD tree
!  nDims=size(dat,2)
!
!  ! If cn is zero, we went straight to a leaf node
!  if (cn==0) then
!    i=minn
!  else
!    i=cn
!  end if
!
!  sqdiff=sum((q-dat(indx(i),:))**2.d0)
!
!  minsqdiff = sqdiff
!  nrst = i
!
!  do i=minn, maxx
!    sqdiff=sum((q-dat(indx(i),:))**2.d0)
!    if (sqdiff<minsqdiff) then
!      minsqdiff=sqdiff
!      nrst=i
!    end if
!  end do
!  end function
!  !====================================================================!
end module

