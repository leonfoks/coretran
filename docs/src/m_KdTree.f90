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
  !!real(r64), allocatable :: x(:), y(:), z(:)
  !!integer(i32) :: N
  !!type(KdTree) :: tree
  !!type(KdSearch) :: search
  !!
  !!N = 1d6
  !!call allocate(x, N)
  !!call allocate(y, N)
  !!call rngNormal(x)
  !!call rngNormal(y)
  !!tree = KdTree(x, y)
  !!ia = search%kNearest(tree, x, y, xQuery=0.d0, yQuery=0.d0)
  !!write(*,'(3a)')'Nearest point to the query location: ', str(x(ia)), str(y(ia))
  !!call tree%deallocate()
  !!
  !!call allocate(z, N)
  !!call rngNormal(z)
  !!tree = KdTree(x, y, z)
  !!ia = search%kNearest(tree, x, y, z, xQuery=0.d0, yQuery=0.d0)
  !!write(*,'(a)')'Nearest point to the query location: '//str(x(ia))//str(y(ia))//str(z(ia))
  !!call tree%deallocate()
  !!call deallocate(x)
  !!call deallocate(y)
  !!call deallocate(z)
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
      !! KdTreeBranch%init() - Initialize the class
    procedure :: deallocate => deallocate_branch
      !! KdTreeBranch%deallocate() - deallocate the branch
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
    procedure, public :: deallocate => deallocate_KdTree
      !! kdTree%deallocate() - deallocate the recursive pointers
  end type
  !====================================================================!
  !====================================================================!
  type, public :: KdTreeSearch
    !!Class to search a KdTree.  See [[m_KdTree]] for more information on how to use this class.
  contains
    generic, public :: kNearest => kNearest2D, kNearest3D, kNearestKD
      !! KdTreeSearch%kNearest() - Perform a k nearest neighbour search
    procedure, private :: kNearest2D => kNearest_2D
      !! Overloaded typebound procedure with KdTreeSearch%kNearest()
    procedure, private :: kNearest3D => kNearest_3D
      !! Overloaded typebound procedure with KdTreeSearch%kNearest()
    procedure, private :: kNearestKD => kNearest_KD
      !! Overloaded typebound procedure with KdTreeSearch%kNearest()
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


  interface KdTree
    !!Overloaded Initializer for a KdTree.
    !!
    !!Can be used to create a 2D, 3D, or ND, KdTree class.
    !!
    !!See [[m_KdTree]] for more information on how to use this class
    !====================================================================!
    module function init2D_KdTree(x, y) result(this)
      !! Overloaded by interface [[KdTree(type)]]
    !====================================================================!
    type(kdTree) :: this
      !! KdTree Class
    real(r64),intent(in) :: x(:)
      !! x-coordinates of the points
    real(r64),intent(in) :: y(:)
      !! y-coordinates of the points
    end function
    !====================================================================!
    !====================================================================!
    module function init3D_KdTree(x, y, z) result(this)
      !! Overloaded by interface [[KdTree(type)]]
    !====================================================================!
    type(kdTree) :: this
      !! KdTree Class
    real(r64),intent(in) :: x(:)
      !! x-coordinates of the points
    real(r64),intent(in) :: y(:)
      !! y-coordinates of the points
    real(r64),intent(in) :: z(:)
      !! z-coordinates of the points
    end function
    !====================================================================!
    !====================================================================!
    module function initKD_KdTree(D) result(this)
      !! Overloaded by interface [[KdTree(type)]]
    !====================================================================!
    type(kdTree) :: this
      !! KdTree Class
    real(r64),intent(in) :: D(:,:)
      !! Coordinates of the points, the k columns contain the k dimensional values.
    end function
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


!  interface
!    !====================================================================!
!    module subroutine init_KdTreeSearch(this)
!      !! Overloaded Type bound procedure KdTreeSearch%init()
!    !====================================================================!
!    class(KdTreeSearch) :: this
!      !! KdTreeSearch class
!    end subroutine
!    !====================================================================!
!  end interface
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
  interface
    !====================================================================!
    module function kNearest_3D(search, tree, x, y, z, xQuery, yQuery, zQuery) result(nearest)
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
    real(r64),intent(in) :: z(:)
      !! z co-ordinates of the points
    real(r64),intent(in) :: xQuery
      !! x co-ordinate of the query location
    real(r64),intent(in) :: yQuery
      !! y co-ordinate of the query location
    real(r64),intent(in) :: zQuery
      !! z co-ordinate of the query location
    integer(i32) :: nearest
      !! Index of the nearest point to the query location
    end function
    !====================================================================!
  end interface
  interface
    !====================================================================!
    module function kNearest_KD(search, tree, D, query) result(nearest)
      !! Overloaded Type bound procedure KdTreeSearch%kNearest()
    !====================================================================!
    class(KdTreeSearch), intent(inout) :: search
      !! KdTreeSearch class
    class(kdTree),intent(in) :: tree
      !! KdTree class
    real(r64),intent(in) :: D(:,:)
      !! Co-ordinates of the points, the k columns contain the k dimensional values.
    real(r64),intent(in) :: query(:)
      !! C-ordinate of the query location
    integer(i32) :: nearest
      !! Index of the nearest point to the query location
    end function
    !====================================================================!
  end interface

end module

