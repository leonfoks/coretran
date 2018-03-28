module m_KdTree
  !!# KdTree
  !!Build and search k-dimensional trees in 2, 3, and K dimensions.
  !!This KdTree is balanced, in that splits are made along the dimension with the largest variance. 
  !!A quickselect is used to quickly find the median in each splitting dimension as the splitting value.
  !!The ends of each branch contain multiple leaves to prevent tail recursion.
  !!An in-depth example is given below on how to use all the aspects of the KdTree and KdTreeSearch classes.
  !!
  !!Important: Once a tree has been built with a set, do not change their values.  The KdTree does NOT make
  !!a copy of the input values used to build it.
  !!Important: Generating the tree does not modify the values used to build it.
  !!
  !!## Building the KdTree
  !!The KdTree object can be initialized on assignment by entering point co-ordinates.
  !!To build a 2-D tree, you can use two 1-D arrays as the x, y, co-ordinates, and optionally a third 1-D array
  !!to build a 3-D tree.
  !!```fortran
  !!use m_KdTree
  !!type(KdTree) :: tree
  !!tree = KdTree(x, y, [z])
  !!```
  !!Or you can build a k-dimensional tree using a 2-D array, where the first dimension is the number of items,
  !!and the second is the number of dimensions, k.
  !!```fortran
  !!use m_KdTree
  !!type(KdTree) :: tree
  !!tree = KdTree(D)
  !!```
  !!
  !!## Querying the KdTree
  !!After the tree is initialized, a search class can be used to perform search for the nearest neighbour, 
  !!the k nearest neighbours, all neighbours within a radius, k nearest within a radius, and items within 
  !!upper and lower bounds in each dimension.
  !!The searches are thread safe and can be used in a parallel region.
  !!
  !!After the KdTree is built, various queries can be carried out.  Searches that return multiple values
  !!are called using the argDynamicArray within coretran. The [[dArgDynamicArray_Class]] contains an integer index
  !!containing the indices into the co-ordinates that are closest, and a double precision that contains the
  !!distance from the query point to those points.
  !!
  !!```fortran
  !!use dArgDynamicArray_Class
  !!use m_KdTree
  !!type(KdTreeSearch) :: search
  !!integer(i32) :: i
  !!type(dArgDynamicArray) :: da
  !!! Nearest Neighbour to (0, 0), for 3D add z, and zQuery
  !!i = search%nearest(tree, x, y, [z], xQuery = 0.d0, yQuery = 0.d0, [zQuery = 0.d0])
  !!! K-Nearest to (0, 0), for 3D add z, and zQuery
  !!da = search%kNearest(tree, x, y, [z], xQuery = 0.d0, yQuery = 0.d0, [zQuery = 0.d0], k = 10)
  !!! Search for all points within a given distance
  !!da = search%kNearest(tree, x, y, [z], xQuery = 0.d0, yQuery = 0.d0, [zQuery = 0.d0], radius = 10.d0)
  !!! Search for all k points within a given distance
  !!da = search%kNearest(tree, x, y, [z], xQuery = 0.d0, yQuery = 0.d0, [zQuery = 0.d0], k = 10, radius = 10.d0)
  !!```
  !!
  !!## Full Example
  !!```fortran
  !!program kdTree_test
  !!use variableKind, only: i32, r64
  !!use m_allocate, only: allocate
  !!use m_deallocate, only: deallocate
  !!use m_random, only: rngNormal
  !!use m_KdTree, only: KdTree, KdTreeSearch
  !!use m_dArgDynamicArray, only: dArgDynamicArray
  !!use m_string, only: str
  !!implicit none
  !!real(r64), allocatable :: x(:), y(:), z(:), D(:,:)
  !!integer(i32) :: ia, N
  !!type(KdTree) :: tree
  !!type(KdSearch) :: search
  !!type(dArgDynamicArray) :: da
  !!
  !!!====================================================================!
  !!! 2D KdTree example
  !!!====================================================================!
  !!! Create some random points in space
  !!N = 1d6
  !!call allocate(x, N)
  !!call allocate(y, N)
  !!call rngNormal(x)
  !!call rngNormal(y)
  !!! Build the tree
  !!tree = KdTree(x, y)
  !!! Get the nearest neighbour to (0, 0)
  !!ia = search%kNearest(tree, x, y, xQuery = 0.d0, yQuery = 0.d0)
  !!write(*,'(a)') 'Nearest point to the query location: '//str(x(ia))//str(y(ia))
  !!! Get the 10 nearest neighbours to the query
  !!da = search%kNearest(tree, x, y, xQuery = 0.d0, yQuery = 0.d0, k = 10)
  !!write(*,'(a)') 'The 10 nearest neighbour indices and distances:'
  !!call da%print()
  !!! Find all the points within a 1.d0
  !!da = search%kNearest(tree, x, y, xQuery = 0.d0, yQuery = 0.d0, radius = 1.d0)
  !!write(*,'(a)') 'The points within a distance of 1.d0'
  !!call da%print()
  !!Deallocate any tree memory
  !!call tree%deallocate()
  !!
  !!!====================================================================!
  !!! 3D KdTree example
  !!!====================================================================!
  !!! Create the third dimension
  !!call allocate(z, N)
  !!call rngNormal(z)
  !!! Build the tree
  !!tree = KdTree(x, y, z)
  !!! Get the nearest neighbour to (0, 0, 0)
  !!ia = search%kNearest(tree, x, y, z, xQuery = 0.d0, yQuery = 0.d0, zQuery = 0.d0)
  !!write(*,'(a)')'Nearest point to the query location: '//str(x(ia))//str(y(ia))//str(z(ia))
  !!! Get the 10 nearest neighbours to the query
  !!da = search%kNearest(tree, x, y, z, xQuery = 0.d0, yQuery = 0.d0, zQuery = 0.d0, k = 10)
  !!write(*,'(a)') 'The 10 nearest neighbour indices and distances:'
  !!call da%print()
  !!! Find all the points within a 1.d0
  !!da = search%kNearest(tree, x, y, z, xQuery = 0.d0, yQuery = 0.d0, zQuery = 0.d0, radius = 1.d0)
  !!write(*,'(a)') 'The points within a distance of 1.d0'
  !!call da%print()
  !!Deallocate any tree memory
  !!call tree%deallocate()
  !!
  !!!====================================================================!
  !!! KD KdTree example
  !!!====================================================================!
  !!call allocate(D, [N, 3])
  !!D(:,1) = x
  !!D(:,2) = y
  !!D(:,3) = z
  !!! Build the tree
  !!tree = KdTree(D)
  !!! Get the nearest neighbour to (0, 0, 0)
  !!ia = search%kNearest(tree, D, query = [0.d0, 0.d0, 0.d0])
  !!write(*,'(a)')'Nearest point to the query location: '//str(D(ia, 1))//str(D(ia, 2))//str(D(ia, 3))
  !!! Get the 10 nearest neighbours to the query
  !!da = search%kNearest(tree, D, query = [0.d0, 0.d0, 0.d0], k = 10)
  !!write(*,'(a)') 'The 10 nearest neighbour indices and distances:'
  !!call da%print()
  !!! Find all the points within a 1.d0
  !!da = search%kNearest(tree, D,  query = [0.d0, 0.d0, 0.d0], radius = 1.d0)
  !!write(*,'(a)') 'The points within a distance of 1.d0'
  !!call da%print()
  !!Deallocate any tree memory
  !!call tree%deallocate()
  !!call deallocate(x)
  !!call deallocate(y)
  !!call deallocate(z)
  !!call deallocate(D)
  !!end program
  !!```
  use variableKind, only: i32, r64
  use m_allocate, only: allocate
  use m_array1D, only: arange
  use m_deallocate, only: deallocate
  use m_errors, only: eMsg
  use m_maths, only: variance
  use iDynamicArray_Class, only: iDynamicArray
  use dArgDynamicArray_Class, only: dArgDynamicArray
  use m_select, only: argSelect
  use m_strings, only: str

  implicit none

  private

  public :: KdTree
  public :: KdTreeSearch

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
  type :: KdTree
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
  type :: KdTreeSearch
    !!Class to search a KdTree.  See [[m_KdTree]] for more information on how to use this class.
  contains
    generic, public :: nearest => nearest2D, nearest3D, nearestKD
      !! KdTreeSearch%nearest() - Perform a nearest neighbour search
    procedure, private :: nearest2D => nearest_2D
      !! Overloaded type bound procedure with KdTreeSearch%nearest()
    procedure, private :: nearest3D => nearest_3D
      !! Overloaded type bound procedure with KdTreeSearch%nearest()
    procedure, private :: nearestKD => nearest_KD
      !! Overloaded type bound procedure with KdTreeSearch%nearest()
    generic, public :: kNearest => kNearest2D, kNearest3D, kNearestKD
      !! KdTreeSearch%kNearest() - Perform a k nearest neighbour search or a radius search.
    procedure, private :: kNearest2D => kNearest_2D
      !! Overloaded type bound procedure with KdTreeSearch%kNearest()
    procedure, private :: kNearest3D => kNearest_3D
      !! Overloaded type bound procedure with KdTreeSearch%kNearest()
    procedure, private :: kNearestKD => kNearest_KD
      !! Overloaded type bound procedure with KdTreeSearch%kNearest()
    generic, public :: rangeSearch => rangeSearch2D, rangeSearch3D, rangeSearchKD
      !! KdTreeSearch%rangeSearch() - Find all points within axis aligned lower and upper bounds
    procedure, private :: rangeSearch2D => rangeSearch_2D
      !! Overloaded type bound procedure with KdTreeSearch%rangeSearch()
    procedure, private :: rangeSearch3D => rangeSearch_3D
      !! Overloaded type bound procedure with KdTreeSearch%rangeSearch()
    procedure, private :: rangeSearchKD => rangeSearch_KD
      !! Overloaded type bound procedure with KdTreeSearch%rangeSearch()
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


interface
    !====================================================================!
    module function nearest_2D(search, tree, x, y, xQuery, yQuery) result(nearest)
      !! Overloaded Type bound procedure KdTreeSearch%nearest()
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
    !====================================================================!
    module function nearest_3D(search, tree, x, y, z, xQuery, yQuery, zQuery) result(nearest)
      !! Overloaded Type bound procedure KdTreeSearch%nearest()
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
    !====================================================================!
    module function nearest_KD(search, tree, D, query) result(nearest)
      !! Overloaded Type bound procedure KdTreeSearch%nearest()
    !====================================================================!
    class(KdTreeSearch), intent(inout) :: search
      !! KdTreeSearch class
    class(kdTree),intent(in) :: tree
      !! KdTree class
    real(r64),intent(in) :: D(:,:)
      !! Co-ordinates of the points, the second dimension contains the k dimensions
    real(r64),intent(in) :: query(:)
      !! C-ordinate of the query location
    integer(i32) :: nearest
      !! Index of the nearest point to the query location
    end function
    !====================================================================!
  end interface


  interface
    !====================================================================!
    module function kNearest_2D(search, tree, x, y, xQuery, yQuery, k, radius) result(kNearest)
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
    integer(i32), intent(in), optional :: k
      !! Number of points to find that are closest to the query
    real(r64), intent(in), optional :: radius
      !! Only find neighbours within this distance from the query
    type(dArgDynamicArray) :: kNearest
      !! Indices of the nearest points to the query location
    end function
    !====================================================================!
    !====================================================================!
    module function kNearest_3D(search, tree, x, y, z, xQuery, yQuery, zQuery, k, radius) result(kNearest)
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
    integer(i32), intent(in), optional :: k
      !! Number of points to find that are closest to the query
    real(r64), intent(in), optional :: radius
      !! Only find neighbours within this distance from the query
    type(dArgDynamicArray) :: kNearest
      !! Indices of the nearest points to the query location
    end function
    !====================================================================!
    !====================================================================!
    module function kNearest_KD(search, tree, D, query, k, radius) result(kNearest)
      !! Overloaded Type bound procedure KdTreeSearch%kNearest()
    !====================================================================!
    class(KdTreeSearch), intent(inout) :: search
      !! KdTreeSearch class
    class(kdTree),intent(in) :: tree
      !! KdTree class
    real(r64),intent(in) :: D(:,:)
      !! Co-ordinates of the points, the second dimension contains the k dimensions
    real(r64),intent(in) :: query(:)
      !! Co-ordinate of the query location
    integer(i32), intent(in), optional :: k
      !! Number of points to find that are closest to the query
    real(r64), intent(in), optional :: radius
      !! Only find neighbours within this distance from the query
    type(dArgDynamicArray) :: kNearest
      !! Indices of the nearest points to the query location
    end function
    !====================================================================!
  end interface


  interface
    !====================================================================!
    module function rangeSearch_2D(search, tree, x, y, lowerBound, upperBound) result(iPoints)
      !! Overloaded Type bound procedure KdTreeSearch%rangeSearch()
    !====================================================================!
    class(KdTreeSearch), intent(inout) :: search
      !! KdTreeSearch class
    class(kdTree),intent(in) :: tree
      !! KdTree class
    real(r64),intent(in) :: x(:)
      !! x co-ordinates of the points.
    real(r64),intent(in) :: y(:)
      !! y co-ordinates of the points.
    real(r64),intent(in) :: lowerBound(2)
      !! Lower bounds in x and y for the range search [xLow, yLow].
    real(r64),intent(in) :: upperBound(2)
      !! Upper bounds in x and y for the range search [xHigh, yHigh].
    type(iDynamicArray) :: iPoints
      !! Indices of the points inside the axis aligned range.
    end function
    !====================================================================!
    !====================================================================!
    module function rangeSearch_3D(search, tree, x, y, z, lowerBound, upperBound) result(iPoints)
      !! Overloaded Type bound procedure KdTreeSearch%rangeSearch()
    !====================================================================!
    class(KdTreeSearch), intent(inout) :: search
      !! KdTreeSearch class
    class(kdTree),intent(in) :: tree
      !! KdTree class
    real(r64),intent(in) :: x(:)
      !! x co-ordinates of the points.
    real(r64),intent(in) :: y(:)
      !! y co-ordinates of the points.
    real(r64),intent(in) :: z(:)
      !! y co-ordinates of the points.
    real(r64),intent(in) :: lowerBound(3)
      !! Lower bounds in x and y for the range search [xLow, yLow].
    real(r64),intent(in) :: upperBound(3)
      !! Upper bounds in x and y for the range search [xHigh, yHigh].
    type(iDynamicArray) :: iPoints
      !! Indices of the points inside the axis aligned range.
    end function
    !====================================================================!
    !====================================================================!
    module function rangeSearch_KD(search, tree, D, lowerBound, upperBound) result(iPoints)
      !! Overloaded Type bound procedure KdTreeSearch%rangeSearch()
    !====================================================================!
    class(KdTreeSearch), intent(inout) :: search
      !! KdTreeSearch class
    class(kdTree),intent(in) :: tree
      !! KdTree class
    real(r64),intent(in) :: D(:,:)
      !! Co-ordinates of the points, the second dimension contains the k dimensions
    real(r64),intent(in) :: lowerBound(size(D,2))
      !! Lower bounds in each dimension for the range search [Low1, Low2, ..., LowK].
    real(r64),intent(in) :: upperBound(size(D,2))
      !! Upper bounds in each dimension for the range search [High1, High2, ..., HighK].
    type(iDynamicArray) :: iPoints
      !! Indices of the points inside the axis aligned range.
    end function
    !====================================================================!
  end interface
end module

