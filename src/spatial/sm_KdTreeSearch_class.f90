submodule (m_KdTree) sm_KdTreeSearch_class
  !! Contains the implementations of overloaded KdTreeSearch class type bound procedures

implicit none

contains

  !====================================================================!
  module procedure init_KdTreeSearch
    !! Overloaded Type bound procedure KdTreeSearch%init()
  !====================================================================!
  !class(KdTreeSearch) :: this
  this%currentNearest = 0
  this%distance = huge(0.d0)
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure kNearest_2D ! (search, tree, x, y, xQuery, yQuery) result(nearest)
    !! Overloaded Type bound procedure KdTreeSearch%kNearest()
  !====================================================================!
  !class(KdTreeSearch), intent(inout) :: search
  !class(kdTree),intent(in) :: tree
  !real(r64),intent(in) :: x(:), y(:)
  !real(r64),intent(in) :: xQuery, yQuery
  !integer(i32) :: nearest

  integer(i32) :: currentNearest
  real(r64) :: distance

  currentNearest = 0 ! No nearest yet
  distance = huge(0.0_r64) ! Max distance to initialize

  nearest = nearestBranch_2D(tree%trunk, x, y, tree%indx, xQuery, yQuery, currentNearest, distance)
  nearest = tree%indx(nearest)
  end procedure
  !====================================================================!
  !====================================================================!
  recursive function nearestBranch_2D(trunk, x, y, indx, xQuery, yQuery, currentNearest, distance) result(nearest)
    !! Recurse through the tree branches to find the nearest branches to the query location
  !====================================================================!
  class(KdTreebranch),intent(in) :: trunk
  real(r64),intent(in) :: x(:),y(:)
  integer,intent(in) :: indx(:)
  real(r64),intent(in) :: xQuery,yQuery
  integer(i32), intent(inout) :: currentNearest
  real(r64), intent(inout) :: distance
  integer(i32) :: nearest

  real(r64) :: query
  integer(i32) :: iNear, iTmp

  ! If the node is not associated, we are at final search block.
  if (.not. associated(trunk%buds)) then
    currentNearest = nearestLeaf_2D(x, y, indx, trunk%left, trunk%right, xQuery, yQuery, currentNearest)
    iTmp = indx(currentNearest)
    distance = dsqrt((xQuery - x(iTmp))**2.d0 + (yQuery - y(iTmp))**2.d0)

  else ! Otherwise, keep searching through the branches

    select case(trunk%splitAlong)
    case(1)
      query = xQuery
    case(2)
      query = yQuery
    end select

    if (query <= trunk%median) then ! If the query is to the left of the current median
      currentNearest = nearestBranch_2D(trunk%buds(1), x, y, indx, xQuery, yQuery, currentNearest, distance)
      if ((query + distance) >= trunk%median) then ! Need to still check the other half if its on the other side of the median
        currentNearest = nearestBranch_2D(trunk%buds(2), x, y, indx, xQuery, yQuery, currentNearest, distance)
      end if
    else ! The query is to the right of the current median
      currentNearest = nearestBranch_2D(trunk%buds(2), x, y, indx, xQuery, yQuery, currentNearest, distance)
      if ((query-distance) <= trunk%median) then ! Need to still check the other half if its on the other side of the median
        currentNearest = nearestBranch_2D(trunk%buds(1), x, y, indx, xQuery, yQuery, currentNearest, distance)
      end if
    end if
  end if
  nearest = currentNearest
  end function
  !====================================================================!
  !====================================================================!
  function nearestLeaf_2D (x, y, indx, left, right, xQuery, yQuery, currentNearest) result(nearest)
    !! For a branch with leaves, perform a brute force minimum distance search over the leaves. The number of leaves should be small for efficiency!
  !====================================================================!
  real(r64), intent(in) :: x(:),y(:)
  integer(i32), intent(in) :: indx(:)
  integer(i32), intent(in) :: left, right
  real(r64), intent(in) :: xQuery, yQuery
  integer(i32), intent(in) :: currentNearest
  integer(i32) :: nearest

  integer(i32) :: i, iTmp, iLeft
  real(r64) ::  distance, minDistance

  ! If the currentNearest is zero (i.e. uninitialized), we went straight to a set of leaves
  i = currentNearest
  iLeft = left
  if (currentNearest == 0) then
    i = left
    iLeft = i+1
  end if

  iTmp = indx(i)
  minDistance=(xQuery - x(iTmp))**2.d0 + (yQuery-y(iTmp))**2.d0
  nearest = i

  ! Find the minimum point distance to the query out of all the leaves
  do i=iLeft, right
    iTmp = indx(i)
    distance = (xQuery - x(iTmp))**2.d0 + (yQuery - y(iTmp))**2.d0
    if (distance < minDistance) then
      minDistance = distance
      nearest = i
    end if
  end do
  end function
  !====================================================================!

  !====================================================================!
  module procedure kNearest_3D ! (search, tree, x, y, z, xQuery, yQuery, zQuery) result(nearest)
    !! Overloaded Type bound procedure KdTreeSearch%kNearest()
  !====================================================================!
  !class(KdTreeSearch), intent(inout) :: search
  !class(kdTree),intent(in) :: tree
  !real(r64),intent(in) :: x(:), y(:), z(:)
  !real(r64),intent(in) :: xQuery, yQuery, zQuery
  !integer(i32) :: nearest

  integer(i32) :: currentNearest
  real(r64) :: distance

  currentNearest = 0 ! No nearest yet
  distance = huge(0.0_r64) ! Max distance to initialize

  nearest = nearestBranch_3D(tree%trunk, x, y, z, tree%indx, xQuery, yQuery, zQuery, currentNearest, distance)
  nearest = tree%indx(nearest)
  end procedure
  !====================================================================!
  !====================================================================!
  recursive function nearestBranch_3D(trunk, x, y, z, indx, xQuery, yQuery, zQuery, currentNearest, distance) result(nearest)
    !! Recurse through the tree branches to find the nearest branches to the query location
  !====================================================================!
  class(KdTreebranch),intent(in) :: trunk
  real(r64),intent(in) :: x(:),y(:),z(:)
  integer,intent(in) :: indx(:)
  real(r64),intent(in) :: xQuery,yQuery,zQuery
  integer(i32), intent(inout) :: currentNearest
  real(r64), intent(inout) :: distance
  integer(i32) :: nearest

  real(r64) :: query
  integer(i32) :: iNear, iTmp

  ! If the node is not associated, we are at final search block.
  if (.not. associated(trunk%buds)) then
    currentNearest = nearestLeaf_3D(x, y, z, indx, trunk%left, trunk%right, xQuery, yQuery, zQuery, currentNearest)
    iTmp = indx(currentNearest)
    distance = dsqrt((xQuery - x(iTmp))**2.d0 + (yQuery - y(iTmp))**2.d0 + (zQuery - z(iTmp))**2.d0)

  else ! Otherwise, keep searching through the branches

    select case(trunk%splitAlong)
    case(1)
      query = xQuery
    case(2)
      query = yQuery
    case(3)
      query = zQuery
    end select

    if (query <= trunk%median) then ! If the query is to the left of the current median
      currentNearest = nearestBranch_3D(trunk%buds(1), x, y, z, indx, xQuery, yQuery, zQuery, currentNearest, distance)
      if ((query + distance) >= trunk%median) then ! Need to still check the other half if its on the other side of the median
        currentNearest = nearestBranch_3D(trunk%buds(2), x, y, z, indx, xQuery, yQuery, zQuery, currentNearest, distance)
      end if
    else ! The query is to the right of the current median
      currentNearest = nearestBranch_3D(trunk%buds(2), x, y, z, indx, xQuery, yQuery, zQuery, currentNearest, distance)
      if ((query-distance) <= trunk%median) then ! Need to still check the other half if its on the other side of the median
        currentNearest = nearestBranch_3D(trunk%buds(1), x, y, z, indx, xQuery, yQuery, zQuery, currentNearest, distance)
      end if
    end if
  end if
  nearest = currentNearest
  end function
  !====================================================================!
  !====================================================================!
  function nearestLeaf_3D (x, y, z, indx, left, right, xQuery, yQuery, zQuery, currentNearest) result(nearest)
    !! For a branch with leaves, perform a brute force minimum distance search over the leaves. The number of leaves should be small for efficiency!
  !====================================================================!
  real(r64), intent(in) :: x(:),y(:),z(:)
  integer(i32), intent(in) :: indx(:)
  integer(i32), intent(in) :: left, right
  real(r64), intent(in) :: xQuery, yQuery, zQuery
  integer(i32), intent(in) :: currentNearest
  integer(i32) :: nearest

  integer(i32) :: i, iTmp, iLeft
  real(r64) ::  distance, minDistance

  ! If the currentNearest is zero (i.e. uninitialized), we went straight to a set of leaves
  i = currentNearest
  iLeft = left
  if (currentNearest == 0) then
    i = left
    iLeft = i+1
  end if

  iTmp = indx(i)
  minDistance=(xQuery - x(iTmp))**2.d0 + (yQuery-y(iTmp))**2.d0 + (zQuery-z(iTmp))**2.d0
  nearest = i

  ! Find the minimum point distance to the query out of all the leaves
  do i=iLeft, right
    iTmp = indx(i)
    distance = (xQuery - x(iTmp))**2.d0 + (yQuery - y(iTmp))**2.d0 + (zQuery-z(iTmp))**2.d0
    if (distance < minDistance) then
      minDistance = distance
      nearest = i
    end if
  end do
  end function
  !====================================================================!


  !====================================================================!
  module procedure kNearest_KD ! (search, tree, D, query) result(nearest)
    !! Overloaded Type bound procedure KdTreeSearch%kNearest()
  !====================================================================!
  !class(KdTreeSearch), intent(inout) :: search
  !class(kdTree),intent(in) :: tree
  !real(r64),intent(in) :: D(:,:)
  !real(r64),intent(in) :: query(:)
  !integer(i32) :: nearest

  integer(i32) :: currentNearest
  real(r64) :: distance

  currentNearest = 0 ! No nearest yet
  distance = huge(0.0_r64) ! Max distance to initialize

  nearest = nearestBranch_KD(tree%trunk, D, tree%indx, query, currentNearest, distance)
  nearest = tree%indx(nearest)
  end procedure
  !====================================================================!
  !====================================================================!
  recursive function nearestBranch_KD(trunk, D, indx, query, currentNearest, distance) result(nearest)
    !! Recurse through the tree branches to find the nearest branches to the query location
  !====================================================================!
  class(KdTreebranch),intent(in) :: trunk
  real(r64),intent(in) :: D(:,:)
  integer,intent(in) :: indx(:)
  real(r64),intent(in) :: query(:)
  integer(i32), intent(inout) :: currentNearest
  real(r64), intent(inout) :: distance
  integer(i32) :: nearest

  real(r64) :: queryTmp
  real(r64) :: test(size(query)) ! Small allocation on stack
  integer(i32) :: iNear, iTmp

  ! If the node is not associated, we are at final search block.
  if (.not. associated(trunk%buds)) then
    currentNearest = nearestLeaf_KD(D, indx, trunk%left, trunk%right, query, currentNearest)
    iTmp = indx(currentNearest)
    test = D(iTmp, :)

    distance = dsqrt(sum((query - test)**2.d0))

  else ! Otherwise, keep searching through the branches
    queryTmp = query(trunk%splitAlong)

    if (queryTmp <= trunk%median) then ! If the query is to the left of the current median
      currentNearest = nearestBranch_KD(trunk%buds(1), D, indx, query, currentNearest, distance)
      if ((queryTmp + distance) >= trunk%median) then ! Need to still check the other half if its on the other side of the median
        currentNearest = nearestBranch_KD(trunk%buds(2), D, indx, query, currentNearest, distance)
      end if
    else ! The query is to the right of the current median
      currentNearest = nearestBranch_KD(trunk%buds(2), D, indx, query, currentNearest, distance)
      if ((queryTmp - distance) <= trunk%median) then ! Need to still check the other half if its on the other side of the median
        currentNearest = nearestBranch_KD(trunk%buds(1), D, indx, query, currentNearest, distance)
      end if
    end if
  end if
  nearest = currentNearest
  end function
  !====================================================================!
  !====================================================================!
  function nearestLeaf_KD (D, indx, left, right, query, currentNearest) result(nearest)
    !! For a branch with leaves, perform a brute force minimum distance search over the leaves. The number of leaves should be small for efficiency!
  !====================================================================!
  real(r64), intent(in) :: D(:,:)
  integer(i32), intent(in) :: indx(:)
  integer(i32), intent(in) :: left, right
  real(r64), intent(in) :: query(:)
  integer(i32), intent(in) :: currentNearest
  integer(i32) :: nearest

  integer(i32) :: i, iTmp, iLeft
  real(r64) ::  distance, minDistance
  real(r64) :: test(size(query)) ! Small allocation on stack

  ! If the currentNearest is zero (i.e. uninitialized), we went straight to a set of leaves
  i = currentNearest
  iLeft = left
  if (currentNearest == 0) then
    i = left
    iLeft = i+1
  end if

  iTmp = indx(i)
  test = D(iTmp,:)
  minDistance = sum((query - test)**2.d0)
  nearest = i

  ! Find the minimum point distance to the query out of all the leaves
  do i=iLeft, right
    iTmp = indx(i)
    test = D(iTmp,:)
    distance = sum((query - test)**2.d0)
    if (distance < minDistance) then
      minDistance = distance
      nearest = i
    end if
  end do
  end function
  !====================================================================!

end submodule
