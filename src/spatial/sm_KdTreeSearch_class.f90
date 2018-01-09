submodule (m_KdTree) sm_KdTreeSearch_class
  !! Contains the implementations of overloaded KdTreeSearch class type bound procedures
implicit none

contains

  !====================================================================!
  module procedure nearest_2D ! (search, tree, x, y, xQuery, yQuery) result(nearest)
    !! Overloaded Type bound procedure KdTreeSearch%nearest()
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
  real(r64),intent(in) :: x(:)
  real(r64),intent(in) :: y(:)
  integer,intent(in) :: indx(:)
  real(r64),intent(in) :: xQuery
  real(r64),intent(in) :: yQuery
  integer(i32), intent(inout) :: currentNearest
  real(r64), intent(inout) :: distance
  integer(i32) :: nearest

  real(r64) :: query
  integer(i32) :: iNear, iTmp

  ! If the node is not associated, we are at final search block.
  if (.not. associated(trunk%buds)) then
    currentNearest = nearestLeaf_2D(x, y, indx, trunk%left, trunk%right, xQuery, yQuery, currentNearest)
    iTmp = indx(currentNearest)
    distance = sqrt((xQuery - x(iTmp))**2.d0 + (yQuery - y(iTmp))**2.d0)

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
  module procedure nearest_3D ! (search, tree, x, y, z, xQuery, yQuery, zQuery) result(nearest)
    !! Overloaded Type bound procedure KdTreeSearch%nearest()
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
    distance = sqrt((xQuery - x(iTmp))**2.d0 + (yQuery - y(iTmp))**2.d0 + (zQuery - z(iTmp))**2.d0)

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
  module procedure nearest_KD ! (search, tree, D, query) result(nearest)
    !! Overloaded Type bound procedure KdTreeSearch%nearest()
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


  !====================================================================!
  module procedure kNearest_2D ! (search, tree, x, y, xQuery, yQuery, k, radius) result(kNearest)
    !! Overloaded Type bound procedure KdTreeSearch%nearest()
  !====================================================================!
  !class(KdTreeSearch), intent(inout) :: search
  !class(kdTree),intent(in) :: tree
  !real(r64),intent(in) :: x(:)
  !real(r64),intent(in) :: y(:)
  !real(r64),intent(in) :: xQuery
  !real(r64),intent(in) :: yQuery
  !integer(i32), intent(in), optional :: k
  !real(r64), intent(in), optional :: radius
  !class(dArgDynamicArray) :: kNearest

  integer(i32) :: k_
  logical :: fixedArray

  if (.not. present(k) .and. .not. present(radius)) call eMsg('KdTreeSearch%kNearest: Must use either k or radius, or both.')

  ! Set an initial value for the memory allocated if k nearest is not specified
  k_ = 16
  ! Set the fixed array as false unless k is specified
  fixedArray = .false.
  if (present(k)) then
    if (k <= 1) call eMsg('KdTreeSearch%kNearest: k must be >= 1')
    k_ = k
    fixedArray = .true.
  endif

  if (present(radius)) then
    if (radius <= 0.d0) call eMsg('KdTreeSearch%kNearest: radius: '//str(radius)//'must be > 0')
  endif

  kNearest = dArgDynamicArray(k, .true., fixedArray)
  kNearest%v%values = huge(0.d0)

  if (present(radius)) then ! Perform a radius search
    call kRadiusBranch_2D(tree%trunk, x, y, tree%indx, xQuery, yQuery, radius**2.d0, kNearest)
  else ! Perform straight k nearest neighbours.
    call kNearestBranch_2D(tree%trunk, x, y, tree%indx, xQuery, yQuery, kNearest)
  endif
  kNearest%v%values = sqrt(kNearest%v%values)
  if (.not. fixedArray) call kNearest%tighten()
  end procedure
  !====================================================================!
  !====================================================================!
  recursive subroutine kNearestBranch_2D(trunk, x, y, indx, xQuery, yQuery, kNearest)
    !! Find the k nearest neighbours to the query location
  !====================================================================!
  class(KdTreebranch),intent(in) :: trunk
  real(r64),intent(in) :: x(:)
  real(r64),intent(in) :: y(:)
  integer,intent(in) :: indx(:)
  real(r64),intent(in) :: xQuery
  real(r64),intent(in) :: yQuery
  class(dArgDynamicArray) :: kNearest

  real(r64) :: query, distance
  integer(i32) :: i, iTmp

  ! If the node is not associated, we are at final search block.
   if (.not. associated(trunk%buds)) then
    call kNearestLeaf_2D(x, y, indx, trunk%left, trunk%right, xQuery, yQuery, kNearest)

  else ! Otherwise, keep searching through the branches

    select case(trunk%splitAlong)
    case(1)
      query = xQuery
    case(2)
      query = yQuery
    end select

    if (query <= trunk%median) then ! If the query is to the left of the current median
      call kNearestBranch_2D(trunk%buds(1), x, y, indx, xQuery, yQuery, kNearest)
      distance = query + sqrt(kNearest%v%values(kNearest%v%N))
      ! If the query point plus the current biggest distance is past the median value.
      if (distance >= trunk%median  .or. .not. kNearest%isFilled()) then ! Need to still check the other half if its on the other side of the median
        call kNearestBranch_2D(trunk%buds(2), x, y, indx, xQuery, yQuery, kNearest)
      end if
    else ! The query is to the right of the current median
      call kNearestBranch_2D(trunk%buds(2), x, y, indx, xQuery, yQuery, kNearest)
      distance = query - sqrt(kNearest%v%values(kNearest%v%N))
      if (distance <= trunk%median .or. .not. kNearest%isFilled()) then ! Need to still check the other half if its on the other side of the median
        call kNearestBranch_2D(trunk%buds(1), x, y, indx, xQuery, yQuery, kNearest)
      end if
    end if
  end if
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine kNearestLeaf_2D (x, y, indx, left, right, xQuery, yQuery, kNearest)
    !! For a branch with leaves, perform a brute force minimum distance search over the leaves. 
    !! The number of leaves should be small for efficiency!
  !====================================================================!
  real(r64), intent(in) :: x(:)
  real(r64), intent(in) :: y(:)
  integer(i32), intent(in) :: indx(:)
  integer(i32), intent(in) :: left
  integer(i32), intent(in) :: right
  real(r64), intent(in) :: xQuery
  real(r64), intent(in) :: yQuery
  class(dArgDynamicArray), intent(inout) :: kNearest

  integer(i32) :: i, iTmp
  real(r64) ::  distance

  ! Check the rest of the leaves and insert as appropriate
  do i = left, right
    iTmp = indx(i)
    distance = (xQuery - x(iTmp))**2.d0 + (yQuery - y(iTmp))**2.d0
    if (kNearest%isEmpty()) then
      call kNearest%insertSorted(iTmp, distance)
    else
      if (distance < kNearest%v%values(kNearest%v%N)  .or. .not. kNearest%isFilled()) then
        call kNearest%insertSorted(iTmp, distance)
      end if
    endif
  end do
  end subroutine
  !====================================================================!
  !====================================================================!
  recursive subroutine kRadiusBranch_2D(trunk, x, y, indx, xQuery, yQuery, radiusSquared, kNearest)
    !! Recurse through the tree branches to find the nearest branches to the query location
  !====================================================================!
  class(KdTreebranch),intent(in) :: trunk
  real(r64),intent(in) :: x(:)
  real(r64),intent(in) :: y(:)
  integer,intent(in) :: indx(:)
  real(r64),intent(in) :: xQuery
  real(r64),intent(in) :: yQuery
  real(r64), intent(in) :: radiusSquared
  class(dArgDynamicArray) :: kNearest

  real(r64) :: query, distance
  integer(i32) :: i, iTmp


  ! If the node is not associated, we are at final search block.
  if (.not. associated(trunk%buds)) then
    if (kNearest%v%fixed) then
      call kRadiusLeaf_2D (x, y, indx, trunk%left, trunk%right, xQuery, yQuery, radiusSquared, kNearest)
    else
      call radiusLeaf_2D (x, y, indx, trunk%left, trunk%right, xQuery, yQuery, radiusSquared, kNearest)
    endif
  else ! Otherwise, keep searching through the branches

    select case(trunk%splitAlong)
    case(1)
      query = xQuery
    case(2)
      query = yQuery
    end select

    if (query <= trunk%median) then ! If the query is to the left of the current median
      call kRadiusBranch_2D(trunk%buds(1), x, y, indx, xQuery, yQuery, radiusSquared, kNearest)
      distance = query + sqrt(radiusSquared)
      ! If the query point plus the current biggest distance is past the median value.
      if (distance >= trunk%median) then ! Need to still check the other half if its on the other side of the median
        call kRadiusBranch_2D(trunk%buds(2), x, y, indx, xQuery, yQuery, radiusSquared, kNearest)
      end if
    else ! The query is to the right of the current median
      call kRadiusBranch_2D(trunk%buds(2), x, y, indx, xQuery, yQuery, radiusSquared, kNearest)
      distance = query - sqrt(radiusSquared)
      if (distance <= trunk%median) then ! Need to still check the other half if its on the other side of the median
        call kRadiusBranch_2D(trunk%buds(1), x, y, indx, xQuery, yQuery, radiusSquared, kNearest)
      end if
    end if
  end if
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine kRadiusLeaf_2D (x, y, indx, left, right, xQuery, yQuery, radiusSquared, kNearest)
    !! For a branch with leaves, perform a brute force minimum distance search over the leaves. 
    !! The number of leaves should be small for efficiency!
  !====================================================================!
  real(r64), intent(in) :: x(:)
  real(r64), intent(in) :: y(:)
  integer(i32), intent(in) :: indx(:)
  integer(i32), intent(in) :: left
  integer(i32), intent(in) :: right
  real(r64), intent(in) :: xQuery
  real(r64), intent(in) :: yQuery
  real(r64), intent(in) :: radiusSquared
  class(dArgDynamicArray), intent(inout) :: kNearest

  integer(i32) :: i, iTmp
  real(r64) :: distance

  ! Check the rest of the leaves and insert as appropriate
  do i = left, right
    iTmp = indx(i)
    distance = (xQuery - x(iTmp))**2.d0 + (yQuery - y(iTmp))**2.d0
    ! If the distance is less than the search radius
    if (distance <= radiusSquared) then
      if (kNearest%isEmpty()) then ! Insert the point if the list is empty
        call kNearest%insertSorted(iTmp, distance)
      else
        ! Since this is a k Nearest within a search radius, even if the point is within
        ! the search, it may be closer than any previous in the heap.
        if (distance < kNearest%v%values(kNearest%v%N) .or. .not. kNearest%isFilled()) then
          call kNearest%insertSorted(iTmp, distance)
        endif
      endif
    endif
  end do
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine radiusLeaf_2D (x, y, indx, left, right, xQuery, yQuery, radiusSquared, kNearest)
    !! For a branch with leaves, perform a brute force minimum distance search over the leaves. 
    !! The number of leaves should be small for efficiency!
  !====================================================================!
  real(r64), intent(in) :: x(:)
  real(r64), intent(in) :: y(:)
  integer(i32), intent(in) :: indx(:)
  integer(i32), intent(in) :: left
  integer(i32), intent(in) :: right
  real(r64), intent(in) :: xQuery
  real(r64), intent(in) :: yQuery
  real(r64), intent(in) :: radiusSquared
  class(dArgDynamicArray), intent(inout) :: kNearest

  integer(i32) :: i, iTmp
  real(r64) ::  distance, minDistance

  ! Easy one this, since the array is dynamic, just insert any point inside the hypersphere.
  do i = left, right
    iTmp = indx(i)
    distance = (xQuery - x(iTmp))**2.d0 + (yQuery - y(iTmp))**2.d0
    if (distance <= radiusSquared) then
      call kNearest%insertSorted(iTmp, distance)
    endif
  end do
  end subroutine
  !====================================================================!


  !====================================================================!
  module procedure kNearest_3D ! (search, tree, x, y, z, xQuery, yQuery, zQuery, k, kNearest)
    !! Overloaded Type bound procedure KdTreeSearch%nearest()
  !====================================================================!
  !class(KdTreeSearch), intent(inout) :: search
  !class(kdTree),intent(in) :: tree
  !real(r64),intent(in) :: x(:)
  !real(r64),intent(in) :: y(:)
  !real(r64),intent(in) :: z(:)
  !real(r64),intent(in) :: xQuery
  !real(r64),intent(in) :: yQuery
  !real(r64),intent(in) :: zQuery
  !integer(i32), intent(in), optional :: k
  !real(r64), intent(in), optional :: radius
  !class(dArgDynamicArray) :: kNearest

  integer(i32) :: k_
  logical :: fixedArray

  if (.not. present(k) .and. .not. present(radius)) call eMsg('KdTreeSearch%kNearest: Must use either k or radius, or both.')

  ! Set an initial value for the memory allocated if k nearest is not specified
  k_ = 16
  ! Set the fixed array as false unless k is specified
  fixedArray = .false.
  if (present(k)) then
    if (k <= 1) call eMsg('KdTreeSearch%kNearest: k must be >= 1')
    k_ = k
    fixedArray = .true.
  endif

  if (present(radius)) then
    if (radius <= 0.d0) call eMsg('KdTreeSearch%kNearest: radius must be > 0')
  endif

  kNearest = dArgDynamicArray(k, .true., fixedArray)

  if (present(radius)) then ! Perform a radius search
    call kRadiusBranch_3D(tree%trunk, x, y, z, tree%indx, xQuery, yQuery, zQuery, radius**2.d0, kNearest)
  else ! Perform straight k nearest neighbours.
    call kNearestBranch_3D(tree%trunk, x, y, z, tree%indx, xQuery, yQuery, zQuery, kNearest)
  endif
  kNearest%v%values = sqrt(kNearest%v%values)
  if (.not. fixedArray) call kNearest%tighten()
  end procedure
  !====================================================================!
  !====================================================================!
  recursive subroutine kNearestBranch_3D(trunk, x, y, z, indx, xQuery, yQuery, zQuery, kNearest)
    !! Recurse through the tree branches to find the nearest branches to the query location
  !====================================================================!
  class(KdTreebranch),intent(in) :: trunk
  real(r64),intent(in) :: x(:)
  real(r64),intent(in) :: y(:)
  real(r64),intent(in) :: z(:)
  integer,intent(in) :: indx(:)
  real(r64),intent(in) :: xQuery
  real(r64),intent(in) :: yQuery
  real(r64),intent(in) :: zQuery
  class(dArgDynamicArray) :: kNearest

  real(r64) :: query, distance
  integer(i32) :: i, iTmp

  ! If the node is not associated, we are at final search block.
  if (.not. associated(trunk%buds)) then
    ! Check the leaves and insert as appropriate
    do i = trunk%left, trunk%right
      iTmp = indx(i)
      distance = (xQuery - x(iTmp))**2.d0 + (yQuery - y(iTmp))**2.d0 + (zQuery-z(iTmp))**2.d0
      if (kNearest%isEmpty()) then
        call kNearest%insertSorted(iTmp, distance)
      else
        if (distance < kNearest%v%values(kNearest%v%N) .or. .not. kNearest%isFilled()) then
          call kNearest%insertSorted(iTmp, distance)
        end if
      endif
    end do

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
      call kNearestBranch_3D(trunk%buds(1), x, y, z, indx, xQuery, yQuery, zQuery, kNearest)
      distance = query + sqrt(kNearest%v%values(kNearest%v%N))
      ! If the query point plus the current biggest distance is past the median value.
      if (distance >= trunk%median .or. .not. kNearest%isFilled()) then ! Need to still check the other half if its on the other side of the median
        call kNearestBranch_3D(trunk%buds(2), x, y, z, indx, xQuery, yQuery, zQuery, kNearest)
      end if
    else ! The query is to the right of the current median
      call kNearestBranch_3D(trunk%buds(2), x, y, z, indx, xQuery, yQuery, zQuery, kNearest)
      distance = query - sqrt(kNearest%v%values(kNearest%v%N))
      if (distance <= trunk%median .or. .not. kNearest%isFilled()) then ! Need to still check the other half if its on the other side of the median
        call kNearestBranch_3D(trunk%buds(1), x, y, z, indx, xQuery, yQuery, zQuery, kNearest)
      end if
    end if
  end if
  end subroutine
  !====================================================================!
  !====================================================================!
  recursive subroutine kRadiusBranch_3D(trunk, x, y, z, indx, xQuery, yQuery, zQuery, radiusSquared, kNearest)
    !! Recurse through the tree branches to find the nearest branches to the query location
  !====================================================================!
  class(KdTreebranch),intent(in) :: trunk
  real(r64),intent(in) :: x(:)
  real(r64),intent(in) :: y(:)
  real(r64),intent(in) :: z(:)
  integer,intent(in) :: indx(:)
  real(r64),intent(in) :: xQuery
  real(r64),intent(in) :: yQuery
  real(r64),intent(in) :: zQuery
  real(r64), intent(in) :: radiusSquared
  class(dArgDynamicArray) :: kNearest

  real(r64) :: query, distance

  ! If the node is not associated, we are at final search block.
  if (.not. associated(trunk%buds)) then
    if (kNearest%v%fixed) then
      call kRadiusLeaf_3D(x, y, z, indx, trunk%left, trunk%right, xQuery, yQuery, zQuery, radiusSquared, kNearest)
    else
      call radiusLeaf_3D(x, y, z, indx, trunk%left, trunk%right, xQuery, yQuery, zQuery, radiusSquared, kNearest)
    endif
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
      call kRadiusBranch_3D(trunk%buds(1), x, y, z, indx, xQuery, yQuery, zQuery, radiusSquared, kNearest)
      distance = query + sqrt(radiusSquared)
      ! If the query point plus the current biggest distance is past the median value.
      if (distance >= trunk%median) then ! Need to still check the other half if its on the other side of the median
        call kRadiusBranch_3D(trunk%buds(2), x, y, z, indx, xQuery, yQuery, zQuery, radiusSquared, kNearest)
      end if
    else ! The query is to the right of the current median
      call kRadiusBranch_3D(trunk%buds(2), x, y, z, indx, xQuery, yQuery, zQuery, radiusSquared, kNearest)
      distance = query - sqrt(radiusSquared)
      if (distance <= trunk%median) then ! Need to still check the other half if its on the other side of the median
        call kRadiusBranch_3D(trunk%buds(1), x, y, z, indx, xQuery, yQuery, zQuery, radiusSquared, kNearest)
      end if
    end if
  end if
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine kRadiusLeaf_3D(x, y, z, indx, left, right, xQuery, yQuery, zQuery, radiusSquared, kNearest)
    !! For a branch with leaves, perform a brute force minimum distance search over the leaves. 
    !! The number of leaves should be small for efficiency!
  !====================================================================!
  real(r64), intent(in) :: x(:)
  real(r64), intent(in) :: y(:)
  real(r64), intent(in) :: z(:)
  integer(i32), intent(in) :: indx(:)
  integer(i32), intent(in) :: left
  integer(i32), intent(in) :: right
  real(r64), intent(in) :: xQuery
  real(r64), intent(in) :: yQuery
  real(r64), intent(in) :: zQuery
  real(r64), intent(in) :: radiusSquared
  class(dArgDynamicArray), intent(inout) :: kNearest

  integer(i32) :: i, iTmp
  real(r64) :: distance

  ! Check the rest of the leaves and insert as appropriate
  do i = left, right
    iTmp = indx(i)
    distance = (xQuery - x(iTmp))**2.d0 + (yQuery - y(iTmp))**2.d0 + (zQuery-z(iTmp))**2.d0
    ! If the distance is less than the search radius
    if (distance <= radiusSquared) then
      if (kNearest%isEmpty()) then ! Insert the point if the list is empty
        call kNearest%insertSorted(iTmp, distance)
      else
        ! Since this is a k Nearest within a search radius, even if the point is within
        ! the search, it may be closer than any previous in the heap.
        if (distance < kNearest%v%values(kNearest%v%N) .or. .not. kNearest%isFilled()) then
          call kNearest%insertSorted(iTmp, distance)
        endif
      endif
    endif
  end do
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine radiusLeaf_3D (x, y, z, indx, left, right, xQuery, yQuery, zQuery, radiusSquared, kNearest)
    !! For a branch with leaves, perform a brute force minimum distance search over the leaves. 
    !! The number of leaves should be small for efficiency!
  !====================================================================!
  real(r64), intent(in) :: x(:)
  real(r64), intent(in) :: y(:)
  real(r64), intent(in) :: z(:)
  integer(i32), intent(in) :: indx(:)
  integer(i32), intent(in) :: left
  integer(i32), intent(in) :: right
  real(r64), intent(in) :: xQuery
  real(r64), intent(in) :: yQuery
  real(r64), intent(in) :: zQuery
  real(r64), intent(in) :: radiusSquared
  class(dArgDynamicArray), intent(inout) :: kNearest

  integer(i32) :: i, iTmp
  real(r64) ::  distance, minDistance

  ! Easy one this, since the array is dynamic, just insert any point inside the hypersphere.
  do i = left, right
    iTmp = indx(i)
    distance = (xQuery - x(iTmp))**2.d0 + (yQuery - y(iTmp))**2.d0 + (zQuery-z(iTmp))**2.d0
    if (distance <= radiusSquared) then
      call kNearest%insertSorted(iTmp, distance)
    endif
  end do
  end subroutine
  !====================================================================!


  !====================================================================!
  module procedure kNearest_KD ! (search, tree, D, query, k, radius) result(kNearest)
    !! Overloaded Type bound procedure KdTreeSearch%nearest()
  !====================================================================!
  !class(KdTreeSearch), intent(inout) :: search
  !class(kdTree),intent(in) :: tree
  !real(r64),intent(in) :: D(:,:)
  !real(r64),intent(in) :: query(:)
  !integer(i32), intent(in), optional :: k
  !real(r64), intent(in), optional :: radius
  !type(dArgDynamicArray) :: kNearest

  integer(i32) :: k_
  logical :: fixedArray

  if (.not. present(k) .and. .not. present(radius)) call eMsg('KdTreeSearch%kNearest: Must use either k or radius, or both.')

  ! Set an initial value for the memory allocated if k nearest is not specified
  k_ = 16
  ! Set the fixed array as false unless k is specified
  fixedArray = .false.
  if (present(k)) then
    if (k <= 1) call eMsg('KdTreeSearch%kNearest: k must be >= 1')
    k_ = k
    fixedArray = .true.
  endif

  if (present(radius)) then
    if (radius <= 0.d0) call eMsg('KdTreeSearch%kNearest: radius must be > 0')
  endif

  kNearest = dArgDynamicArray(k, .true., fixedArray)

  if (present(radius)) then ! Perform a radius search
    call kRadiusBranch_KD(tree%trunk, D, tree%indx, query, radius**2.d0, kNearest)
  else ! Perform straight k nearest neighbours.
    call kNearestBranch_KD(tree%trunk, D, tree%indx, query, kNearest)
  endif
  kNearest%v%values = sqrt(kNearest%v%values)
  if (.not. fixedArray) call kNearest%tighten()
  end procedure
  !====================================================================!
  !====================================================================!
  recursive subroutine kNearestBranch_KD(trunk, D, indx, query, kNearest)
    !! Recurse through the tree branches to find the nearest branches to the query location
  !====================================================================!
  class(KdTreebranch),intent(in) :: trunk
  real(r64),intent(in) :: D(:,:)
  integer,intent(in) :: indx(:)
  real(r64),intent(in) :: query(:)
  class(dArgDynamicArray), intent(inout) :: kNearest

  real(r64) :: distance, queryTmp
  integer(i32) :: i, iTmp
  real(r64) :: test(size(query)) ! Small allocation on stack
  

  ! If the node is not associated, we are at final search block.
  if (.not. associated(trunk%buds)) then
    ! Check the rest of the leaves and insert as appropriate
    do i = trunk%left, trunk%right
      iTmp = indx(i)
      test = D(iTmp, :)
      distance = sum((query - test)**2.d0)
      if (kNearest%isEmpty()) then
        call kNearest%insertSorted(iTmp, distance)
      else
        if (distance < kNearest%v%values(kNearest%v%N) .or. .not. kNearest%isFilled()) then
          call kNearest%insertSorted(iTmp, distance)
        end if
      endif
    end do
    
  else ! Otherwise, keep searching through the branches
    queryTmp = query(trunk%splitAlong)

    if (queryTmp <= trunk%median) then ! If the query is to the left of the current median
      call kNearestBranch_KD(trunk%buds(1), D, indx, query, kNearest)
      distance = queryTmp + sqrt(kNearest%v%values(kNearest%v%N))
      if (distance >= trunk%median .or. .not. kNearest%isFilled()) then ! Need to still check the other half if its on the other side of the median
        call kNearestBranch_KD(trunk%buds(2), D, indx, query, kNearest)
      end if
    else ! The query is to the right of the current median
      call kNearestBranch_KD(trunk%buds(2), D, indx, query, kNearest)
      distance = queryTmp - sqrt(kNearest%v%values(kNearest%v%N))
      if (distance <= trunk%median .or. .not. kNearest%isFilled()) then ! Need to still check the other half if its on the other side of the median
        call kNearestBranch_KD(trunk%buds(1), D, indx, query, kNearest)
      end if
    end if
  end if
  end subroutine
  !====================================================================!
  !====================================================================!
  recursive subroutine kRadiusBranch_KD(trunk, D, indx, query, radiusSquared, kNearest)
    !! Recurse through the tree branches to find the nearest branches to the query location
  !====================================================================!
  class(KdTreebranch),intent(in) :: trunk
  real(r64),intent(in) :: D(:,:)
  integer,intent(in) :: indx(:)
  real(r64),intent(in) ::query(:)
  real(r64), intent(in) :: radiusSquared
  class(dArgDynamicArray) :: kNearest

  real(r64) :: queryTmp, distance

  ! If the node is not associated, we are at final search block.
  if (.not. associated(trunk%buds)) then
    if (kNearest%v%fixed) then
      call kRadiusLeaf_KD(D, indx, trunk%left, trunk%right, query, radiusSquared, kNearest)
    else
      call radiusLeaf_KD(D, indx, trunk%left, trunk%right, query, radiusSquared, kNearest)
    endif
  else ! Otherwise, keep searching through the branches

    queryTmp = query(trunk%splitAlong)

    if (queryTmp <= trunk%median) then ! If the query is to the left of the current median
      call kRadiusBranch_KD(trunk%buds(1), D, indx, query, radiusSquared, kNearest)
      distance = queryTmp + sqrt(radiusSquared)
      ! If the query point plus the current biggest distance is past the median value.
      if (distance >= trunk%median) then ! Need to still check the other half if its on the other side of the median
        call kRadiusBranch_KD(trunk%buds(2), D, indx, query, radiusSquared, kNearest)
      end if
    else ! The query is to the right of the current median
      call kRadiusBranch_KD(trunk%buds(2), D, indx, query, radiusSquared, kNearest)
      distance = queryTmp - sqrt(radiusSquared)
      if (distance <= trunk%median) then ! Need to still check the other half if its on the other side of the median
        call kRadiusBranch_KD(trunk%buds(1), D, indx, query, radiusSquared, kNearest)
      end if
    end if
  end if
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine kRadiusLeaf_KD(D, indx, left, right, query, radiusSquared, kNearest)
    !! For a branch with leaves, perform a brute force minimum distance search over the leaves. 
    !! The number of leaves should be small for efficiency!
  !====================================================================!
  real(r64), intent(in) :: D(:,:)
  integer(i32), intent(in) :: indx(:)
  integer(i32), intent(in) :: left
  integer(i32), intent(in) :: right
  real(r64), intent(in) :: query(:)
  real(r64), intent(in) :: radiusSquared
  class(dArgDynamicArray), intent(inout) :: kNearest

  integer(i32) :: i, iTmp
  real(r64) :: distance, test(size(query))

  ! Check the rest of the leaves and insert as appropriate
  do i = left, right
    iTmp = indx(i)
    test = D(iTmp, :)
    distance = sum((query - test)**2.d0)
    ! If the distance is less than the search radius
    if (distance <= radiusSquared) then
      if (kNearest%isEmpty()) then ! Insert the point if the list is empty
        call kNearest%insertSorted(iTmp, distance)
      else
        ! Since this is a k Nearest within a search radius, even if the point is within
        ! the search, it may be closer than any previous in the heap.
        if (distance < kNearest%v%values(kNearest%v%N) .or. .not. kNearest%isFilled()) then
          call kNearest%insertSorted(iTmp, distance)
        endif
      endif
    endif
  end do
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine radiusLeaf_KD (D, indx, left, right, query, radiusSquared, kNearest)
    !! For a branch with leaves, perform a brute force minimum distance search over the leaves. 
    !! The number of leaves should be small for efficiency!
  !====================================================================!
  real(r64), intent(in) :: D(:,:)
  integer(i32), intent(in) :: indx(:)
  integer(i32), intent(in) :: left
  integer(i32), intent(in) :: right
  real(r64), intent(in) :: query(:)
  real(r64), intent(in) :: radiusSquared
  class(dArgDynamicArray), intent(inout) :: kNearest

  integer(i32) :: i, iTmp
  real(r64) ::  distance, test(size(query))

  ! Easy one this, since the array is dynamic, just insert any point inside the hypersphere.
  do i = left, right
    iTmp = indx(i)
    test = D(iTmp, :)
    distance = sum((query - test)**2.d0)
    if (distance <= radiusSquared) then
      call kNearest%insertSorted(iTmp, distance)
    endif
  end do
  end subroutine
  !====================================================================!


  !====================================================================!
  module procedure rangeSearch_2D!(search, tree, x, y, lowerBound, upperBound) result(iPoints)
    !! Overloaded Type bound procedure KdTreeSearch%rangeSearch()
  !====================================================================!
  !class(KdTreeSearch), intent(inout) :: search
  !class(kdTree),intent(in) :: tree
  !real(r64),intent(in) :: x(:)
  !real(r64),intent(in) :: y(:)
  !real(r64),intent(in) :: lowerBound(2)
  !real(r64),intent(in) :: upperBound(2)
  !type(iDynamicArray) :: iPoints
  
  iPoints = iDynamicArray(16, .true., .false.)
  iPoints%values = -1

  call rangeSearchBranch_2D(tree%trunk, x, y, tree%indx, lowerBound, upperBound, iPoints)

  call iPoints%tighten()
  end procedure
  !====================================================================!
  !====================================================================!
  recursive subroutine rangeSearchBranch_2D(trunk, x, y, indx, lowerBound, upperBound, iPoints)
    !! Recurse through the tree branches to find the points inside the range
  !====================================================================!
  class(KdTreebranch),intent(in) :: trunk
  real(r64),intent(in) :: x(:)
  real(r64),intent(in) :: y(:)
  integer,intent(in) :: indx(:)
  real(r64),intent(in) :: lowerBound(2)
  real(r64),intent(in) :: upperBound(2)
  class(iDynamicArray) :: iPoints

  real(r64) :: lower, upper
  real(r64) :: xTmp, yTmp
  integer(i32) :: i, iTmp

  ! If the node is not associated, we are at final search block.
  if (.not. associated(trunk%buds)) then
    do i = trunk%left, trunk%right
      iTmp = indx(i)
      xTmp = x(iTmp)
      if (xTmp >= lowerBound(1) .and. xTmp <= upperBound(1)) then
        yTmp = y(iTmp)
        if (yTmp >= lowerBound(2) .and. yTmp <= upperBound(2)) then
          call iPoints%insertSorted(iTmp)
        endif
      endif 
    end do
  else ! Otherwise, keep searching through the branches

    lower = lowerBound(trunk%splitAlong)
    upper = upperBound(trunk%splitAlong)

    ! If the median is to the right of the box, search the left branch
    if (trunk%median > lower) then
      call rangeSearchBranch_2D(trunk%buds(1), x, y, indx, lowerBound, upperBound, iPoints)
      ! If the median is within the box, search 
      if (trunk%median < upper) then
        call rangeSearchBranch_2D(trunk%buds(2), x, y, indx, lowerBound, upperBound, iPoints)
      endif
    ! If the median is the the left of the box, search the right branch
    else
      call rangeSearchBranch_2D(trunk%buds(2), x, y, indx, lowerBound, upperBound, iPoints)
      if (trunk%median < upper) then
        call rangeSearchBranch_2D(trunk%buds(1), x, y, indx, lowerBound, upperBound, iPoints)
      endif
    endif
  endif
  end subroutine
  !====================================================================!

  !====================================================================!
  module procedure rangeSearch_3D!(search, tree, x, y, z, lowerBound, upperBound) result(iPoints)
    !! Overloaded Type bound procedure KdTreeSearch%rangeSearch()
  !====================================================================!
  !class(KdTreeSearch), intent(inout) :: search
  !class(kdTree),intent(in) :: tree
  !real(r64),intent(in) :: x(:)
  !real(r64),intent(in) :: y(:)
  !real(r64),intent(in) :: z(:)
  !real(r64),intent(in) :: lowerBound(3)
  !real(r64),intent(in) :: upperBound(3)
  !type(iDynamicArray) :: iPoints
  
  iPoints = iDynamicArray(16, .true., .false.)
  iPoints%values = -1

  call rangeSearchBranch_3D(tree%trunk, x, y, z, tree%indx, lowerBound, upperBound, iPoints)

  call iPoints%tighten()
  end procedure
  !====================================================================!
  !====================================================================!
  recursive subroutine rangeSearchBranch_3D(trunk, x, y, z, indx, lowerBound, upperBound, iPoints)
    !! Recurse through the tree branches to find the points inside the range
  !====================================================================!
  class(KdTreebranch),intent(in) :: trunk
  real(r64),intent(in) :: x(:)
  real(r64),intent(in) :: y(:)
  real(r64),intent(in) :: z(:)
  integer,intent(in) :: indx(:)
  real(r64),intent(in) :: lowerBound(3)
  real(r64),intent(in) :: upperBound(3)
  class(iDynamicArray) :: iPoints

  real(r64) :: lower, upper
  real(r64) :: xTmp, yTmp, zTmp
  integer(i32) :: i, iTmp

  ! If the node is not associated, we are at final search block.
  if (.not. associated(trunk%buds)) then
    do i = trunk%left, trunk%right
      iTmp = indx(i)
      xTmp = x(iTmp)
      if (xTmp >= lowerBound(1) .and. xTmp <= upperBound(1)) then
        yTmp = y(iTmp)
        if (yTmp >= lowerBound(2) .and. yTmp <= upperBound(2)) then
          zTmp = z(iTmp)
          if (zTmp >= lowerBound(3) .and. zTmp <= upperBound(3)) then
            call iPoints%insertSorted(iTmp)
          endif
        endif
      endif 
    end do
  else ! Otherwise, keep searching through the branches

    lower = lowerBound(trunk%splitAlong)
    upper = upperBound(trunk%splitAlong)

    ! If the median is to the right of the box, search the left branch
    if (trunk%median > lower) then
      call rangeSearchBranch_3D(trunk%buds(1), x, y, z, indx, lowerBound, upperBound, iPoints)
      ! If the median is within the box, search 
      if (trunk%median < upper) then
        call rangeSearchBranch_3D(trunk%buds(2), x, y, z, indx, lowerBound, upperBound, iPoints)
      endif
    ! If the median is the the left of the box, search the right branch
    else
      call rangeSearchBranch_3D(trunk%buds(2), x, y, z, indx, lowerBound, upperBound, iPoints)
      if (trunk%median < upper) then
        call rangeSearchBranch_3D(trunk%buds(1), x, y, z, indx, lowerBound, upperBound, iPoints)
      endif
    endif
  endif
  end subroutine
  !====================================================================!

  !====================================================================!
  module procedure rangeSearch_KD!(search, tree, D, lowerBound, upperBound) result(iPoints)
    !! Overloaded Type bound procedure KdTreeSearch%rangeSearch()
  !====================================================================!
  !class(KdTreeSearch), intent(inout) :: search
  !class(kdTree),intent(in) :: tree
  !real(r64),intent(in) :: D(:,:)
  !real(r64),intent(in) :: lowerBound(size(D,2))
  !real(r64),intent(in) :: upperBound(size(D,2))
  !type(iDynamicArray) :: iPoints
  
  iPoints = iDynamicArray(16, .true., .false.)
  iPoints%values = -1

  call rangeSearchBranch_KD(tree%trunk, D, tree%indx, lowerBound, upperBound, iPoints)

  call iPoints%tighten()
  end procedure
  !====================================================================!
  !====================================================================!
  recursive subroutine rangeSearchBranch_KD(trunk, D, indx, lowerBound, upperBound, iPoints)
    !! Recurse through the tree branches to find the points inside the range
  !====================================================================!
  class(KdTreebranch),intent(in) :: trunk
  real(r64),intent(in) :: D(:,:)
  integer,intent(in) :: indx(:)
  real(r64),intent(in) :: lowerBound(size(D,2))
  real(r64),intent(in) :: upperBound(size(D,2))
  class(iDynamicArray) :: iPoints

  real(r64) :: lower, upper
  real(r64) :: test
  integer(i32) :: i, iTmp, j, k
  logical :: insert


  ! If the node is not associated, we are at final search block.
  if (.not. associated(trunk%buds)) then
    ! Get the number of dimensions
    k = size(D,2)
    ! For each leaf
    do i = trunk%left, trunk%right
      iTmp = indx(i)
      ! Test the first dimension
      j = 1
      test = D(iTmp, j)
      insert = (test >= lowerBound(j) .and. test <= upperBound(j))
      ! While loop allows early exit if point is outside any bounds.
      do while (insert .and. j < k)
        j = j + 1
        test = D(iTmp, j)
        insert = (test >= lowerBound(j) .and. test <= upperBound(j))
      enddo
      ! Insert the point if point is within range.
      if (insert) call iPoints%insertSorted(iTmp)
    end do

  else ! Otherwise, keep searching through the branches

    lower = lowerBound(trunk%splitAlong)
    upper = upperBound(trunk%splitAlong)

    ! If the median is to the right of the box, search the left branch
    if (trunk%median > lower) then
      call rangeSearchBranch_KD(trunk%buds(1), D, indx, lowerBound, upperBound, iPoints)
      ! If the median is within the box, search 
      if (trunk%median < upper) then
        call rangeSearchBranch_KD(trunk%buds(2), D, indx, lowerBound, upperBound, iPoints)
      endif
    ! If the median is the the left of the box, search the right branch
    else
      call rangeSearchBranch_KD(trunk%buds(2), D, indx, lowerBound, upperBound, iPoints)
      if (trunk%median < upper) then
        call rangeSearchBranch_KD(trunk%buds(1), D, indx, lowerBound, upperBound, iPoints)
      endif
    endif
  endif
  end subroutine
  !====================================================================!
end submodule
