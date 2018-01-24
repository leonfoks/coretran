  module m_indexing
    !! Contains routines to obtain packed and unpacked indices into arrays
    
  use variableKind, only: i32, r64

  implicit none

  contains

  !====================================================================!
  function ind2sub(iG,nSub) result(iSub)
    !! Compute the indices in each dimension from the global index
  !====================================================================!
  integer(i32), intent(in) :: iG !! Index into a global vector
  integer(i32), intent(in) :: nSub(:) !! Size in each dimension
  integer(i32) :: iSub(size(nSub)) !! Indices in each dimension to return
  integer(i32) :: i,iGtmp,iTmp
  integer(i32) :: nDims
  integer(i32) :: prod

  nDims=size(nSub)
  if (nDims == 1) then
    iSub(1) = iG
    return
  end if

  prod = product(nSub)
  iGtmp = iG
  do i = nDims, 1, -1
    prod = prod / nSub(i)
    iTmp = mod(iGtmp-1,prod)+1
    iSub(i) = (iGtmp - iTmp)/prod + 1
    iGtmp = iTmp
  end do

  end function
  !====================================================================!
  !====================================================================!
  function sub2ind(iSub,nSub) result(iG)
    !! Given component indices, get the global vector location.
  !====================================================================!
  integer(i32), intent(in) :: iSub(:) !! Indices in each dimension. The first entry in iL is the left most index
  integer(i32), intent(in) :: nSub(:) !! Size in each dimension
  integer(i32) :: iG !! Index in the global vector
  integer(i32) :: i
  integer(i32) :: nDims
  integer(i32) :: prod
  nDims=size(iSub)

  prod = 1
  iG = 1
  do i = 1,nDims
    iG = iG + (iSub(i)-1)*prod
    prod = prod * nSub(i)
  end do

  end function
  !====================================================================!
  !====================================================================!
  function integerBin(this,n,bound) result(i)
  !====================================================================!
  ! Given a real number this, that is assumed to lie between 1 and N
  ! Round the real number and if it is outside 1 or N, limit the output to
  ! either of those values
  ! Automatically limits the output between 1 and N, unless bound is .false.
  ! TODO: This is not an integer bin, its the integer Nearest.
  real(r64) :: this
  integer(i32) :: n
  logical :: bound
  integer(i32) :: i
  i=idnint(this)
  if (bound) then
    if (i < 1) i=1
    if (i > n) i=n
  endif
  end function
  !====================================================================!
  end module
