  module m_indexing
    !! Contains routines to obtain packed and unpacked indices into arrays
  use variableKind
  use m_errors, only: msg
  use m_unitTester, only: tester
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
  !====================================================================!
  subroutine indexing_test(test)
    !! graph: false
  !====================================================================!
  class(tester) :: test

  integer(i32) :: ia1D(3), ic1D(3)
  integer(i32) :: ia

  call Msg('==========================')
  call Msg('Testing : Indexing')
  call Msg('==========================')

  ia1D = [3,4,7]
  ic1D = [3,5,9]
  ia = - 1
  ia = sub2ind(ia1D,ic1D)
  write(*,1) 'sub2ind([3,4,7],[3,5,9]) = 102'
  call test%test(ia == 102,'sub2ind')
  ia=99
  ia1D = 0
  ia1D = ind2sub(ia,ic1D)
  write(*,1) 'ind2sub(99,[3,5,9]) = [3,3,7]'
  call test%test(all(ia1D == [3,3,7]),'ind2sub')
  1 format(a)
  end subroutine
  !====================================================================!
  end module
