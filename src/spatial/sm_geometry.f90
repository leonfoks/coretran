submodule (m_geometry) sm_geometry

use m_fileIO, only: openFile, closeFile
use m_allocate, only: allocate
use m_parameters, only: zero

implicit none

real(r64) :: splitter
real(r64) :: eps

real(r64) :: resErrBound
real(r64) :: o2DBound_A
real(r64) :: o2DBound_B
real(r64) :: o2DBound_C
real(r64) :: o3DBound_A
real(r64) :: o3DBound_B
real(r64) :: o3DBound_C
real(r64) :: iccBound_A
real(r64) :: iccBound_B
real(r64) :: iccBound_C
real(r64) :: icsBound_A
real(r64) :: icsBound_B
real(r64) :: icsBound_C

contains
  !====================================================================!
  subroutine init_Geometry()
  !====================================================================!
  real(r64) :: half
  real(r64) :: check, lastCheck
  logical :: everyOther
  real(r64) :: epsCheck

  everyOther = .true.
  half = 0.5d0
  eps = 1.d0
  epsCheck = epsilon(0.0_r64)
  lastCheck = 1.d0
  splitter = 1.d0
  check = 0.d0

  do while(eps /= epsCheck)
    if (everyOther) splitter = splitter * 2.d0
    everyOther = .not. everyOther
    lastCheck  = check
    eps = eps * half
    check = 1.d0 + eps
  enddo
  splitter = splitter + 1.d0 

  ! Set the error bounds for orientation and incircle tests
  resErrBound = (3.d0 + 8.d0 * eps) * eps
  ! Orient 2D error bounds
  o2DBound_A = (3.d0 + 16.d0 * eps) * eps
  o2DBound_B = (2.d0 + 12.d0 * eps) * eps
  o2DBound_C = (9.d0 + 64.d0 * eps) * eps * eps
  ! Orient 3D error bounds
  o3DBound_A = (7.d0 + 56.d0 * eps) * eps
  o3DBound_B = (3.d0 + 28.d0 * eps) * eps
  o3DBound_C = (26.d0 + 288.d0 * eps) * eps * eps
  ! Incircle error bounds
  iccBound_A = (10.d0 + 96.d0 * eps) * eps
  iccBound_B = (4.d0 + 48.d0 * eps) * eps
  iccBound_C = (44.d0 + 576.d0 * eps) * eps * eps
  ! Insphere error bounds
  icsBound_A = (16.d0 + 224.d0 * eps) * eps
  icsBound_B = (5.d0 + 72.d0 * eps) * eps
  icsBound_C = (71.d0 + 1408.d0 * eps) * eps * eps
  end subroutine
  !====================================================================!
  !====================================================================!
  function estimate(nE, e) result(eSum)
    !! Sums the entries of e from 1:nE. Called estimate because this is
    !! not an expansion sum, just a regular floating point sum.
  !====================================================================!
  integer(i32) :: nE
  real(r64) :: e(:)
  real(r64) :: eSum

  integer(i32) :: i

  eSum = e(1)
  do i = 2, nE
    eSum = eSum + e(i)
  enddo
  end function
  !====================================================================!
  !====================================================================!
  subroutine fastExpansionSum(nE, nF, e, f, h, hIndex)
    !! Given that e and f are sorted on input, except for zeros,
    !! zips e and f together and computes the cumulative sum.
    !! Ignores zeros in e or f.
    !! Rather than sorting the incoming list, unpack the merge of e and f,
    !! such that this is only an O(n) operation.
  !====================================================================!
  integer(i32), intent(in) :: nE
    !! Size of e
  integer(i32), intent(in) :: nF
    !! Size of f
  real(r64), intent(in) :: e(:)
    !! First sequence
  real(r64), intent(in) :: f(:)
    !! Second sequence
  real(r64), intent(inout) :: h(:)
    !! Output for cumulative sum
  integer(i32), intent(out) :: hIndex
    !! Index for the final sum

  real(r64) :: eNow
  real(r64) :: fNow
  real(r64) :: q, hh, qNew
  integer(i32) :: eIndex
  integer(i32) :: fIndex

  ! Get the first elements in each array
  eIndex = 1
  eNow = e(1)
  fIndex = 1
  fNow = f(1)

  ! Get the first two elements to sum together
  if ((fNow > eNow) .eqv. (fNow > -eNow)) then ! First
    q = eNow
    eIndex = eIndex + 1
    eNow = e(eIndex)
  else ! (fNow > eNow)
    q = fNow
    fIndex = fIndex + 1
    fNow = f(fIndex)
  endif

  hIndex = 1
  ! If both arrays are larger than 1 element, perform the 
  ! fast two sum and two sums.
  if (eIndex < nE .and. fIndex < nF) then
    if ((fNow > eNow) .eqv. (fNow > -eNow)) then ! First
      call fastTwoSum(eNow, q, qNew, hh)
      eIndex = eIndex + 1
      eNow = e(eIndex)
    else ! (fNow > eNow)
      call fastTwoSum(fNow, q, qNew, hh)
      fIndex = fIndex + 1
      fNow = f(fIndex)
    endif

    q = qNew
    ! Only add the error if its != 0
    if (hh /= 0.d0) then
      h(hIndex) = hh
      hIndex = hIndex + 1
    endif

    ! Do the rest of the elements in e and f.
    do while(eIndex < nE .and. fIndex < nF)
      if ((fNow > eNow) .eqv. (fNow > -eNow)) then ! First
        call twoSum(q, eNow, qNew, hh)
        eIndex = eIndex + 1
        eNow = e(eIndex)
      else ! (fNow > eNow)
        call twoSum(q, fNow, qNew, hh)
        fIndex = fIndex + 1
        fNow = f(fIndex)
      endif

      q = qNew
      ! Only add the error if its != 0
      if (hh /= 0.d0) then
        h(hIndex) = hh
        hIndex = hIndex + 1
      endif
    enddo
  endif

  ! At this point either e is finished, f is finished, or both.
  ! So account for the Errs of either e or f
  do while (eIndex < nE) 
    call twoSum(q, eNow, qNew, hh)
    eIndex = eIndex + 1
    eNow = e(eIndex)

    q = qNew
    ! Only add the error if its != 0
    if (hh /= 0.d0) then
      h(hIndex) = hh
      hIndex = hIndex + 1
    endif
  enddo
  do while (fIndex < nF)
    call twoSum(q, fNow, qNew, hh)
    fIndex = fIndex + 1
    fNow = f(fIndex)

    q = qNew
    ! Only add the sum if its != 0
    if (hh /= 0.d0) then
      h(hIndex) = hh
      hIndex = hIndex + 1
    endif
  enddo

  ! Finish the last components of e and f
  hh = 0.d0
  call twoSum(q, eNow, qNew, hh)
  q = qNew
  ! Only add the error if its != 0
  if (hh /= 0.d0) then
    h(hIndex) = hh
    hIndex = hIndex + 1
  endif
  call twoSum(q, fNow, qNew, hh)
  q = qNew
  ! Only add the error if its != 0
  if (hh /= 0.d0) then
    h(hIndex) = hh
    hIndex = hIndex + 1
  endif

  ! Account for the edge case, if nothing was added to the errors
  if (hIndex == 1 .or. q /= 0.d0) then
    h(hIndex) = q
    hIndex = hIndex + 1
  endif
  
  hIndex = hIndex - 1

  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine fastTwoDiff(a, b, x, y)
    !! Compute the difference two numbers and compute the numerical round-off error.
    !! This should only be used if you know that the magnitude of a is greater than or equal to b, otherwise, you should use the slower twoDiff routine
  !====================================================================!
    real(r64), intent(in) :: a
      !! First number
    real(r64), intent(in) :: b
      !! Second number
    real(r64), intent(out) :: x
      !! Result
    real(r64), intent(out) :: y
      !! Error
    real(r64) :: bVirtual
    x = a - b
    bVirtual = a - x
    y = bVirtual - b
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine fastTwoSum(a, b, x, y)
    !! Compute the sum of two numbers and compute the numerical round-off error.
    !! This should only be used if you know that the magnitude of a is greater than or equal to b, otherwise, you should use the slower twoSum routine
  !====================================================================!
    real(r64), intent(in) :: a 
      !! First number
    real(r64), intent(in) :: b 
      !! Second number
    real(r64), intent(out) :: x 
      !! Result
    real(r64), intent(out) :: y 
      !! Error
    real(r64) :: bVirtual
    x = a + b
    bVirtual = x - a
    y = b - bVirtual
  end subroutine
  !====================================================================!
  !====================================================================!
  module procedure inCircle!(ax, ay, bx, by, cx, cy, dx, dy) result(determinant)
  !   !! Determines whether the point d is inside the circumcircle of the triangle formed by a-b-c
  !   !! Returns a positive value if d is inside.
  !   !! Returns a negative value if d is outside.
  !   !! Returns a zero if the four points are cocircular.
  !   !!
  !   !! Uses an adaptive floating method by Shewchuk. Only exact computations
  !   !! are carried out when needed.
  ! !====================================================================!
  ! real(r64), intent(in) :: ax
  !   !! x co-ordinate of the first point
  ! real(r64), intent(in) :: ay
  !   !! y co-ordinate of the first point
  ! real(r64), intent(in) :: bx
  !   !! x co-ordinate of the second point
  ! real(r64), intent(in) :: by
  !   !! y co-ordinate of the second point
  ! real(r64), intent(in) :: cx
  !   !! x co-ordinate of the third point
  ! real(r64), intent(in) :: cy
  !   !! y co-ordinate of the third point
  ! real(r64), intent(in) :: dx
  !   !! x co-ordinate of the fourth point
  ! real(r64), intent(in) :: dy
  !   !! y co-ordinate of the fourth point
  ! real(r64) :: determinant
  !   !! [-ve, 0, +ve] for point d [outside, on, inside] the circumcircle of a -> b -> c

  real(r64), dimension(4)  :: aa, ab, abtt
  real(r64), dimension(8)  :: abt, axbc, aybc, axtbc, axtbctt, aytbc, aytbctt, axtbb, aytbb, axtcc, aytcc
  integer(i32) :: nAbtt, nAbt, nAxbc, nAybc, nAxtbc, nAxtbctt, nAytbc, nAytbctt, nAxtbb, nAytbb, nAxtcc, nAytcc
  real(r64), dimension(16) :: axtbct, aytbct, axxbc, ayybc
  real(r64), dimension(32) :: aDet
  real(r64), dimension(64) :: abDet
  integer(i32) :: nAxtbct, nAytbct, nAxxbc, nAyybc, nA, nAB
  real(r64), dimension(4)  :: bb, bc, bctt
  real(r64), dimension(8)  :: bct, bxca, byca, bxtca, bxtcatt, bytca, bytcatt, bxtaa, bytaa, bxtcc, bytcc
  integer(i32) :: nBctt, nBct, nBxca, nByca, nBxtca, nBxtcatt, nBytca, nBytcatt, nBxtaa, nBytaa, nBxtcc, nBytcc
  real(r64), dimension(16) :: bxtcat, bytcat, bxxca, byyca
  real(r64), dimension(32) :: bDet
  integer(i32) :: nBxtcat, nBytcat, nBxxca, nByyca, nB
  real(r64), dimension(4)  :: cc, ca, catt
  real(r64), dimension(8)  :: cat, cxab, cyab, cxtab, cxtabtt, cytab, cytabtt, cxtaa, cytaa, cxtbb, cytbb
  integer(i32) :: nCatt, nCat, nCxab, nCyab, nCxtab, nCxtabtt, nCytab, nCytabtt, nCxtaa, nCytaa, nCxtbb, nCytbb
  real(r64), dimension(16) :: cxtabt, cytabt, cxxab, cyyab
  real(r64), dimension(32) :: cDet
  integer(i32) :: nCxtabt, nCytabt, nCxxab, nCyyab, nC
  real(r64) :: fin1(1152), fin2(1152)
  integer(i32) :: nFin1, nFin2
  real(r64), dimension(8)  :: t8
  real(r64), dimension(16) :: t16a, t16b, t16c
  real(r64), dimension(32) :: t32a, t32b
  real(r64), dimension(48) :: t48
  real(r64), dimension(64) :: t64
  integer(i32) :: nT8, nT16a, nT16b, nT16c, nT32a, nT32b, nT48, nT64
  
  real(r64), dimension(4) :: u
  real(r64), dimension(4) :: v

  real(r64) :: adx, ady, adz, adxbdy, adxcdy, aLift
  real(r64) :: adxErr, adyErr, adzErr
  real(r64) :: bdx, bdy, bdz, bdxady, bdxcdy, bLift
  real(r64) :: bdxErr, bdyErr, bdzErr
  real(r64) :: cdx, cdy, cdz, cdxady, cdxbdy, cLift
  real(r64) :: cdxErr, cdyErr, cdzErr
  real(r64) :: errBound
  
  real(r64) :: keep
  real(r64) :: negate
  real(r64) :: tmp1, tmp2, tmp3, tmp4
    
  adx = ax - dx
  bdx = bx - dx
  cdx = cx - dx
  ady = ay - dy
  bdy = by - dy
  cdy = cy - dy

  bdxcdy = bdx * cdy
  cdxbdy = cdx * bdy
  aLift = adx * adx + ady * ady

  cdxady = cdx * ady
  adxcdy = adx * cdy
  bLift = bdx * bdx + bdy * bdy

  adxbdy = adx * bdy  
  bdxady = bdx * ady
  cLift = cdx * cdx + cdy * cdy
  
  determinant = (aLift * (bdxcdy - cdxbdy)) &
              + (bLift * (cdxady - adxcdy)) &
              + (cLift * (adxbdy - bdxady))

  keep = (abs(bdxcdy) + abs(cdxbdy)) * aLift &
       + (abs(cdxady) + abs(adxcdy)) * bLift &
       + (abs(adxbdy) + abs(bdxady)) * cLift

  errBound = o3DBound_A * keep

  ! First escape at A
  if ((determinant > errBound) .or. (-determinant > errBound)) return

  call twoProduct(bdx, cdy, tmp1, tmp2)
  call twoProduct(cdx, bdy, tmp3, tmp4)
  call twoTwoDiff(tmp1, tmp2, tmp3, tmp4, bc)
  call scaleExpansion(4, bc, adx, axbc, nAxbc)
  call scaleExpansion(nAxbc, axbc, adx, axxbc, nAxxbc)
  call scaleExpansion(4, bc, ady, aybc, nAybc)
  call scaleExpansion(nAybc, aybc, ady, ayybc, nAyybc)
  call fastExpansionSum(nAxxbc, nAyybc, axxbc, ayybc, aDet, nA)

  call twoProduct(cdx, ady, tmp1, tmp2)
  call twoProduct(adx, cdy, tmp3, tmp4)
  call twoTwoDiff(tmp1, tmp2, tmp3, tmp4, ca)
  call scaleExpansion(4, ca, bdx, bxca, nBxca)
  call scaleExpansion(nBxca, bxca, bdx, bxxca, nBxxca)
  call scaleExpansion(4, ca, bdy, byca, nByca)
  call scaleExpansion(nByca, byca, bdy, byyca, nByyca)
  call fastExpansionSum(nBxxca, nByyca, bxxca, byyca, bDet, nB)

  call twoProduct(adx, bdy, tmp1, tmp2)
  call twoProduct(bdx, ady, tmp3, tmp4)
  call twoTwoDiff(tmp1, tmp2, tmp3, tmp4, ab)
  call scaleExpansion(4, ab, cdx, cxab, nCxab)
  call scaleExpansion(nCxab, cxab, cdx, cxxab, nCxxab)
  call scaleExpansion(4, ab, cdy, cyab, nCyab)
  call scaleExpansion(nCyab, cyab, cdy, cyyab, nCyyab)
  call fastExpansionSum(nCxxab, nCyyab, cxxab, cyyab, cDet, nC)

  call fastExpansionSum(nA, nB, aDet, bDet, abDet, nAB)
  call fastExpansionSum(nAB, nC,  abDet, cDet, fin1, nFin1)

  determinant = estimate(nFin1, fin1)

  errBound = iccBound_B * keep

  ! Second escape at B
  if ((determinant >= errBound) .or. (-determinant >= errBound)) return

  call twoDiffErr(ax, dx, adx, adxErr)
  call twoDiffErr(ay, dy, ady, adyErr)
  call twoDiffErr(bx, dx, bdx, bdxErr)
  call twoDiffErr(by, dy, bdy, bdyErr)
  call twoDiffErr(cx, dx, cdx, cdxErr)
  call twoDiffErr(cy, dy, cdy, cdyErr)

  if ((adxErr == 0.d0) .and. (bdxErr == 0.d0) .and. &
      (adyErr == 0.d0) .and. (bdyErr == 0.d0) .and. &
      (adzErr == 0.d0) .and. (cdyErr == 0.d0)) return

  errBound = (iccBound_C * keep) + (resErrBound * abs(determinant))

  determinant = determinant &
              + (aLift * ((bdx * cdyErr + cdy * bdxErr)  &
                       -  (bdy * cdxErr + cdx * bdyErr)) &
              + 2.d0 * (adx * adxErr + ady * adyErr) * (bdx * cdy - bdy * cdx)) &
              + (bLift * ((cdx * adyErr + ady * cdxErr)  &
                       -  (cdy * adxErr + adx * cdyErr)) &
              + 2.d0 * (bdx * bdxErr +bdy * bdyErr) * (cdx * ady - cdy * adx))      &
              + (cLift * ((adx * bdyErr + bdy * adxErr)  &
                       -  (ady * bdxErr + bdx * adyErr)) &
              + 2.d0 * (cdx * cdxErr + cdy * cdyErr) * (adx * bdy - ady * bdx))
        
  ! Third escape at C
  if ((determinant > errBound) .or. (-determinant > errBound)) return

  if ((bdxErr /= 0.d0) .or. (bdyErr /= 0.d0) .or. (cdxErr /= 0.d0) .or. (cdyErr /= 0.d0)) then
    call square(adx, tmp1, tmp2)
    call square(ady, tmp3, tmp4)
    call twoTwoSum(tmp1, tmp2, tmp3, tmp4, aa)
  endif
  if ((cdxErr /= 0.d0) .or. (cdyErr /= 0.d0) .or. (adxErr /= 0.d0) .or. (adyErr /= 0.d0)) then
    call square(bdx, tmp1, tmp2)
    call square(bdy, tmp3, tmp4)
    call twoTwoSum(tmp1, tmp2, tmp3, tmp4, bb)
  endif
  if ((adxErr /= 0.d0) .or. (adyErr /= 0.d0) .or. (bdxErr /= 0.d0) .or. (bdyErr /= 0.d0)) then
    call square(cdx, tmp1, tmp2)
    call square(cdy, tmp3, tmp4)
    call twoTwoSum(tmp1, tmp2, tmp3, tmp4, cc)
  endif

  if (adxErr /= 0.d0) then
    call scaleExpansion(4, bc, adxErr, axtbc, nAxtbc)
    call scaleExpansion(nAxtbc, axtbc, 2.d0 * adx, t16a, nT16a)

    call scaleExpansion(4, cc, adxErr, axtcc, nAxtcc)
    call scaleExpansion(nAxtcc, axtcc, bdy, t16b, nT16b)

    call scaleExpansion(4, bb, adxErr, axtbb, nAxtbb)
    call scaleExpansion(nAxtbb, axtbb, -cdy, t16c, nT16c)

    call fastExpansionSum(nT16a, nT16b, t16a, t16b, t32a, nT32a)
    call fastExpansionSum(nT16c, nT32a, t16c, t32a, t48, nT48)
    call fastExpansionSum(nFin1, nT48, fin1, t48, fin2, nFin2)

    call move(nFin2, fin2, fin1, nFin1)
  endif
  if (adyErr /= 0.d0) then
    call scaleExpansion(4, bc, adyErr, aytbc, nAytbc)
    call scaleExpansion(nAytbc, aytbc, 2.d0 * ady, t16a, nT16a)

    call scaleExpansion(4, bb, adyErr, aytbb, nAytbb)
    call scaleExpansion(nAytbb, aytbb, cdx, t16b, nT16b)

    call scaleExpansion(4, cc, adyErr, aytcc, nAytcc)
    call scaleExpansion(nAytcc, aytcc, -bdx, t16c, nT16c)

    call fastExpansionSum(nT16a, nT16b, t16a, t16b, t32a, nT32a)
    call fastExpansionSum(nT16c, nT32a, t16c, t32a, t48, nT48)
    call fastExpansionSum(nFin1, nT48, fin1, t48, fin2, nFin2)

    call move(nFin2, fin2, fin1, nFin1)
  endif
  if (bdxErr /= 0.d0) then
    call scaleExpansion(4, ca, bdxErr, bxtca, nBxtca)
    call scaleExpansion(nBxtca, bxtca, 2.d0 * bdx, t16a, nT16a)

    call scaleExpansion(4, aa, bdxErr, bxtaa, nBxtaa)
    call scaleExpansion(nBxtaa, bxtaa, cdy, t16b, nT16b)

    call scaleExpansion(4, cc, bdxErr, bxtcc, nBxtcc)
    call scaleExpansion(nBxtcc, bxtcc, -ady, t16c, nT16c)

    call fastExpansionSum(nT16a, nT16b, t16a, t16b, t32a, nT32a)
    call fastExpansionSum(nT16c, nT32a, t16c, t32a, t48, nT48)
    call fastExpansionSum(nFin1, nT48, fin1, t48, fin2, nFin2)

    call move(nFin2, fin2, fin1, nFin1)
  endif
  if (bdyErr /= 0.d0) then
    call scaleExpansion(4, ca, bdyErr, bytca, nBytca)
    call scaleExpansion(nBytca, bytca, 2.d0 * bdy, t16a, nT16a)

    call scaleExpansion(4, cc, bdyErr, bytcc, nBytcc)
    call scaleExpansion(nBytcc, bytcc, adx, t16b, nT16b)

    call scaleExpansion(4, aa, bdyErr, bytaa, nBytaa)
    call scaleExpansion(nBytaa, bytaa, -cdx, t16c, nT16c)

    call fastExpansionSum(nT16a, nT16b, t16a, t16b, t32a, nT32a)
    call fastExpansionSum(nT16c, nT32a, t16c, t32a, t48, nT48)
    call fastExpansionSum(nFin1, nT48, fin1, t48, fin2, nFin2)

    call move(nFin2, fin2, fin1, nFin1)
  endif
  if (cdxErr /= 0.d0) then
    call scaleExpansion(4, ab, cdxErr, cxtab, nCxtab)
    call scaleExpansion(nCxtab, cxtab, 2.d0 * cdx, t16a, nT16a)

    call scaleExpansion(4, bb, cdxErr, cxtbb, nCxtbb)
    call scaleExpansion(nCxtbb, cxtbb, ady, t16b, nT16b)

    call scaleExpansion(4, aa, cdxErr, cxtaa, nCxtaa)
    call scaleExpansion(nCxtaa, cxtaa, -bdy, t16c, nT16c)

    call fastExpansionSum(nT16a, nT16b, t16a, t16b, t32a, nT32a)
    call fastExpansionSum(nT16c, nT32a, t16c, t32a, t48, nT48)
    call fastExpansionSum(nFin1, nT48, fin1, t48, fin2, nFin2)

    call move(nFin2, fin2, fin1, nFin1)
  endif
  if (cdyErr /= 0.d0) then
    call scaleExpansion(4, ab, cdyErr, cytab, nCytab)
    call scaleExpansion(nCytab, cytab, 2.d0 * cdy, t16a, nT16a)

    call scaleExpansion(4, aa, cdyErr, cytaa, nCytaa)
    call scaleExpansion(nCytaa, cytaa, bdx, t16b, nT16b)

    call scaleExpansion(4, bb, cdyErr, cytbb, nCytbb)
    call scaleExpansion(nCytbb, cytbb, -adx, t16c, nT16c)

    call fastExpansionSum(nT16a, nT16b, t16a, t16b, t32a, nT32a)
    call fastExpansionSum(nT16c, nT32a, t16c, t32a, t48, nT48)
    call fastExpansionSum(nFin1, nT48, fin1, t48, fin2, nFin2)

    call move(nFin2, fin2, fin1, nFin1)
  endif


  if ((adxErr /= 0.d0) .or. (adyErr /= 0.d0)) then
    if ((bdxErr /= 0.d0) .or. (bdyErr /= 0.d0) .or. (cdxErr /= 0.d0) .or. (cdyErr /= 0.d0)) then
      call twoProduct(bdxErr, cdy, tmp1, tmp2)
      call twoProduct(bdx, cdyErr, tmp3, tmp4)
      call twoTwoSum(tmp1, tmp2, tmp3, tmp4, u)
      negate = -bdy
      call twoProduct(cdxErr, negate, tmp1, tmp2)
      negate = -bdyErr
      call twoProduct(cdx, negate, tmp3, tmp4)
      call twoTwoSum(tmp1, tmp2, tmp3, tmp4, v)
      call fastExpansionSum(4, 4, u, v, bct, nBct)

      call twoProduct(bdxErr, cdyErr, tmp1, tmp2)
      call twoProduct(cdxErr, bdyErr, tmp3, tmp4)
      call twoTwoDiff(tmp1, tmp2, tmp3, tmp4, bctt)
      nBctt = 4
    else
      bct(1) = 0.d0
      nBct = 1
      bctt(1) = 0.d0
      nBctt = 1
    endif

    if (adxErr /= 0.d0) then
      call scaleExpansion(nAxtbc, axtbc, adxErr, t16a, nT16a)
      call scaleExpansion(nBct, bct, adxErr, axtbct, nAxtbct)
      call scaleExpansion(nAxtbct, axtbct, 2.d0 * adx, t32a, nT32a)
      call fastExpansionSum(nT16a, nT32a, t16a, t32a, t48, nT48)
      call fastExpansionSum(nFin1, nT48, fin1, t48, fin2, nFin2)
      call move(nFin2, fin2, fin1, nFin1)

      if (bdyErr /= 0.d0) then
        call scaleExpansion(4, cc, adxErr, t8, nT8)
        call scaleExpansion(nT8, t8, bdyErr, t16a, nT16a)
        call fastExpansionSum(nFin1, nT16a, fin1, t16a, fin2, nFin2)
        call move(nFin2, fin2, fin1, nFin1)
      endif
      if (cdyErr /= 0.d0) then
        call scaleExpansion(4, bb, -adxErr, t8, nT8)
        call scaleExpansion(nT8, t8, cdyErr, t16a, nT16a)
        call fastExpansionSum(nFin1, nT16a, fin1, t16a, fin2, nFin2)
        call move(nFin2, fin2, fin1, nFin1)
      endif
      call scaleExpansion(nAxtbct, axtbct, adxErr, t32a, nT32a)
      call scaleExpansion(nBctt, bctt, adxErr, axtbctt, nAxtbctt)
      call scaleExpansion(nAxtbctt, axtbctt, 2.d0 * adx, t16a, nT16a)
      call scaleExpansion(nAxtbctt, axtbctt, adxErr, t16b, nT16b)
      call fastExpansionSum(nT16a, nT16b, t16a, t16b, t32b, nT32b)
      call fastExpansionSum(nT32a,  nT32b, t32a, t32b, t64, nT64)
      call fastExpansionSum(nFin1, nT64, fin1, t64, fin2, nFin2)
      call move(nFin2, fin2, fin1, nFin1)
    endif
    if (adyErr /= 0.d0) then
      call scaleExpansion(nAytbc, aytbc, adyErr, t16a, nT16a)
      call scaleExpansion(nBct, bct, adyErr, aytbct, nAytbct)
      call scaleExpansion(nAytbct, aytbct, 2.d0 * ady, t32a, nT32a)
      call fastExpansionSum(nT16a, nT32a, t16a, t32a, t48, nT48)
      call fastExpansionSum(nFin1, nT48, fin1, t48, fin2, nFin2)
      call move(nFin2, fin2, fin1, nFin1)

      call scaleExpansion(nAytbct, aytbct, adyErr, t32a, nT32a)
      call scaleExpansion(nBctt, bctt, adyErr, aytbctt, nAytbctt)
      call scaleExpansion(nAytbctt, aytbctt, 2.d0 * ady, t16a, nT16a)
      call scaleExpansion(nAytbctt, aytbctt, adyErr, t16b, nT16b)
      call fastExpansionSum(nT16a, nT16b, t16a, t16b, t32b, nT32b)
      call fastExpansionSum(nT32a, nT32b, t32a, t32b, t64, nT64)
      call fastExpansionSum(nFin1, nT64, fin1, t64, fin2, nFin2)
      call move(nFin2, fin2, fin1, nFin1)
    endif
  endif
    
  if ((bdxErr /= 0.d0) .or. (bdyErr /= 0.d0)) then
    if ((cdxErr /= 0.d0) .or. (cdyErr /= 0.d0) .or. (adxErr /= 0.d0) .or. (adyErr /= 0.d0)) then
      call twoProduct(cdxErr, ady, tmp1, tmp2)
      call twoProduct(cdx, adyErr, tmp3, tmp4)
      call twoTwoSum(tmp1, tmp2, tmp3, tmp4, u)
      negate = -cdy
      call twoProduct(adxErr, negate, tmp1, tmp2)
      negate = -cdyErr
      call twoProduct(adx, negate, tmp3, tmp4)
      call twoTwoSum(tmp1, tmp2, tmp3, tmp4, v)
      call fastExpansionSum(4, 4, u, v, cat, nCat)

      call twoProduct(cdxErr, adyErr, tmp1, tmp2)
      call twoProduct(adxErr, cdyErr, tmp3, tmp4)
      call twoTwoDiff(tmp1, tmp2, tmp3, tmp4, catt)
      nCatt = 4
    else
      cat(1) = 0.d0
      nCat = 1
      catt(1) = 0.d0
      nCatt = 1
    endif

    if (bdxErr /= 0.d0) then
      call scaleExpansion(nBxtca, bxtca, bdxErr, t16a, nT16a)
      call scaleExpansion(nCat, cat, bdxErr, bxtcat, nBxtcat)
      call scaleExpansion(nBxtcat, bxtcat, 2.d0 * bdx, t32a, nT32a)
      call fastExpansionSum(nT16a, nT32a, t16a, t32a, t48, nT48)
      call fastExpansionSum(nFin1, nt48, fin1, t48, fin2, nFin2)
      call move(nFin2, fin2, fin1, nFin1)

      if (cdyErr /= 0.d0) then
        call scaleExpansion(4, aa, bdxErr, t8, nT8)
        call scaleExpansion(nT8, t8, cdyErr, t16a, nT16a)
        call fastExpansionSum(nFin1, nT16a, fin1, t16a, fin2, nFin2)
        call move(nFin2, fin2, fin1, nFin1)
      endif
      if (adyErr /= 0.d0) then
        call scaleExpansion(4, cc, -bdxErr, t8, nT8)
        call scaleExpansion(nT8, t8, adyErr, t16a, nT16a)
        call fastExpansionSum(nFin1, nT16a, fin1, t16a, fin2, nFin2)
        call move(nFin2, fin2, fin1, nFin1)
      endif
      call scaleExpansion(nBxtcat, bxtcat, bdxErr, t32a, nT32a)
      call scaleExpansion(nCatt, catt, bdxErr, bxtcatt, nBxtcatt)
      call scaleExpansion(nBxtcatt, bxtcatt, 2.d0 * bdx, t16a, nT16a)
      call scaleExpansion(nBxtcatt, bxtcatt, bdxErr, t16b, nT16b)
      call fastExpansionSum(nT16a, nT16b, t16a, t16b, t32b, nT32b)
      call fastExpansionSum(nT32a, nT32b, t32a, t32b, t64, nT64)
      call fastExpansionSum(nFin1, nT64, fin1, t64, fin2, nFin2)
      call move(nFin2, fin2, fin1, nFin1)
    endif
    if (bdyErr /= 0.d0) then
      call scaleExpansion(nBytca, bytca, bdyErr, t16a, nT16a)
      call scaleExpansion(nCat, cat, bdyErr, bytcat, nBytcat)
      call scaleExpansion(nBytcat, bytcat, 2.d0 * bdy, t32a, nT32a)
      call fastExpansionSum(nT16a, nT32a, t16a, t32a, t48, nT48)
      call fastExpansionSum(nFin1, nT48, fin1, t48, fin2, nFin2)
      call move(nFin2, fin2, fin1, nFin1)

      call scaleExpansion(nBytcat, bytcat, bdyErr, t32a, nT32a)
      call scaleExpansion(nCatt, catt, bdyErr, bytcatt, nBytcatt)
      call scaleExpansion(nBytcatt, bytcatt, 2.d0 * bdy, t16a, nT16a)
      call scaleExpansion(nBytcatt, bytcatt, bdyErr, t16b, nT16b)
      call fastExpansionSum(nT16a, nT16b, t16a, t16b, t32b, nT32b)
      call fastExpansionSum(nT32a, nT32b, t32a, t32b, t64, nT64)
      call fastExpansionSum(nFin1, nT64, fin1, t64, fin2, nFin2)
      call move(nFin2, fin2, fin1, nFin1)
    endif
  endif

  if ((cdxErr /= 0.d0) .or. (cdyErr /= 0.d0)) then
    if ((adxErr /= 0.d0) .or. (adyErr /= 0.d0) .or. (bdxErr /= 0.d0) .or. (bdyErr /= 0.d0)) then
      call twoProduct(adxErr, bdy, tmp1, tmp2)
      call twoProduct(adx, bdyErr, tmp3, tmp4)
      call twoTwoSum(tmp1, tmp2, tmp3, tmp4, u)
      negate = -ady
      call twoProduct(bdxErr, negate, tmp1, tmp2)
      negate = -adyErr
      call twoProduct(bdx, negate, tmp3, tmp4)
      call twoTwoSum(tmp1, tmp2, tmp3, tmp4, v)
      call fastExpansionSum(4, 4, u, v, abt, nAbt)

      call twoProduct(adxErr, bdyErr, tmp1, tmp2)
      call twoProduct(bdxErr, adyErr, tmp3, tmp4)
      call twoTwoDiff(tmp1, tmp2, tmp3, tmp4, abtt)
      nAbtt = 4
    else
      abt(1) = 0.d0
      nAbt = 1
      abtt(1) = 0.d0
      nAbtt = 1
    endif

    if (cdxErr /= 0.d0) then
      call scaleExpansion(nCxtab, cxtab, cdxErr, t16a, nT16a)
      call scaleExpansion(nAbt, abt, cdxErr, cxtabt, nCxtabt)
      call scaleExpansion(nCxtabt, cxtabt, 2.d0 * cdx, t32a, nT32a)
      call fastExpansionSum(nT16a, nT32a, t16a, t32a, t48, nT48)
      call fastExpansionSum(nFin1, nt48, fin1, t48, fin2, nFin2)
      call move(nFin2, fin2, fin1, nFin1)

      if (adyErr /= 0.d0) then
        call scaleExpansion(4, bb, cdxErr, t8, nT8)
        call scaleExpansion(nT8, t8, adyErr, t16a, nT16a)
        call fastExpansionSum(nFin1, nT16a, fin1, t16a, fin2, nFin2)
        call move(nFin2, fin2, fin1, nFin1)
      endif
      if (bdyErr /= 0.d0) then
        call scaleExpansion(4, aa, -cdxErr, t8, nT8)
        call scaleExpansion(nT8, t8, bdyErr, t16a, nT16a)
        call fastExpansionSum(nFin1, nT16a, fin1, t16a, fin2, nFin2)
        call move(nFin2, fin2, fin1, nFin1)
      endif
      call scaleExpansion(nCxtabt, cxtabt, cdxErr, t32a, nT32a)
      call scaleExpansion(nAbtt, abtt, cdxErr, cxtabtt, nCxtabtt)
      call scaleExpansion(nCxtabtt, cxtabtt, 2.d0 * cdx, t16a, nT16a)
      call scaleExpansion(nCxtabtt, cxtabtt, cdxErr, t16b, nT16b)
      call fastExpansionSum(nT16a, nT16b, t16a, t16b, t32b, nT32b)
      call fastExpansionSum(nT32a, nT32b, t32a, t32b, t64, nT64)
      call fastExpansionSum(nFin1, nT64, fin1, t64, fin2, nFin2)
      call move(nFin2, fin2, fin1, nFin1)
    endif
    if (cdyErr /= 0.d0) then
      call scaleExpansion(nCytab, cytab, cdyErr, t16a, nT16a)
      call scaleExpansion(nAbt, abt, cdyErr, cytabt, nCytabt)
      call scaleExpansion(nCytabt, cytabt, 2.d0 * cdy, t32a, nT32a)
      call fastExpansionSum(nT16a, nT32a, t16a, t32a, t48, nT48)
      call fastExpansionSum(nFin1, nT48, fin1, t48, fin2, nFin2)
      call move(nFin2, fin2, fin1, nFin1)

      call scaleExpansion(nCytabt, cytabt, cdyErr, t32a, nT32a)
      call scaleExpansion(nAbtt, abtt, cdyErr, cytabtt, nCytabtt)
      call scaleExpansion(nCytabtt, cytabtt, 2.d0 * cdy, t16a, nT16a)
      call scaleExpansion(nCytabtt, cytabtt, cdyErr, t16b, nT16b)
      call fastExpansionSum(nT16a, nT16b, t16a, t16b, t32b, nT32b)
      call fastExpansionSum(nT32a, nT32b, t32a, t32b, t64, nT64)
      call fastExpansionSum(nFin1, nT64, fin1, t64, fin2, nFin2)
      call move(nFin2, fin2, fin1, nFin1)
    endif
  endif

  determinant = fin1(nFin1)
  
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure inSphere!(ax, ay, az, bx, by, bz, cx, cy, cz, dx, dy, dz, ex, ey, ez) result(determinant)
  !   !! Determines whether the point e is inside the circumsphere of the tetrahedron formed by a-b-c-d
  !   !! Returns a positive value if e is inside.
  !   !! Returns a negative value if e is outside.
  !   !! Returns a zero if the five points are cospherical.
  !   !! a -> b -> c -> d must be ordered in a clockwise manner as defined by orient3D
  !   !!
  !   !! Uses an adaptive floating method by Shewchuk. Only exact computations
  !   !! are carried out when needed.
  ! !====================================================================!
  ! real(r64), intent(in) :: ax
  !   !! x co-ordinate of the first point
  ! real(r64), intent(in) :: ay
  !   !! y co-ordinate of the first point
  ! real(r64), intent(in) :: az
  !   !! z co-ordinate of the first point
  ! real(r64), intent(in) :: bx
  !   !! x co-ordinate of the second point
  ! real(r64), intent(in) :: by
  !   !! y co-ordinate of the second point
  ! real(r64), intent(in) :: bz
  !   !! z co-ordinate of the second point
  ! real(r64), intent(in) :: cx
  !   !! x co-ordinate of the third point
  ! real(r64), intent(in) :: cy
  !   !! y co-ordinate of the third point
  ! real(r64), intent(in) :: cz
  !   !! z co-ordinate of the third point
  ! real(r64), intent(in) :: dx
  !   !! x co-ordinate of the fourth point
  ! real(r64), intent(in) :: dy
  !   !! y co-ordinate of the fourth point
  ! real(r64), intent(in) :: dz
  !   !! z co-ordinate of the fourth point
  ! real(r64), intent(in) :: ex
  !   !! x co-ordinate of the fifth point
  ! real(r64), intent(in) :: ey
  !   !! y co-ordinate of the fifth point
  ! real(r64), intent(in) :: ez
  !   !! z co-ordinate of the fifth point
  ! real(r64) :: determinant
  !   !! [-ve, 0, +ve] for point e [outside, on, inside] the circumsphere defined by points a -> b -> c -> d

  real(r64), dimension(4)  :: ab, bc, cd, da, ac, bd
  real(r64), dimension(8)  :: t8a, t8b, t8c
  real(r64), dimension(16)  :: t16
  real(r64), dimension(24)  :: t24
  real(r64), dimension(48)  :: t48
  integer(i32) :: nT8a, nT8b, nT8c, nT16, nT24, nT48
  real(r64), dimension(96) :: xDet
  real(r64), dimension(96) :: yDet
  real(r64), dimension(96) :: zDet
  real(r64), dimension(192) :: xyDet
  integer(i32) :: nX, nY, nZ, nXY
  real(r64), dimension(288)  :: aDet
  real(r64), dimension(288)  :: bDet
  real(r64), dimension(288)  :: cDet
  real(r64), dimension(288)  :: dDet
  real(r64), dimension(576)  :: abDet, cdDet
  integer(i32) :: nA, nB, nC, nD, nAB, nCD
  real(r64) :: fin1(1152)
  integer(i32) :: nFin1

  real(r64) :: aex, bex, cex, dex
  real(r64) :: aey, bey, cey, dey
  real(r64) :: aez, bez, cez, dez
  real(r64) :: aexbey, bexaey, bexcey, cexbey, cexdey, dexcey, dexaey, aexdey
  real(r64) :: aexcey, cexaey, bexdey, dexbey
  real(r64) :: alift, blift, clift, dlift
  real(r64) :: ab0, bc0, cd0, da0, ac0, bd0
  real(r64) :: abc, bcd, cda, dab
  real(r64) :: aezplus, bezplus, cezplus, dezplus
  real(r64) :: aexbeyplus, bexaeyplus, bexceyplus, cexbeyplus
  real(r64) :: cexdeyplus, dexceyplus, dexaeyplus, aexdeyplus
  real(r64) :: aexceyplus, cexaeyplus, bexdeyplus, dexbeyplus
  real(r64) :: abeps, aceps, bceps, bdeps, cdeps, daeps
  real(r64) :: aexErr, aeyErr, aezErr
  real(r64) :: bexErr, beyErr, bezErr
  real(r64) :: cexErr, ceyErr, cezErr
  real(r64) :: dexErr, deyErr, dezErr
  real(r64) :: errBound
  real(r64) :: keep
  real(r64) :: tmp1, tmp2, tmp3, tmp4

  aex = ax - ex
  bex = bx - ex
  cex = cx - ex
  dex = dx - ex
  aey = ay - ey
  bey = by - ey
  cey = cy - ey
  dey = dy - ey
  aez = az - ez
  bez = bz - ez
  cez = cz - ez
  dez = dz - ez

  aexbey = aex * bey
  bexaey = bex * aey
  ab0 = aexbey - bexaey
  bexcey = bex * cey
  cexbey = cex * bey
  bc0 = bexcey - cexbey
  cexdey = cex * dey
  dexcey = dex * cey
  cd0 = cexdey - dexcey
  dexaey = dex * aey
  aexdey = aex * dey
  da0 = dexaey - aexdey
  aexcey = aex * cey
  cexaey = cex * aey
  ac0 = aexcey - cexaey
  bexdey = bex * dey
  dexbey = dex * bey
  bd0 = bexdey - dexbey

  abc = aez * bc0 - bez * ac0 + cez * ab0
  bcd = bez * cd0 - cez * bd0 + dez * bc0
  cda = cez * da0 + dez * ac0 + aez * cd0
  dab = dez * ab0 + aez * bd0 + bez * da0

  alift = aex * aex + aey * aey + aez * aez
  blift = bex * bex + bey * bey + bez * bez
  clift = cex * cex + cey * cey + cez * cez
  dlift = dex * dex + dey * dey + dez * dez

  determinant = (dlift * abc - clift * dab) + (blift * cda - alift * bcd)

  aezplus = abs(aez)
  bezplus = abs(bez)
  cezplus = abs(cez)
  dezplus = abs(dez)
  aexbeyplus = abs(aexbey)
  bexaeyplus = abs(bexaey)
  bexceyplus = abs(bexcey)
  cexbeyplus = abs(cexbey)
  cexdeyplus = abs(cexdey)
  dexceyplus = abs(dexcey)
  dexaeyplus = abs(dexaey)
  aexdeyplus = abs(aexdey)
  aexceyplus = abs(aexcey)
  cexaeyplus = abs(cexaey)
  bexdeyplus = abs(bexdey)
  dexbeyplus = abs(dexbey)

  keep = ((cexdeyplus + dexceyplus) * bezplus  &
       +  (dexbeyplus + bexdeyplus) * cezplus  &
       +  (bexceyplus + cexbeyplus) * dezplus) * alift &
       + ((dexaeyplus + aexdeyplus) * cezplus  &
       +  (aexceyplus + cexaeyplus) * dezplus  &
       +  (cexdeyplus + dexceyplus) * aezplus) * blift &
       + ((aexbeyplus + bexaeyplus) * dezplus  &
       +  (bexdeyplus + dexbeyplus) * aezplus  &
       +  (dexaeyplus + aexdeyplus) * bezplus) * clift &
       + ((bexceyplus + cexbeyplus) * aezplus  &
       +  (cexaeyplus + aexceyplus) * bezplus  &
       +  (aexbeyplus + bexaeyplus) * cezplus) * dlift

  errBound = icsBound_A * keep

  if ((determinant > errBound) .or. (-determinant > errBound)) return

  call twoProduct(aex, bey, tmp1, tmp2)
  call twoProduct(bex, aey, tmp3, tmp4)
  call twoTwoDiff(tmp1, tmp2, tmp3, tmp4, ab)

  call twoProduct(bex, cey, tmp1, tmp2)
  call twoProduct(cex, bey, tmp3, tmp4)
  call twoTwoDiff(tmp1, tmp2, tmp3, tmp4, bc)

  call twoProduct(cex, dey, tmp1, tmp2)
  call twoProduct(dex, cey, tmp3, tmp4)
  call twoTwoDiff(tmp1, tmp2, tmp3, tmp4, cd)

  call twoProduct(dex, aey, tmp1, tmp2)
  call twoProduct(aex, dey, tmp3, tmp4)
  call twoTwoDiff(tmp1, tmp2, tmp3, tmp4, da)

  call twoProduct(aex, cey, tmp1, tmp2)
  call twoProduct(cex, aey, tmp3, tmp4)
  call twoTwoDiff(tmp1, tmp2, tmp3, tmp4, ac)

  call twoProduct(bex, dey, tmp1, tmp2)
  call twoProduct(dex, bey, tmp3, tmp4)
  call twoTwoDiff(tmp1, tmp2, tmp3, tmp4, bd)

  call scaleExpansion(4, cd, bez, t8a, nT8a)
  call scaleExpansion(4, bd, -cez, t8b, nT8b)
  call scaleExpansion(4, bc, dez, t8c, nT8c)
  call fastExpansionSum(nT8a, nT8b, t8a, t8b, t16, nT16)
  call fastExpansionSum(nT8c, nT16, t8c, t16, t24, nT24)
  call scaleExpansion(nT24, t24, aex, t48, nT48)
  call scaleExpansion(nT48, t48, -aex, xDet, nX)
  call scaleExpansion(nT24, t24, aey, t48, nT48)
  call scaleExpansion(nT48, t48, -aey, yDet, nY)
  call scaleExpansion(nT24, t24, aez, t48, nT48)
  call scaleExpansion(nT48, t48, -aez, zDet, nZ)
  call fastExpansionSum(nX, nY, xDet, yDet, xyDet, nXY)
  call fastExpansionSum(nXY, nZ, xyDet, zDet, aDet, nA)

  call scaleExpansion(4, da, cez, t8a, nT8a)
  call scaleExpansion(4, ac, dez, t8b, nT8b)
  call scaleExpansion(4, cd, aez, t8c, nT8c)
  call fastExpansionSum(nT8a, nT8b, t8a, t8b, t16, nT16)
  call fastExpansionSum(nT8c, nT16, t8c, t16, t24, nT24)
  call scaleExpansion(nT24, t24, bex, t48, nT48)
  call scaleExpansion(nT48, t48, bex, xdet, nX)
  call scaleExpansion(nT24, t24, bey, t48, nT48)
  call scaleExpansion(nT48, t48, bey, ydet, nY)
  call scaleExpansion(nT24, t24, bez, t48, nT48)
  call scaleExpansion(nT48, t48, bez, zdet, nZ)
  call fastExpansionSum(nX, nY, xDet, yDet, xyDet, nXY)
  call fastExpansionSum(nXY, nZ, xyDet, zDet, bDet, nB) 

  call scaleExpansion(4, ab, dez, t8a, nT8a)
  call scaleExpansion(4, bd, aez, t8b, nT8b)
  call scaleExpansion(4, da, bez, t8c, nT8c)
  call fastExpansionSum(nT8a, nT8b, t8a, t8b, t16, nT16)
  call fastExpansionSum(nT8c, nT16, t8c, t16, t24, nT24)
  call scaleExpansion(nT24, t24, cex, t48, nT48)
  call scaleExpansion(nT48, t48, -cex, xDet, nX)
  call scaleExpansion(nT24, t24, cey, t48, nT48)
  call scaleExpansion(nT48, t48, -cey, yDet, nY)
  call scaleExpansion(nT24, t24, cez, t48, nT48)
  call scaleExpansion(nT48, t48, -cez, zDet, nZ)
  call fastExpansionSum(nX, nY, xDet, yDet, xyDet, nXY)
  call fastExpansionSum(nXY, nZ, xyDet, zDet, cDet, nC)

  call scaleExpansion(4, bc, aez, t8a, nT8a)
  call scaleExpansion(4, ac, -bez, t8b, nT8b)
  call scaleExpansion(4, ab, cez, t8c, nT8c)
  call fastExpansionSum(nT8a, nT8b, t8a, t8b, t16, nT16)
  call fastExpansionSum(nT8c, nT16, t8c, t16, t24, nT24)
  call scaleExpansion(nT24, t24, dex, t48, nT48)
  call scaleExpansion(nT48, t48, dex, xDet, nX)
  call scaleExpansion(nT24, t24, dey, t48, nT48)
  call scaleExpansion(nT48, t48, dey, yDet, nY)
  call scaleExpansion(nT24, t24, dez, t48, nT48)
  call scaleExpansion(nT48, t48, dez, zDet, nZ)
  call fastExpansionSum(nX, nY, xDet, yDet, xyDet, nXY)
  call fastExpansionSum(nXY, nZ, xyDet, zDet, dDet, nD)

  call fastExpansionSum(nA, nB, aDet, bDet, abDet, nAB)
  call fastExpansionSum(nC, nD, cDet, dDet, cdDet, nCD)
  call fastExpansionSum(nAB, nCD, abDet, cdDet, fin1, nFin1)

  determinant = estimate(nFin1, fin1)

  errBound = icsBound_B * keep
  
  if ((determinant >= errBound) .or. (-determinant >= errBound)) return 

  call twoDiffErr(ax, ex, aex, aexErr)
  call twoDiffErr(ay, ey, aey, aeyErr)
  call twoDiffErr(az, ez, aez, aezErr)
  call twoDiffErr(bx, ex, bex, bexErr)
  call twoDiffErr(by, ey, bey, beyErr)
  call twoDiffErr(bz, ez, bez, bezErr)
  call twoDiffErr(cx, ex, cex, cexErr)
  call twoDiffErr(cy, ey, cey, ceyErr)
  call twoDiffErr(cz, ez, cez, cezErr)
  call twoDiffErr(dx, ex, dex, dexErr)
  call twoDiffErr(dy, ey, dey, deyErr)
  call twoDiffErr(dz, ez, dez, dezErr)

  if ((aexErr == 0.0) .and. (aeyErr == 0.0) .and. (aezErr == 0.0) .and. &
      (bexErr == 0.0) .and. (beyErr == 0.0) .and. (bezErr == 0.0) .and. &
      (cexErr == 0.0) .and. (ceyErr == 0.0) .and. (cezErr == 0.0) .and. &
      (dexErr == 0.0) .and. (deyErr == 0.0) .and. (dezErr == 0.0)) return

  errBound = icsBound_C * keep + resErrBound * abs(determinant)

  abeps = (aex * beyErr + bey * aexErr) - (aey * bexErr + bex * aeyErr)
  bceps = (bex * ceyErr + cey * bexErr) - (bey * cexErr + cex * beyErr)
  cdeps = (cex * deyErr + dey * cexErr) - (cey * dexErr + dex * ceyErr)
  daeps = (dex * aeyErr + aey * dexErr) - (dey * aexErr + aex * deyErr)
  aceps = (aex * ceyErr + cey * aexErr) - (aey * cexErr + cex * aeyErr)
  bdeps = (bex * deyErr + dey * bexErr) - (bey * dexErr + dex * beyErr)

  determinant = determinant &
    + (((bex * bex + bey * bey + bez * bez) &
     * ((cez * daeps + dez * aceps + aez * cdeps) &
     +  (cezErr * da(4) + dezErr * ac(4) + aezErr * cd(4))) &
     + (dex * dex + dey * dey + dez * dez) &
     * ((aez * bceps - bez * aceps + cez * abeps) &
     +  (aezErr * bc(4) - bezErr * ac(4) + cezErr * ab(4)))) &
     - ((aex * aex + aey * aey + aez * aez) &
     * ((bez * cdeps - cez * bdeps + dez * bceps) &
     +  (bezErr * cd(4) - cezErr * bd(4) + dezErr * bc(4))) &
     + (cex * cex + cey * cey + cez * cez) &
     * ((dez * abeps + aez * bdeps + bez * daeps) &
     +  (dezErr * ab(4) + aezErr * bd(4) + bezErr * da(4))))) &
    + 2.0  &
     * (((bex * bexErr + bey * beyErr + bez * bezErr) &
     *  (cez * da(4) + dez * ac(4) + aez * cd(4)) &
     +   (dex * dexErr + dey * deyErr + dez * dezErr) &
     *  (aez * bc(4) - bez * ac(4) + cez * ab(4))) &
     - ((aex * aexErr + aey * aeyErr + aez * aezErr) &
     *  (bez * cd(4) - cez * bd(4) + dez * bc(4)) &
     +   (cex * cexErr + cey * ceyErr + cez * cezErr) &
     *  (dez * ab(4) + aez * bd(4) + bez * da(4))))

  if ((determinant >= errBound) .or. (-determinant >= errBound)) return

  determinant = inSphere_exact(ax, ay, az, bx, by, bz, cx, cy, cz, dx, dy, dz, ex, ey, ez)
  end procedure
  !====================================================================!
  !====================================================================!
  function inSphere_exact(ax, ay, az, bx, by, bz, cx, cy, cz, dx, dy, dz, ex, ey, ez) result(determinant)
  !====================================================================!
  real(r64), intent(in) :: ax
    !! x co-ordinate of the first point
  real(r64), intent(in) :: ay
    !! y co-ordinate of the first point
  real(r64), intent(in) :: az
    !! z co-ordinate of the first point
  real(r64), intent(in) :: bx
    !! x co-ordinate of the second point
  real(r64), intent(in) :: by
    !! y co-ordinate of the second point
  real(r64), intent(in) :: bz
    !! z co-ordinate of the second point
  real(r64), intent(in) :: cx
    !! x co-ordinate of the third point
  real(r64), intent(in) :: cy
    !! y co-ordinate of the third point
  real(r64), intent(in) :: cz
    !! z co-ordinate of the third point
  real(r64), intent(in) :: dx
    !! x co-ordinate of the fourth point
  real(r64), intent(in) :: dy
    !! y co-ordinate of the fourth point
  real(r64), intent(in) :: dz
    !! z co-ordinate of the fourth point
  real(r64), intent(in) :: ex
    !! x co-ordinate of the fifth point
  real(r64), intent(in) :: ey
    !! y co-ordinate of the fifth point
  real(r64), intent(in) :: ez
    !! z co-ordinate of the fifth point
  real(r64) :: determinant
    !! [-ve, 0, +ve] for point e [outside, on, inside] the circumsphere defined by points a -> b -> c -> d

  real(r64) :: aex, bex, cex, dex, aey, bey, cey, dey, aez, bez, cez, dez
  real(r64) :: aexErr, bexErr, cexErr, dexErr
  real(r64) :: aeyErr, beyErr, ceyErr, deyErr
  real(r64) :: aezErr, bezErr, cezErr, dezErr
  real(r64) :: negate, negateErr
  real(r64), dimension(8) :: axby, bxcy, cxdy, dxay, axcy, bxdy
  real(r64), dimension(8) :: bxay, cxby, dxcy, axdy, cxay, dxby
  real(r64), dimension(16) :: ab, bc, cd, da, ac, bd
  integer(i32) :: nAB, nBC, nCD, nDA, nAC, nBD
  real(r64), dimension(32) :: t32a, t32b
  real(r64), dimension(64) :: t64a, t64b, t64c
  real(r64), dimension(128) :: t128
  real(r64), dimension(192) :: t192
  integer(i32) :: nT32a, nT32b, nT64a, nT64b, nT64c, nT128, nT192
  real(r64), dimension(384) :: xDet, xtDet
  real(r64), dimension(768) :: xxDet, xxtDet, xtxtDet
  real(r64), dimension(1536) :: x1
  real(r64), dimension(2304) :: x2
  integer(i32) :: nX, nXX, nXt, nXxt, nXtxt, nX1, nX2
  real(r64), dimension(384) :: yDet, ytDet
  real(r64), dimension(768) :: yyDet, yytDet, ytytDet
  real(r64), dimension(1536) :: y1
  real(r64), dimension(2304) :: y2
  integer(i32) :: nY, nYY, nYt, nYyt, nYtyt, nY1, nY2
  real(r64), dimension(384) :: zDet, ztDet
  real(r64), dimension(768) :: zzDet, zztDet, ztztDet
  real(r64), dimension(1536) :: z1
  real(r64), dimension(2304) :: z2
  integer(i32) :: nZ, nZZ, nZt, nZzt, nZtzt, nZ1, nZ2
  real(r64), dimension(4608) :: xyDet
  integer(i32) :: nXY
  real(r64), dimension(6912) :: aDet, bDet, cDet, dDet
  real(r64), dimension(13824) :: abDet, cdDet
  real(r64), dimension(27648) :: d
  integer(i32) :: nA, nB, nC, nD
  integer(i32) :: nD2
  integer(i32) :: i

  call twoDiff(ax, ex, aex, aexErr)
  call twoDiff(ay, ey, aey, aeyErr)
  call twoDiff(az, ez, aez, aezErr)
  call twoDiff(bx, ex, bex, bexErr)
  call twoDiff(by, ey, bey, beyErr)
  call twoDiff(bz, ez, bez, bezErr)
  call twoDiff(cx, ex, cex, cexErr)
  call twoDiff(cy, ey, cey, ceyErr)
  call twoDiff(cz, ez, cez, cezErr)
  call twoDiff(dx, ex, dex, dexErr)
  call twoDiff(dy, ey, dey, deyErr)
  call twoDiff(dz, ez, dez, dezErr)

  call twoTwoProduct(aex, aexErr, bey, beyErr, axby)
  negate = -aey
  negateErr = -aeyErr
  call twoTwoProduct(bex, bexErr, negate, negateErr, bxay)
  call fastExpansionSum(8, 8, axby, bxay, ab, nAB)
  call twoTwoProduct(bex, bexErr, cey, ceyErr, bxcy)
  negate = -bey
  negateErr = -beyErr
  call twoTwoProduct(cex, cexErr, negate, negateErr, cxby)
  call fastExpansionSum(8, 8, bxcy, cxby, bc, nBC)
  call twoTwoProduct(cex, cexErr, dey, deyErr, cxdy)
  negate = -cey
  negateErr = -ceyErr
  call twoTwoProduct(dex, dexErr, negate, negateErr, dxcy)
  call fastExpansionSum(8, 8, cxdy, dxcy, cd, nCD)
  call twoTwoProduct(dex, dexErr, aey, aeyErr, dxay)
  negate = -dey
  negateErr = -deyErr
  call twoTwoProduct(aex, aexErr, negate, negateErr, axdy)
  call fastExpansionSum(8, 8, dxay, axdy, da, nDA)
  call twoTwoProduct(aex, aexErr, cey, ceyErr, axcy)
  negate = -aey
  negateErr = -aeyErr
  call twoTwoProduct(cex, cexErr, negate, negateErr, cxay)
  call fastExpansionSum(8, 8, axcy, cxay, ac, nAC)
  call twoTwoProduct(bex, bexErr, dey, deyErr, bxdy)
  negate = -bey
  negateErr = -beyErr
  call twoTwoProduct(dex, dexErr, negate, negateErr, dxby)
  call fastExpansionSum(8, 8, bxdy, dxby, bd, nBD)

  call scaleExpansion(nCD, cd, -bez, t32a, nT32a)
  call scaleExpansion(nCD, cd, -bezErr, t32b, nT32b)
  call fastExpansionSum(nT32a, nT32b, t32a, t32b, t64a, nT64a)
  call scaleExpansion(nBD, bd, cez, t32a, nT32a)
  call scaleExpansion(nBD, bd, cezErr, t32b, nT32b)
  call fastExpansionSum(nT32a, nT32b, t32a, t32b, t64b, nT64b)
  call scaleExpansion(nBC, bc, -dez, t32a, nT32a)
  call scaleExpansion(nBC, bc, -dezErr, t32b, nT32b)
  call fastExpansionSum(nT32a, nT32b, t32a, t32b, t64c, nT64c)
  call fastExpansionSum(nT64a, nT64b, t64a, t64b, t128, nT128)
  call fastExpansionSum(nT64c, nT128, t64c, t128, t192, nT192)
  
  call scaleExpansion(nT192, t192, aex, xDet, nX)
  call scaleExpansion(nX, xDet, aex, xxDet, nXX)
  call scaleExpansion(nT192, t192, aexErr, xtDet, nXt)
  call scaleExpansion(nXt, xtDet, aex, xxtDet,nXxt)
  do i = 1, nXxt
    xxtDet(i) = 2.d0 * xxtDet(i)
  enddo
  call scaleExpansion(nXt, xtDet, aexErr, xtxtDet, nXtxt)
  call fastExpansionSum(nXX, nXxt, xxDet, xxtDet, x1, nX1)
  call fastExpansionSum(nX1, nXtxt, x1, xtxtDet, x2, nX2)

  call scaleExpansion(nT192, t192, aey, yDet, nY)
  call scaleExpansion(nY, yDet, aey, yyDet, nYY)
  call scaleExpansion(nT192, t192, aeyErr, ytDet, nYt)
  call scaleExpansion(nYt, ytDet, aey, yytDet, nYyt)
  do i = 1, nYyt
    yytDet(i) = 2.d0 * yytDet(i)
  enddo
  call scaleExpansion(nYt, ytDet, aeyErr, ytytDet, nYtyt)
  call fastExpansionSum(nYY, nYyt, yyDet, yytDet, y1, nY1)
  call fastExpansionSum(nY1, nYtyt, y1, ytytDet, y2, nY2)

  call scaleExpansion(nT192, t192, aez, zDet, nZ)
  call scaleExpansion(nZ, zDet, aez, zzDet, nZZ)
  call scaleExpansion(nT192, t192, aezErr, ztDet, nZt)
  call scaleExpansion(nZt, ztDet, aez, zztDet, nZzt)
  do i = 1, nZzt
    zztDet(i) = 2.d0 * zztDet(i)
  enddo
  call scaleExpansion(nZt, ztDet, aezErr, ztztDet, nZtzt)
  call fastExpansionSum(nZZ, nZzt, zzDet, zztDet, z1, nZ1)
  call fastExpansionSum(nZ1, nZtzt, z1, ztztDet, z2, nZ2)

  call fastExpansionSum(nX2, nY2, x2, y2, xyDet, nXY)
  call fastExpansionSum(nZ2, nXY, z2, xyDet, aDet, nA)

  call scaleExpansion(nDA, da, cez, t32a, nT32a)
  call scaleExpansion(nDA, da, cezErr, t32b, nT32b)
  call fastExpansionSum(nT32a, nT32b, t32a, t32b, t64a, nT64a)
  call scaleExpansion(nAC, ac, dez, t32a, nT32a)
  call scaleExpansion(nAC, ac, dezErr, t32b, nT32b)
  call fastExpansionSum(nT32a, nT32b, t32a, t32b, t64b, nT64b)
  call scaleExpansion(nCD, cd, aez, t32a, nT32a)
  call scaleExpansion(nCD, cd, aezErr, t32b, nT32b)
  call fastExpansionSum(nT32a, nT32b, t32a, t32b, t64c, nT64c)
  call fastExpansionSum(nT64a, nT64b, t64a, t64b, t128, nT128)
  call fastExpansionSum(nT64c, nT128, t64c, t128, t192, nT192)

  call scaleExpansion(nT192, t192, bex, xDet, nX)
  call scaleExpansion(nX, xDet, bex, xxDet, nXX)
  call scaleExpansion(nT192, t192, bexErr, xtDet, nXt)
  call scaleExpansion(nXt, xtDet, bex, xxtDet, nXxt)
  do i = 1, nXxt
    xxtDet(i) = 2.d0 * xxtDet(i)
  enddo
  call scaleExpansion(nXt, xtDet, bexErr, xtxtDet, nXtxt)
  call fastExpansionSum(nXX, nXxt, xxDet, xxtDet, x1, nX1)
  call fastExpansionSum(nX1, nXtxt, x1, xtxtDet, x2, nX2)

  call scaleExpansion(nT192, t192, bey, yDet, nY)
  call scaleExpansion(nY, yDet, bey, yyDet, nYY)
  call scaleExpansion(nT192, t192, beyErr, ytDet, nYt)
  call scaleExpansion(nYt, ytDet, bey, yytDet, nYyt)
  do i = 1, nYyt
    yytDet(i) = 2.d0 * yytDet(i)
  enddo
  call scaleExpansion(nYt, ytDet, beyErr, ytytDet, nYtyt)
  call fastExpansionSum(nYY, nYyt, yyDet, yytDet, y1, nY1)
  call fastExpansionSum(nY1, nYtyt, y1, ytytDet, y2, nY2)

  call scaleExpansion(nT192, t192, bez, zDet, nZ)
  call scaleExpansion(nZ, zDet, bez, zzDet, nZZ)
  call scaleExpansion(nT192, t192, bezErr, ztDet, nZt)
  call scaleExpansion(nZt, ztDet, bez, zztDet, nZzt)
  do i = 1, nZzt
    zztDet(i) = 2.d0 * zztDet(i)
  enddo
  call scaleExpansion(nZt, ztDet, bezErr, ztztDet, nZtzt)
  call fastExpansionSum(nZZ, nZzt, zzDet, zztDet, z1, nZ1)
  call fastExpansionSum(nZ1, nZtzt, z1, ztztDet, z2, nZ2)

  call fastExpansionSum(nX2, nY2, x2, y2, xyDet, nXY)
  call fastExpansionSum(nZ2, nXY, z2, xyDet, bDet, nB)

  call scaleExpansion(nAB, ab, -dez, t32a, nT32a)
  call scaleExpansion(nAB, ab, -dezErr, t32b, nT32b)
  call fastExpansionSum(nT32a, nT32b, t32a, t32b, t64a, nT64a)
  call scaleExpansion(nBD, bd, -aez, t32a, nT32a)
  call scaleExpansion(nBD, bd, -aezErr, t32b, nT32b)
  call fastExpansionSum(nT32a, nT32b, t32a, t32b, t64b, nT64b)
  call scaleExpansion(nDA, da, -bez, t32a, nT32a)
  call scaleExpansion(nDA, da, -bezErr, t32b, nT32b)
  call fastExpansionSum(nT32a, nT32b, t32a, t32b, t64c, nT64c)
  call fastExpansionSum(nT64a, nT64b, t64a, t64b, t128, nT128)
  call fastExpansionSum(nT64c, nT128, t64c, t128, t192, nT192)

  call scaleExpansion(nT192, t192, cex, xDet, nX)
  call scaleExpansion(nX, xDet, cex, xxDet, nXX)
  call scaleExpansion(nT192, t192, cexErr, xtDet, nXt)
  call scaleExpansion(nXt, xtDet, cex, xxtDet, nXxt)
  do i = 1, nXxt
    xxtDet(i) = 2.d0 * xxtDet(i)
  enddo
  call scaleExpansion(nXt, xtDet, cexErr, xtxtDet, nXtxt)
  call fastExpansionSum(nXX, nXxt, xxDet, xxtDet, x1, nX1)
  call fastExpansionSum(nX1, nXtxt, x1, xtxtDet, x2, nX2)

  call scaleExpansion(nT192, t192, cey, yDet, nY)
  call scaleExpansion(nY, yDet, cey, yyDet, nYY)
  call scaleExpansion(nT192, t192, ceyErr, ytDet, nYt)
  call scaleExpansion(nYt, ytDet, cey, yytDet, nYyt)
  do i = 1, nYyt
    yytDet(i) = 2.d0 * yytDet(i)
  enddo
  call scaleExpansion(nYt, ytDet, ceyErr, ytytDet, nYtyt)
  call fastExpansionSum(nYY, nYyt, yyDet, yytDet, y1, nY1)
  call fastExpansionSum(nY1, nYtyt, y1, ytytDet, y2, nY2)

  call scaleExpansion(nT192, t192, cez, zDet, nZ)
  call scaleExpansion(nZ, zDet, cez, zzDet, nZZ)
  call scaleExpansion(nT192, t192, cezErr, ztDet, nZt)
  call scaleExpansion(nZt, ztDet, cez, zztDet, nZzt)
  do i = 1, nZzt
    zztDet(i) = 2.d0 * zztDet(i)
  enddo
  call scaleExpansion(nZt, ztDet, cezErr, ztztDet, nZtzt)
  call fastExpansionSum(nZZ, nZzt, zzDet, zztDet, z1, nZ1)
  call fastExpansionSum(nZ1, nZtzt, z1, ztztDet, z2, nZ2)

  call fastExpansionSum(nX2, nY2, x2, y2, xyDet, nXY)
  call fastExpansionSum(nZ2, nXY, z2, xyDet, cDet, nC)

  call scaleExpansion(nBC, bc, aez, t32a, nT32a)
  call scaleExpansion(nBC, bc, aezErr, t32b, nT32b)
  call fastExpansionSum(nT32a, nT32b, t32a, t32b, t64a, nT64a)
  call scaleExpansion(nAC, ac, -bez, t32a, nT32a)
  call scaleExpansion(nAC, ac, -bezErr, t32b, nT32b)
  call fastExpansionSum(nT32a, nT32b, t32a, t32b, t64b, nT64b)
  call scaleExpansion(nAB, ab, cez, t32a, nT32a)
  call scaleExpansion(nAB, ab, cezErr, t32b, nT32b)
  call fastExpansionSum(nT32a, nT32b, t32a, t32b, t64c, nT64c)
  call fastExpansionSum(nT64a, nT64b, t64a, t64b, t128, nT128)
  call fastExpansionSum(nT64c, nT128, t64c, t128, t192, nT192)

  call scaleExpansion(nT192, t192, dex, xDet, nX)
  call scaleExpansion(nX, xDet, dex, xxDet, nXX)
  call scaleExpansion(nT192, t192, dexErr, xtDet, nXt)
  call scaleExpansion(nXt, xtDet, dex, xxtDet, nXxt)
  do i = 1, nXxt
    xxtDet(i) = 2.d0 * xxtDet(i)
  enddo
  call scaleExpansion(nXt, xtDet, dexErr, xtxtDet, nXtxt)
  call fastExpansionSum(nXX, nXxt, xxDet, xxtDet, x1, nX1)
  call fastExpansionSum(nX1, nXtxt, x1, xtxtDet, x2, nX2)

  call scaleExpansion(nT192, t192, dey, yDet, nY)
  call scaleExpansion(nY, yDet, dey, yyDet, nYY)
  call scaleExpansion(nT192, t192, deyErr, ytDet, nYt)
  call scaleExpansion(nYt, ytDet, dey, yytDet, nYYt)
  do i = 1, nYyt
    yytDet(i) = 2.d0 * ytDet(i)
  enddo
  call scaleExpansion(nYt, ytDet, deyErr, ytytDet, nYtyt)
  call fastExpansionSum(nYY, nYyt, yyDet, yytDet, y1, nY1)
  call fastExpansionSum(nY1, nYtyt, y1, ytytDet, y2, nY2)

  call scaleExpansion(nT192, t192, dez, zDet, nZ)
  call scaleExpansion(nZ, zDet, dez, zzDet, nZZ)
  call scaleExpansion(nT192, t192, dezErr, ztDet, nZt)
  call scaleExpansion(nZt, ztDet, dez, zztDet, nZZt)
  do i = 1, nZzt
    zztDet(i) = 2.d0 * zztDet(i)
  enddo
  call scaleExpansion(nZt, ztDet, dezErr, ztztDet, nZtzt)
  call fastExpansionSum(nZZ, nZzt, zzDet, zztDet, z1, nZ1)
  call fastExpansionSum(nZ1, nZtzt, z1, ztztDet, z2, nZ2)

  call fastExpansionSum(nX2, nY2, x2, y2, xyDet, nXY)
  call fastExpansionSum(nZ2, nXY, z2, xyDet, dDet, nD)
  call fastExpansionSum(nA, nB, aDet, bDet, abDet, nAB)
  call fastExpansionSum(nC, nD, cDet, dDet, cdDet, nCD)
  call fastExpansionSum(nAB, nCD, abDet, cdDet, d, nD2)

  determinant = d(nD2)

  end function
  !====================================================================!
  !====================================================================!
  subroutine move(N, this, that, M)
    !! copies the contents of this to that.
  !====================================================================!
  integer(i32), intent(in) :: N
  real(r64), intent(in) :: this(:)
  real(r64), intent(out) :: that(:)
  integer(i32), intent(out) :: M

  real(r64) :: tmp
  integer(i32) :: i
  do i = 1, N
    that(i) = this(i)
  enddo
  M = N
  end subroutine
  !====================================================================!
  !====================================================================!
  module procedure orient2D!(ax, ay, bx, by, cx, cy) result(determinant)
  !   !! Determines whether three points a, b, c are in clockwise order.
  !   !! i.e. is the point c to the left or right of line a-b?
  !   !! Returns a positive value if a-b-c are in an anticlockwise order.
  !   !! Returns a negative value if a-b-c are in clockwise order.
  !   !! Returns a zero if the points are colinear.
  !   !!
  !   !! Uses an adaptive floating method by Shewchuk. Only exact computations
  !   !! are carried out when needed.
  ! !====================================================================!
  ! real(r64), intent(in) :: ax
  !   !! x co-ordinate of the first point
  ! real(r64), intent(in) :: ay
  !   !! y co-ordinate of the first point
  ! real(r64), intent(in) :: bx
  !   !! x co-ordinate of the second point
  ! real(r64), intent(in) :: by
  !   !! y co-ordinate of the second point
  ! real(r64), intent(in) :: cx
  !   !! x co-ordinate of the third point
  ! real(r64), intent(in) :: cy
  !   !! y co-ordinate of the third point
  ! real(r64) :: determinant
  !   !! Orientation of the points

  real(r64) :: acx, acxErr, acy, acyErr
  real(r64) :: b(4), bcx, bcxErr, bcy, bcyErr
  real(r64) :: c1(8), c2(12)
  real(r64) :: d(16), detSum
  real(r64) :: errBound
  real(r64) :: lDet, lDetErr
  real(r64) :: rDet, rDetErr

  real(r64) :: s0, s1
  real(r64) :: t0, t1
  real(r64) :: u(4)

  integer(i32) :: nC1, nC2, nD

  acx = ax - cx
  bcx = bx - cx
  acy = ay - cy
  bcy = by - cy

  lDet = acx * bcy
  rDet = acy * bcx
  determinant = lDet - rDet

  if (lDet > 0.0_r64) then
    if (rDet <= 0.0_r64) return
    detSum = lDet + rDet
  elseif (lDet < 0.0_r64) then
    if (rDet >= 0.0_r64) return
    detSum = -lDet - rDet  
  else
    return
  endif

  errBound = o2DBound_A * detSum

  ! First escape at A
  if ((determinant >= errBound) .or. (-determinant >= errBound)) return
   
  call twoProduct(acx, bcy, lDet, lDetErr)
  call twoProduct(acy, bcx, rDet, rDetErr)
  call twoTwoDiff(lDet, lDetErr, rDet, rDetErr, b)
  ! is b(4) volatile?

  determinant = estimate(4, b)
  errBound = o2DBound_B * detSum
  
  ! Second escape at B
  if ((determinant >= errBound) .or. (-determinant >= errBound)) return

  call twoDiffErr(ax, cx, acx, acxErr)
  call twoDiffErr(bx, cx, bcx, bcxErr)
  call twoDiffErr(ay, cy, acy, acyErr)
  call twoDiffErr(by, cy, bcy, bcyErr)

  if ((acxErr == 0.d0) .and. (acyErr == 0.d0) .and. &
      (bcxErr == 0.d0) .and. (bcyErr == 0.d0)) return

  errBound = o2DBound_B * detSum + (resErrBound * abs(determinant))
  determinant = determinant + (acx * bcyErr + bcy * acxErr) &
                            - (acy * bcxErr + bcx * acyErr)

  ! Third escape at C
  if ((determinant >= errBound) .or. (-determinant >= errBound)) return
  
  call twoProduct(acxErr, bcy, s1, s0)
  call twoProduct(acyErr, bcx, t1, t0)
  call twoTwoDiff(s1, s0, t1, t0, u)
  call fastExpansionSum(4, 4, b, u, c1, nC1)

  call twoProduct(acx, bcyErr, s1, s0)
  call twoProduct(acy, bcxErr, t1, t0)
  call twoTwoDiff(s1, s0, t1, t0, u)
  call fastExpansionSum(nC1, 4, c1, u, c2, nC2)

  call twoProduct(acxErr, bcyErr, s1, s0)
  call twoProduct(acyErr, bcxErr, t1, t0)
  call twoTwoDiff(s1, s0, t1, t0, u)
  call fastExpansionSum(nC2, 4, c2, u, d, nD)

  determinant = d(nD)

  end procedure
  !====================================================================!
  !====================================================================!
  module procedure orient3D!(ax, ay, az, bx, by, bz, cx, cy, cz, dx, dy, dz) result(determinant)
  !   !! Determines whether the points a, b, c, c defining a polyhedron are ordered
  !   !! in an anticlockwise manner.
  !   !! Returns a positive value if a-b-c-d are in an anticlockwise order.
  !   !! Returns a negative value if a-b-c-d are in clockwise order.
  !   !! Returns zero if they are coplanar.
  !   !! Clockwise is defined when viewed from above the plane defined by a-b-c.
  !   !!
  !   !! Uses an adaptive floating method by Shewchuk. Only exact computations
  !   !! are carried out when needed.
  ! !====================================================================!
  ! real(r64), intent(in) :: ax
  !   !! x co-ordinate of the first point
  ! real(r64), intent(in) :: ay
  !   !! y co-ordinate of the first point
  ! real(r64), intent(in) :: az
  !   !! z co-ordinate of the first point
  ! real(r64), intent(in) :: bx
  !   !! x co-ordinate of the second point
  ! real(r64), intent(in) :: by
  !   !! y co-ordinate of the second point
  ! real(r64), intent(in) :: bz
  !   !! z co-ordinate of the second point
  ! real(r64), intent(in) :: cx
  !   !! x co-ordinate of the third point
  ! real(r64), intent(in) :: cy
  !   !! y co-ordinate of the third point
  ! real(r64), intent(in) :: cz
  !   !! z co-ordinate of the third point
  ! real(r64), intent(in) :: dx
  !   !! x co-ordinate of the fourth point
  ! real(r64), intent(in) :: dy
  !   !! y co-ordinate of the fourth point
  ! real(r64), intent(in) :: dz
  !   !! z co-ordinate of the fourth point
  ! real(r64) :: determinant
  !   !! Orientation of the points

  real(r64) :: ab(4), abt(8), atB(4), atC(4), adx, ady, adz, adxbdy, adxcdy
  real(r64) :: aDet(8), abDet(8), adxErr, adyErr, adzErr
  real(r64) :: bc(4), bct(8), btA(4), btC(4), bdx, bdy, bdz, bdxady, bdxcdy
  real(r64) :: bDet(8), bdxErr, bdyErr, bdzErr
  real(r64) :: ca(4), cat(8), ctA(4), ctB(4), cdx, cdy, cdz, cdxady, cdxbdy
  real(r64) :: cDet(8), cdxErr, cdyErr, cdzErr
  real(r64) :: errBound
  real(r64) :: fin1(192), fin2(192)
  real(r64) :: keep
  real(r64) :: negate
  real(r64) :: tmp1, tmp2, tmp3, tmp4
  real(r64) :: u(4)
  real(r64) :: v(12)
  real(r64) :: w(16)

  integer(i32) :: nA, nAB, nAbt, nAtb, nAtc
  integer(i32) :: nB, nBct, nBta, nBtc
  integer(i32) :: nC, nCat, nCta, nCtb
  integer(i32) :: nFin1, nFin2, nU, nV, nW

  adx = ax - dx
  bdx = bx - dx
  cdx = cx - dx
  ady = ay - dy
  bdy = by - dy
  cdy = cy - dy
  adz = az - dz
  bdz = bz - dz
  cdz = cz - dz

  bdxcdy = bdx * cdy
  cdxbdy = cdx * bdy

  cdxady = cdx * ady
  adxcdy = adx * cdy

  adxbdy = adx * bdy  
  bdxady = bdx * ady
  
  determinant = (adz * (bdxcdy - cdxbdy)) &
              + (bdz * (cdxady - adxcdy)) &
              + (cdz * (adxbdy - bdxady))

  keep = (abs(bdxcdy) + abs(cdxbdy)) * abs(adz) &
       + (abs(cdxady) + abs(adxcdy)) * abs(bdz) &
       + (abs(adxbdy) + abs(bdxady)) * abs(cdz)

  errBound = o3DBound_A * keep

  !! First escape at A
  if ((determinant > errBound) .or. (-determinant > errBound)) return

  call twoProduct(bdx, cdy, tmp1, tmp2)
  call twoProduct(cdx, bdy, tmp3, tmp4)
  call twoTwoDiff(tmp1, tmp2, tmp3, tmp4, bc)
  call scaleExpansion(4, bc, adz, aDet, nA)

  call twoProduct(cdx, ady, tmp1, tmp2)
  call twoProduct(adx, cdy, tmp3, tmp4)
  call twoTwoDiff(tmp1, tmp2, tmp3, tmp4, ca)
  call scaleExpansion(4, ca, bdz, bDet, nB)

  call twoProduct(adx, bdy, tmp1, tmp2)
  call twoProduct(bdx, ady, tmp3, tmp4)
  call twoTwoDiff(tmp1, tmp2, tmp3, tmp4, ab)
  call scaleExpansion(4, ab, cdz, cDet, nC)

  call fastExpansionSum(nA, nB, aDet, bDet, abDet, nAB)
  call fastExpansionSum(nAB, nC, abDet, cDet, fin2, nFin2)

  determinant = estimate(nFin2, fin2)
  errBound = o3DBound_B * keep

  !! Second escape at B
  if ((determinant > errBound) .or. (-determinant > errBound)) return

  call twoDiffErr(ax, dx, adx, adxErr)
  call twoDiffErr(bx, dx, bdx, bdxErr)
  call twoDiffErr(cx, dx, cdx, cdxErr)
  call twoDiffErr(ay, dy, ady, adyErr)
  call twoDiffErr(by, dy, bdy, bdyErr)
  call twoDiffErr(cy, dy, cdy, cdyErr)
  call twoDiffErr(az, dz, adz, adzErr)
  call twoDiffErr(bz, dz, bdz, bdzErr)
  call twoDiffErr(cz, dz, cdz, cdzErr)

  if ((adxErr == 0.d0) .and. (bdxErr == 0.d0) .and. (cdxErr == 0.d0) .and. &
      (adyErr == 0.d0) .and. (bdyErr == 0.d0) .and. (cdyErr == 0.d0) .and. &
      (adzErr == 0.d0) .and. (cdyErr == 0.d0) .and. (cdzErr == 0.d0)) return

  errBound = (o3DBound_C * keep) + (resErrBound * abs(determinant))

  determinant = determinant &
              + (adz * ((bdx * cdyErr + cdy * bdxErr)  &
                      - (bdy * cdxErr + cdx * bdyErr)) &
              + adzErr * (bdx * cdy - bdy * cdx))      &
              + (bdz * ((cdx * adyErr + ady * cdxErr)  &
                      - (cdy * adxErr + adx * cdyErr)) &
              + bdzErr * (cdx * ady - cdy * adx))      &
              + (cdz * ((adx * bdyErr + bdy * adxErr)  &
                      - (ady * bdxErr + bdx * adyErr)) &
              + cdzErr * (adx * bdy - ady * bdx))
        
  if ((determinant > errBound) .or. (-determinant > errBound)) return

  ! Final leg, exact computation
  if (adxErr == 0.d0 ) then
    if (adyErr == 0.d0) then
      atB(1) = 0.d0
      nAtb = 1
      atC(1) = 0.d0
      nAtc = 1
    else
      negate = -adyErr
      call twoProduct(negate, bdx, atB(2), atB(1))
      nAtb = 2
      call twoProduct(adyErr, cdx, atC(2), atC(1))
      nAtc = 2
    endif
  else
    if (adyErr == 0.d0) then
      call twoProduct(adxErr, bdy, atB(2), atB(1))
      nAtb = 2
      negate = -adxErr
      call twoProduct(negate, cdy, atC(2), atC(1))
      nAtc = 2
    else
      call twoProduct(adxErr, bdy, tmp1, tmp2)
      call twoProduct(adyErr, bdx, tmp3, tmp4)
      call twoTwoDiff(tmp1, tmp2, tmp3, tmp4, atB)
      nAtb = 4
      call twoProduct(adyErr, cdx, tmp1, tmp2)
      call twoProduct(adxErr, cdy, tmp3, tmp4)
      call twoTwoDiff(tmp1, tmp2, tmp3, tmp4, atC)
      nAtc = 4
    endif
  endif

  if (bdxErr == 0.d0 ) then
    if (bdyErr == 0.d0) then
      btC(1) = 0.d0
      nBtc = 1
      btA(1) = 0.d0
      nBta = 1
    else
      negate = -bdyErr
      call twoProduct(negate, cdx, btC(2), btC(1))
      nBtc = 2
      call twoProduct(bdyErr, adx, btA(2), btA(1))
      nBta = 2
    endif
  else
    if (bdyErr == 0.d0) then
      call twoProduct(bdxErr, cdy, btC(2), btC(1))
      nBtc = 2
      negate = -bdxErr
      call twoProduct(negate, ady, btA(2), btA(1))
      nBta = 2
    else
      call twoProduct(bdxErr, cdy, tmp1, tmp2)
      call twoProduct(bdyErr, cdx, tmp3, tmp4)
      call twoTwoDiff(tmp1, tmp2, tmp3, tmp4, btC)
      nBtc = 4
      call twoProduct(bdyErr, adx, tmp1, tmp2)
      call twoProduct(bdxErr, ady, tmp3, tmp4)
      call twoTwoDiff(tmp1, tmp2, tmp3, tmp4, btA)
      nBta = 4
    endif
  endif

  if (cdxErr == 0.d0 ) then
    if (cdyErr == 0.d0) then
      ctA(1) = 0.d0
      nCta = 1
      ctB(1) = 0.d0
      nCta = 1
    else
      negate = -cdyErr
      call twoProduct(negate, adx, ctA(2), ctA(1))
      nCta = 2
      call twoProduct(cdyErr, bdx, ctB(2), ctB(1))
      nCtb = 2
    endif
  else
    if (cdyErr == 0.d0) then
      call twoProduct(cdxErr, ady, ctA(2), ctA(1))
      nCta = 2
      negate = -cdxErr
      call twoProduct(negate, bdy, ctB(2), ctB(1))
      nCtb = 2
    else
      call twoProduct(cdxErr, ady, tmp1, tmp2)
      call twoProduct(cdyErr, adx, tmp3, tmp4)
      call twoTwoDiff(tmp1, tmp2, tmp3, tmp4, ctA)
      nCta = 4
      call twoProduct(cdyErr, bdx, tmp1, tmp2)
      call twoProduct(cdxErr, bdy, tmp3, tmp4)
      call twoTwoDiff(tmp1, tmp2, tmp3, tmp4, ctB)
      nCtb = 4
    endif
  endif

  call fastExpansionSum(nBtc, nCtb, btC, ctB, bct, nBct)
  call scaleExpansion(nBct, bct, adz, w, nW)
  call fastExpansionSum(nFin2, nW, fin2, w, fin1, nFin1)
  
  call fastExpansionSum(nCta, nAtc, ctA, atC, cat, nCat)
  call scaleExpansion(nCat, cat, bdz, w, nW)
  call fastExpansionSum(nFin1, nW, fin1, w, fin2, nFin2)

  call fastExpansionSum(nAtb, nBta, atB, btA, abt, nAbt)
  call scaleExpansion(nAbt, abt, cdz, w, nW)
  call fastExpansionSum(nFin2, nW, fin2, w, fin1, nFin1)

  if (adzErr /= 0.d0) then
    call scaleExpansion(4, bc, adzErr, v, nV)
    call fastExpansionSum(nFin1, nV, fin1, v, fin2, nFin2)
    call move(nFin2, fin2, fin1, nFin1)
  endif
  if (bdzErr /= 0.d0) then
    call scaleExpansion(4, ca, bdzErr, v, nV)
    call fastExpansionSum(nFin1, nV, fin1, v, fin2, nFin2)
    call move(nFin2, fin2, fin1, nFin1)
  endif
  if (cdzErr /= 0.d0) then
    call scaleExpansion(4, ab, cdzErr, v, nV)
    call fastExpansionSum(nFin1, nV, fin1, v, fin2, nFin2)
    call move(nFin2, fin2, fin1, nFin1)
  endif

  if (adxErr /= 0.d0) then
    if (bdyErr /= 0.d0) then
      call twoProduct(adxErr, bdyErr, tmp1, tmp2)
      call twoOneProduct(tmp1, tmp2, cdz, u)
      call fastExpansionSum(nFin1, 4, fin1, u, fin2, nFin2)
      call move(nFin2, fin2, fin1, nFin1)
      if (cdzErr /= 0.d0) then
        call twoOneProduct(tmp1, tmp2, cdzErr, u)
        call fastExpansionSum(nFin1, 4, fin1, u, fin2, nFin2)
        call move(nFin2, fin2, fin1, nFin1)
      endif
    endif
    if (cdyErr /= 0.d0) then
      negate = -adxErr
      call twoProduct(negate, cdyErr, tmp1, tmp2)
      call twoOneProduct(tmp1, tmp2, bdz, u)
      call fastExpansionSum(nFin1, 4, fin1, u, fin2, nFin2)
      call move(nFin2, fin2, fin1, nFin1)
      if (bdzErr /= 0.d0) then
        call twoOneProduct(tmp1, tmp2, bdzErr, u)
        call fastExpansionSum(nFin1, 4, fin1, u, fin2, nFin2)
        call move(nFin2, fin2, fin1, nFin1)
      endif
    endif
  endif

  if (bdxErr /= 0.d0) then
    if (cdyErr /= 0.d0) then
      call twoProduct(bdxErr, cdyErr, tmp1, tmp2)
      call twoOneProduct(tmp1, tmp2, adz, u)
      call fastExpansionSum(nFin1, 4, fin1, u, fin2, nFin2)
      call move(nFin2, fin2, fin1, nFin1)
      if (adzErr /= 0.d0) then
        call twoOneProduct(tmp1, tmp2, adzErr, u)
        call fastExpansionSum(nFin1, 4, fin1, u, fin2, nFin2)
        call move(nFin2, fin2, fin1, nFin1)
      endif
    endif
    if (adyErr /= 0.d0) then
      negate = -bdxErr
      call twoProduct(negate, adyErr, tmp1, tmp2)
      call twoOneProduct(tmp1, tmp2, cdz, u)
      call fastExpansionSum(nFin1, 4, fin1, u, fin2, nFin2)
      call move(nFin2, fin2, fin1, nFin1)
      if (cdzErr /= 0.d0) then
        call twoOneProduct(tmp1, tmp2, cdzErr, u)
        call fastExpansionSum(nFin1, 4, fin1, u, fin2, nFin2)
        call move(nFin2, fin2, fin1, nFin1)
      endif
    endif
  endif

  if (cdxErr /= 0.d0) then
    if (adyErr /= 0.d0) then
      call twoProduct(cdxErr, adyErr, tmp1, tmp2)
      call twoOneProduct(tmp1, tmp2, bdz, u)
      call fastExpansionSum(nFin1, 4, fin1, u, fin2, nFin2)
      call move(nFin2, fin2, fin1, nFin1)
      if (bdzErr /= 0.d0) then
        call twoOneProduct(tmp1, tmp2, bdzErr, u)
        call fastExpansionSum(nFin1, 4, fin1, u, fin2, nFin2)
        call move(nFin2, fin2, fin1, nFin1)
      endif
    endif
    if (bdyErr /= 0.d0) then
      negate = -cdxErr
      call twoProduct(negate, bdyErr, tmp1, tmp2)
      call twoOneProduct(tmp1, tmp2, adz, u)
      call fastExpansionSum(nFin1, 4, fin1, u, fin2, nFin2)
      call move(nFin2, fin2, fin1, nFin1)
      if (adzErr /= 0.d0) then
        call twoOneProduct(tmp1, tmp2, adzErr, u)
        call fastExpansionSum(nFin1, 4, fin1, u, fin2, nFin2)
        call move(nFin2, fin2, fin1, nFin1)
      endif
    endif
  endif

  if (adzErr /= 0.d0) then
    call scaleExpansion(nBct, bct, adzErr, w, nW)
    call fastExpansionSum(nFin1, nW, fin1, w, fin2, nFin2)
    call move(nFin2, fin2, fin1, nFin1)  
  endif
  if (bdzErr /= 0.d0) then
    call scaleExpansion(nCat, cat, bdzErr, w, nW)
    call fastExpansionSum(nFin1, nW, fin1, w, fin2, nFin2)
    call move(nFin2, fin2, fin1, nFin1)  
  endif
  if (cdzErr /= 0.d0) then
    call scaleExpansion(nAbt, abt, cdzErr, w, nW)
    call fastExpansionSum(nFin1, nW, fin1, w, fin2, nFin2)
    call move(nFin2, fin2, fin1, nFin1)  
  endif

  determinant = fin1(nFin1)  

  end procedure
  !====================================================================!
  !====================================================================!
  subroutine scaleExpansion(nE, e, b, h, hIndex)
    !! Multiples an array e by a number b
    !!
  !====================================================================!
  integer(i32), intent(in) :: nE
  real(r64), intent(in) :: e(:)
  real(r64), intent(in) :: b
  real(r64), intent(inout) :: h(:)
  integer(i32), intent(inout) :: hIndex

  real(r64) :: eNow
  real(r64) :: bhi, blo
  real(r64) :: q, qSum, hh
  real(r64) :: T, tErr
  integer(i32) :: i

  ! split b for use in the loop
  call split(b, bhi, blo)

  call twoProductBsplit(e(1), b, bhi, blo, q, hh)

  hIndex = 1
  if (hh /= 0.d0) then
    h(hIndex) = hh
    hIndex = hIndex + 1
  endif

  do i = 2, nE
    eNow = e(i)
    call twoProductBsplit(eNow, b, bhi, blo, T, tErr)

    call twoSum(q, tErr, qSum, hh)
    if (hh /= 0.d0) then
      h(hIndex) = hh
      hIndex = hIndex + 1
    endif

    call fastTwoSum(T, qSum, q, hh)
    if (hh /= 0.d0) then
      h(hIndex) = hh
      hIndex = hIndex + 1
    endif
  enddo

  if (q /= 0.d0 .or. hIndex == 1) then
    h(hIndex) = q
    hIndex = hIndex + 1
  endif

  hIndex = hIndex - 1

  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine split(a, ahi, alo)
    !! Produces a (p-s) bit value ahi and a nonoverlapping (s-1) bit value alo
    !! such that |ahi| >= |alo| and a = ahi + alo
  !====================================================================!
  real(r64), intent(in) :: a
  real(r64), intent(out) :: ahi
  real(r64), intent(out) :: alo

  ahi = splitter * a
  alo = ahi - a
  ahi = ahi - alo
  alo = a - ahi

  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine square(a, x, y)
    !! Compute the square of a and compute the numerical round-off error.
  !====================================================================!
  real(r64), intent(in) :: a
  real(r64), intent(out) :: x
  real(r64), intent(out) :: y

  x = a * a
  call squareErr(a, x, y)

  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine squareErr(a, x, y)
  !====================================================================!
  real(r64), intent(in) :: a
  real(r64), intent(inout) :: x
  real(r64), intent(out) :: y

  real(r64) :: ahi, alo
  real(r64) :: e1, e2, e3

  call split(a, ahi, alo)
  e1 = ahi * ahi
  e2 = x - e2
  e3 = e2 - (e2 * alo)
  y = (alo * alo) - e3

  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine twoDiff(a, b, x, y)
    !! Compute the difference between two numbers and compute the numerical round-off error.
    !! If you know that the magnitude of a is greater than or equal to b, use fastTwoDiff
  !====================================================================!
    real(r64), intent(in) :: a 
      !! First number
    real(r64), intent(in) :: b 
      !! Second number
    real(r64), intent(out) :: x 
      !! Result
    real(r64), intent(out) :: y 
      !! Error
    x = a - b
    call twoDiffErr(a, b, x, y)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine twoDiffErr(a, b, x, y)
  !====================================================================!
  real(r64), intent(in) :: a 
    !! First number
  real(r64), intent(in) :: b 
    !! Second number
  real(r64), intent(inout) :: x 
    !! Result
  real(r64), intent(out) :: y 
    !! Error
  real(r64) :: aVirtual, bVirtual
  real(r64) :: aRoundoff, bRoundoff
  bVirtual = a - x
  aVirtual = x + bVirtual
  bRoundoff = bVirtual - b
  aRoundoff = a - aVirtual
  y = aRoundoff + bRoundoff
  end subroutine 
  !====================================================================!
  !====================================================================!
  subroutine twoOneDiff(a1, a0, b, x2, x1, x0)
  !====================================================================!
  real(r64), intent(in) :: a1
  real(r64), intent(in) :: a0
  real(r64), intent(in) :: b
  real(r64), intent(out) :: x2
  real(r64), intent(out) :: x1
  real(r64), intent(out) :: x0

  real(r64) :: tmp ! volatile?

  call twoDiff(a0, b, tmp, x0)
  call twoSum(a1, tmp, x2, x1)

  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine twoTwoDiff(a1, a0, b1, b0, x)
  !====================================================================!
  real(r64), intent(in) :: a1
  real(r64), intent(in) :: a0
  real(r64), intent(in) :: b1
  real(r64), intent(in) :: b0
  real(r64), intent(out) :: x(4)

  real(r64) :: tmp1, tmp2 !volatile?

  call twoOneDiff(a1, a0, b0, tmp1, tmp2, x(1))
  call twoOneDiff(tmp1, tmp2, b1, x(4), x(3), x(2))

  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine twoOneProduct(a1, a0, b, x)
  !====================================================================!
  real(r64), intent(in) :: a1
  real(r64), intent(in) :: a0
  real(r64), intent(in) :: b
  real(r64), intent(out) :: x(4)

  real(r64) :: bhi, blo
  real(r64) :: tmp1, tmp2, tmp3, tmp4

  call split(b, bhi, blo)
  call twoProductBsplit(a0, b, bhi, blo, tmp1, x(1))
  call twoProductBsplit(a1, b, bhi, blo, tmp2, tmp3)
  call twoSum(tmp1, tmp3, tmp4, x(2))
  call fastTwoSum(tmp2, tmp4, x(4), x(3))

  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine twoProduct(a, b, x, y)
  !====================================================================!
  real(r64) :: a
  real(r64) :: b
  real(r64) :: x
  real(r64) :: y

  real(r64) :: ahi, alo
  real(r64) :: bhi, blo

  x = a * b

  call split(a, ahi, alo)
  call split(b, bhi, blo)

  y = x - (ahi * bhi)
  y = y - (alo * bhi)
  y = y - (ahi * blo)
  y = (alo * blo) - y

  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine twoProductBsplit(a, b, bhi, blo, x, y)
    !! Does the two product with b already split to save time.
  !====================================================================!
  real(r64), intent(in) :: a
  real(r64), intent(in) :: b
  real(r64), intent(in) :: bhi
  real(r64), intent(in) :: blo
  real(r64), intent(out) :: x
  real(r64), intent(out) :: y

  real(r64) :: ahi, alo
  real(r64) :: e1, e2, e3

  x = a * b

  call split(a, ahi, alo)

  e1 = x - (ahi * bhi)
  e2 = e1 - (alo * bhi)
  e3 = e2 - (ahi * blo)
  y = (alo * blo) - e3

  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine twoProductABsplit(a, ahi, alo, b, bhi, blo, x, y)
    !! Two Product where both a and b are already split. Saves time.
  !====================================================================!
  real(r64), intent(in) :: a
  real(r64), intent(in) :: ahi
  real(r64), intent(in) :: alo
  real(r64), intent(in) :: b
  real(r64), intent(in) :: bhi
  real(r64), intent(in) :: blo
  real(r64), intent(out) :: x
  real(r64), intent(out) :: y

  real(r64) :: e1, e2, e3

  x = a * b
  e1 = x - (ahi * bhi)
  e2 = e1 - (alo * bhi)
  e3 = e2 - (ahi * blo)
  y = (alo * blo) - e3
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine twoTwoProduct(a1, a0, b1, b0, x)
  !====================================================================!
  real(r64), intent(in) :: a1
  real(r64), intent(in) :: a0
  real(r64), intent(in) :: b1
  real(r64), intent(in) :: b0
  real(r64), intent(out) :: x(8)

  real(r64) :: a0hi, a0lo, a1hi, a1lo
  real(r64) :: bhi, blo
  real(r64) :: tmp0, tmp1, tmp2, tmpI, tmpJ, tmpK, tmpL, tmpM, tmpN, tmpO
  
  call split(a0, a0hi, a0lo)
  call split(b0, bhi, blo)
  call twoProductABsplit(a0, a0hi, a0lo, b0, bhi, blo, tmpI, x(1))
  call split(a1, a1hi, a1lo)
  call twoProductABsplit(a1, a1hi, a1lo, b0, bhi, blo, tmpJ, tmp0)
  call twoSum(tmpI, tmp0, tmpK, tmp1)
  call fastTwoSum(tmpJ, tmpK, tmpL, tmp2)
  call split(b1, bhi, blo)
  call twoProductABsplit(a0, a0hi, a0lo, b1, bhi, blo, tmpI, tmp0)
  call twoSum(tmp1, tmp0, tmpK, x(2))
  call twoSum(tmp2, tmpK, tmpJ, tmp1)
  call twoSum(tmpL, tmpJ, tmpM, tmp2)
  call twoProductABsplit(a1, a1hi, a1lo, b1, bhi, blo, tmpJ, tmpO)
  call twoSum(tmpI, tmpO, tmpN, tmp0)
  call twoSum(tmp1, tmp0, tmpI, x(3))
  call twoSum(tmp2, tmpI, tmpK, tmp1)
  call twoSum(tmpM, tmpK, tmpL, tmp2)
  call twoSum(tmpJ, tmpN, tmpK, tmp0)
  call twoSum(tmp1, tmp0, tmpJ, x(4))
  call twoSum(tmp2, tmpJ, tmpI, tmp1)
  call twoSum(tmpL, tmpI, tmpM, tmp2)
  call twoSum(tmp1, tmpK, tmpI, x(5))
  call twoSum(tmp2, tmpI, tmpK, x(6))
  call twoSum(tmpM, tmpK, x(8), x(7))

  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine twoSum(a, b, x, y)
    !! Compute the sum of two numbers and compute the numerical round-off error.
    !! If you know that the magnitude of a is greater than or equal to b, use fastTwoSum
  !====================================================================!
    real(r64), intent(in) :: a 
      !! First number
    real(r64), intent(in) :: b 
      !! Second number
    real(r64), intent(out) :: x 
      !! Result
    real(r64), intent(out) :: y 
      !! Error
    real(r64) :: aVirtual, bVirtual
    real(r64) :: aRoundoff, bRoundoff
    x = a + b
    bVirtual = x - a
    aVirtual = x - bVirtual
    bRoundoff = b - bVirtual
    aRoundoff = a - aVirtual
    y = aRoundoff + bRoundoff
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine twoOneSum(a1, a0, b, x2, x1, x0)
  !====================================================================!
  real(r64), intent(in) :: a1
  real(r64), intent(in) :: a0
  real(r64), intent(in) :: b
  real(r64), intent(out) :: x2
  real(r64), intent(out) :: x1
  real(r64), intent(out) :: x0

  real(r64) :: tmp

  call twoSum(a0, b, tmp, x0)
  call twoSum(a1, tmp, x2, x1)

  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine twoTwoSum(a1, a0, b1, b0, x)
  !====================================================================!
  real(r64), intent(in) :: a1
  real(r64), intent(in) :: a0
  real(r64), intent(in) :: b1
  real(r64), intent(in) :: b0
  real(r64), intent(out) :: x(4)

  real(r64) :: tmp1, tmp2

  call twoOneSum(a1, a0, b0, tmp1, tmp2, x(1))
  call twoOneSum(tmp1, tmp2, b1, x(4), x(3), x(2))

  end subroutine
  !====================================================================!



  module procedure geometryTest

  real(r64) :: a, b, x, y, z, q
  real(r64) :: x0, y0
  real(r64) :: a1D(5), b1D(5), c1D(10)
  integer(i32) :: i, ii, j, jj, k, istat, iunit, m
  integer(i32) :: iU1, iU2
  real(r64) :: ax, ay, az, bx, by, bz, cx, cy, cz, dx, dy, dz
  real(r64) :: px, py, pz
  logical :: goX, goY
  real(r64) :: x1(512), y1(512)

  call init_Geometry()

  ! a = 4.56d0
  ! b = 2.453d0

  ! call fastTwoSum(a, b, x, y)
  ! call test%test(abs(y) < 1.d-15,'fastTwoSum')

  ! call twoSum(a, b, x, y)
  ! call test%test(abs(y) < 1.d-15,'twoSum')

  ! a1D = [1.d0, 2.d0, 3.d0, 5.d0, 6.d0]
  ! b1D = [1.5d0, 3.6d0, 4.d0, 4.56d0, 7.d0]
  ! write(*,*) 'a1D: ',a1D
  ! write(*,*) 'b1D: ',b1D

  ! call fastExpansionSum(5, 5, a1D, b1D, c1D, i)
  ! write(*,*) 'fastExpansionSum'
  ! write(*,*) 'c1D: ',c1D(1:i)

  ! call split(4.56d0, a, b)
  ! write(*,*) 'split: ',4.56d0, a, b

  ! call scaleExpansion(5, a1D, 4.56d0, c1D, i)
  ! write(*,*) 'scaleExpansion'
  ! write(*,*) 'c1D: ',c1D(1:i)

  ! write(*,*) '2D anticlockwise'
  ! write(*,*) orient2D(0.d0, 0.d0, 1.d0, 0.d0, 1.d0, 1.d0)
  ! write(*,*) orient2D(0.d0, 0.d0, 1.d0, 0.d0, 0.3d0, 1d-6)
  ! write(*,*) orient2D(0.d0, 0.d0, 1.d0, 0.d0, 0.3d0, 1d-16)
  ! write(*,*) orient2D(0.d0, 0.d0, 1.d0, 0.d0, 0.3d0, 1d-26)

  ! write(*,*) '2D clockwise'
  ! write(*,*) orient2D(0.d0, 0.d0, 0.d0, 1.d0, 1.d0, 1.d0)
  ! write(*,*) orient2D(0.d0, 0.d0, 1.d0, 0.d0, 0.3d0, -1d-6)
  ! write(*,*) orient2D(0.d0, 0.d0, 1.d0, 0.d0, 0.3d0, -1d-16)
  ! write(*,*) orient2D(0.d0, 0.d0, 1.d0, 0.d0, 0.3d0, -1d-26)

  ! write(*,*) 'Colinear'
  ! write(*,*) orient2D(0.d0, 0.d0, 1.d0, 0.d0, 0.3d0, 0.d0)
  ! write(*,*) orient2D(1.d-16, 1.d-16, 1.d0+1.d-16, 1.d-16, 0.3d0+1d-16, 1.d-16)


  ! ax = orient3D(1.d0, 0.d0 , 0.d0, 0.d0, 1.d0, 0.d0, 0.d0, 0.d0, 1.d0, 0.0d0, 0.0d0, 0.d0)
  ! write(*,*) 'O3D: ', ax

  !! Set up some perturbations
  
  q = 2.d0**(-53)
  px = 0.5d0

  ii = 1 
  i = 1 
  x0 = 1000.d0
  goX = .true.
  do while (goX)
    x = px + dble(i - 1) * q
    do while (x == x0)
      i = i + 1
      x = px + dble(i - 1) * q
    enddo
    x0 = x
    x1(ii) = x
    ii = ii + 1
    goX = ii <= 512
  enddo

  y1 = x1

  

  write(*,*) 'Orient2D'
  !!! Orient2D Test
  call openFile('orient2D.txt', iUnit, 'unknown', istat)
  
  px = 0.5d0
  py = 0.5d0 
  bx = 12.d0
  by = 12.d0
  cx = 24.d0
  cy = 24.d0

  do j = 1, 512
    y = x1(j)
    do i = 1, 512
      x = x1(i)
      a = orient2D(x, y, bx, by, cx, cy)
      if (a /= 0.d0) a = sign(1.d0, a)
      write(iunit, *) x, y, a
    enddo
  enddo
  call closeFile('orient2D.txt', iunit, '', istat)
  
  ! call openFile('coplanar_test.txt', iUnit, 'unknown', istat)
  
  ! px = 0.5d0 py = 9.d0 pz = 0.5d0
  ! ax = 12.d0 ay = 6.d0 az = 12.d0
  ! bx = 12.d0 by = 12.d0 bz = 12.d0
  ! cx = 24.d0 cy = 9.d0 cz = 24.d0

  ! q = 2.d0**(-53)
  ! do i = 1, 256
  !   x = px + dble(i - 1) * q
  !   do j = 1, 256
  !     y = py + dble(j - 1) * q
  !     do k = 1, 256     
  !       z = pz + dble(k - 1) * q
  !       !write(*,*) 'i,j,k', i,j,k
  !       !write(*,'(12(e23.15e3, 2x))') ax, ay, az, bx, by, bz, cx, cy, cz, x, y, z
  !       a = orient3D(ax, ay, az, bx, by, bz, cx, cy, cz, x, y, z)

  !       write(iunit, *) i, j, k, a
  !     enddo
  !   enddo
  ! enddo
  ! call closeFile('coplanar_test.txt', iunit, '', istat)

  write(*,*) 'Incircle'
  !!! Incircle Test
  call openFile('incircle.txt', iUnit, 'unknown', istat)
  
  px = 0.5d0
  py = 0.5d0 
  ax = 0.5d0
  ay = -11.d0
  bx = 12.d0 
  by = -11.d0
  cx = 12.d0 
  cy = 0.5d0

  q = eps
  do j = 1, 512
    y = x1(j)
    do i = 1, 512
      x = x1(i)
      a = inCircle(ax, ay, bx, by, cx, cy, x, y)
      if (a /= 0.d0) a = sign(1.d0, a)
      write(iunit, *) x, y, a
    enddo
  enddo
  call closeFile('incircle.txt', iunit, '', istat)

  
  write(*,*) 'InSphere'

  call openFile('inSphere.txt', iUnit, 'unknown', istat)
  
  ax = 0.5d0
  ay = 12.d0
  az = -11.d0
  bx = 12.d0 
  by = 12.d0
  bz = -11.d0
  cx = 12.d0 
  cy = 0.5d0
  cz = -11.d0
  dx = 12.d0
  dy = 12.d0
  dz = 0.5d0
  
  !do k = 1, 512
  
    !z = x1(k)
    z = 0.5d0
    do j = 1, 512
      y = y1(j)
      do i = 1,512
        x = x1(i)  
        ! write(*,*) i,j,k
        ! write(*,*) x, y, z
        a = inSphere(ax, ay, az, bx, by, bz, cx, cy, cz, dx, dy, dz, x, y, z)
        ! if (abs(a) > 10000) stop
        !if (a /= 0.d0) a = sign(1.d0, a)
        write(iUnit, *) x, y, z, a
        
      enddo
    enddo
  !enddo
  call closeFile('inSphere.txt', iunit, '', istat)
    

  end procedure


  
end submodule
