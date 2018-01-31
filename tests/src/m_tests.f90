module m_tests

use variableKind
use m_allocate, only: allocate
use m_array1D
use m_copy, only: copy
use m_deallocate, only: deallocate
use m_dDynamicArray, only: dDynamicArray
use m_iDynamicArray, only: iDynamicArray
use m_idDynamicArray, only: idDynamicArray
use m_rDynamicArray, only: rDynamicArray
use m_dArgDynamicArray, only: dArgDynamicArray
use m_iArgDynamicArray, only: iArgDynamicArray
use m_idArgDynamicArray, only: idArgDynamicArray
use m_rArgDynamicArray, only: rArgDynamicArray
use m_errors
use m_indexing, only: ind2sub, sub2ind
use m_KdTree, only: KdTree, KdTreeSearch
use m_maths
use m_random, only: rngInteger, rngNormal
use m_reallocate, only: reallocate
use m_select, only: argSelect, select
use m_sort, only: argSort, sort
use Prng_Class, only: Prng, getRandomSeed
use m_strings
use m_time
use m_unitTester, only: tester

use omp_lib

implicit none


contains

  !====================================================================!
  subroutine dDynamicArray_test(test)
    !! graph: false
  !====================================================================!
  class(tester) :: test

  type(dDynamicArray) :: da, da2

  integer(i32) :: ia

  da = dDynamicArray(10)
  call test%test(size(da%values)==10, 'dDynamicArray')
  call test%test(da%N==0, 'dDynamicArray')
  call da%insertAt(1, 10.d0)
  call test%test(da%values(1) == 10, 'dDynamicArray%insert')
  call da%insertAt(1, 20.d0)
  call test%test(all(da%values(1:2) == [20.d0, 10.d0]), 'dDynamicArray%insert')
  call da%prepend(30.d0)
  call test%test(all(da%values(1:3) == [30.d0, 20.d0, 10.d0]), 'dDynamicArray%prepend')
  call da%append(40.d0)
  call test%test(all(da%values(1:4) == [30.d0, 20.d0, 10.d0, 40.d0]), 'dDynamicArray%append')
  call da%remove(2)
  call test%test(all(da%values(1:3) == [30.d0, 10.d0, 40.d0]), 'dDynamicArray%remove')
  call da%tighten()
  call test%test(size(da%values) == 3, 'dDynamicArray%tighten')
  da2 = da
  call test%test(all(da2%values == da%values), 'dDynamicArray%copy')
  da2%values(2) = 50.d0
  call test%test(da2%values(2) /= da%values(2), 'dDynamicArray%copy')
  call da%deallocate()
  call test%test(.not. allocated(da%values), 'dDynamicArray%deallocate')
  call da2%deallocate()

  da = dDynamicArray(3, sorted=.true.)
  call da%insertSorted(20.d0)
  call da%insertSorted(30.d0)
  call da%insertSorted(10.d0)
  call test%test(all(da%values==[10.d0, 20.d0, 30.d0]), 'dDynamicArray%insertSorted')
  ia = da%locationOf(20.d0)
  call test%test(ia == 2, 'dDynamicArray%locationOf')
  call da%insertSortedUnique(10.d0)
  call test%test(all(da%values==[10.d0, 20.d0, 30.d0]), 'dDynamicArray%insertSortedUnique')
  call da%insertSortedUnique(15.d0)
  call test%test(all(da%values(1:da%N)==[10.d0, 15.d0, 20.d0, 30.d0]), 'dDynamicArray%insertSortedUnique')
  call test%test(size(da%values) == 6, 'dDynamicArray%insert')
  call da%deallocate()

  da = dDynamicArray(3, sorted=.true., fixed=.true.)
  call da%insertSorted(20.d0)
  call da%insertSorted(30.d0)
  call da%insertSorted(10.d0)
  call test%test(all(da%values(1:da%N)==[10.d0, 20.d0, 30.d0]), 'dDynamicArray%insertSorted')
  ia = da%locationOf(20.d0)
  call test%test(ia == 2, 'dDynamicArray%locationOf')
  call da%insertSortedUnique(10.d0)
  call test%test(all(da%values(1:da%N)==[10.d0, 20.d0, 30.d0]), 'dDynamicArray%insertSortedUnique')
  call da%insertSortedUnique(15.d0)
  call test%test(all(da%values(1:da%N)==[10.d0, 15.d0, 20.d0]), 'dDynamicArray%insertSortedUnique')
  call test%test(size(da%values) == 3, 'dDynamicArray%insert')
  call da%deallocate()
  end subroutine
  !====================================================================!

  !====================================================================!
  subroutine iDynamicArray_test(test)
    !! graph: false
  !====================================================================!
  class(tester) :: test

  type(iDynamicArray) :: ida, ida2

  integer(i32) :: ia

  ida = iDynamicArray(10)
  call test%test(size(ida%values)==10, 'iDynamicArray')
  call test%test(ida%N==0, 'iDynamicArray')
  call ida%insertAt(1, 10)
  call test%test(ida%values(1) == 10, 'iDynamicArray%insert')
  call ida%insertAt(1, 20)
  call test%test(all(ida%values(1:2) == [20, 10]), 'iDynamicArray%insert')
  call ida%prepend(30)
  call test%test(all(ida%values(1:3) == [30, 20, 10]), 'iDynamicArray%prepend')
  call ida%append(40)
  call test%test(all(ida%values(1:4) == [30, 20, 10, 40]), 'iDynamicArray%append')
  call ida%remove(2)
  call test%test(all(ida%values(1:3) == [30, 10, 40]), 'iDynamicArray%remove')
  call ida%tighten()
  call test%test(size(ida%values) == 3, 'iDynamicArray%tighten')
  ida2 = ida
  call test%test(all(ida2%values == ida%values), 'iDynamicArray%copy')
  ida2%values(2) = 50
  call test%test(ida2%values(2) /= ida%values(2), 'iDynamicArray%copy')
  call ida%deallocate()
  call test%test(.not. allocated(ida%values), 'iDynamicArray%deallocate')
  call ida2%deallocate()

  ida = iDynamicArray(3, sorted=.true.)
  call ida%insertSorted(20)
  call ida%insertSorted(30)
  call ida%insertSorted(10)
  call test%test(all(ida%values==[10, 20, 30]), 'iDynamicArray%insertSorted')
  ia = ida%locationOf(20)
  call test%test(ia == 2, 'iDynamicArray%locationOf')
  call ida%insertSortedUnique(10)
  call test%test(all(ida%values==[10, 20, 30]), 'iDynamicArray%insertSortedUnique')
  call ida%insertSortedUnique(15)
  call test%test(all(ida%values(1:ida%N)==[10, 15, 20, 30]), 'iDynamicArray%insertSortedUnique')
  call test%test(size(ida%values) == 6, 'iDynamicArray%insert')
  call ida%deallocate()

  ida = iDynamicArray(3, sorted=.true., fixed=.true.)
  call ida%insertSorted(20)
  call ida%insertSorted(30)
  call ida%insertSorted(10)
  call test%test(all(ida%values(1:ida%N)==[10, 20, 30]), 'iDynamicArray%insertSorted')
  ia = ida%locationOf(20)
  call test%test(ia == 2, 'iDynamicArray%locationOf')
  call ida%insertSortedUnique(10)
  call test%test(all(ida%values(1:ida%N)==[10, 20, 30]), 'iDynamicArray%insertSortedUnique')
  call ida%insertSortedUnique(15)
  call test%test(all(ida%values(1:ida%N)==[10, 15, 20]), 'iDynamicArray%insertSortedUnique')
  call test%test(size(ida%values) == 3, 'iDynamicArray%insert')
  call ida%deallocate()
  end subroutine
  !====================================================================!

  !====================================================================!
  subroutine idDynamicArray_test(test)
    !! graph: false
  !====================================================================!
  class(tester) :: test

  type(idDynamicArray) :: idda, idda2

  integer(i32) :: ia
  idda = idDynamicArray(10)
  call test%test(size(idda%values)==10, 'idDynamicArray')
  call test%test(idda%N==0, 'idDynamicArray')
  call idda%insertAt(1, 10_i64)
  call test%test(idda%values(1) == 10, 'idDynamicArray%insert')
  call idda%insertAt(1, 20_i64)
  call test%test(all(idda%values(1:2) == [20_i64, 10_i64]), 'idDynamicArray%insert')
  call idda%prepend(30_i64)
  call test%test(all(idda%values(1:3) == [30_i64, 20_i64, 10_i64]), 'idDynamicArray%prepend')
  call idda%append(40_i64)
  call test%test(all(idda%values(1:4) == [30_i64, 20_i64, 10_i64, 40_i64]), 'idDynamicArray%append')
  call idda%remove(2)
  call test%test(all(idda%values(1:3) == [30_i64, 10_i64, 40_i64]), 'idDynamicArray%remove')
  call idda%tighten()
  call test%test(size(idda%values) == 3, 'idDynamicArray%tighten')
  idda2 = idda
  call test%test(all(idda2%values == idda%values), 'idDynamicArray%copy')
  idda2%values(2) = 50_i64
  call test%test(idda2%values(2) /= idda%values(2), 'idDynamicArray%copy')
  call idda%deallocate()
  call test%test(.not. allocated(idda%values), 'idDynamicArray%deallocate')
  call idda2%deallocate()

  idda = idDynamicArray(3, sorted=.true.)
  call idda%insertSorted(20_i64)
  call idda%insertSorted(30_i64)
  call idda%insertSorted(10_i64)
  call test%test(all(idda%values==[10_i64, 20_i64, 30_i64]), 'idDynamicArray%insertSorted')
  ia = idda%locationOf(20_i64)
  call test%test(ia == 2, 'idDynamicArray%locationOf')
  call idda%insertSortedUnique(10_i64)
  call test%test(all(idda%values==[10_i64, 20_i64, 30_i64]), 'idDynamicArray%insertSortedUnique')
  call idda%insertSortedUnique(15_i64)
  call test%test(all(idda%values(1:idda%N)==[10_i64, 15_i64, 20_i64, 30_i64]), 'idDynamicArray%insertSortedUnique')
  call test%test(size(idda%values) == 6, 'idDynamicArray%insert')
  call idda%deallocate()

  idda = idDynamicArray(3, sorted=.true., fixed=.true.)
  call idda%insertSorted(20_i64)
  call idda%insertSorted(30_i64)
  call idda%insertSorted(10_i64)
  call test%test(all(idda%values(1:idda%N)==[10_i64, 20_i64, 30_i64]), 'idDynamicArray%insertSorted')
  ia = idda%locationOf(20_i64)
  call test%test(ia == 2, 'idDynamicArray%locationOf')
  call idda%insertSortedUnique(10_i64)
  call test%test(all(idda%values(1:idda%N)==[10_i64, 20_i64, 30_i64]), 'idDynamicArray%insertSortedUnique')
  call idda%insertSortedUnique(15_i64)
  call test%test(all(idda%values(1:idda%N)==[10_i64, 15_i64, 20_i64]), 'idDynamicArray%insertSortedUnique')
  call test%test(size(idda%values) == 3, 'idDynamicArray%insert')
  call idda%deallocate()
  end subroutine
  !====================================================================!

  !====================================================================!
  subroutine rDynamicArray_test(test)
    !! graph: false
  !====================================================================!
  class(tester) :: test

  type(rDynamicArray) :: da, da2

  integer(i32) :: ia

  call Msg('==========================')
  call Msg('Testing : Dynamic Arrays')
  call Msg('==========================')

  da = rDynamicArray(10)
  call test%test(size(da%values)==10, 'rDynamicArray')
  call test%test(da%N==0, 'rDynamicArray')
  call da%insertAt(1, 10.0)
  call test%test(da%values(1) == 10, 'rDynamicArray%insert')
  call da%insertAt(1, 20.0)
  call test%test(all(da%values(1:2) == [20.0, 10.0]), 'rDynamicArray%insert')
  call da%prepend(30.0)
  call test%test(all(da%values(1:3) == [30.0, 20.0, 10.0]), 'rDynamicArray%prepend')
  call da%append(40.0)
  call test%test(all(da%values(1:4) == [30.0, 20.0, 10.0, 40.0]), 'rDynamicArray%append')
  call da%remove(2)
  call test%test(all(da%values(1:3) == [30.0, 10.0, 40.0]), 'rDynamicArray%remove')
  call da%tighten()
  call test%test(size(da%values) == 3, 'rDynamicArray%tighten')
  da2 = da
  call test%test(all(da2%values == da%values), 'rDynamicArray%copy')
  da2%values(2) = 50.0
  call test%test(da2%values(2) /= da%values(2), 'rDynamicArray%copy')
  call da%deallocate()
  call test%test(.not. allocated(da%values), 'rDynamicArray%deallocate')
  call da2%deallocate()

  da = rDynamicArray(3, sorted=.true.)
  call da%insertSorted(20.0)
  call da%insertSorted(30.0)
  call da%insertSorted(10.0)
  call test%test(all(da%values==[10.0, 20.0, 30.0]), 'rDynamicArray%insertSorted')
  ia = da%locationOf(20.0)
  call test%test(ia == 2, 'rDynamicArray%locationOf')
  call da%insertSortedUnique(10.0)
  call test%test(all(da%values==[10.0, 20.0, 30.0]), 'rDynamicArray%insertSortedUnique')
  call da%insertSortedUnique(15.0)
  call test%test(all(da%values(1:da%N)==[10.0, 15.0, 20.0, 30.0]), 'rDynamicArray%insertSortedUnique')
  call test%test(size(da%values) == 6, 'rDynamicArray%insert')
  call da%deallocate()

  da = rDynamicArray(3, sorted=.true., fixed=.true.)
  call da%insertSorted(20.0)
  call da%insertSorted(30.0)
  call da%insertSorted(10.0)
  call test%test(all(da%values(1:da%N)==[10.0, 20.0, 30.0]), 'rDynamicArray%insertSorted')
  ia = da%locationOf(20.0)
  call test%test(ia == 2, 'rDynamicArray%locationOf')
  call da%insertSortedUnique(10.0)
  call test%test(all(da%values(1:da%N)==[10.0, 20.0, 30.0]), 'rDynamicArray%insertSortedUnique')
  call da%insertSortedUnique(15.0)
  call test%test(all(da%values(1:da%N)==[10.0, 15.0, 20.0]), 'rDynamicArray%insertSortedUnique')
  call test%test(size(da%values) == 3, 'rDynamicArray%insert')
  call da%deallocate()
  end subroutine
  !====================================================================!

  !====================================================================!
  subroutine dArgDynamicArray_test(test)
    !! graph: false
  !====================================================================!
  class(tester) :: test

  type(dArgDynamicArray) :: da, da2

  integer(i32) :: ia

  call Msg('==========================')
  call Msg('Testing : dArgDynamic Arrays')
  call Msg('==========================')

  da = dArgDynamicArray(10)
  call test%test(size(da%v%values)==10 .and. size(da%i%values)==10, 'dArgDynamicArray')
  call test%test(da%v%N == 0 .and. da%i%N == 0, 'dArgDynamicArray')
  call da%insertAt(1, 10, 10.d0)
  call test%test(da%i%values(1) == 10 .and. da%v%values(1) == 10.d0, 'dArgDynamicArray%insert')
  call da%insertAt(1, 20, 20.d0)
  call test%test(all(da%i%values(1:2) == [20, 10]) .and. all(da%v%values(1:2) == [20.d0, 10.d0]), 'dArgDynamicArray%insert')
  call da%prepend(30, 30.d0)
  call test%test(all(da%i%values(1:3) == [30, 20, 10]) .and. all(da%v%values(1:3) == [30.d0, 20.d0, 10.d0]), 'dArgDynamicArray%prepend')
  call da%append(40, 40.d0)
  call test%test(all(da%i%values(1:4) == [30, 20, 10, 40]) .and. all(da%v%values(1:4) == [30.d0, 20.d0, 10.d0, 40.d0]), 'dArgDynamicArray%append')
  call da%remove(2)
  call test%test(all(da%i%values(1:3) == [30, 10, 40]) .and. all(da%v%values(1:3) == [30.d0, 10.d0, 40.d0]), 'dArgDynamicArray%remove')
  call da%tighten()
  call test%test(size(da%i%values) == 3 .and. size(da%v%values) == 3, 'dArgDynamicArray%tighten')
  da2 = da
  call test%test(all(da2%i%values == da%i%values) .and. all(da2%v%values == da%v%values), 'dArgDynamicArray%copy')
  da2%v%values(2) = 50.d0
  call test%test(da2%v%values(2) /= da%v%values(2), 'dArgDynamicArray%copy')
  call da%deallocate()
  call test%test(.not. allocated(da%i%values) .and. .not. allocated(da%v%values), 'dArgDynamicArray%deallocate')
  call da2%deallocate()

  da = dArgDynamicArray(3, sorted=.true.)
  call da%insertSorted(1, 20.d0)
  call da%insertSorted(2, 30.d0)
  call da%insertSorted(3, 10.d0)
  call test%test(all(da%i%values(1:3)==[3, 1, 2]) .and. all(da%v%values(1:3)==[10.d0, 20.d0, 30.d0]), 'dArgDynamicArray%insertSorted')
  ia = da%locationOf(20.d0)
  call test%test(ia == 2, 'dArgDynamicArray%locationOf')
  ia = da%argOf(20.d0)
  call test%test(ia == 1, 'dArgDynamicArray%argOf')
  call da%insertSortedUnique(4, 10.d0)
  call test%test(all(da%i%values(1:3)==[3, 1, 2]) .and. all(da%v%values(1:3)==[10.d0, 20.d0, 30.d0]), 'dArgDynamicArray%insertSortedUnique')
  call da%insertSortedUnique(4, 15.d0)
  call test%test(all(da%i%values(1:4)==[3, 4, 1, 2]) .and. all(da%v%values(1:4)==[10.d0, 15.d0, 20.d0, 30.d0]), 'dArgDynamicArray%insertSortedUnique')
  call test%test(size(da%i%values) == 6 .and. size(da%v%values) == 6, 'dArgDynamicArray%insert')
  call da%deallocate()

  da = dArgDynamicArray(3, sorted=.true., fixed=.true.)
  call da%insertSorted(1, 20.d0)
  call da%insertSorted(2, 30.d0)
  call da%insertSorted(3, 10.d0)
  call test%test(all(da%i%values(1:3)==[3, 1, 2]) .and. all(da%v%values(1:3)==[10.d0, 20.d0, 30.d0]), 'dArgDynamicArray%insertSorted')
  ia = da%locationOf(20.d0)
  call test%test(ia == 2, 'dArgDynamicArray%locationOf')
  ia = da%argOf(20.d0)
  call test%test(ia == 1, 'dArgDynamicArray%argOf')
  call da%insertSortedUnique(4, 10.d0)
  call test%test(all(da%i%values(1:3)==[3, 1, 2]) .and. all(da%v%values(1:3)==[10.d0, 20.d0, 30.d0]), 'dArgDynamicArray%insertSortedUnique')
  call da%insertSortedUnique(4, 15.d0)
  call test%test(all(da%i%values(1:3)==[3, 4, 1]) .and. all(da%v%values(1:3)==[10.d0, 15.d0, 20.d0]), 'dArgDynamicArray%insertSortedUnique')
  call test%test(size(da%i%values) == 3 .and. size(da%v%values) == 3, 'dArgDynamicArray%insert')
  call da%deallocate()
  end subroutine
  !====================================================================!

  !====================================================================!
  subroutine iArgDynamicArray_test(test)
    !! graph: false
  !====================================================================!
  class(tester) :: test

  type(iArgDynamicArray) :: ida, ida2

  integer(i32) :: ia

  call Msg('==========================')
  call Msg('Testing : iArgDynamic Arrays')
  call Msg('==========================')

  ida = iArgDynamicArray(10)
  call test%test(size(ida%v%values)==10 .and. size(ida%i%values)==10, 'iArgDynamicArray')
  call test%test(ida%v%N == 0 .and. ida%i%N == 0, 'iArgDynamicArray')
  call ida%insertAt(1, 10, 10)
  call test%test(ida%i%values(1) == 10 .and. ida%v%values(1) == 10, 'iArgDynamicArray%insert')
  call ida%insertAt(1, 20, 20)
  call test%test(all(ida%i%values(1:2) == [20, 10]) .and. all(ida%v%values(1:2) == [20, 10]), 'iArgDynamicArray%insert')
  call ida%prepend(30, 30)
  call test%test(all(ida%i%values(1:3) == [30, 20, 10]) .and. all(ida%v%values(1:3) == [30, 20, 10]), 'iArgDynamicArray%prepend')
  call ida%append(40, 40)
  call test%test(all(ida%i%values(1:4) == [30, 20, 10, 40]) .and. all(ida%v%values(1:4) == [30, 20, 10, 40]), 'iArgDynamicArray%append')
  call ida%remove(2)
  call test%test(all(ida%i%values(1:3) == [30, 10, 40]) .and. all(ida%v%values(1:3) == [30, 10, 40]), 'iArgDynamicArray%remove')
  call ida%tighten()
  call test%test(size(ida%i%values) == 3 .and. size(ida%v%values) == 3, 'iArgDynamicArray%tighten')
  ida2 = ida
  call test%test(all(ida2%i%values == ida%i%values) .and. all(ida2%v%values == ida%v%values), 'iArgDynamicArray%copy')
  ida2%v%values(2) = 50
  call test%test(ida2%v%values(2) /= ida%v%values(2), 'iArgDynamicArray%copy')
  call ida%deallocate()
  call test%test(.not. allocated(ida%i%values) .and. .not. allocated(ida%v%values), 'iArgDynamicArray%deallocate')
  call ida2%deallocate()

  ida = iArgDynamicArray(3, sorted=.true.)
  call ida%insertSorted(1, 20)
  call ida%insertSorted(2, 30)
  call ida%insertSorted(3, 10)
  call test%test(all(ida%i%values(1:3)==[3, 1, 2]) .and. all(ida%v%values(1:3)==[10, 20, 30]), 'iArgDynamicArray%insertSorted')
  ia = ida%locationOf(20)
  call test%test(ia == 2, 'iArgDynamicArray%locationOf')
  ia = ida%argOf(20)
  call test%test(ia == 1, 'iArgDynamicArray%argOf')
  call ida%insertSortedUnique(4, 10)
  call test%test(all(ida%i%values(1:3)==[3, 1, 2]) .and. all(ida%v%values(1:3)==[10, 20, 30]), 'iArgDynamicArray%insertSortedUnique')
  call ida%insertSortedUnique(4, 15)
  call test%test(all(ida%i%values(1:4)==[3, 4, 1, 2]) .and. all(ida%v%values(1:4)==[10, 15, 20, 30]), 'iArgDynamicArray%insertSortedUnique')
  call test%test(size(ida%i%values) == 6 .and. size(ida%v%values) == 6, 'iArgDynamicArray%insert')
  call ida%deallocate()

  ida = iArgDynamicArray(3, sorted=.true., fixed=.true.)
  call ida%insertSorted(1, 20)
  call ida%insertSorted(2, 30)
  call ida%insertSorted(3, 10)
  call test%test(all(ida%i%values(1:3)==[3, 1, 2]) .and. all(ida%v%values(1:3)==[10, 20, 30]), 'iArgDynamicArray%insertSorted')
  ia = ida%locationOf(20)
  call test%test(ia == 2, 'iArgDynamicArray%locationOf')
  ia = ida%argOf(20)
  call test%test(ia == 1, 'iArgDynamicArray%argOf')
  call ida%insertSortedUnique(4, 10)
  call test%test(all(ida%i%values(1:3)==[3, 1, 2]) .and. all(ida%v%values(1:3)==[10, 20, 30]), 'iArgDynamicArray%insertSortedUnique')
  call ida%insertSortedUnique(4, 15)
  call test%test(all(ida%i%values(1:3)==[3, 4, 1]) .and. all(ida%v%values(1:3)==[10, 15, 20]), 'iArgDynamicArray%insertSortedUnique')
  call test%test(size(ida%i%values) == 3 .and. size(ida%v%values) == 3, 'iArgDynamicArray%insert')
  call ida%deallocate()
  end subroutine 
  !====================================================================!

  !====================================================================!
  subroutine idArgDynamicArray_test(test)
    !! graph: false
  !====================================================================!
  class(tester) :: test

  type(idArgDynamicArray) :: idda, idda2

  integer(i32) :: ia

  call Msg('==========================')
  call Msg('Testing : idArgDynamic Arrays')
  call Msg('==========================')

  idda = idArgDynamicArray(10)
  call test%test(size(idda%v%values)==10 .and. size(idda%i%values)==10, 'idArgDynamicArray')
  call test%test(idda%v%N == 0 .and. idda%i%N == 0, 'idArgDynamicArray')
  call idda%insertAt(1, 10, 10_i64)
  call test%test(idda%i%values(1) == 10_i64 .and. idda%v%values(1) == 10_i64, 'idArgDynamicArray%insert')
  call idda%insertAt(1, 20, 20_i64)
  call test%test(all(idda%i%values(1:2) == [20_i64, 10_i64]) .and. all(idda%v%values(1:2) == [20_i64, 10_i64]), 'idArgDynamicArray%insert')
  call idda%prepend(30, 30_i64)
  call test%test(all(idda%i%values(1:3) == [30_i64, 20_i64, 10_i64]) .and. all(idda%v%values(1:3) == [30_i64, 20_i64, 10_i64]), 'idArgDynamicArray%prepend')
  call idda%append(40, 40_i64)
  call test%test(all(idda%i%values(1:4) == [30_i64, 20_i64, 10_i64, 40_i64]) .and. all(idda%v%values(1:4) == [30_i64, 20_i64, 10_i64, 40_i64]), 'idArgDynamicArray%append')
  call idda%remove(2)
  call test%test(all(idda%i%values(1:3) == [30_i64, 10_i64, 40_i64]) .and. all(idda%v%values(1:3) == [30_i64, 10_i64, 40_i64]), 'idArgDynamicArray%remove')
  call idda%tighten()
  call test%test(size(idda%i%values) == 3 .and. size(idda%v%values) == 3, 'idArgDynamicArray%tighten')
  idda2 = idda
  call test%test(all(idda2%i%values == idda%i%values) .and. all(idda2%v%values == idda%v%values), 'idArgDynamicArray%copy')
  idda2%v%values(2) = 50_i64
  call test%test(idda2%v%values(2) /= idda%v%values(2), 'idArgDynamicArray%copy')
  call idda%deallocate()
  call test%test(.not. allocated(idda%i%values) .and. .not. allocated(idda%v%values), 'idArgDynamicArray%deallocate')
  call idda2%deallocate()

  idda = idArgDynamicArray(3, sorted=.true.)
  call idda%insertSorted(1, 20_i64)
  call idda%insertSorted(2, 30_i64)
  call idda%insertSorted(3, 10_i64)
  call test%test(all(idda%i%values(1:3)==[3, 1, 2]) .and. all(idda%v%values(1:3)==[10_i64, 20_i64, 30_i64]), 'idArgDynamicArray%insertSorted')
  ia = idda%locationOf(20_i64)
  call test%test(ia == 2, 'idArgDynamicArray%locationOf')
  ia = idda%argOf(20_i64)
  call test%test(ia == 1, 'idArgDynamicArray%argOf')
  call idda%insertSortedUnique(4, 10_i64)
  call test%test(all(idda%i%values(1:3)==[3, 1, 2]) .and. all(idda%v%values(1:3)==[10_i64, 20_i64, 30_i64]), 'idArgDynamicArray%insertSortedUnique')
  call idda%insertSortedUnique(4, 15_i64)
  call test%test(all(idda%i%values(1:4)==[3, 4, 1, 2]) .and. all(idda%v%values(1:4)==[10_i64, 15_i64, 20_i64, 30_i64]), 'idArgDynamicArray%insertSortedUnique')
  call test%test(size(idda%i%values) == 6 .and. size(idda%v%values) == 6, 'idArgDynamicArray%insert')
  call idda%deallocate()

  idda = idArgDynamicArray(3, sorted=.true., fixed=.true.)
  call idda%insertSorted(1, 20_i64)
  call idda%insertSorted(2, 30_i64)
  call idda%insertSorted(3, 10_i64)
  call test%test(all(idda%i%values(1:3)==[3, 1, 2]) .and. all(idda%v%values(1:3)==[10_i64, 20_i64, 30_i64]), 'idArgDynamicArray%insertSorted')
  ia = idda%locationOf(20_i64)
  call test%test(ia == 2, 'idArgDynamicArray%locationOf')
  ia = idda%argOf(20_i64)
  call test%test(ia == 1, 'idArgDynamicArray%argOf')
  call idda%insertSortedUnique(4, 10_i64)
  call test%test(all(idda%i%values(1:3)==[3, 1, 2]) .and. all(idda%v%values(1:3)==[10_i64, 20_i64, 30_i64]), 'idArgDynamicArray%insertSortedUnique')
  call idda%insertSortedUnique(4, 15_i64)
  call test%test(all(idda%i%values(1:3)==[3, 4, 1]) .and. all(idda%v%values(1:3)==[10_i64, 15_i64, 20_i64]), 'idArgDynamicArray%insertSortedUnique')
  call test%test(size(idda%i%values) == 3 .and. size(idda%v%values) == 3, 'idArgDynamicArray%insert')
  call idda%deallocate()
  end subroutine
  !====================================================================!

  !====================================================================!
  subroutine rArgDynamicArray_test(test)
    !! graph: false
  !====================================================================!
  class(tester) :: test

  type(rArgDynamicArray) :: rda, rda2

  integer(i32) :: ia

  call Msg('==========================')
  call Msg('Testing : rArgDynamic Arrays')
  call Msg('==========================')

  rda = rArgDynamicArray(10)
  call test%test(size(rda%v%values)==10 .and. size(rda%i%values)==10, 'rArgDynamicArray')
  call test%test(rda%v%N == 0 .and. rda%i%N == 0, 'rArgDynamicArray')
  call rda%insertAt(1, 10, 10.0)
  call test%test(rda%i%values(1) == 10 .and. rda%v%values(1) == 10.0, 'rArgDynamicArray%insert')
  call rda%insertAt(1, 20, 20.0)
  call test%test(all(rda%i%values(1:2) == [20, 10]) .and. all(rda%v%values(1:2) == [20.0, 10.0]), 'rArgDynamicArray%insert')
  call rda%prepend(30, 30.0)
  call test%test(all(rda%i%values(1:3) == [30, 20, 10]) .and. all(rda%v%values(1:3) == [30.0, 20.0, 10.0]), 'rArgDynamicArray%prepend')
  call rda%append(40, 40.0)
  call test%test(all(rda%i%values(1:4) == [30, 20, 10, 40]) .and. all(rda%v%values(1:4) == [30.0, 20.0, 10.0, 40.0]), 'rArgDynamicArray%append')
  call rda%remove(2)
  call test%test(all(rda%i%values(1:3) == [30, 10, 40]) .and. all(rda%v%values(1:3) == [30.0, 10.0, 40.0]), 'rArgDynamicArray%remove')
  call rda%tighten()
  call test%test(size(rda%i%values) == 3 .and. size(rda%v%values) == 3, 'rArgDynamicArray%tighten')
  rda2 = rda
  call test%test(all(rda2%i%values == rda%i%values) .and. all(rda2%v%values == rda%v%values), 'rArgDynamicArray%copy')
  rda2%v%values(2) = 50.0
  call test%test(rda2%v%values(2) /= rda%v%values(2), 'rArgDynamicArray%copy')
  call rda%deallocate()
  call test%test(.not. allocated(rda%i%values) .and. .not. allocated(rda%v%values), 'rArgDynamicArray%deallocate')
  call rda2%deallocate()

  rda = rArgDynamicArray(3, sorted=.true.)
  call rda%insertSorted(1, 20.0)
  call rda%insertSorted(2, 30.0)
  call rda%insertSorted(3, 10.0)
  call test%test(all(rda%i%values(1:3)==[3, 1, 2]) .and. all(rda%v%values(1:3)==[10.0, 20.0, 30.0]), 'rArgDynamicArray%insertSorted')
  ia = rda%locationOf(20.0)
  call test%test(ia == 2, 'rArgDynamicArray%locationOf')
  ia = rda%argOf(20.0)
  call test%test(ia == 1, 'rArgDynamicArray%argOf')
  call rda%insertSortedUnique(4, 10.0)
  call test%test(all(rda%i%values(1:3)==[3, 1, 2]) .and. all(rda%v%values(1:3)==[10.0, 20.0, 30.0]), 'rArgDynamicArray%insertSortedUnique')
  call rda%insertSortedUnique(4, 15.0)
  call test%test(all(rda%i%values(1:4)==[3, 4, 1, 2]) .and. all(rda%v%values(1:4)==[10.0, 15.0, 20.0, 30.0]), 'rArgDynamicArray%insertSortedUnique')
  call test%test(size(rda%i%values) == 6 .and. size(rda%v%values) == 6, 'rArgDynamicArray%insert')
  call rda%deallocate()

  rda = rArgDynamicArray(3, sorted=.true., fixed=.true.)
  call rda%insertSorted(1, 20.0)
  call rda%insertSorted(2, 30.0)
  call rda%insertSorted(3, 10.0)
  call test%test(all(rda%i%values(1:3)==[3, 1, 2]) .and. all(rda%v%values(1:3)==[10.0, 20.0, 30.0]), 'rArgDynamicArray%insertSorted')
  ia = rda%locationOf(20.0)
  call test%test(ia == 2, 'rArgDynamicArray%locationOf')
  ia = rda%argOf(20.0)
  call test%test(ia == 1, 'rArgDynamicArray%argOf')
  call rda%insertSortedUnique(4, 10.0)
  call test%test(all(rda%i%values(1:3)==[3, 1, 2]) .and. all(rda%v%values(1:3)==[10.0, 20.0, 30.0]), 'rArgDynamicArray%insertSortedUnique')
  call rda%insertSortedUnique(4, 15.0)
  call test%test(all(rda%i%values(1:3)==[3, 4, 1]) .and. all(rda%v%values(1:3)==[10.0, 15.0, 20.0]), 'rArgDynamicArray%insertSortedUnique')
  call test%test(size(rda%i%values) == 3 .and. size(rda%v%values) == 3, 'rArgDynamicArray%insert')
  call rda%deallocate()
  end subroutine
  !====================================================================!

  !====================================================================!
  subroutine array1D_test(test)
  !====================================================================!
  class(tester) :: test

  real(r32) :: ar
  real(r32), allocatable :: ar1D(:), br1D(:)
  real(r64) :: a
  real(r64), allocatable :: a1D(:), b1D(:)
  integer(i32) :: ia, ib, ic
  integer(i32), allocatable :: ia1D(:), ib1D(:)
  integer(i64) :: iad
  integer(i64), allocatable :: iad1D(:), ibd1D(:)

  call Msg('==========================')
  call Msg('Testing : Array 1D')
  call Msg('==========================')

  call allocate(ar1D, 3)
  call allocate(a1D, 3)
  call allocate(ia1D, 3)
  call allocate(iad1D, 3)
  call allocate(br1D, 2)
  call allocate(b1D, 2)
  call allocate(ib1D, 2)
  call allocate(ibd1D, 2)

  call arange(ar1D,1.0, 3.0, 1.0)
  call test%test(all(ar1D==[1.0,2.0,3.0]),'arange_r1D')
  call arange(a1D,1.d0, 3.d0, 1.d0)
  call test%test(all(a1D==[1.d0,2.d0,3.d0]),'arange_d1D')
  call arange(ia1D,1, 3, 1)
  call test%test(all(ia1D==[1,2,3]),'arange_i1D')
  call arange(iad1D,1_i64, 3_i64, 1_i64)
  call test%test(all(iad1D==[1,2,3]),'arange_id1D')

  call diff(ar1D, br1D)
  call test%test(all(br1D==[1.0,1.0]),'diff_r1D')
  call diff(a1D, b1D)
  call test%test(all(b1D==[1.d0,1.d0]),'diff_d1D')
  call diff(ia1d, ib1D)
  call test%test(all(ib1D==[1,1]),'diff_i1D')
  call diff(iad1d, ibd1D)
  call test%test(all(ibd1D==[1,1]),'diff_id1D')

  call allocate(br1D, size(ar1D)*3)
  call allocate(b1D, size(a1D)*3)
  call allocate(ib1D, size(ia1D)*3)
  call allocate(ibd1D, size(iad1D)*3)

  call repeat(ar1D, 3, br1D)
  call test%test(all(br1D == [1.0,1.0,1.0,2.0,2.0,2.0,3.0,3.0,3.0]), 'repeat_r1D')
  call repeat(a1D,3, b1D)
  call test%test(all(b1D == [1.d0,1.d0,1.d0,2.d0,2.d0,2.d0,3.d0,3.d0,3.d0]), 'repeat_d1D')
  call repeat(ia1D,3, ib1D)
  call test%test(all(ib1D == [1,1,1,2,2,2,3,3,3]), 'repeat_ib1D')
  call repeat(iad1D,3, ibd1D)
  call test%test(all(ibd1D == [1,1,1,2,2,2,3,3,3]), 'repeat_ibd1D')

  call deallocate(ar1D)
  call deallocate(a1D)
  call deallocate(ia1D)
  call deallocate(iad1D)
  call deallocate(br1D)
  call deallocate(b1D)
  call deallocate(ib1D)
  call deallocate(ibd1D)

  end subroutine
  !====================================================================!

  !====================================================================!
  subroutine copy_test(test)
    !! graph: false
  !====================================================================!
  class(tester) :: test

  real(r32), allocatable :: ar1D(:), ar2D(:,:), ar3D(:,:,:)
  real(r64), allocatable :: a1D(:), a2D(:,:), a3D(:,:,:)
  integer(i32), allocatable :: ia1D(:), ia2D(:,:), ia3D(:,:,:)
  integer(i64), allocatable :: iad1D(:), iad2D(:,:), iad3D(:,:,:)
  complex(r32), allocatable :: za1D(:), za2D(:,:), za3D(:,:,:)
  complex(r64), allocatable :: zza1D(:), zza2D(:,:), zza3D(:,:,:)
  logical, allocatable :: la1D(:), la2D(:,:), la3D(:,:,:)

  real(r32), allocatable :: br1D(:), br2D(:,:), br3D(:,:,:)
  real(r64), allocatable :: b1D(:), b2D(:,:), b3D(:,:,:)
  integer(i32), allocatable :: ib1D(:), ib2D(:,:), ib3D(:,:,:)
  integer(i64), allocatable :: ibd1D(:), ibd2D(:,:), ibd3D(:,:,:)
  complex(r32), allocatable :: zb1D(:), zb2D(:,:), zb3D(:,:,:)
  complex(r64), allocatable :: zzb1D(:), zzb2D(:,:), zzb3D(:,:,:)
  logical, allocatable :: lb1D(:), lb2D(:,:), lb3D(:,:,:)

  call Msg('==========================')
  call Msg('Testing : Copy')
  call Msg('==========================')

  call allocate(ar1D, 10); ar1D = 1.0
  call allocate(ar2D, [5,6]); ar2D = 2.0
  call allocate(ar3D, [10,9,8]); ar3D = 3.0
  call allocate(a1D, 10); a1D = 1.d0
  call allocate(a2D, [5,6]); a2D = 2.d0
  call allocate(a3D, [10,9,8]); a3D = 3.d0
  call allocate(ia1D, 10); ia1D = 1
  call allocate(ia2D, [5,6]); ia2D = 2
  call allocate(ia3D, [10,9,8]); ia3D = 3
  call allocate(iad1D, 10); iad1D = 1_i64
  call allocate(iad2D, [5,6]); iad2D = 2_i64
  call allocate(iad3D, [10,9,8]); iad3D = 3_i64
  call allocate(za1D, 10); za1D = (1.0, 1.0)
  call allocate(za2D, [5,6]); za2D = (2.0, 1.0)
  call allocate(za3D, [10,9,8]); za3D = (3.0, 1.0)
  call allocate(zza1D, 10); zza1D = (1.d0, 1.d0)
  call allocate(zza2D, [5,6]); zza2D = (2.d0, 1.d0)
  call allocate(zza3D, [10,9,8]); zza3D = (3.d0, 1.d0)
  call allocate(la1D, 10); la1D = .true.
  call allocate(la2D, [5,6]); la2D = .true.
  call allocate(la3D, [10,9,8]); la3D = .true.

  call copy(ar1D,br1D)
  call test%test(all(ar1D == br1D),'copy_r1D')
  call copy(ar2D,br2D)
  call test%test(all(ar2D == br2D),'copy_r2D')
  call copy(ar3D,br3D)
  call test%test(all(ar3D == br3D),'copy_r3D')
  call copy(a1D,b1D)
  call test%test(all(a1D == b1D),'copy_d1D')
  call copy(a2D,b2D)
  call test%test(all(a2D == b2D),'copy_d2D')
  call copy(a3D,b3D)
  call test%test(all(a3D == b3D),'copy_d3D')
  call copy(ia1D,ib1D)
  call test%test(all(ia1D == ib1D),'copy_i1D')
  call copy(ia2D,ib2D)
  call test%test(all(ia2D == ib2D),'copy_i2D')
  call copy(ia3D,ib3D)
  call test%test(all(ia3D == ib3D),'copy_i3D')
  call copy(iad1D,ibd1D)
  call test%test(all(iad1D == ibd1D),'copy_id1D')
  call copy(iad2D,ibd2D)
  call test%test(all(iad2D == ibd2D),'copy_id2D')
  call copy(iad3D,ibd3D)
  call test%test(all(iad3D == ibd3D),'copy_id3D')
  call copy(za1D,zb1D)
  call test%test(all(za1D == zb1D),'copy_c1D')
  call copy(za2D,zb2D)
  call test%test(all(za2D == zb2D),'copy_c2D')
  call copy(za3D,zb3D)
  call test%test(all(za3D == zb3D),'copy_c3D')
  call copy(zza1D,zzb1D)
  call test%test(all(zza1D == zzb1D),'copy_z1D')
  call copy(zza2D,zzb2D)
  call test%test(all(zza2D == zzb2D),'copy_z2D')
  call copy(zza3D,zzb3D)
  call test%test(all(zza3D == zzb3D),'copy_z3D')
  call copy(la1D,lb1D)
  call test%test(all(la1D .eqv. lb1D),'copy_l1D')
  call copy(la2D,lb2D)
  call test%test(all(la2D .eqv. lb2D),'copy_l2D')
  call copy(la3D,lb3D)
  call test%test(all(la3D .eqv. lb3D),'copy_l3D')

  deallocate(ar1D,ar2D,ar3D,a1D,a2D,a3D,ia1D,ia2D,ia3D)
  deallocate(iad1D,iad2D,iad3D,za1D,za2D,za3D,zza1D,zza2D,zza3D)
  deallocate(la1D,la2D,la3D)

  deallocate(br1D,br2D,br3D,b1D,b2D,b3D,ib1D,ib2D,ib3D)
  deallocate(ibd1D,ibd2D,ibd3D,zb1D,zb2D,zb3D,zzb1D,zzb2D,zzb3D)
  deallocate(lb1D,lb2D,lb3D)
  end subroutine
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

  !====================================================================!
  subroutine KdTree_test(test, N)
  !====================================================================!
  class(tester) :: test

  real(r64) :: a,b,c
  real(r64),allocatable :: a1D(:), b1D(:), c1D(:), d1D(:)
  real(r64),allocatable :: a2D(:,:)
  integer(i32) :: ia, N
  integer(i32), allocatable :: ia1D(:)
  type(KdTree) :: tree
  type(KdTreeSearch) :: search

  type(iDynamicArray) :: ida, ida2
  type(dArgDynamicArray) :: da
  integer(i32) :: iSearch(3) ! Used for testing kNearest.

  call Msg('==========================')
  call Msg('Testing : Spatial')
  call Msg('==========================')

  call allocate(a1D, N)
  call allocate(b1D, N)
  call allocate(c1D, N)
  call allocate(d1D, N)
  call allocate(ia1D, N)

  a1D = 0.d0; b1D = 0.d0; c1D = 0.d0

  ! 2D KdTree
  call rngNormal(a1D)
  call rngNormal(b1D)

  tree = KdTree(a1D, b1D)

  c1D = a1D**2.d0
  c1D = c1D + b1D**2.d0
  call arange(ia1D, 1, N)  
  call argSort(c1D, ia1D)

  ia = search%nearest(tree, a1D, b1D, 0.d0, 0.d0)
 
  call test%test(ia == ia1D(1), '2D - KdTreeSearch%nearest')

  da = search%kNearest(tree, a1D, b1D, 0.d0, 0.d0, k = 10)

  call test%test(all(da%i%values == ia1D(1:10)) .and. all(abs(da%v%values - sqrt(c1D(ia1D(1:10)))) <= 1.d-15), '2D - KdTreeSearch%kNearest, k nearest')

  c1D = sqrt(c1D(ia1D))
  a = c1D(15) + 1.d-14

  call da%deallocate()

  da = search%kNearest(tree, a1D, b1D, 0.d0, 0.d0, radius = a)

  call test%test(all(da%i%values == ia1D(1:15)) .and. all(abs(da%v%values - (c1D(1:15))) <= 1.d-15), '2D - KdTreeSearch%kNearest, radius search')

  call da%deallocate()
  da = search%kNearest(tree, a1D, b1D, 0.d0, 0.d0, k=10, radius = a)
  call test%test(all(da%i%values == ia1D(1:10)) .and. all(abs(da%v%values - (c1D(1:10))) <= 1.d-15), '2D - KdTreeSearch%kNearest, k radius search')

  ida = search%rangeSearch(tree, a1D, b1D, [-0.2d0, -0.2d0], [0.2d0, 0.2d0])

  ida2 = iDynamicArray(16, .true., .false.)
  do ia = 1, N
    if (a1D(ia) >= -0.2d0 .and. a1D(ia) <= 0.2d0) then
      if (b1D(ia) >= -0.2d0 .and. b1D(ia) <= 0.2d0) then
        call ida2%insertSorted(ia)
      endif
    endif
  enddo
  call ida2%tighten()

  if (ida%isEmpty()) then
    call test%test(ida2%isEmpty(), '2D - KdTreeSearch%rangeSearch')
  else
    call test%test(all(ida%values == ida2%values), '2D - KdTreeSearch%rangeSearch')
  endif
 
  call ida%deallocate()
  call ida2%deallocate()
  call tree%deallocate()



  ! 3D KdTree
  call rngNormal(c1D)

  tree = KdTree(a1D, b1D, c1D)

  ia = search%nearest(tree, a1D, b1D, c1D, 0.d0, 0.d0, 0.d0)
  da = search%kNearest(tree, a1D, b1D, c1D, 0.d0, 0.d0, 0.d0, 10)

  d1D = a1D**2.d0
  d1D = d1D + b1D**2.d0
  d1D = d1D + c1D**2.d0
  call arange(ia1D, 1, N)
  call argSort(d1D, ia1D)

  call test%test(ia == ia1D(1), '3D - KdTreeSearch%nearest')
  call test%test(all(da%i%values == ia1D(1:10)) .and. all(abs(da%v%values - sqrt(d1D(ia1D(1:10)))) <= 1.d-15), '3D - KdTreeSearch%kNearest')

  d1D = sqrt(d1D(ia1D))
  a = d1D(15) + 1.d-14

  call da%deallocate()
  da = search%kNearest(tree, a1D, b1D, c1D, 0.d0, 0.d0, 0.d0, radius = a)

  call test%test(all(da%i%values == ia1D(1:15)) .and. all(abs(da%v%values - (d1D(1:15))) <= 1.d-15), '3D - KdTreeSearch%kNearest, radius search')

  call da%deallocate()
  da = search%kNearest(tree, a1D, b1D, c1D, 0.d0, 0.d0, 0.d0, k=10, radius = a)
  call test%test(all(da%i%values == ia1D(1:10)) .and. all(abs(da%v%values - (d1D(1:10))) <= 1.d-15), '3D - KdTreeSearch%kNearest, k radius search')

  ida = search%rangeSearch(tree, a1D, b1D, c1D, [-0.2d0, -0.2d0, -0.2d0], [0.2d0, 0.2d0, 0.2d0])

  ida2 = iDynamicArray(16, .true., .false.)
  do ia = 1, N
    if (a1D(ia) >= -0.2d0 .and. a1D(ia) <= 0.2d0) then
      if (b1D(ia) >= -0.2d0 .and. b1D(ia) <= 0.2d0) then
        if (c1D(ia) >= -0.2d0 .and. c1D(ia) <= 0.2d0) then
          call ida2%insertSorted(ia)
        endif
      endif
    endif
  enddo
  call ida2%tighten()

  if (ida%isEmpty()) then
    call test%test(ida2%isEmpty(), '3D - KdTreeSearch%rangeSearch')
  else
    call test%test(all(ida%values == ida2%values), '3D - KdTreeSearch%rangeSearch')
  endif
 
  call ida%deallocate()
  call ida2%deallocate()

  call tree%deallocate()

  ! KD KdTree
  call allocate(a2D, [N, 2])
  a2D(:,1) = a1D
  a2D(:,2) = b1D

  tree = KdTree(a2D)

  ia = search%nearest(tree, a2D, [0.d0, 0.d0])
  da = search%kNearest(tree, a2D, [0.d0, 0.d0], 10)

  c1D = a1D**2.d0
  c1D = c1D + b1D**2.d0

  call arange(ia1D, 1, N)  
  call argSort(c1D, ia1D)

  call test%test(ia == ia1d(1), 'KD - KdTreeSearch%nearest')
  call test%test(all(da%i%values == ia1D(1:10)) .and. all(abs(da%v%values - sqrt(c1D(ia1D(1:10)))) <= 1.d-15), 'KD - KdTreeSearch%kNearest')

  do ia = 1, N
    d1D(ia) = sqrt(c1D(ia1D(ia)))
  enddo
  a = d1D(15) + 1.d-14

  call da%deallocate()
  da = search%kNearest(tree, a2D, [0.d0, 0.d0], radius = a)

  call test%test(all(da%i%values == ia1D(1:15)) .and. all(abs(da%v%values - (d1D(1:15))) <= 1.d-15), 'KD - KdTreeSearch%kNearest, radius search')

  call da%deallocate()
  da = search%kNearest(tree, a2D, [0.d0, 0.d0], k=10, radius = a)
  call test%test(all(da%i%values == ia1D(1:10)) .and. all(abs(da%v%values - (d1D(1:10))) <= 1.d-15), 'KD - KdTreeSearch%kNearest, k radius search')

  ida = search%rangeSearch(tree, a2D, [-0.2d0, -0.2d0], [0.2d0, 0.2d0])

  ida2 = iDynamicArray(16, .true., .false.)
  do ia = 1, N
    if (a2D(ia,1) >= -0.2d0 .and. a2D(ia,1) <= 0.2d0) then
      if (a2D(ia,2) >= -0.2d0 .and. a2D(ia,2) <= 0.2d0) then
        call ida2%insertSorted(ia)
      endif
    endif
  enddo
  call ida2%tighten()

  if (ida%isEmpty()) then
    call test%test(ida2%isEmpty(), 'KD - KdTreeSearch%rangeSearch')
  else
    call test%test(all(ida%values == ida2%values), 'KD - KdTreeSearch%rangeSearch')
  endif
 
  call ida%deallocate()
  call ida2%deallocate()

  call tree%deallocate()
  end subroutine
  !====================================================================!

  !====================================================================!
  subroutine maths_test(test)
    !! graph: false
  !====================================================================!
  class(tester) :: test

  real(r32) :: ar, br
  real(r32), allocatable :: ar1D(:), br1D(:), cr1D(:)
  real(r64) :: a, b
  real(r64), allocatable :: a1D(:), b1D(:), c1D(:)
  integer(i32) :: ia, ib, ic
  integer(i32), allocatable :: ia1D(:), ib1D(:)
  integer(i64) :: iad
  integer(i64), allocatable :: iad1D(:), ibd1D(:)

  call Msg('==========================')
  call Msg('Testing : Maths')
  call Msg('==========================')


  call allocate(ar1D, 3)
  call allocate(a1D, 3)
  call allocate(ia1D, 3)
  call allocate(iad1D, 3)
  call allocate(br1D, 2)
  call allocate(b1D, 2)
  call allocate(ib1D, 2)
  call allocate(ibd1D, 2)

  call arange(ar1D,1.0, 3.0, 1.0)
  call arange(a1D,1.d0, 3.d0, 1.d0)
  call arange(ia1D,1, 3, 1)
  call arange(iad1D,1_i64, 3_i64, 1_i64)


  br1D = [5.0,6.0,7.0]
  b1D = [5.d0,6.d0,7.d0]
  ib1D = [5,6,7]
  ibd1D = [5_i64, 6_i64, 7_i64]

  cr1D=crossproduct(ar1D, br1D)
  call test%test(all(cr1D==[-4.0,8.0,-4.0]),'crossproduct_r1D')
  c1D=crossproduct(a1D,b1D)
  call test%test(all(cr1D==[-4.d0,8.d0,-4.d0]),'crossproduct_d1D')

  cr1D=cumprod(ar1D)
  call test%test(all(cr1D==[1.0,2.0,6.0]),'cumprod_r1D')
  c1D=cumprod(a1D)
  call test%test(all(c1D==[1.d0,2.d0,6.d0]),'cumprod_d1D')
  ib1D=cumprod(ia1D)
  call test%test(all(ib1D==[1,2,6]),'cumprod_i1D')
  ibd1D=cumprod(iad1D)
  call test%test(all(ibd1D==[1,2,6]),'cumprod_id1D')

  cr1D=cumsum(ar1D)
  call test%test(all(cr1D==[1.0,3.0,6.0]),'cumsum_r1D')
  c1D=cumsum(a1D)
  call test%test(all(c1D==[1.d0,3.d0,6.d0]),'cumsum_d1D')
  ib1D=cumsum(ia1D)
  call test%test(all(ib1D==[1,3,6]),'cumsum_i1D')
  ibd1D=cumsum(iad1D)
  call test%test(all(ibd1D==[1,3,6]),'cumsum_id1D')

  ar1D=[1.0,2.0,3.0]
  br1D=[0.0,0.5,0.0]
  a1D=[1.d0,2.d0,3.d0]
  b1D=[0.d0,0.5d0,0.d0]

  cr1D=project(ar1D,br1D)
  call test%test(all(cr1D==[0.0,2.0,0.0]),'project_r1D')
  c1D=project(a1D,b1D)
  call test%test(all(c1D==[0.d0,2.d0,0.d0]),'project_d1D')

  a=mean(ar1D)
  call test%test(a==2.d0,'mean_r1D')
  a=mean(a1D)
  call test%test(a==2.d0,'mean_d1D')
  a=mean(ia1D)
  call test%test(a==2.d0,'mean_i1D')
  a=mean(iad1D)
  call test%test(a==2.d0,'mean_id1D')

  a=norm1(ar1D)
  call test%test(a==6.d0,'norm1_r1D')
  a=norm1(a1D)
  call test%test(a==6.d0,'norm1_d1D')
  a=norm1(a1D)
  call test%test(a==6.d0,'norm1_i1D')
  a=norm1(a1D)
  call test%test(a==6.d0,'norm1_id1D')

  ar=normI(ar1D)
  call test%test(ar==3.0,'normI_r1D')
  a=normI(a1D)
  call test%test(a==3.d0,'normI_d1D')
  ia=normI(ia1D)
  call test%test(a==3,'normI_i1D')
  iad=normI(iad1D)
  call test%test(a==3,'normI_id1D')

  a=geometricMean(ar1D)
  call test%test(a==216.d0,'geometricMean_r1D')
  a=geometricMean(a1D)
  call test%test(a==216.d0,'geometricMean_d1D')
  a=geometricMean(ia1D)
  call test%test(a==216.d0,'geometricMean_i1D')
  a=geometricMean(iad1D)
  call test%test(a==216.d0,'geometricMean_id1D')


  call allocate(a1D,2)
  call allocate(ar1D,2)
  ar = 0.2
  br = 0.1
  a = 0.2d0
  b = 0.1d0
  ar1D = twoSum(ar,br)
  call test%test(abs(ar1D(2)) < 1.d-7,'twoSum_r')
  a1D = twoSum(a,b)
  call test%test(abs(a1D(2)) < 1.d-15,'twoSum_d')
  ar1D = fastTwoSum(ar,br)
  call test%test(abs(ar1D(2)) < 1.d-7,'fastTwoSum_r')
  a1D = fastTwoSum(a,b)
  call test%test(abs(a1D(2)) < 1.d-15,'fastTwoSum_d')

  ar1D=[1.0,2.0,3.0]
  br1D=[0.0,0.5,0.0]
  a1D=[1.d0,2.d0,3.d0]
  b1D=[0.d0,0.5d0,0.d0]

  a=std(ar1D)
  call test%test(a==1.d0,'std_r1D')
  a=std(a1D)
  call test%test(a==1.d0,'std_d1D')
  a=std(ia1D)
  call test%test(a==1.d0,'std_i1D')
  a=std(iad1D)
  call test%test(a==1.d0,'std_id1D')

  a=variance(ar1D)
  call test%test(a==1.d0,'variance_r1D')
  a=variance(a1D)
  call test%test(a==1.d0,'variance_d1D')
  a=variance(ia1D)
  call test%test(a==1.d0,'variance_i1D')
  a=variance(iad1D)
  call test%test(a==1.d0,'variance_id1D')

  a=median(ar1D)
  call test%test(a==2.d0,'median_r1D')
  a=median(a1D)
  call test%test(a==2.d0,'median_d1D')
  a=median(ia1D)
  call test%test(a==2.d0,'median_i1D')
  a=median(iad1D)
  call test%test(a==2.d0,'median_id1D')

  call deallocate(ar1D)
  call deallocate(a1D)
  call deallocate(ia1D)
  call deallocate(iad1D)
  call deallocate(br1D)
  call deallocate(b1D)
  call deallocate(ib1D)
  call deallocate(ibd1D)
  call deallocate(cr1D)
  call deallocate(c1D)

  end subroutine
  !====================================================================!

  !====================================================================!
  subroutine Prng_test(test)
  !====================================================================!
  class(tester) :: test

  type(Prng) :: rng
  type(Prng), allocatable :: rngs(:)
  integer(i64) :: seed(16)
  integer(i32) :: i, id

  integer(i32) :: iThread, nThreads

  real(r64) :: a

  rng = Prng(big = .true.)

  call rng%rngUniform(a)

  call rng%rngInteger(id, 1, 100)

  ! Get the number of threads available
  !$omp parallel 
    nThreads = omp_get_num_threads()
  !$omp end parallel

  ! Allocate an array of Prngs, one for each thread
  allocate(rngs(nThreads))

  ! In parallel, initialize each Prng with the same seed, and jump each prng by the thread ID it is associated with.
  ! This allows all Prngs to draw from the same stream, but at different points along the stream.
  ! This is better than giving each Prng its own randomly generated seed

  call getRandomSeed(seed, big = .true.)

  !$omp parallel shared(rng, seed) private(iThread, a)
    iThread = omp_get_thread_num()
    rngs(iThread + 1) = Prng(seed, big = .true.)
    call rngs(iThread + 1)%jump(iThread) ! Jump the current thread's Prng by its thread number.
    call rngs(iThread + 1)%rngNormal(a) ! Draw from normal distribution on each thread
  !$omp end parallel

  write(*,*) 'First seed on each thread should be different'
  write(*,*) (rngs(i)%seed(0), i = 1, nThreads)

  end subroutine
  !====================================================================!

  !====================================================================!
  subroutine random_test(test, fixedSeed)
    !! graph: false
  !====================================================================!
  class(tester) :: test
  logical :: fixedSeed
  integer(i32) :: ia
  integer(i32), allocatable :: ia1D(:)
  real(r64) :: a, a1D(10), a2D(10, 10)
  character(len=:), allocatable :: cTest

  type(Prng) :: rng

  call Msg('==========================')
  call Msg('Testing : Random')
  call Msg('==========================')
!
!  write(*,1) 'Setting the random seed'
!
  rng = Prng(big = .true.)
  
!  !call setRNG([546420601, 1302718556, 802583095, 136684118, 1163051410, 592779069, 660876855, 767615536, 1788597594, 775517554, 657867655, 1334969129])
  
  call allocate(ia1D, 3)
  ia=1
  call rng%rngInteger(ia1D,ia, 5)
  write(*,1) 'Random integers'
  write(*,1) str(ia1D)
  call rng%rngNormal(a)
  write(*,1) 'Dble random number'
  write(*,1) str(a)
  call rng%rngNormal(a1D)
  write(*,1) '~N(mean=0.0,std=1.0)'
  write(*,1) str(a1D)
  call rng%rngNormal(a1D, 1.d0, 5.d0)
  write(*,1) '~N(mean=1.0,std=5.0) 1D array'
  write(*,1) str(a1D)
  a2D = 0.d0
  call rng%rngNormal(a2D,50.d0,10.d0)
  write(*,1) '~N(mean=50.0,std=10.0) 2D array reduced output'
  cTest = str(a2D)
  write(*,1) cTest
  1 format(a)
  end subroutine
  !====================================================================!

    !====================================================================!
  subroutine reallocate_test(test)
    !! graph: false
  !====================================================================!
  class(tester) :: test

  real(r32), allocatable :: ar1D(:), ar2D(:,:), ar3D(:,:,:)
  real(r64), allocatable :: a1D(:), a2D(:,:), a3D(:,:,:)
  integer(i32), allocatable :: ia1D(:), ia2D(:,:), ia3D(:,:,:)
  integer(i64), allocatable :: iad1D(:), iad2D(:,:), iad3D(:,:,:)
  complex(r32), allocatable :: z1D(:), z2D(:,:), z3D(:,:,:)
  complex(r64), allocatable :: zz1D(:), zz2D(:,:), zz3D(:,:,:)
  logical, allocatable :: la1D(:), la2D(:,:), la3D(:,:,:)

  call Msg('==========================')
  call Msg('Testing : Reallocate')
  call Msg('==========================')

  call allocate(ar1D, 10)
  ar1D = 1.0
  call reallocate(ar1D,50)
  call test%test(size(ar1D) == 50 .and. all(ar1D(1:10) == 1.0) .and. all(ar1D(11:50) == 0.0),'reallocate_r1D')

  call allocate(ar2D,[10,10])
  ar2D = 2.0
  call reallocate(ar2D,[5,7])
  call test%test(all(shape(ar2D) == [5,7]) .and. all(ar2D == 2.0),'reallocate_r2D')

  call allocate(ar3D,[10,10,10])
  ar3D = 3.0
  call reallocate(ar3D,[2,20,3])
  call test%test(all(shape(ar3D) == [2,20,3]) .and. all(ar3D(:,1:10,:) == 3.0) .and. all(ar3D(:,11:20,:) == 0.0),'reallocate_r3D')

  call allocate(a1D,100)
  a1D = 1.d0
  call reallocate(a1D,50)
  call test%test(size(a1D) == 50 .and. all(a1D == 1.d0),'reallocate_d1D')

  call allocate(a2D,[10,10])
  a2D = 2.d0
  call reallocate(a2D,[20,20])
  call test%test(all(shape(a2D) == [20,20]) .and. all(a2D(1:10,1:10) == 2.d0) .and. all(a2D(11:20,11:20) == 0.d0),'reallocate_d2D')

  call allocate(a3D,[10,10,10])
  a3D = 3.d0
  call reallocate(a3D,[20,20,20])
  call test%test(all(shape(a3D) == [20,20,20]) .and. all(a3D(1:10,1:10,1:10) == 3.d0) .and. all(a3D(11:20,11:20,11:20) == 0.d0),'reallocate_d3D')

  call allocate(ia1D,100)
  ia1D = 1
  call reallocate(ia1D,50)
  call test%test(size(ia1D) == 50 .and. all(ia1d == 1),'reallocate_i1D')

  call allocate(ia2D,[10,10])
  ia2D = 2
  call reallocate(ia2D,[20,20])
  call test%test(all(shape(ia2D) == [20,20]) .and. all(ia2d(1:10,1:10) == 2) .and. all(ia2D(11:20,11:20) == 0),'reallocate_i2D')

  call allocate(ia3D,[10,10,10])
  ia3D = 3
  call reallocate(ia3D,[20,20,20])
  call test%test(all(shape(ia3D) == [20,20,20]) .and. all(ia3d(1:10,1:10,1:10) == 3) .and. all(ia3D(11:20,11:20,11:20) == 0),'reallocate_i3D')

  call allocate(iad1D,100)
  iad1D = 1
  call reallocate(iad1D,50)
  call test%test(size(iad1D) == 50 .and. all(iad1d == 1),'reallocate_id1D')

  call allocate(iad2D,[10,10])
  iad2d = 2
  call reallocate(iad2D,[20,20])
  call test%test(all(shape(iad2D) == [20,20]) .and. all(iad2d(1:10,1:10) == 2) .and. all(iad2D(11:20,11:20) == 0),'reallocate_id2D')

  call allocate(iad3D,[10,10,10])
  iad3d = 3
  call reallocate(iad3D,[20,20,20])
  call test%test(all(shape(iad3D) == [20,20,20]) .and. all(iad3d(1:10,1:10,1:10) == 3)  .and. all(iad3D(11:20,11:20,11:20) == 0),'reallocate_id3D')

  call allocate(z1D,100)
  z1D = (1.d0, 0.d0)
  call reallocate(z1D,50)
  call test%test(size(z1D) == 50 .and. all(z1D == (1.d0,0.d0)),'reallocate_c1D')

  call allocate(z2D,[10,10])
  z2D = (2.d0, 0.d0)
  call reallocate(z2D,[20,20])
  call test%test(all(shape(z2D) == [20,20]) .and. all(z2D(1:10,1:10) == (2.d0, 0.d0)) .and. all(z2D(11:20,11:20) == (0.d0, 0.d0)),'reallocate_c2D')

  call allocate(z3D,[10,10,10])
  z3D = (3.d0, 0.d0)
  call reallocate(z3D,[20,20,20])
  call test%test(all(shape(z3D) == [20,20,20]) .and. all(z3D(1:10,1:10,1:10) == (3.d0, 0.d0)) .and. all(z3D(11:20,11:20,11:20) == (0.d0, 0.d0)),'reallocate_c3D')

  call allocate(zz1D,100)
  zz1D = (1.d0, 0.d0)
  call reallocate(zz1D,50)
  call test%test(size(zz1D) == 50 .and. all(zz1D == (1.d0,0.d0)),'reallocate_z1D')

  call allocate(zz2D,[10,10])
  zz2D = (2.d0, 0.d0)
  call reallocate(zz2D,[20,20])
  call test%test(all(shape(zz2D) == [20,20]) .and. all(zz2D(1:10,1:10) == (2.d0, 0.d0)) .and. all(zz2D(11:20,11:20) == (0.d0, 0.d0)),'reallocate_z2D')

  call allocate(zz3D,[10,10,10])
  zz3D = (3.d0, 0.d0)
  call reallocate(zz3D,[20,20,20])
  call test%test(all(shape(zz3D) == [20,20,20]) .and. all(zz3D(1:10,1:10,1:10) == (3.d0, 0.d0)) .and. all(zz3D(11:20,11:20,11:20) == (0.d0, 0.d0)),'reallocate_z3D')

  call allocate(la1D,100)
  la1D = .true.
  call reallocate(la1D,50)
  call test%test(size(la1D) == 50 .and. all(la1D .eqv. .true.),'reallocate_l1D')

  call allocate(la2D,[10,10])
  la2D = .true.
  call reallocate(la2D,[20,20])
  call test%test(all(shape(la2D) == [20,20]) .and. all(la2D(1:10,1:10) .eqv. .true.) .and. all(la2D(11:20,11:20) .eqv. .false.),'reallocate_l2D')

  call allocate(la3D,[10,10,10])
  la3D = .true.
  call reallocate(la3D,[20,20,20])
  call test%test(all(shape(la3D) == [20,20,20]) .and. all(la3D(1:10,1:10,1:10) .eqv. .true.) .and. all(la3D(11:20,11:20,11:20) .eqv. .false.),'reallocate_l3D')

  deallocate(ar1D,ar2D,ar3D,a1D,a2D,a3D,ia1D,ia2D,ia3D)
  deallocate(iad1D,iad2D,iad3D,z1D,z2D,z3D,zz1D,zz2D,zz3D)
  deallocate(la1D,la2D,la3D)

  end subroutine
  !====================================================================!

  !====================================================================!
  subroutine select_test(test, N)
    !! graph: false
  !====================================================================!
  class(tester) :: test
  integer(i32) :: N

  real(r32) :: ar
  real(r32), allocatable :: ar1D(:), br1D(:), cr1D(:)
  real(r64) :: a
  real(r64), allocatable :: a1D(:), b1D(:), c1D(:)
  integer(i32) :: i,ia, ib, ic
  integer(i32), allocatable :: ia1D(:), ib1D(:), ic1D(:), id1D(:)
  integer(i64) :: iad
  integer(i64), allocatable :: iad1D(:), ibd1D(:), icd1D(:)
  logical :: la, lb

  call Msg('==========================')
  call Msg('Testing : Select')
  call Msg('==========================')

  call allocate(ar1D, N)
  call allocate(br1D, N)
  call allocate(cr1D, N)

  call allocate(a1D, N)

  call allocate(ia1D, N)
  call allocate(ic1D, N)

  call rngNormal(a1D)
  ar1D = real(a1D)
  call rngInteger(ia1D, 1, N)

  br1D = ar1D
  ic = (size(br1D)+1)/2 ! Get the median
  call select(br1D, ic, ar)

  la = all(br1D(1:ic-1) <= br1D(ic)) .and. all(br1D(ic+1:N) >= br1D(ic))
  call sort(br1D)
  call test%test(ar == br1D(ic) .and. la, 'quickselect_r1D')

  br1D = ar1D
  ic = 3
  call select(br1D, ic, ar)

  la = all(br1D(1:ic-1) <= br1D(ic)) .and. all(br1D(ic+1:N) >= br1D(ic))
  call sort(br1D)
  call test%test(ar == br1D(ic) .and. la, 'quickselect_r1D')

  br1D = ar1D
  call arange(ic1D, 1, N)
  ic = (size(br1D)+1)/2 ! Get the median
  call argSelect(br1D,ic1D, ic, ia)
  do i = 1, N
    cr1D(i) = br1D(ic1D(i))
  enddo
  
  la = all(cr1D(1:ic-1) <= cr1D(ic)) .and. all(cr1D(ic+1:N) >= cr1D(ic))
  call test%test(la,'argQuickSelect_r1D')

  call deallocate(ar1D)
  call deallocate(br1D)
  call deallocate(cr1D)

  call allocate(b1D, N)
  call allocate(c1D, N)

  b1D = a1D
  ic = (size(b1D)+1)/2 ! Get the median
  call select(b1D, ic, a)

  la = all(b1D(1:ic-1) <= b1D(ic)) .and. all(b1D(ic+1:N) >= b1D(ic))
  call sort(b1D)
  call test%test(a == b1D(ic) .and. la, 'quickselect_d1D')

  b1D = a1D
  ic = (size(b1D)+1)/2 ! Get the median
  call arange(ic1D, 1, N)
  call argSelect(b1D, ic1D, ic, ia)
  do i = 1, N
    c1D(i) = b1D(ic1D(i))
  enddo
  lb = all(c1D(1:ic-1) < c1D(ic)) .and. all(c1D(ic+1:N) > c1D(ic))
  call test%test(la, 'argQuickselect_d1D')

  call deallocate(a1D)
  call deallocate(b1D)
  call deallocate(c1D)

  call allocate(ib1D, N)
  call allocate(id1D, N)

  ib1D = ia1D
  ic = (size(ib1D)+1)/2 ! Get the median
  call select(ib1D, ic, ia)

  la = all(ib1D(1:ic-1) <= ib1D(ic)) .and. all(ib1D(ic+1:N) >= ib1D(ic))
  call sort(ib1D)
  call test%test(ia == ib1D(ic) .and. la, 'quickselect_i1D')

  ib1D = ia1D
  ic = (size(ib1D) + 1)/2 ! Get the median
  call arange(ic1D, 1, N)
  call argSelect(ib1D, ic1D, ic, ia)
  do i = 1, N
    id1D(i) = ib1D(ic1D(i))
  enddo
  lb = all(id1D(1:ic-1) < id1D(ic)) .and. all(id1D(ic+1:N) > id1D(ic))
  call test%test(la, 'argQuickselect_i1D')

  call allocate(iad1D, N)
  call allocate(ibd1D, N)
  call allocate(icd1D, N)

  ibd1D = ia1D
  ic = (size(ibd1D+1))/2 ! Get the median
  call select(ibd1D, ic, iad)

  la = all(ibd1D(1:ic-1) <= ibd1D(ic)) .and. all(ibd1D(ic+1:N) >= ibd1D(ic))
  call sort(ibd1D)
  call test%test(iad == ibd1D(ic) .and. la, 'quickselect_id1D')

  ibd1D = ia1D
  ic = (size(ibd1D+1))/2 ! Get the median
  call arange(ic1D, 1, N)
  call argSelect(ibd1D, ic1D, ic, ia)
  do i = 1, N
    icd1D(i) = ibd1D(ic1D(i))
  enddo
  lb = all(icd1D(1:ic-1) < icd1D(ic)) .and. all(icd1D(ic+1:N) > icd1D(ic))
  call test%test(la, 'argQuickselect_id1D')

  end subroutine
  !====================================================================!

  !====================================================================!
  subroutine sorting_test(test, N)
    !! graph: false
  !====================================================================!
  class(tester) :: test
  integer(i32) :: N

  real(r32), allocatable :: ar1D(:), br1D(:)
  real(r64), allocatable :: a1D(:), b1D(:)
  integer(i32), allocatable :: ia1D(:), ib1D(:), ic1D(:)
  integer(i64), allocatable :: iad1D(:), ibd1D(:)

  call Msg('==========================')
  call Msg('Testing : Sorting')
  call Msg('==========================')

!   Initial setup for testing
  call allocate(ar1D, N)
  call allocate(br1D, N)

  call allocate(a1D, N)
  call allocate(b1D, N)

  call allocate(ia1D, N)
  call allocate(ib1D, N)
  call allocate(ic1D, N)

  call allocate(iad1D, N)
  call allocate(ibd1D, N)

  call rngNormal(a1D)
  ar1D = real(a1D)

  br1D = ar1D
  call sort(br1D)
  call test%test(isSorted(br1D),'Introsort_r1D')

  br1D = ar1D
  call arange(ia1D, 1, N)
  call argsort(br1D, ia1D)
  call test%test(isSorted(br1D,ia1D),'argIntrosort_r1D')

  br1D = ar1D
  call Sort(br1D,.true.)
  call test%test(isSorted(br1D),'Mergesort_r1D')

  br1D = ar1D
  call arange(ia1D, 1, N)
  call argsort(br1D, ia1D,.true.)
  call test%test(isSorted(br1D,ia1D),'argMergesort_r1D')

  b1D = a1D
  call sort(b1D)
  call test%test(isSorted(b1D),'Introsort_d1D on Sorted array')

  b1D = a1D
  call arange(ia1D, 1, N)
  call argsort(b1D, ia1D)
  call test%test(isSorted(b1D,ia1D),'argIntrosort_d1D')

  b1D = a1D
  call Sort(b1D,.true.)
  call test%test(isSorted(b1D),'Mergesort_d1D')

  b1D = a1D
  call arange(ia1D, 1, N)
  call argsort(b1D, ia1D,.true.)
  call test%test(isSorted(b1D,ia1D),'argMergesort_d1D')

  call rngInteger(ia1D,1, N)
  ib1D = ia1D
  call sort(ib1D)
  call test%test(isSorted(ib1D),'Introsort_i1D')

  ib1D = ia1D
  call arange(ic1D, 1, N)
  call argsort(ib1D,ic1D)
  call test%test(isSorted(ib1D,ic1D),'argIntrosort_i1D')

  ib1D = ia1D
  call sort(ib1D,.true.)
  call test%test(isSorted(ib1D),'Mergesort_i1D')

  ib1D = ia1D
  call arange(ic1D, 1, N)
  call argsort(ib1D,ic1D,.true.)
  call test%test(isSorted(ib1D,ic1D),'argMergesort_i1D')

  end subroutine
  !====================================================================!

  !====================================================================!
  subroutine strings_test(test)
    !! graph: false
  !====================================================================!
  class(tester) :: test

  character(len=:), allocatable :: cTest
  real(r64) :: a
  real(r64), allocatable :: a1D(:), a2D(:,:)
  integer(i32) :: ia, istat


  call Msg('==========================')
  call Msg('Testing : Strings')
  call Msg('==========================')

  cTest = 'aBcDeFgH   7483027401'
  call test%test(lowerCase(cTest) == 'abcdefgh   7483027401','lowerCase')
  call test%test(upperCase(cTest) == 'ABCDEFGH   7483027401','upperCase')
  call test%test(isString(cTest,'aBcDeFgH   7483027401') .eqv. .true.,'isString')
  call test%test(isString(cTest,'abcdefgh   7483027401',.true.) .eqv. .false.,'isString')
  a = 1.d0
  write(*,1) str(a)
  call test%test(str(a) == '1. ', 'str(r64)')
  a = 5.6d-5
  write(*,1) str(a)
  call test%test(str(a) == '5.600E-05 ','str(r64)')

  a = 3.217986d24
  printOptions%precision = 6
  write(*,1) str(a)
  call test%test(str(a) == '3.217986E+24 ','str(r64)')

  a = 3.217986d-24
  printOptions%precision = 8
  write(*,1) str(a)
  call test%test(str(a) == '3.21798600E-24 ','str(r64)')

  a = 0.d0
  printOptions%precision = 6
  write(*,1) str(a)
  call test%test(str(a) == '0. ','str(r64)')

  a = 4.d3
  printOptions%precision = 3
  write(*,1) str(a)
  call test%test(str(a) == '4000. ','str(r64)')

  ia = 9999
  write(*,1) str(ia)
  call test%test(str(ia) == '9999 ','str(i32)')

  call allocate(a1D, 5)
  a1D = 0.d0
  cTest = str(a1D)
  write(*,1) 'str(1D dble array)'//new_line('a')//trim(cTest)
  call test%test(trim(cTest) == '0. 0. 0. 0. 0.','str(1D dble array)')

  call allocate(a2D, [3,3])
  a2D = 0.d0
  cTest = str(a2D(1:3,1:3))
  write(*,1) 'str(2D dble array(3x3))'//new_line('a')//trim(cTest)
  call test%test(trim(cTest) == '0. 0. 0. '//new_line('a')//'0. 0. 0. '//new_line('a')//'0. 0. 0.','str(2D dble array(3x3))')

  call allocate(a2D, [10,10])
  a2D = 0.d0
  cTest = str(a2D)
  write(*,1) 'str(2D dble array(10x10)) with reduced output'//new_line('a')//trim(cTest)
  call test%test(trim(cTest) == &
    '0. 0. 0. ... 0. 0. 0. '//new_line('a') &
  //'0. 0. 0. ... 0. 0. 0. '//new_line('a') &
  //'0. 0. 0. ... 0. 0. 0. '//new_line('a') &
  //'...'//new_line('a')&
  //'0. 0. 0. ... 0. 0. 0. '//new_line('a') &
  //'0. 0. 0. ... 0. 0. 0. '//new_line('a') &
  //'0. 0. 0. ... 0. 0. 0. ','str(2D dble array(10x10))')

  call test%test(str(.true.) == 'True ','str(L)')
  call test%test(str(.false.) == 'False ','str(L)')
  cTest = 'a      b, c; '//achar(9)//'d. e f g '
  call compact(cTest)
  call test%test(trim(cTest) == 'a b, c; d. e f g','compact')
  call test%test(countEntries(cTest) == 7,'countEntries')
  call test%test(hasNentries(cTest,7),'hasNentries')
  cTest = prependString(cTest,'Stuff',';')
  call test%test(trim(cTest) == 'Stuff;a b, c; d. e f g','prependString')
  cTest = appendString(cTest,'Stuff','#')
  call test%test(trim(cTest) == 'Stuff;a b, c; d. e f g#Stuff','prependString')
  call replaceDelim(cTest,';',' ')
  call test%test(trim(cTest) == 'Stuff a b, c  d. e f g#Stuff','replaceDelim')
  cTest = 'stuff ! Here is a comment'
  call removeComments(cTest)
  call test%test(trim(cTest) == 'stuff','removeComments')
  cTest = '1 2 3 4'
  call read1Integer(cTest,ia,istat)
  call test%test(ia == 1,'read1integer')

  call deallocate(a1D)
  call deallocate(a2D)
  1 format(a)
  end subroutine
  !====================================================================!

  !=====================================================================!
  subroutine time_test(test)
    !! graph: false
  !=====================================================================!
  class(tester) :: test
  call Msg('==========================')
  call Msg('Testing : time')
  call Msg('==========================')
  call test%test(timeinseconds([0,0,0,0,0,0,0,8]) == 8.d-3,'timeinseconds')
  call test%test(timeinseconds([0,0,0,0,0,0,1,0]) == 1.d0 ,'timeinseconds')
  call test%test(timeinseconds([0,0,0,0,0,1,0,0]) == 60.d0,'timeinseconds')
  call test%test(timeinseconds([0,0,0,0,1,0,0,0]) == 3600.d0,'timeinseconds')
  call test%test(timeinseconds([0,0,1,0,0,0,0,0]) == 86400.d0,'timeinseconds')
  call test%test(timeinseconds([0,0,1,0,1,1,1,8]) == 90061.008d0,'timeinseconds')
  call test%test(daysinMonth(2,2012) == 29,'daysinMonth')
  call test%test(daysinMonth(2,2014) == 28,'daysinMonth')
  call test%test(daysinYear(2012) == 366,'daysinYear')
  call test%test(isLeapYear(2012).eqv. .true.,'isLeapYear')
  call test%test(secondsToHMS(90031.008d0) == '25: 0:31.  8 (h:m:s)','secondsToHMS')
  end subroutine
  !=====================================================================!
end module