program test_libFortran
  !! Test program for the libFortran library
  use iso_fortran_env
  use variableKind
  use m_array1D
  use m_errors
  use m_fileIO
  use m_indexing
  use m_random
  use m_readLine
  use m_strings
  use m_writeLine
  use m_time
  use m_allocate
  use m_reallocate
  use m_copy
  ! Sorting routines
  use m_Sort
  !use m_PartialQuicksort
  use m_quickSelect
  use m_maths

  use Stopwatch_Class
  use ProgressBar_Class
  implicit none

  character(len=100) :: fName
  character(len=128) :: sa
  character(len=:), allocatable :: cTest
  logical :: lTest
  integer(i32) :: iTest,istat,N, nIterations
  real(r32) :: ar, br, cr
  real(r32),allocatable :: ar1D(:),br1D(:),cr1D(:)
  real(r32),allocatable :: ar2D(:,:),br2D(:,:)
  real(r32),allocatable :: ar3D(:,:,:),br3D(:,:,:)
  real(r64) :: a,b,c
  real(r64),allocatable :: a1D(:),b1D(:),c1D(:)
  real(r64),allocatable :: a2D(:,:),b2D(:,:)
  real(r64),allocatable :: a3D(:,:,:),b3D(:,:,:)
  integer(i32) :: ia,ib,ic,id
  integer(i32), allocatable :: ia1D(:),ib1D(:),ic1D(:)
  integer(i32), allocatable :: ia2D(:,:),ib2D(:,:)
  integer(i32), allocatable :: ia3D(:,:,:),ib3D(:,:,:)
  integer(i64) :: iad,ibd,icd
  integer(i64), allocatable :: iad1D(:),ibd1D(:),icd1D(:)
  integer(i64), allocatable :: iad2D(:,:),ibd2D(:,:)
  integer(i64), allocatable :: iad3D(:,:,:),ibd3D(:,:,:)
  complex(r32) :: x,y,z
  complex(r32), allocatable :: za1D(:),zb1D(:)
  complex(r32), allocatable :: za2D(:,:),zb2D(:,:)
  complex(r32), allocatable :: za3D(:,:,:),zb3D(:,:,:)
  complex(r64), allocatable :: zza1D(:),zzb1D(:)
  complex(r64), allocatable :: zza2D(:,:),zzb2D(:,:)
  complex(r64), allocatable :: zza3D(:,:,:),zzb3D(:,:,:)
  logical :: la,lb,lc
  logical, allocatable :: la1D(:),lb1D(:)
  logical, allocatable :: la2D(:,:),lb2D(:,:)
  logical, allocatable :: la3D(:,:,:),lb3D(:,:,:)
  integer(i32) :: passCount = 0
  integer(i32) :: testTotal = 0
  type(Stopwatch) :: clk
  type(ProgressBar) :: P

! Get an integer from command line argument
  ib = command_argument_count()
  if (ib < 2) then
    write(*,'(a)') 'Error with input options'
    write(*,'(a)') 'Usage: libFortranTest size iterations'
    write(*,'(a)') '  size : Size of the array to run tests on'
    write(*,'(a)') '  iterations : Number of times to run a test that is being timed'
    stop
  end if
  call get_command_argument(1, sa)
  read(sa,*) N

  call get_command_argument(2, sa)
  read(sa,*) nIterations




  fName = 'testFile.txt'

  a = 1.d0 ; b = 2.d0 ; c = 3.d0
  a1D = [0.d0,1.d0,2.d0,3.d0,4.d0]
  b1D = [5.d0,6.d0,7.d0,8.d0,9.d0]
  c1D = [10.d0,11.d0,12.d0,13.d0,14.d0]

  call Msg('==========================')
  call Msg('LibFortran Testing')
  call Msg('==========================')

  call Msg('==========================')
  call Msg('Testing : file IO')
  call Msg('==========================')
  call deleteFile(fName) ! Make sure tests can work!
  call test(fileExists(fName) .eqv. .false.,'fileExists')
  call test(hasExtension(fName,'txt'),'hasExtension')
  call test(getExtension(fName) == 'txt','getExtension')
  call test(trimExtension(fName) == 'testFile','trimExtension')
  call test(.not. isOpen(fName),'isOpen')
  call openFile(fName,iTest,'unknown',istat)
  call test(istat == 0,'openFile')
  call test(isOpen(fName),'isOpen')
  call writeLine(a,fName,iTest)
  call writeLine(a,b,fName,iTest)
  call writeLine(a,b,c,fName,iTest)
  call writeLine(a,b1D,fName,iTest)
  call writeLine(a1D,b1D,c1D,fName,iTest)
  call closeFile(fName,iTest,'',istat)
  call test(istat == 0,'closeFile')
  lTest = fileExists(fName)
  call test(lTest .eqv. .true.,'fileExists')
  if (lTest .eqv. .false.) call eMsg('Make sure you change to the directory containing the executable before running the test')
  call test(.not.isOpen(fName),'isOpen')
  iTest = getFileSize(fName)
  call test(itest > 0,'getFileSize '//str(iTest)//'bytes')
  call openFile(fName,iTest,'unknown',istat)
  call skipFileLines(iTest,1)
  a = 0.d0 ; b = 0.d0 ; c = 0.d0
  a1D = 0.d0 ; b1D = 0.d0 ; c1D = 0.d0
  call readLine(a,b,fName,iTest)
  call readLine(a,b,c,fName,iTest)
  call readLine(a,b1D,fName,iTest)
  call readLine(a1D,b1D,c1D,fName,iTest)
  call test(a == 1.d0,'writeLine/readLine')
  call test(b == 2.d0,'writeLine/readLine')
  call test(c == 3.d0,'writeLine/readLine')
  call test(all(a1D == [0.d0,1.d0,2.d0,3.d0,4.d0]),'writeLine/readLine')
  call test(all(b1D == [5.d0,6.d0,7.d0,8.d0,9.d0]),'writeLine/readLine')
  call test(all(c1D == [10.d0,11.d0,12.d0,13.d0,14.d0]),'writeLine/readLine')
  call closeFile(fName,iTest,'delete',istat)
  call test(istat == 0,'closeFile + Delete')

  call Msg('==========================')
  call Msg('Testing : Strings')
  call Msg('==========================')

  cTest = 'aBcDeFgH   7483027401'
  call test(lowerCase(cTest) == 'abcdefgh   7483027401','lowerCase')
  call test(upperCase(cTest) == 'ABCDEFGH   7483027401','upperCase')
  call test(isString(cTest,'aBcDeFgH   7483027401') .eqv. .true.,'isString')
  call test(isString(cTest,'abcdefgh   7483027401',.true.) .eqv. .false.,'isString')
  a = 1.d0
  write(*,1) str(a)
  call test(str(a) == '1. ', 'str(r64)')
  a = 5.6d-5
  write(*,1) str(a)
  call test(str(a) == '5.600E-05 ','str(r64)')

  a = 3.217986d24
  printOptions%precision = 6
  write(*,1) str(a)
  call test(str(a) == '3.217986E+24 ','str(r64)')

  a = 3.217986d-24
  printOptions%precision = 8
  write(*,1) str(a)
  call test(str(a) == '3.21798600E-24 ','str(r64)')

  a = 0.d0
  printOptions%precision = 6
  write(*,1) str(a)
  call test(str(a) == '0. ','str(r64)')

  a = 4.d3
  printOptions%precision = 3
  write(*,1) str(a)
  call test(str(a) == '4000. ','str(r64)')

  ia = 9999
  write(*,1) str(ia)
  call test(str(ia) == '9999 ','str(i32)')

  a1D = 0.d0
  cTest = str(a1D)
  write(*,1) 'str(1D dble array)'//new_line('a')//trim(cTest)
  call test(trim(cTest) == '0. 0. 0. 0. 0.','str(1D dble array)')
  allocate(a2D(3,3))
  a2D = 0.d0
  cTest = str(a2D(1:3,1:3))
  write(*,1) 'str(2D dble array(3x3))'//new_line('a')//trim(cTest)
  call test(trim(cTest) == '0. 0. 0. '//new_line('a')//'0. 0. 0. '//new_line('a')//'0. 0. 0.','str(2D dble array(3x3))')
  deallocate(a2D)
  allocate(a2D(10,10))
  a2D = 0.d0
  cTest = str(a2D)
  write(*,1) 'str(2D dble array(10x10)) with reduced output'//new_line('a')//trim(cTest)
  call test(trim(cTest) == &
    '0. 0. 0. ... 0. 0. 0. '//new_line('a') &
  //'0. 0. 0. ... 0. 0. 0. '//new_line('a') &
  //'0. 0. 0. ... 0. 0. 0. '//new_line('a') &
  //'...'//new_line('a')&
  //'0. 0. 0. ... 0. 0. 0. '//new_line('a') &
  //'0. 0. 0. ... 0. 0. 0. '//new_line('a') &
  //'0. 0. 0. ... 0. 0. 0. ','str(2D dble array(10x10))')

  call test(str(.true.) == 'True ','str(L)')
  call test(str(.false.) == 'False ','str(L)')
  cTest = 'a      b, c; '//achar(9)//'d. e f g '
  call compact(cTest)
  call test(trim(cTest) == 'a b, c; d. e f g','compact')
  call test(countEntries(cTest) == 7,'countEntries')
  call test(hasNentries(cTest,7),'hasNentries')
  cTest = prependString(cTest,'Stuff',';')
  call test(trim(cTest) == 'Stuff;a b, c; d. e f g','prependString')
  cTest = appendString(cTest,'Stuff','#')
  call test(trim(cTest) == 'Stuff;a b, c; d. e f g#Stuff','prependString')
  call replaceDelim(cTest,';',' ')
  call test(trim(cTest) == 'Stuff a b, c  d. e f g#Stuff','replaceDelim')
  cTest = 'stuff ! Here is a comment'
  call removeComments(cTest)
  call test(trim(cTest) == 'stuff','removeComments')
  cTest = '1 2 3 4'
  call read1Integer(cTest,ia,istat)
  call test(ia == 1,'read1integer')

  call Msg('==========================')
  call Msg('Testing : Indexing')
  call Msg('==========================')

  ia1D = [3,4,7]
  ic1D = [3,5,9]
  ia = - 1
  ia = sub2ind(ia1D,ic1D)
  write(*,1) 'sub2ind([3,4,7],[3,5,9]) = 102'
  call test(ia == 102,'sub2ind')
  ia=99
  ia1D = 0
  ia1D = ind2sub(ia,ic1D)
  write(*,1) 'ind2sub(99,[3,5,9]) = [3,3,7]'
  call test(all(ia1D == [3,3,7]),'ind2sub')

  call Msg('==========================')
  call Msg('Testing : Random')
  call Msg('==========================')

  write(*,1) 'Setting the random seed'

  call setRNG(.true.)
  !call setRNG([546420601, 1302718556, 802583095, 136684118, 1163051410, 592779069, 660876855, 767615536, 1788597594, 775517554, 657867655, 1334969129])
!  ia=1
!  call rngInteger(ia1D,ia)
  write(*,1) 'Random integers'
  write(*,1) str(ia1D)
  call rngNormal(a)
  write(*,1) 'Dble random number'
  write(*,1) str(a)
  call rngNormal(a1D)
  write(*,1) '~N(0.0,1.0)'
  write(*,1) str(a1D)
  call rngNormal(a1D, 1.d0, 5.d0)
  write(*,1) '~N(1.0,5.0) 1D array'
  write(*,1) str(a1D)
  a2D = 0.d0
  call rngNormal(a2D,50.d0,10.d0)
  write(*,1) '~N(50.0,10.0) 2D array reduced output'
  cTest = str(a2D)
  write(*,1) cTest

  call Msg('==========================')
  call Msg('Testing : time')
  call Msg('==========================')
  call test(timeinseconds([0,0,0,0,0,0,0,8]) == 8.d-3,'timeinseconds')
  call test(timeinseconds([0,0,0,0,0,0,1,0]) == 1.d0 ,'timeinseconds')
  call test(timeinseconds([0,0,0,0,0,1,0,0]) == 60.d0,'timeinseconds')
  call test(timeinseconds([0,0,0,0,1,0,0,0]) == 3600.d0,'timeinseconds')
  call test(timeinseconds([0,0,1,0,0,0,0,0]) == 86400.d0,'timeinseconds')
  call test(timeinseconds([0,0,1,0,1,1,1,8]) == 90061.008d0,'timeinseconds')
  call test(daysinMonth(2,2012) == 29,'daysinMonth')
  call test(daysinMonth(2,2014) == 28,'daysinMonth')
  call test(daysinYear(2012) == 366,'daysinYear')
  call test(isLeapYear(2012).eqv. .true.,'isLeapYear')
  call test(absTimetoHMS(90031.008d0) == '25: 0:31.  8 (h:m:s)','absTimetoHMS')

  call Msg('==========================')
  call Msg('Testing : Stopwatch Class')
  call Msg('==========================')
  call clk%start('Generating Random Number')
  ic = nIterations
  do ib = 1, ic
    call rngNormal(a2D)
  enddo
  call clk%stop()
  write(output_unit,'(a)') 'Elapsed time '//clk%elapsed()
  write(output_unit,'(a)') 'Finished on '//clk%dateAndTime()

  call Msg('==========================')
  call Msg('Testing : ProgressBar Class')
  call Msg('==========================')
  P = ProgressBar(N=ic)
  call P%print(int(0,i64))
  do ib = 1, nIterations
    call rngNormal(a2D)
    call P%print(ib)
  enddo
  P=ProgressBar(N=ic,time = .true.)
  call P%print(int(0,i64))
  do ib = 1, nIterations
    call rngNormal(a2D)
    call P%print(ib)
  enddo

  call Msg('==========================')
  call Msg('Testing : Allocate')
  call Msg('==========================')

  call allocate(ar1D, 100)
  call test(size(ar1D) == 100,'allocate_r1D')
  call allocate(ar2D, [5,6])
  call test(all(shape(ar2D) == [5,6]),'allocate_r2D')
  call allocate(ar3D, [10,9,8])
  call test(all(shape(ar3D) == [10,9,8]),'allocate_r3D')
  call allocate(a1D, 100)
  call test(size(a1D) == 100,'allocate_d1D')
  call allocate(a2D, [5,6])
  call test(all(shape(a2D) == [5,6]),'allocate_d2D')
  call allocate(a3D, [10,9,8])
  call test(all(shape(a3D) == [10,9,8]),'allocate_d3D')
  call allocate(ia1D, 100)
  call test(size(ia1D) == 100,'allocate_i1D')
  call allocate(ia2D, [5,6])
  call test(all(shape(ia2D) == [5,6]),'allocate_i2D')
  call allocate(ia3D, [10,9,8])
  call test(all(shape(ia3D) == [10,9,8]),'allocate_i3D')
  call allocate(iad1D, 100)
  call test(size(iad1D) == 100,'allocate_id1D')
  call allocate(iad2D, [5,6])
  call test(all(shape(iad2D) == [5,6]),'allocate_id2D')
  call allocate(iad3D, [10,9,8])
  call test(all(shape(iad3D) == [10,9,8]),'allocate_id3D')
  call allocate(za1D, 100)
  call test(size(za1D) == 100,'allocate_c1D')
  call allocate(za2D, [5,6])
  call test(all(shape(za2D) == [5,6]),'allocate_c2D')
  call allocate(za3D, [10,9,8])
  call test(all(shape(za3D) == [10,9,8]),'allocate_c3D')
  call allocate(zza1D, 100)
  call test(size(zza1D) == 100,'allocate_z1D')
  call allocate(zza2D, [5,6])
  call test(all(shape(zza2D) == [5,6]),'allocate_z2D')
  call allocate(zza3D, [10,9,8])
  call test(all(shape(zza3D) == [10,9,8]),'allocate_z3D')
  call allocate(la1D, 100)
  call test(size(la1D) == 100,'allocate_l1D')
  call allocate(la2D, [5,6])
  call test(all(shape(la2D) == [5,6]),'allocate_l2D')
  call allocate(la3D, [10,9,8])
  call test(all(shape(la3D) == [10,9,8]),'allocate_l3D')

  call Msg('==========================')
  call Msg('Testing : Reallocate')
  call Msg('==========================')

  deallocate(a1D,a2D,ia1D)
  ar = 1.d0
  ar1D = ar
  call reallocate(ar1D,50)
  call test(size(ar1D) == 50 .and. all(ar1D == ar),'reallocate_r1D')

  call allocate(ar2D,[10,10])
  ar=2.d0
  ar2D = ar
  call reallocate(ar2D,[5,7])
  call test(all(shape(ar2D) == [5,7]) .and. all(ar2D == ar),'reallocate_r2D')

  call allocate(ar3D,[10,10,10])
  ar3D = 3.d0
  call reallocate(ar3D,[2,20,3])
  call test(all(shape(ar3D) == [2,20,3]) .and. all(ar3D(:,1:10,:) == 3.d0) .and. all(ar3D(:,11:20,:) == 0.d0),'reallocate_r3D')

  call allocate(a1D,100)
  a1D = 1.d0
  call reallocate(a1D,50)
  call test(size(a1D) == 50 .and. all(a1D == 1.d0),'reallocate_d1D')

  call allocate(a2D,[10,10])
  a2D = 2.d0
  call reallocate(a2D,[20,20])
  call test(all(shape(a2D) == [20,20]) .and. all(a2D(1:10,1:10) == 2.d0) .and. all(a2D(11:20,11:20) == 0.d0),'reallocate_d2D')

  call allocate(a3D,[10,10,10])
  a3D = 3.d0
  call reallocate(a3D,[20,20,20])
  call test(all(shape(a3D) == [20,20,20]) .and. all(a3D(1:10,1:10,1:10) == 3.d0) .and. all(a3D(11:20,11:20,11:20) == 0.d0),'reallocate_d3D')

  call allocate(ia1D,100)
  ia1D = 1
  call reallocate(ia1D,50)
  call test(size(ia1D) == 50 .and. all(ia1d == 1),'reallocate_i1D')

  call allocate(ia2D,[10,10])
  ia2D = 2
  call reallocate(ia2D,[20,20])
  call test(all(shape(ia2D) == [20,20]) .and. all(ia2d(1:10,1:10) == 2) .and. all(ia2D(11:20,11:20) == 0),'reallocate_i2D')

  call allocate(ia3D,[10,10,10])
  ia3D = 3
  call reallocate(ia3D,[20,20,20])
  call test(all(shape(ia3D) == [20,20,20]) .and. all(ia3d(1:10,1:10,1:10) == 3) .and. all(ia3D(11:20,11:20,11:20) == 0),'reallocate_i3D')

  call allocate(iad1D,100)
  iad1D = 1
  call reallocate(iad1D,50)
  call test(size(iad1D) == 50 .and. all(iad1d == 1),'reallocate_id1D')

  call allocate(iad2D,[10,10])
  iad2d = 2
  call reallocate(iad2D,[20,20])
  call test(all(shape(iad2D) == [20,20]) .and. all(iad2d(1:10,1:10) == 2) .and. all(iad2D(11:20,11:20) == 0),'reallocate_id2D')

  call allocate(iad3D,[10,10,10])
  iad3d = 3
  call reallocate(iad3D,[20,20,20])
  call test(all(shape(iad3D) == [20,20,20]) .and. all(iad3d(1:10,1:10,1:10) == 3)  .and. all(iad3D(11:20,11:20,11:20) == 0),'reallocate_id3D')

  call allocate(za1D,100)
  za1D = (1.d0, 0.d0)
  call reallocate(za1D,50)
  call test(size(za1D) == 50 .and. all(za1D == (1.d0,0.d0)),'reallocate_c1D')

  call allocate(za2D,[10,10])
  za2D = (2.d0, 0.d0)
  call reallocate(za2D,[20,20])
  call test(all(shape(za2D) == [20,20]) .and. all(za2D(1:10,1:10) == (2.d0, 0.d0)) .and. all(za2D(11:20,11:20) == (0.d0, 0.d0)),'reallocate_c2D')

  call allocate(za3D,[10,10,10])
  za3D = (3.d0, 0.d0)
  call reallocate(za3D,[20,20,20])
  call test(all(shape(za3D) == [20,20,20]) .and. all(za3D(1:10,1:10,1:10) == (3.d0, 0.d0)) .and. all(za3D(11:20,11:20,11:20) == (0.d0, 0.d0)),'reallocate_c3D')

  call allocate(zza1D,100)
  zza1D = (1.d0, 0.d0)
  call reallocate(zza1D,50)
  call test(size(zza1D) == 50 .and. all(zza1D == (1.d0,0.d0)),'reallocate_z1D')

  call allocate(zza2D,[10,10])
  zza2D = (2.d0, 0.d0)
  call reallocate(zza2D,[20,20])
  call test(all(shape(zza2D) == [20,20]) .and. all(zza2D(1:10,1:10) == (2.d0, 0.d0)) .and. all(zza2D(11:20,11:20) == (0.d0, 0.d0)),'reallocate_z2D')

  call allocate(zza3D,[10,10,10])
  zza3D = (3.d0, 0.d0)
  call reallocate(zza3D,[20,20,20])
  call test(all(shape(zza3D) == [20,20,20]) .and. all(zza3D(1:10,1:10,1:10) == (3.d0, 0.d0)) .and. all(zza3D(11:20,11:20,11:20) == (0.d0, 0.d0)),'reallocate_z3D')

  call allocate(la1D,100)
  la1D = .true.
  call reallocate(la1D,50)
  call test(size(la1D) == 50 .and. all(la1D .eqv. .true.),'reallocate_l1D')

  call allocate(la2D,[10,10])
  la2D = .true.
  call reallocate(la2D,[20,20])
  call test(all(shape(la2D) == [20,20]) .and. all(la2D(1:10,1:10) .eqv. .true.) .and. all(la2D(11:20,11:20) .eqv. .false.),'reallocate_l2D')

  call allocate(la3D,[10,10,10])
  la3D = .true.
  call reallocate(la3D,[20,20,20])
  call test(all(shape(la3D) == [20,20,20]) .and. all(la3D(1:10,1:10,1:10) .eqv. .true.) .and. all(la3D(11:20,11:20,11:20) .eqv. .false.),'reallocate_l3D')

  call Msg('==========================')
  call Msg('Testing : Copy')
  call Msg('==========================')

  call copy(ar1D,br1D)
  call test(all(ar1D == br1D),'copy_r1D')
  call copy(ar2D,br2D)
  call test(all(ar2D == br2D),'copy_r2D')
  call copy(ar3D,br3D)
  call test(all(ar3D == br3D),'copy_r3D')
  call copy(a1D,b1D)
  call test(all(a1D == b1D),'copy_d1D')
  call copy(a2D,b2D)
  call test(all(a2D == b2D),'copy_d2D')
  call copy(a3D,b3D)
  call test(all(a3D == b3D),'copy_d3D')
  call copy(ia1D,ib1D)
  call test(all(ia1D == ib1D),'copy_i1D')
  call copy(ia2D,ib2D)
  call test(all(ia2D == ib2D),'copy_i2D')
  call copy(ia3D,ib3D)
  call test(all(ia3D == ib3D),'copy_i3D')
  call copy(iad1D,ibd1D)
  call test(all(iad1D == ibd1D),'copy_id1D')
  call copy(iad2D,ibd2D)
  call test(all(iad2D == ibd2D),'copy_id2D')
  call copy(iad3D,ibd3D)
  call test(all(iad3D == ibd3D),'copy_id3D')
  call copy(za1D,zb1D)
  call test(all(za1D == zb1D),'copy_c1D')
  call copy(za2D,zb2D)
  call test(all(za2D == zb2D),'copy_c2D')
  call copy(za3D,zb3D)
  call test(all(za3D == zb3D),'copy_c3D')
  call copy(zza1D,zzb1D)
  call test(all(zza1D == zzb1D),'copy_z1D')
  call copy(zza2D,zzb2D)
  call test(all(zza2D == zzb2D),'copy_z2D')
  call copy(zza3D,zzb3D)
  call test(all(zza3D == zzb3D),'copy_z3D')
  call copy(la1D,lb1D)
  call test(all(la1D .eqv. lb1D),'copy_l1D')
  call copy(la2D,lb2D)
  call test(all(la2D .eqv. lb2D),'copy_l2D')
  call copy(la3D,lb3D)
  call test(all(la3D .eqv. lb3D),'copy_l3D')

  deallocate(ar1D,ar2D,ar3D)
  deallocate(a1D,a2D,a3D)
  deallocate(ia1D,ia2D,ia3D)
  deallocate(iad1D,iad2D,iad3D)
  deallocate(za1D,za2D,za3D)
  deallocate(zza1D,zza2D,zza3D)
  deallocate(la1D,la2D,la3D)

  deallocate(br1D,br2D,br3D)
  deallocate(b1D,b2D,b3D)
  deallocate(ib1D,ib2D,ib3D)
  deallocate(ibd1D,ibd2D,ibd3D)
  deallocate(zb1D,zb2D,zb3D)
  deallocate(zzb1D,zzb2D,zzb3D)
  deallocate(lb1D,lb2D,lb3D)

  call Msg('==========================')
  call Msg('Testing : Sorting')
  call Msg('==========================')

  ! Initial setup for testing
  call allocate(ar1D, N)
  call allocate(br1D, N)
  allocate(a1D(N))
  allocate(ia1D(N))
  call allocate(ic1D, N)
  call allocate(b1D, N)


  call rngNormal(a1D)
  ar1D = real(a1D)

  br1D = ar1D
  call sort(br1D)
  call test(isSorted(br1D),'Introsort_r1D')

  br1D = ar1D
  ia1D = [(ia, ia=1, N)]
  call argsort(br1D, ia1D)
  call test(isSorted(br1D(ia1D)),'argIntrosort_r1D')

  br1D = ar1D
  call Sort(br1D,.true.)
  call test(isSorted(br1D),'Mergesort_r1D')

  br1D = ar1D
  ia1D = [(ia, ia=1, N)]
  call argsort(br1D, ia1D,.true.)
  call test(isSorted(br1D(ia1D)),'argMergesort_r1D')


  call rngNormal(a1D)

  b1D = a1D
  call sort(b1D)
  call test(isSorted(b1D),'Introsort_d1D on Sorted array')

  b1D = a1D
  ia1D = [(ia, ia=1, N)]
  call argsort(b1D, ia1D)
  call test(isSorted(b1D(ia1D)),'argIntrosort_d1D')

  b1D = a1D
  call Sort(b1D,.true.)
  call test(isSorted(b1D),'Mergesort_d1D')
  b=b1D(N/2)

  b1D = a1D
  ia1D = [(ia, ia=1, N)]
  call argsort(b1D, ia1D,.true.)
  call test(isSorted(b1D(ia1D)),'argMergesort_d1D')

  call rngInteger(ia1D,1)
  ib1D = ia1D
  call sort(ib1D)
  call test(isSorted(ib1D),'Introsort_i1D')

  ib1D = ia1D
  ic1D = [(ib, ib=1, N)]
  call argsort(ib1D,ic1D)
  call test(isSorted(ib1D(ic1D)),'argIntrosort_i1D')

  ib1D = ia1D
  call sort(ib1D,.true.)
  call test(isSorted(ib1D),'Mergesort_i1D')

  ib1D = ia1D
  ic1D = [(ib, ib=1, N)]
  call argsort(ib1D,ic1D,.true.)
  call test(isSorted(ib1D(ic1D)),'argMergesort_i1D')

  call Msg('---  Timing the double precision introspection sort ---')
  call clk%restart()
  ib = nIterations
  do ia = 1, ib
    b1D = a1D
    call sort(b1D)
  enddo
  call clk%stop()
  write(*,'(a)') 'Total time for '//str(ib)//' sorts of length '//str(N)//str(clk%elapsedInSeconds())
  write(*,'(a)') 'Time: '//str(clk%elapsedInSeconds() / dble(ib))//'seconds per sort of length '//str(N)
  call test(isSorted(b1D),'Introsort_d1D')

  call Msg('---  Timing an already sorted double precision introspection sort ---')
  call clk%restart()
  ib = nIterations
  b1D = a1D
  call sort(b1D)
  do ia = 1, ib
    call sort(b1D)
  enddo
  call clk%stop()
  write(*,'(a)') 'Total time for '//str(ib)//' sorts of length '//str(N)//str(clk%elapsedInSeconds())
  write(*,'(a)') 'Time: '//str(clk%elapsedInSeconds() / dble(ib))//'seconds per sort of length '//str(N)
  call test(isSorted(b1D),'Introsort_d1D')



  call Msg('==========================')
  call Msg('Testing : Quick Select')
  call Msg('==========================')

  b1D = a1D
  ia = 1
  ib = size(b1D)
  ic = (ib+ia)/2

  b1D = a1D
  ic = ib/2 ! Middle location
  a = quickSelect(b1D, ic)

  la = all(b1D(1:ic-1) <= b1D(ic)) .and. all(b1D(ic+1:ib) >= b1D(ic))
  call sort(b1D)
  call test(a == b1D(ic) .and. la, 'quickselect_d1D')

  b1D = a1D
  ia1D = [(ia, ia=1, N)]
  a = argQuickSelect(b1D, ia1D, ic)
  la = a == b1D(ia1D(ic))
  lb = all(b1D(ia1D(1:ic-1)) < b1D(ia1D(ic))) .and. all(b1D(ia1D(ic+1:ib)) > b1D(ia1D(ic)))
  call sort(b1D)
  call test(la .and. lb, 'argQuickselect_d1D')

  call Msg('---  Timing the quick select on random numbers ---')
  call clk%restart()
  ib = nIterations
  do ia = 1, ib
    b1D = a1D
    a = quickselect(b1D, ic)
  enddo
  call clk%stop()
  write(*,'(a)') 'Total time for '//str(ib)//' quickselects of length '//str(N)//str(clk%elapsedInSeconds())
  write(*,'(a)') 'Time: '//str(clk%elapsedInSeconds() / dble(ib))//'seconds per select of length '//str(N)

  call Msg('---  Timing the quick select on already sorted numbers ---')
  call clk%restart()
  ib = nIterations
  c1D = a1D
  call sort(c1D)
  do ia = 1, ib
    b1D = c1D
    a = quickselect(b1D, ic)
  enddo
  call clk%stop()
  write(*,'(a)') 'Total time for '//str(ib)//' quickselects of length '//str(N)//str(clk%elapsedInSeconds())
  write(*,'(a)') 'Time: '//str(clk%elapsedInSeconds() / dble(ib))//'seconds per select of length '//str(N)


  call reallocate(ar1D,3)
  call reallocate(a1D,3)
  call reallocate(ia1D,3)
  call allocate(iad1D,3)
  call allocate(cr1D,2)
  call allocate(c1D,2)
  call allocate(ic1D,2)
  call allocate(icd1D,2)
  call allocate(br1D,3)
  call allocate(cr1D,2)
  call allocate(b1D,3)
  call allocate(c1D,2)
  call allocate(ib1D,3)
  call allocate(ibd1D,3)

  call Msg('==========================')
  call Msg('Testing : Array 1D')
  call Msg('==========================')

  call arange(ar1D,1.0, 3.0, 1.0)
  call test(all(ar1D==[1.0,2.0,3.0]),'arange_r1D')
  call arange(a1D,1.d0, 3.d0, 1.d0)
  call test(all(a1D==[1.d0,2.d0,3.d0]),'arange_d1D')
  call arange(ia1D,1, 3, 1)
  call test(all(ia1D==[1,2,3]),'arange_i1D')
  call arange(iad1D,1_i64, 3_i64, 1_i64)
  call test(all(iad1D==[1,2,3]),'arange_id1D')

  call diff(ar1D, cr1D)
  call test(all(cr1D==[1.0,1.0]),'diff_r1D')
  call diff(a1D, c1D)
  call test(all(c1D==[1.d0,1.d0]),'diff_d1D')
  call diff(ia1d, ic1D)
  call test(all(ic1D==[1,1]),'diff_i1D')
  call diff(iad1d, icd1D)
  call test(all(icd1D==[1,1]),'diff_id1D')

  call allocate(br1D, size(ar1D)*3)
  call allocate(b1D, size(a1D)*3)
  call allocate(ib1D, size(ia1D)*3)
  call allocate(ibd1D, size(iad1D)*3)

  call repeat(ar1D, 3, br1D)
  call test(all(br1D == [1.0,1.0,1.0,2.0,2.0,2.0,3.0,3.0,3.0]), 'repeat_r1D')
  call repeat(a1D,3, b1D)
  call test(all(b1D == [1.d0,1.d0,1.d0,2.d0,2.d0,2.d0,3.d0,3.d0,3.d0]), 'repeat_d1D')
  call repeat(ia1D,3, ib1D)
  call test(all(ib1D == [1,1,1,2,2,2,3,3,3]), 'repeat_ib1D')
  call repeat(iad1D,3, ibd1D)
  call test(all(ibd1D == [1,1,1,2,2,2,3,3,3]), 'repeat_ibd1D')

  call Msg('==========================')
  call Msg('Testing : Maths')
  call Msg('==========================')

  br1D = [5.0,6.0,7.0]
  b1D = [5.d0,6.d0,7.d0]
  ib1D = [5,6,7]
  ibd1D = [5_i64, 6_i64, 7_i64]

  cr1D=crossproduct(ar1D, br1D)
  call test(all(cr1D==[-4.0,8.0,-4.0]),'crossproduct_r1D')
  c1D=crossproduct(a1D,b1D)
  call test(all(cr1D==[-4.d0,8.d0,-4.d0]),'crossproduct_d1D')

  cr1D=cumprod(ar1D)
  call test(all(cr1D==[1.0,2.0,6.0]),'cumprod_r1D')
  c1D=cumprod(a1D)
  call test(all(c1D==[1.d0,2.d0,6.d0]),'cumprod_d1D')
  ib1D=cumprod(ia1D)
  call test(all(ib1D==[1,2,6]),'cumprod_i1D')
  ibd1D=cumprod(iad1D)
  call test(all(ibd1D==[1,2,6]),'cumprod_id1D')

  cr1D=cumsum(ar1D)
  call test(all(cr1D==[1.0,3.0,6.0]),'cumsum_r1D')
  c1D=cumsum(a1D)
  call test(all(c1D==[1.d0,3.d0,6.d0]),'cumsum_d1D')
  ib1D=cumsum(ia1D)
  call test(all(ib1D==[1,3,6]),'cumsum_i1D')
  ibd1D=cumsum(iad1D)
  call test(all(ibd1D==[1,3,6]),'cumsum_id1D')

  ar1D=[1.0,2.0,3.0]
  br1D=[0.0,0.5,0.0]
  a1D=[1.d0,2.d0,3.d0]
  b1D=[0.d0,0.5d0,0.d0]

  cr1D=project(ar1D,br1D)
  call test(all(cr1D==[0.0,2.0,0.0]),'project_r1D')
  c1D=project(a1D,b1D)
  call test(all(c1D==[0.d0,2.d0,0.d0]),'project_d1D')

  a=mean(ar1D)
  call test(a==2.d0,'mean_r1D')
  a=mean(a1D)
  call test(a==2.d0,'mean_d1D')
  a=mean(ia1D)
  call test(a==2.d0,'mean_i1D')
  a=mean(iad1D)
  call test(a==2.d0,'mean_id1D')

  a=norm1(ar1D)
  call test(a==6.d0,'norm1_r1D')
  a=norm1(a1D)
  call test(a==6.d0,'norm1_d1D')
  a=norm1(a1D)
  call test(a==6.d0,'norm1_i1D')
  a=norm1(a1D)
  call test(a==6.d0,'norm1_id1D')

  ar=normI(ar1D)
  call test(ar==3.0,'normI_r1D')
  a=normI(a1D)
  call test(a==3.d0,'normI_d1D')
  ia=normI(ia1D)
  call test(a==3,'normI_i1D')
  iad=normI(iad1D)
  call test(a==3,'normI_id1D')

  a=geometricMean(ar1D)
  call test(a==216.d0,'geometricMean_r1D')
  a=geometricMean(a1D)
  call test(a==216.d0,'geometricMean_d1D')
  a=geometricMean(ia1D)
  call test(a==216.d0,'geometricMean_i1D')
  a=geometricMean(iad1D)
  call test(a==216.d0,'geometricMean_id1D')


  call allocate(a1D,2)
  call allocate(ar1D,2)
  ar = 0.2
  br = 0.1
  a = 0.2d0
  b = 0.1d0
  ar1D = twoSum(ar,br)
  call test(abs(ar1D(2)) < 1.d-8,'twoSum_r')
  a1D = twoSum(a,b)
  call test(abs(a1D(2)) < 1.d-16,'twoSum_d')
  ar1D = fastTwoSum(ar,br)
  call test(abs(ar1D(2)) < 1.d-8,'fastTwoSum_r')
  a1D = fastTwoSum(a,b)
  call test(abs(a1D(2)) < 1.d-16,'fastTwoSum_d')

  ar1D=[1.0,2.0,3.0]
  br1D=[0.0,0.5,0.0]
  a1D=[1.d0,2.d0,3.d0]
  b1D=[0.d0,0.5d0,0.d0]

  a=std(ar1D)
  call test(a==1.d0,'std_r1D')
  a=std(a1D)
  call test(a==1.d0,'std_d1D')
  a=std(ia1D)
  call test(a==1.d0,'std_i1D')
  a=std(iad1D)
  call test(a==1.d0,'std_id1D')

  a=variance(ar1D)
  call test(a==1.d0,'variance_r1D')
  a=variance(a1D)
  call test(a==1.d0,'variance_d1D')
  a=variance(ia1D)
  call test(a==1.d0,'variance_i1D')
  a=variance(iad1D)
  call test(a==1.d0,'variance_id1D')

  a=median(ar1D)
  call test(a==2.d0,'median_r1D')
  a=median(a1D)
  call test(a==2.d0,'median_d1D')
  a=median(ia1D)
  call test(a==2.d0,'median_i1D')
  a=median(iad1D)
  call test(a==2.d0,'median_id1D')


  call Msg('==========================')
  write(output_unit,1) trim(str(passCount))//'/'//trim(str(testTotal))//' tests passed'
  call Msg('==========================')

  stop
1 format(a)
contains

subroutine test(l,msg)
logical :: l
character(len=*) :: msg
  testTotal = testTotal + 1
  if (l) then
    write(output_unit,1) msg//' Passed!'
    passCount = passCount + 1
  else
    write(output_unit,1) msg//' Failed!'
  endif
1 format(a)
end subroutine

end program
