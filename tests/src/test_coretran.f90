program test_coretran
  !! Test program for the coretran library
  use m_unitTester
  use iso_fortran_env
  use variableKind
  use m_array1D
  use m_errors
  use m_fileIO
  use m_indexing
  use m_random
  use Prng_Class
  use m_readLine
  use m_strings
  use m_writeLine
  use m_time
  use m_allocate
  use m_deallocate
  use m_reallocate
  use m_copy
  ! Sorting routines
  use m_Sort
  use m_Select
  use m_Searching
  use m_maths
  use m_KdTree

  use Stopwatch_Class
  use ProgressBar_Class
  use m_rDynamicArray
  use m_dDynamicArray
  use m_iDynamicArray
  use m_idDynamicArray

  use m_rArgDynamicArray
  use m_dArgDynamicArray
  use m_iArgDynamicArray
  use m_idArgDynamicArray

  use m_tests

  implicit none

  type(tester) :: test

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
  real(r64),allocatable :: a1D(:),b1D(:),c1D(:), d1D(:)
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
    write(*,'(a)') 'Usage: coretranTest size iterations'
    write(*,'(a)') '  size : Size of the array to run tests on'
    write(*,'(a)') '  iterations : Number of times to run a test that is being timed'
    stop
  end if
  call get_command_argument(1, sa)
  read(sa,*) N

  if (N < 15) call eMsg('Please use size >= 15')

  call get_command_argument(2, sa)
  read(sa,*) nIterations

  test = tester()



  call Msg('==========================')
  call Msg('LibFortran Testing')
  call Msg('==========================')

  call setPrng(big = .true., display = .true.)

  call strings_test(test)
  
  !call fileIO_test(test)
  
  !call random_test(test, .false.)

  call Prng_test(test)
  

  call time_test(test)
  
  call indexing_test(test)
  
  call allocate_test(test)
  
  call reallocate_test(test)
  
  call copy_test(test)
  
  call sorting_test(test, N)
  
  call select_test(test, N)
  
  call array1D_test(test)
  
  call maths_test(test)


!  call Msg('==========================')
!  call Msg('Testing : Stopwatch Class')
!  call Msg('==========================')
!  call clk%start('Generating Random Number')
!  ic = nIterations
!  do ib = 1, ic
!    call rngNormal(a2D)
!  enddo
!  call clk%stop()
!  write(output_unit,'(a)') 'Elapsed time '//clk%elapsed()
!  write(output_unit,'(a)') 'Finished on '//clk%dateAndTime()
!
!  call Msg('==========================')
!  call Msg('Testing : ProgressBar Class')
!  call Msg('==========================')
!  P = ProgressBar(N=ic)
!  call P%print(int(0,i64))
!  do ib = 1, nIterations
!    call rngNormal(a2D)
!    call P%print(ib)
!  enddo
!  P=ProgressBar(N=ic,time = .true.)
!  call P%print(int(0,i64))
!  do ib = 1, nIterations
!    call rngNormal(a2D)
!    call P%print(ib)
!  enddo


  call KdTree_test(test, N)
  

  call Msg('==========================')
  call Msg('Testing : Dynamic Arrays')
  call Msg('==========================')
  call rDynamicArray_test(test)
  call dDynamicArray_test(test)
  call iDynamicArray_test(test)
  call idDynamicArray_test(test)
  
  call Msg('==========================')
  call Msg('Testing : ArgDynamic Arrays')
  call Msg('==========================')
  call rArgDynamicArray_test(test)
  call dArgDynamicArray_test(test)
  call iArgDynamicArray_test(test)
  call idArgDynamicArray_test(test)

  call test%summary()

  stop
1 format(a)

end program



