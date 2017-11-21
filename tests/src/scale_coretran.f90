program scaleTest_coretran

!! Test program for the coretran library
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
  use m_Select
  !use m_medianOfMedians
  !use m_Select_FloydRivest
  !use m_heapSelect
  use m_maths

  use Stopwatch_Class
  use ProgressBar_Class
  use m_KdTree

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
  type(KdTree) :: tree
  type(KdTreeSearch) :: search

  N=2**24

  ! Set a fixed seed
  call allocate(ia1D, 33)
  ia1D = 546420601
  call setRNG(ia1D)


  call Msg('==========================')
  call Msg('Testing : Sorting')
  call Msg('==========================')

  ! Initial setup for testing
  call allocate(ar1D, N)
  call allocate(br1D, N)
  call allocate(a1D, N)
  call allocate(b1D, N)
  call allocate(c1D, N)
  call allocate(ia1D,N)


  call rngNormal(a1D)
  ar1D = real(a1D)

  call Msg('Size  Time(s)')
  call Msg('---  Timing the double precision introspection sort ---')

  do ia = 0, 5
    ib = 2**(24-ia)
    b1D(1:ib) = a1D(1:ib)

    call clk%restart()
    call sort(b1D(1:ib))
    call clk%stop()
    write(*,'(i10,1x,f0.3)') ib,clk%elapsedInSeconds()
    if (ia == 0) c1D=b1D
  enddo

  call Msg('---  Timing the already sorted double precision introspection sort ---')

  b1D = c1D

  do ia = 0, 5
    ib = 2**(24-ia)
    call clk%restart()
    call sort(b1D(1:ib))
    call clk%stop()
    write(*,'(i10,1x,f0.3)') ib,clk%elapsedInSeconds()
  enddo

  call Msg('---  Timing the double precision merge sort ---')
  do ia = 0, 5
    ib = 2**(24-ia)
    b1D(1:ib) = a1D(1:ib)

    call clk%restart()
    call sort(b1D(1:ib), stable=.true.)
    call clk%stop()
    write(*,'(i10,1x,f0.3)') ib,clk%elapsedInSeconds()
  enddo

  call Msg('---  Timing the already sorted double precision merge sort ---')
  b1D = c1D
  do ia = 0, 5
    ib = 2**(24-ia)

    call clk%restart()
    call sort(b1D(1:ib), stable=.true.)
    call clk%stop()
    write(*,'(i10,1x,f0.3)') ib,clk%elapsedInSeconds()
  enddo

  call Msg('==========================')
  call Msg('Testing : Selection')
  call Msg('==========================')

  call Msg('---  Timing the double precision quick select ---')
  do ia = 0, 5
    ib = 2**(24-ia)
    b1D(1:ib) = a1D(1:ib)

    ic = (ib+1)/2

    call clk%restart()
    call select(b1D(1:ib), ic, a)
    call clk%stop()
    write(*,'(i10,1x,f0.3)') ib,clk%elapsedInSeconds()
  enddo

  call Msg('---  Timing the already sorted double precision quick select ---')
  b1D = c1D
  do ia = 0, 5
    ib = 2**(24-ia)

    ic = (ib+1)/2

    call clk%restart()
    call select(b1D(1:ib), ic, a)
    call clk%stop()
    write(*,'(i10,1x,f0.3)') ib,clk%elapsedInSeconds()
  enddo


  call Msg('==========================')
  call Msg('Testing : Spatial')
  call Msg('==========================')
  call Msg('---  Timing the KdTree ---')
  call rngNormal(b1D)
  call rngNormal(c1D)
  do ia = 0, 5
    ib = 2**(24-ia)

    call clk%restart()
    call tree%init(a1D(1:ib), b1D(1:ib))
    call clk%stop()
    call tree%deallocate()
    write(*,'(i10,1x,f0.3)') ib,clk%elapsedInSeconds()
  enddo


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

