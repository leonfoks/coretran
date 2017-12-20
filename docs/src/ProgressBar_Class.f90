  module ProgressBar_Class
    !! An inline and updateable command prompt progress bar
    !!
    !! Example usage
    !!```fortran
    !!use variableKind
    !!use ProgressBar_Class
    !!type(ProgressBar) :: P
    !!integer(i32) :: i, N
    !!N = 100000
    !!call P%set(N, time = .false.)
    !!call P%print(0)
    !!do i = 1, N
    !!  Compute some stuff
    !!  call P%print(i)
    !!enddo
    !!call P%set(N, time = .true.)
    !!call P%print(0)
    !!do i = 1, N
    !!  Compute some stuff
    !!  call P%print(i)
    !!enddo
    !!```
  use iso_fortran_env, only: output_unit
  use variableKind, only: i32,i64,r32,r64
  use Stopwatch_Class
  use m_time, only: absTimetoHMS
  implicit none

  private

  type, public :: ProgressBar
    !! A progress bar that displays a moving counter with percentage and optional estimated time remaining
    private
    type(Stopwatch) :: clk !! Stopwatch to estimate time remaining
    character(len=14) :: title !! Give a title for the progress bar
    integer(i64) :: N !! Total number of iterations
    integer(i32) :: old1p !! 1% counter
    integer(i32) :: old10p !! 10% counter
    integer(i32) :: next1p !! 1% counter
    integer(i32) :: next10p !! 10% counter
    logical :: time !! Provide an estimated time remaining?
    integer(i32) :: iTime !! Counter to estimate time
    real(r64) :: cumTime !! Cumulative time
    character(len=88) :: bar !! String to write the progress bar to output
  contains
    !generic, public :: set => set_i1_ProgressBar_,set_id1_ProgressBar_
    !! Print the progress Bar
    !procedure, private :: set_i1_ProgressBar_ => set_i1_ProgressBar
    !procedure, private :: set_id1_ProgressBar_ => set_id1_ProgressBar
    !! Initialize the Progress Bar
    generic, public :: print => print_i1_ProgressBar_,print_id1_ProgressBar_
      !! Print the progress Bar
    procedure, private :: print_i1_ProgressBar_ => print_i1_ProgressBar
    procedure, private :: print_id1_ProgressBar_ => print_id1_ProgressBar
  end type

  interface ProgressBar
    module procedure ProgressBar_i1
      !! Interfaced with class instantiation ProgressBar()
    module procedure ProgressBar_id1
      !! Interfaced with class instantiation ProgressBar()
  end interface

  contains
  !====================================================================!
  function ProgressBar_i1(N,title,time) result(res)
    !! Interfaced with class instantiation ProgressBar()
  !====================================================================!
  integer(i32), intent(in) :: N !! Maximum expected number of iterations
  character(len=*), intent(in), optional :: title !! Title to give the progress bar
  logical, intent(in), optional :: time !! Show an estimated time to completion?
  type(ProgressBar) :: res
  res=ProgressBar_id1(int(N, i64),title,time)
  end function
  !====================================================================!
  !====================================================================!
  function ProgressBar_id1(N,title,time) result(res)
    !! Interfaced with class instantiation ProgressBar()
  !====================================================================!
  integer(i64), intent(in) :: N !! Maximum expected number of iterations
  character(len=*), intent(in), optional :: title !! Title to give the progress bar
  logical, intent(in), optional :: time !! Show an estimated time to completion?
  type(ProgressBar) :: res
  res%old1p=0
  res%old10p=0
  res%N=N
  write(res%title,'(14x)')
  if(present(title)) res%title=title
  res%time=.false.
  if (present(time)) then
    res%time=time
!    call res%clk%start()
    res%cumTime=0.d0
    res%iTime=0
  end if
  if (res%time) then
    res%bar = '     Completed:'//res%title//'-    % |          | Est. Remaining --:--:--.--- (h:m:s)'
    write(output_unit,'(a1,a88)',advance='no') char(13),res%bar
  else
    res%bar = '     Completed:'//res%title//'-    % |          |'
    write(output_unit,'(a1,a48)',advance='no') char(13),res%bar
  endif
  flush(output_unit)
  end function
  !====================================================================!
  !====================================================================!
  subroutine print_i1_ProgressBar(this,i)
    !! Interfaced with ProgressBar%print()
  !====================================================================!
  class(ProgressBar) :: this
    !! ProgressBar Class
  integer(i32), intent(in) :: i
    !! Current iteration number
  call print_id1_ProgressBar(this,int(i, i64))
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine print_id1_ProgressBar(this,i)
    !! Interfaced with ProgressBar%print()
  !====================================================================!
  class(ProgressBar) :: this
    !! ProgressBar Class
  integer(i64), intent(in) :: i
    !! Current iteration number
  integer(i32) :: kk
  real(r64) :: tmp
  real(r64) :: avgTime
  integer(i64) :: iTmp
  logical :: printMe
  ! If we are estimating the time left till completion
  printMe=.false.
  tmp=dble(i)/dble(this%N)
  ! Get the current 1% discretization
  this%next1p=int(tmp*100.d0) ! Current 1%
  ! Get the current 10% discretization
  this%next10p=int(mod(tmp*10.d0,10.d0)) ! Current 10%
  ! Check for 100%
  if (this%next1p == 100) this%next10p=10 ! Avoids write out error
  ! If the 10% level has changed, add a counter to the progress bar
  kk=37+this%next10p
  if (this%next10p > this%old10p) then
    this%bar(kk:kk)='=' ! Write the bar
    this%old10p=this%next10p
  end if
  ! If the 1% has changed, update the bar and print
  if (this%next1p > this%old1p) then
    ! Write % to the bar
    write(this%bar(32:34),'(i3)') this%next1p
    ! Update the output
    this%old1p = this%next1p
    printMe=.true.
  end if

  if (this%time) then
    this%iTime=this%iTime+1
    iTmp=this%N/200
    if (this%iTime == iTmp) then
      ! Get the time for this many iterations
      this%cumTime=this%cumTime+this%clk%lapInSeconds()
      ! Get the average time per iteration
      avgTime=this%cumTime/dble(i)
      ! Estimate time left my multiplying average time by iterations left.
      avgTime=avgTime*dble(this%N-i)
      if (avgTime > 1.d-7) write(this%bar(65:88),'(a22)') absTimetoHMS(avgTime)
      printMe=.true.
    endif
    if (this%iTime > iTmp) this%iTime=0
    if (this%next10p == 10) this%bar(50:88) = 'Finished in '//this%clk%elapsed()
  end if

  if (printMe) then
    if (this%time) then
      write(output_unit,'(a1,a88)',advance='no') char(13),this%bar
    else
      write(output_unit,'(a1,a48)',advance='no') char(13),this%bar
    endif
    flush(output_unit)
  endif
  if (this%next10p==10) write(output_unit,*)
  end subroutine
  !====================================================================!
  end module

