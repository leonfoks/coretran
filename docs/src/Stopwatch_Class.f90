  module Stopwatch_Class
    !! Contains code timing capabilities
    !!
    !! Example usage
    !!```fortran
    !!use variableKind
    !!use Stopwatch_Class
    !!type(Stopwatch) :: clk
    !!integer(i32) :: i, N
    !!call clk%start('Some Title')
    !!N = 100000
    !!do i = 1, N
    !!  Compute some stuff
    !!enddo
    !!call clk%stop()
    !!call clk%elapsed()
    !!write(*,'(a)') 'Finished on '//clk%datetime()
    !!```
  use iso_fortran_env, only: output_unit
  use variableKind, only: i32,r64
  use m_strings, only: str
  use m_time, only: absTimetoHMS,timeinseconds
  implicit none

  private

  type, public :: Stopwatch
    !! Class for timing sections of code
    private
    logical :: running = .false.
    integer(i32) :: LapTime_(8)=0
    integer(i32) :: startTime_(8)=0
    integer(i32) :: stopTime_(8)=0
  contains
  procedure :: start      => start_Stopwatch !! Start the Stopwatch
  procedure :: stop       => stop_Stopwatch  !! Stop the Stopwatch
  procedure :: reset      => reset_Stopwatch !! Reset the Stopwatch
  procedure :: restart    => restart_Stopwatch !! Restart the Stopwatch
  procedure :: time       => time_Stopwatch !! Get the current or stopped time
  procedure :: lap    => lap_Stopwatch !! Lap the Stopwatch
  procedure :: lapInSeconds => lapInSeconds_Stopwatch !! Get the lap time in seconds
  procedure :: elapsed    => elapsed_Stopwatch !! Get the current elapsed time
  procedure :: elapsedInSeconds => elapsedInSeconds_Stopwatch !! Get the elapsed time in seconds
  procedure :: date       => date_Stopwatch !! Print the date
  procedure :: dateAndTime   => dateAndTime_Stopwatch !! Print the date and time
  end type

  contains
  !=====================================================================!
  subroutine start_Stopwatch(this,title,iunit)
    !! Start the Stopwatch
  !=====================================================================!
  class(Stopwatch) :: this
    !! Stopwatch Class
  character(len=*),optional :: title
    !! Name to label the start of the stopwatch
  integer(i32),optional :: iunit
    !! File ID to write to
  if (.not.this%running) then
    this%running=.true.
    call date_and_time(values=this%StartTime_)
    this%LapTime_=this%StartTime_
    if (present(title))then
      if (present(iunit)) then
        write(iunit,'(a)') trim(title)//' started on: '//trim(this%dateAndTime())
        write(output_unit,'(a)') trim(title)//' started on: '//trim(this%dateAndTime())
      else
        write(output_unit,'(a)') trim(title)//' started on: '//trim(this%dateAndTime())
      endif
    endif
  endif
  end subroutine
  !=====================================================================!
  !=====================================================================!
  subroutine stop_Stopwatch(this,title,iunit)
    !! Stop the Stopwatch
  !=====================================================================!
  class(Stopwatch) :: this
    !! Stopwatch Class
  character(len=*),optional :: title
    !! Name to label the stopped stopwatch
  integer(i32),optional :: iunit
    !! File ID to write to
  if (this%running) then
    call date_and_time(values=this%StopTime_)
    this%running=.false.
    if (present(title))then
      if (present(iunit)) then
        write(iunit,'(a)') trim(title)//' finished on: '//trim(this%date())//' in '//str(this%elapsedInSeconds())//'(s)'
        write(output_unit,'(a)') trim(title)//' finished on: '//trim(this%date())//' in '//str(this%elapsedInSeconds())//'(s)'
      else
        write(output_unit,'(a)') trim(title)//' finished on: '//trim(this%date())//' in '//str(this%elapsedInSeconds())//'(s)'
      endif
    endif
  endif
  end subroutine
  !=====================================================================!
  !=====================================================================!
  subroutine reset_Stopwatch(this)
    !! Reset the Stopwatch
  !=====================================================================!
  class(Stopwatch) :: this
    !! Stopwatch Class
  call this%stop()
  this%StartTime_=0;this%LapTime_=0;this%StopTime_=0
  end subroutine
  !=====================================================================!
  subroutine restart_Stopwatch(this)
    !! Restart the Stopwatch
  !=====================================================================!
  class(Stopwatch) :: this
    !! Stopwatch Class
  call this%reset()
  call this%start()
  end subroutine
  !=====================================================================!
  !=====================================================================!
  function elapsed_Stopwatch(this) result(res)
    !! Get the elapsed time
  !=====================================================================!
  class(Stopwatch) :: this
    !! Stopwatch Class
  character(len=:), allocatable :: res
    !! Elapsed time in format HH:MM:SS.MSEC
  res=absTimetoHMS(this%elapsedInSeconds())
  end function
  !=====================================================================!
  !=====================================================================!
  function lap_Stopwatch(this) result(res)
    !! Get the lap time
  !=====================================================================!
  class(Stopwatch) :: this
    !! Stopwatch Class
  character(len=:), allocatable :: res
    !! Lap time in format HH:MM:SS.MSEC
  res=absTimetoHMS(this%lapInSeconds())
  end function
  !=====================================================================!
  !=====================================================================!
  function lapInSeconds_Stopwatch(this) result(res)
    !! Get the lap time in seconds
  !=====================================================================!
  class(Stopwatch) :: this
    !! Stopwatch Class
  real(r64) :: res
    !! Laptime in seconds
  integer(i32) :: tmp(8)
  if (this%running) then
    call date_and_time(values=tmp)
    res=timeinseconds(tmp)-timeinseconds(this%lapTime_)
    this%laptime_=tmp
  else
    res=timeinseconds(this%StopTime_)-timeinseconds(this%lapTime_)
  endif
  end function
  !=====================================================================!
  !=====================================================================!
  function elapsedInSeconds_Stopwatch(this) result(res)
    !! Get the elapsed time in seconds
  !=====================================================================!
  class(Stopwatch) :: this
    !! Stopwatch Class
  real(r64) :: res
    !! Elapsed time in seconds
  integer(i32) :: tmp(8)
  if (this%running) then
    call date_and_time(values=tmp)
    res = timeinseconds(tmp)-timeinseconds(this%StartTime_)
  else
    res = timeinseconds(this%StopTime_)-timeinseconds(this%StartTime_)
  endif
  end function
  !=====================================================================!
  !=====================================================================!
  function dateAndTime_Stopwatch(this) result(res)
    !! Get the date and time
  !=====================================================================!
  class(Stopwatch) :: this
    !! Stopwatch Class
  character(len=:), allocatable :: res
    !! Date and time in format DD/MM/YYYY HH:MM:SS.MSEC
  res = trim(this%date())//' at '//trim(this%time())
  end function
  !=====================================================================!
  !=====================================================================!
  function date_Stopwatch(this) result(res)
    !! Get the date
  !=====================================================================!
  class(Stopwatch) :: this
    !! Stopwatch Class
  character(len=:), allocatable :: res
    !! Date in format DD/MM/YYYY
  integer(i32) :: tmp(8)
  res=''
  if (this%running) then
    call date_and_time(values=tmp)
    res = trim(str(tmp(3)))//'/'//trim(str(tmp(2)))//'/'//trim(str(tmp(1)))
  else
    res = trim(str(this%StopTime_(3)))//'/'//trim(str(this%StopTime_(2)))//'/'//trim(str(this%StopTime_(1)))
  endif
  end function
  !=====================================================================!
  !=====================================================================!
  function time_Stopwatch(this) result(res)
    !! Get the time
  !=====================================================================!
  class(Stopwatch) :: this
    !! Stopwatch Class
  character(len=:), allocatable :: res
    !! Time in format HH:MM:SS.MSEC
  integer :: tmp(8)
  res = ''
  if (this%running) then
    call date_and_time(values=tmp)
    res = trim(str(tmp(5)))//':'//trim(str(tmp(6)))//':'//trim(str(tmp(7)))//'.'//trim(str(tmp(8)))
  else
    res = trim(str(this%StopTime_(5)))//':'//trim(str(this%StopTime_(6)))//':'//trim(str(this%StopTime_(7)))//'.'//trim(str(this%StopTime_(8)))
  endif
  end function
  !=====================================================================!
end module


