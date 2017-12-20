module m_time
  !! Contains functions that handle time
use variableKind
use m_errors, only: msg
use m_unitTester, only: tester
implicit none
contains
  !=====================================================================!
  function timeinseconds(values) result(res)
    !! Convert hours minutes seconds etc. to seconds
  !=====================================================================!
  integer(i32) :: values(8)
    !! values containing amounts of days hours etc.
  real(r64) :: res
    !! time in seconds
  res = dble((86400*values(3))+(3600*values(5)) + (60*values(6)) + values(7))&
    + (0.001d0 * dble(values(8)))
  end function
  !=====================================================================!
  !=====================================================================!
  function daysinMonth(month,year) result(days)
    !! Get the number of days in a month. Accounts for leap years
  !=====================================================================!
  integer(i32) :: month
    !! How many days in this month
  integer(i32),optional :: year
    !! Check if a leap year?
  integer(i32) :: days
    !! Number of days
  days=0
  select case(month)
  case(1,3,5,7,8,10,12)
    days=31
  case(2)
    days=28
    if (present(year)) then
      if (isLeapYear(year)) days=29
    endif
  case(4,6,9,11)
    days=30
  end select

  end function
  !=====================================================================!
  !=====================================================================!
  function daysinYear(year) result(days)
    !! Get the number of days in a year, accounts for leap years
  !=====================================================================!
  integer(i32) :: year
    !! How many days in this year
  integer(i32) :: days
    !! Number of days
  days=365
  if (isLeapYear(year)) days=366
  end function
  !=====================================================================!
  !=====================================================================!
  function isLeapYear(year) result(yes)
    !! Determine whether the year is a leap year
  !=====================================================================!
  integer(i32) :: year
    !! Year to check
  logical :: yes
    !! is a leap year
  yes=.false.
  if (iand(year,3) == 0 .and. (mod(year,25) /= 0 .or. iand(year,15) == 0)) yes=.true.
  end function
  !=====================================================================!
  !=====================================================================!
  function absTimetoHMS(t) result(res)
    !! Convert an absolute time to HH:MM:SS.MSEC
  !=====================================================================!
  real(r64) :: t
    !! Time in seconds
  character(len=22) :: res
    !! Resulting string contains the time
  integer(i32) :: days,hrs,min,sec,msec
  real(r64)  :: tmp
  days=floor(t/86400.d0);tmp=t-(dble(days*86400))
  hrs=floor(tmp/3600.d0);tmp=tmp-(dble(hrs*3600))
  hrs=hrs+(24*days)
  min=floor(tmp/60.d0);  tmp=tmp-(dble(min*60))
  sec=floor(tmp);        msec=int(1000*(tmp-(dble(sec))))
  write(res,10) hrs,min,sec,msec
10 format(i0,':',i2,':',i2,'.',i3,' (h:m:s)')
  end function
  !=====================================================================!
  !=====================================================================!
  subroutine time_test(test)
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
  call test%test(absTimetoHMS(90031.008d0) == '25: 0:31.  8 (h:m:s)','absTimetoHMS')
  end subroutine
  !=====================================================================!
end module
