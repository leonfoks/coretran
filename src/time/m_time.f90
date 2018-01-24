module m_time
  !! Contains functions that handle time
  
use variableKind, only: i32, i64, r64

implicit none

contains

  !=====================================================================!
  function daysInMonth(month,year) result(days)
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
  function daysInYear(year) result(days)
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
  function secondsToHMS(t) result(res)
    !! Convert a time in seconds to HH:MM:SS.MSEC
  !=====================================================================!
  real(r64) :: t
    !! Time in seconds
  character(len=22) :: res
    !! Resulting string contains the time
  integer(i32) :: days,hrs,min,sec,msec
  real(r64)  :: tmp
  days=floor(t/86400.d0)
  tmp=t-(dble(days*86400))

  hrs=floor(tmp/3600.d0)
  tmp=tmp-(dble(hrs*3600))
  hrs=hrs+(24*days)

  min=floor(tmp/60.d0)
  tmp=tmp-(dble(min*60))

  sec=floor(tmp)
  msec=int(1000.d0*(tmp-(dble(sec))))

  write(res,10) hrs,min,sec,msec
10 format(i0,':',i2,':',i2,'.',i3,' (h:m:s)')
  end function
  !=====================================================================!
  !=====================================================================!
  function timeInSeconds(values) result(res)
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
  function timeToInteger(values) result(res)
    !! Convert hours minutes seconds etc. to an integer.  Use 64bit to prevent the 2038 problem.
  !=====================================================================!
  integer(i32) :: values(8)
    !! Values containing amounts of days hours etc.
  integer(i64) :: res
    !! Time as an integer.

  ! Account for leap years
  values(1) = values(1) * daysInYear(values(1))
  ! Accounts for different length months
  values(2) = values(2) * daysInMonth(values(2))
  
  res = ( values(1) * 86400000 & ! Years
      + values(2) * 86400000 & ! Months
      + values(3) * 86400000 & ! Days
      + values(5) * 3600000 & ! Hours
      + values(6) * 60000 & ! Minutes
      + values(7) * 1000 & ! Seconds
      + values(8)) ! Milliseconds
  end function
  !=====================================================================!
end module
