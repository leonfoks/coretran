module m_compare
  use variableKind

  implicit none

  interface compare
    module procedure :: compare_d1
  end interface

  contains

  function compare_d1(this,that) result(res)
    real(r64) :: this
    real(r64) :: that
    integer(i32) :: res

    if (this < that) then
      res = -1
    elseif (this > that) then
      res = 1
    else
      res = 0
    end if
  end function

end module
