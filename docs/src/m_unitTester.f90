module m_unitTester

use iso_fortran_env, only: output_unit
use variableKind, only: i32
use m_errors, only: msg

implicit none

private

public :: tester

type tester
  private
  integer(i32) :: total
  integer(i32) :: success
contains
  procedure, public :: test => test_tester
  procedure, public :: summary => summary_tester
end type

interface tester
  module procedure :: init_tester
end interface

contains

function init_tester() result(this)
  type(tester) :: this
  this%total = 0
  this%success = 0
end function

subroutine test_tester(this, l, msg)
  class(tester) :: this
  logical :: l
  character(len=*) :: msg
  this%total = this%total + 1
  if (l) then
    write(output_unit,1) msg//' Passed!'
    this%success = this%success + 1
  else
    write(output_unit,1) msg//' Failed!'
  endif
1 format(a)
end subroutine

subroutine summary_tester(this)
  class(tester) :: this
  call Msg('==========================')
  write(output_unit,1) this%success,'/',this%total,' tests passed'
  call Msg('==========================')
1 format(i0,a,i0,a)
end subroutine


end module
