module m_debugging
  !! Contains routines for debugging
  use variableKind
  use m_strings
  logical :: verbose = .false.
    !! Verbose option for debugging codes
  integer :: debugLevel
    !! Integer level for debug messages

  interface debug
    !! Interface for printing primitives with a name for debug purposes
    !! All may be called using 'call debug(args)'
  module procedure :: debug_d,debug_d1D,debug_i,debug_i1D
  end interface
  private :: debug_d,debug_d1D,debug_i,debug_i1D

  contains
  !====================================================================!
  subroutine debug_s(name)
  !====================================================================!
  character(len=*) :: name
  real(r64) :: var
  if (verbose) write(*,'(a)') trim(name)//': '//str(var)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine debug_D(name,var)
  !====================================================================!
  character(len=*) :: name
  real(r64) :: var
  if (verbose) write(*,'(a)') trim(name)//': '//str(var)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine debug_D1D(name,var)
  !====================================================================!
  character(len=*) :: name
  real(r64) :: var(:)
  if (verbose) write(*,'(a)') trim(name)//': '//str(var)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine debug_I(name,var)
  !====================================================================!
  character(len=*) :: name
  integer(i32) :: var
  if (verbose) write(*,'(a)') trim(name)//': '//str(var)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine debug_I1D(name,var)
  !====================================================================!
  character(len=*) :: name
  integer(i32) :: var(:)
  if (verbose) write(*,'(a)') trim(name)//': '//str(var)
  end subroutine
  !====================================================================!


end module
