module formatting
  !! Contains routines that handle string formatting
  use variableKind, only: i32
implicit none

contains
!  !====================================================================!
!  function getNFormats(N, formt) result(FMT)
!    !! Create a format string with N times strings for writing on a single line
!  !====================================================================!
!  integer(i32), intent(in) :: N !! Number of times to repeat the format
!  character(len=*), intent(in), optional :: formt !! Optional format to repeat
!  character(len=20), intent(out) :: FMT
!    !! Format statement with (N('a'))
!  if (present(formt)) then
!    write(FMT,'("(",i0,a,")")') N,trim(formt)
!  else
!    write(FMT,'("(",i0,"(a))")') N
!  endif
!  end function
!  !====================================================================!

end module
