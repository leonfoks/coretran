module m_writeline
  !! Contains core routines to write different combinations of scalars and 1D arrays to a file
  use iso_fortran_env, only: output_unit
  use variableKind
  use m_errors, only: Ferr
  use m_strings,only: str
  implicit none
  private

  public :: writeline

  interface writeline
    !! Write multiple items to a line
    !!
    !! Example usage
    !!```fortran
    !!use variableKind
    !!use m_writeline
    !!use m_fileIO, only: openFile, closeFile
    !!character(len=:),allocatable :: fName
    !!real(r64) :: a,b,c
    !!real(r64) :: a1D(3)
    !!integer(i32) :: iunit, istat
    !!a = 1.d0
    !!b = 2.d0
    !!c = 3.d0
    !!a1D = [1.d0, 2.d0, 3.d0]
    !!fName = 'writeline.txt'
    !!call openFile(fName, iunit, 'unknown', istat)
    !!call writeline(a, b, c, fName, iunit)
    !!call writeline(a1D, fName, iunit)
    !!call closeFile(fName, iunit, '', istat)
    !!```
  module procedure :: writeLine_a,writeLine_ab,writeLine_abc,writeLine_abcd,writeLine_abcde
  module procedure :: writeLine_av,writeLine_avbv,writeLine_avbvcv,writeLine_abv,writeLine_abvcv,writeLine_abcdev
  module procedure :: writeLine_abcdevfv,writeLine_abcdv,writeLine_abcdvev,writeLine_abcv,writeLine_abcvdv
  module procedure :: writeLine_abcdefv,writeLine_abcdefvgv
  end interface

contains

!====================================================================!
  subroutine writeLine_a(a,fname,iunit)
    !! Interfaced with writeLine()
  !====================================================================!
  ! Writes a on a single line
  real(r64) :: a
  character(len=*),optional :: fname
  integer(i32),optional :: iunit
  integer(i32) :: istat
  if (present(iunit)) then
    write(iunit,'(a)',iostat=istat) str(a)
    call Ferr(istat,fname,3)
  else
    write(output_unit,'(a)') str(a)
  endif
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine writeLine_ab(a,b,fname,iunit)
    !! Interfaced with writeLine()
  !====================================================================!
  ! Writes a b on a single line
  real(r64) :: a,b
  character(len=*),optional :: fname
  integer(i32),optional :: iunit
  integer(i32) :: istat
  if (present(iunit)) then
    write(iunit,'(a)',iostat=istat) str(a)//str(b)
    call Ferr(istat,fname,3)
  else
    write(output_unit,'(a)') str(a)//str(b)
  endif
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine writeLine_abc(a,b,c,fname,iunit)
    !! Interfaced with writeLine()
  !====================================================================!
  ! Writes a b c on a single line
  real(r64) :: a,b,c
  character(len=*),optional :: fname
  integer(i32),optional :: iunit
  integer(i32) :: istat
  if (present(iunit)) then
    write(iunit,'(a)',iostat=istat) str(a)//str(b)//str(c)
    call Ferr(istat,fname,3)
  else
    write(output_unit,'(a)') str(a)//str(b)//str(c)
  endif
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine writeLine_abcd(a,b,c,d,fname,iunit)
    !! Interfaced with writeLine()
  !====================================================================!
  ! Writes a b c d on a single line
  real(r64) :: a,b,c,d
  character(len=*),optional :: fname
  integer(i32),optional :: iunit
  integer(i32) :: istat
  if (present(iunit)) then
    write(iunit,'(a)',iostat=istat) str(a)//str(b)//str(c)//str(d)
    call Ferr(istat,fname,3)
  else
    write(output_unit,'(a)') str(a)//str(b)//str(c)//str(d)
  endif
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine writeLine_abcde(a,b,c,d,e,fname,iunit)
    !! Interfaced with writeLine()
  !====================================================================!
  ! Writes a b c on a single line
  real(r64) :: a,b,c,d,e
  character(len=*),optional :: fname
  integer(i32),optional :: iunit
  integer(i32) :: istat
  if (present(iunit)) then
    write(iunit,'(a)',iostat=istat) str(a)//str(b)//str(c)//str(d)//str(e)
    call Ferr(istat,fname,3)
  else
    write(output_unit,'(a)') str(a)//str(b)//str(c)//str(d)//str(e)
  endif
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine writeLine_av(a,fname,iunit)
    !! Interfaced with writeLine()
  !====================================================================!
  ! Writes a(1),...,a(Na) on a single line
  real(r64) :: a(:)
  character(len=*),optional :: fname
  integer(i32),optional :: iunit
  integer(i32) :: istat
  if (present(iunit)) then
    write(iunit,'(a)',iostat=istat) str(a)
    call Ferr(istat,fname,3)
  else
    write(output_unit,'(a)') str(a)
  endif
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine writeLine_avbv(a,b,fname,iunit)
    !! Interfaced with writeLine()
  !====================================================================!
  ! Writes a(1),...,a(Na),b(1),...,b(Nb) on a single line
  real(r64) :: a(:),b(:)
  character(len=*),optional :: fname
  integer(i32),optional :: iunit
  integer(i32) :: istat
  if (present(iunit)) then
    write(iunit,'(a)',iostat=istat) str(a)//str(b)
    call Ferr(istat,fname,3)
  else
    write(output_unit,'(a)') str(a)//str(b)
  endif
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine writeLine_avbvcv(a,b,c,fname,iunit)
    !! Interfaced with writeLine()
  !====================================================================!
  ! Writes a(1),...,a(Na),b(1),...,b(Nb) on a single line
  real(r64) :: a(:),b(:),c(:)
  character(len=*),optional :: fname
  integer(i32),optional :: iunit
  integer(i32) :: istat
  if (present(iunit)) then
    write(iunit,'(a)',iostat=istat) str(a)//str(b)//str(c)
    call Ferr(istat,fname,3)
  else
    write(output_unit,'(a)') str(a)//str(b)//str(c)
  endif
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine writeLine_abv(a,b,fname,iunit)
    !! Interfaced with writeLine()
  !====================================================================!
  ! Writes a,b(1),...,b(Nb) on a single line
  real(r64) :: a,b(:)
  character(len=*),optional :: fname
  integer(i32),optional :: iunit
  integer(i32) :: istat
  if (present(iunit)) then
    write(iunit,'(a)',iostat=istat) str(a)//str(b)
    call Ferr(istat,fname,3)
  else
    write(output_unit,'(a)') str(a)//str(b)
  endif
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine writeLine_abvcv(a,b,c,fname,iunit)
    !! Interfaced with writeLine()
  !====================================================================!
  ! Writes a,b(1),...,b(Nb),c(1),...,c(Nc) on a single line
  real(r64) :: a,b(:),c(:)
  character(len=*),optional :: fname
  integer(i32),optional :: iunit
  integer(i32) :: istat
  if (present(iunit)) then
    write(iunit,'(a)',iostat=istat) str(a)//str(b)//str(c)
    call Ferr(istat,fname,3)
  else
    write(output_unit,'(a)') str(a)//str(b)//str(c)
  endif
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine writeLine_abcv(a,b,c,fname,iunit)
    !! Interfaced with writeLine()
  !====================================================================!
  ! Writes a,b,c(1),...,c(Nc) on a single line
  real(r64) :: a,b,c(:)
  character(len=*),optional :: fname
  integer(i32),optional :: iunit
  integer(i32) :: istat
  if (present(iunit)) then
    write(iunit,'(a)',iostat=istat) str(a)//str(b)//str(c)
    call Ferr(istat,fname,3)
  else
    write(output_unit,'(a)') str(a)//str(b)//str(c)
  endif
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine writeLine_abcvdv(a,b,c,d,fname,iunit)
    !! Interfaced with writeLine()
  !====================================================================!
  ! Writes a,b,c(1),...,c(Nc),d(1),...,d(Nd) on a single line
  real(r64) :: a,b,c(:),d(:)
  character(len=*),optional :: fname
  integer(i32),optional :: iunit
  integer(i32) :: istat
  if (present(iunit)) then
    write(iunit,'(a)',iostat=istat) str(a)//str(b)//str(c)//str(d)
    call Ferr(istat,fname,3)
  else
    write(output_unit,'(a)') str(a)//str(b)//str(c)//str(d)
  endif
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine writeLine_abcdv(a,b,c,d,fname,iunit)
    !! Interfaced with writeLine()
  !====================================================================!
  ! Writes a,b,c,d(1),...,d(Nd) on a single line
  real(r64) :: a,b,c,d(:)
  character(len=*),optional :: fname
  integer(i32),optional :: iunit
  integer(i32) :: istat
  if (present(iunit)) then
    write(iunit,'(a)',iostat=istat) str(a)//str(b)//str(c)//str(d)
    call Ferr(istat,fname,3)
  else
    write(output_unit,'(a)') str(a)//str(b)//str(c)//str(d)
  endif
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine writeLine_abcdvev(a,b,c,d,e,fname,iunit)
    !! Interfaced with writeLine()
  !====================================================================!
  ! Writes a,b,c,d(1),...,d(Nd),e(1),...,e(Ne) on a single line
  real(r64) :: a,b,c,d(:),e(:)
  character(len=*),optional :: fname
  integer(i32),optional :: iunit
  integer(i32) :: istat
  if (present(iunit)) then
    write(iunit,'(a)',iostat=istat) str(a)//str(b)//str(c)//str(d)//str(e)
    call Ferr(istat,fname,3)
  else
    write(output_unit,'(a)') str(a)//str(b)//str(c)//str(d)//str(e)
  endif
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine writeLine_abcdev(a,b,c,d,e,fname,iunit)
    !! Interfaced with writeLine()
  !====================================================================!
  ! Writes a,b,c,d,e(1),...,e(Ne) on a single line
  real(r64) :: a,b,c,d,e(:)
  character(len=*),optional :: fname
  integer(i32),optional :: iunit
  integer(i32) :: istat
  if (present(iunit)) then
    write(iunit,'(a)',iostat=istat) str(a)//str(b)//str(c)//str(d)//str(e)
    call Ferr(istat,fname,3)
  else
    write(output_unit,'(a)') str(a)//str(b)//str(c)//str(d)//str(e)
  endif
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine writeLine_abcdevfv(a,b,c,d,e,f,fname,iunit)
    !! Interfaced with writeLine()
  !====================================================================!
  ! Writes a,b,c,d,e(1),...,e(Ne) on a single line
  real(r64) :: a,b,c,d,e(:),f(:)
  character(len=*),optional :: fname
  integer(i32),optional :: iunit
  integer(i32) :: istat
  if (present(iunit)) then
    write(iunit,'(a)',iostat=istat) str(a)//str(b)//str(c)//str(d)//str(e)//str(f)
    call Ferr(istat,fname,3)
  else
    write(output_unit,'(a)') str(a)//str(b)//str(c)//str(d)//str(e)//str(f)
  endif
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine writeLine_abcdefv(a,b,c,d,e,f,fname,iunit)
    !! Interfaced with writeLine()
  !====================================================================!
  ! Writes a,b,c,d,e,f(1),...,f(Nf) on a single line
  real(r64) :: a,b,c,d,e,f(:)
  character(len=*),optional :: fname
  integer(i32),optional :: iunit
  integer(i32) :: istat
  if (present(iunit)) then
    write(iunit,'(a)',iostat=istat) str(a)//str(b)//str(c)//str(d)//str(e)//str(f)
    call Ferr(istat,fname,3)
  else
    write(output_unit,'(a)') str(a)//str(b)//str(c)//str(d)//str(e)//str(f)
  endif
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine writeLine_abcdefvgv(a,b,c,d,e,f,g,fname,iunit)
    !! Interfaced with writeLine()
  !====================================================================!
  ! Writes a,b,c,d,e,f(1),...,f(Nf),g(1),...,g(Ng) on a single line
  real(r64) :: a,b,c,d,e,f(:),g(:)
  character(len=*),optional :: fname
  integer(i32),optional :: iunit
  integer(i32) :: istat
  if (present(iunit)) then
    write(iunit,'(a)',iostat=istat) str(a)//str(b)//str(c)//str(d)//str(e)//str(f)//str(g)
    call Ferr(istat,fname,3)
  else
    write(output_unit,'(a)') str(a)//str(b)//str(c)//str(d)//str(e)//str(f)//str(g)
  endif
  end subroutine
  !====================================================================!
end module
