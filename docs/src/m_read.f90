module m_readline
  !! Contains core routines to read different combinations of scalars and 1D arrays from a file
  use variableKind
  use m_errors, only: Ferr
  use m_strings, only: isString
  implicit none

  private

  public :: readline

  interface readline
    !! Read in multiple items from a line
    !!
    !! Example usage
    !!```fortran
    !!use variableKind
    !!use m_readline
    !!character(len=:),allocatable :: s
    !!real(r64) :: a,b,c
    !!real(r64) :: a1D(3)
    !!s = '1.0 2.0 3.0'
    !!call readline(a, b, c, 'No File Name', 0, s) ! Read 3 scalars from a string
    !!write(*,*) 'a should equal 1.0 ',a == 1.d0
    !!write(*,*) 'b should equal 2.0 ',b == 2.d0
    !!write(*,*) 'c should equal 3.0 ',c == 3.d0
    !!call readline(a1D, 'No File Name', 0, s) ! Read a length 3 1D array from a string
    !!write(*,*) 'a1D should equal [1.0,2.0,3.0] ',all(a == [1.d0,2.d0,3.d0])
    !!```
  module procedure :: readLine_a,readLine_ab,readLine_abc,readLine_abcd,readLine_abcde
  module procedure :: readLine_av,readLine_avbv,readLine_avbvcv,readLine_abv,readLine_abvcv,readLine_abcdev
  module procedure :: readLine_abcdv,readLine_abcdvev,readLine_abcv,readLine_abcvdv,readLine_abcdefv,readLine_abcdefvgv
  end interface

contains
  !====================================================================!
  subroutine readLine_a(a,fname,iunit,buf)
    !! Interfaced with readLine()
  !====================================================================!
  real(r64), intent(out) :: a
    !! Number
  character(len=*), intent(in) :: fname
    !! File name
  integer(i32), intent(in) :: iunit
    !! File id number to read from
  character(len=*), intent(in), optional :: buf
    !! Character string to read from instead of the line in the file
  integer(i32) :: istat
  if (present(buf)) then
    read(buf,*,iostat=istat) a
  else
    read(iunit,*,iostat=istat) a
  endif
  call Ferr(istat,fname,2)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine readLine_ab(a,b,fname,iunit,buf)
    !! Interfaced with readLine()
  !====================================================================!
  real(r64), intent(out) :: a
      !! Number
  real(r64), intent(out) :: b
      !! Number
  character(len=*), intent(in) :: fname
    !! File name
  integer(i32), intent(in) :: iunit
    !! File id number to read from
  character(len=*), intent(in), optional :: buf
    !! Character string to read from instead of the line in the file
  integer(i32) :: istat
  if (present(buf)) then
    read(buf,*,iostat=istat) a,b
  else
    read(iunit,*,iostat=istat) a,b
  endif
  call Ferr(istat,fname,2)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine readLine_abc(a,b,c,fname,iunit,buf)
    !! Interfaced with readLine()
  !====================================================================!
  ! reads a b c on a single line
  real(r64), intent(out) :: a
      !! Number
  real(r64), intent(out) :: b
      !! Number
  real(r64), intent(out) :: c
      !! Number
  character(len=*), intent(in) :: fname
    !! File name
  integer(i32), intent(in) :: iunit
    !! File id number to read from
  character(len=*), intent(in), optional :: buf
    !! Character string to read from instead of the line in the file
  integer(i32) :: istat
  if (present(buf)) then
    read(buf,*,iostat=istat) a,b,c
  else
    read(iunit,*,iostat=istat) a,b,c
  endif
  call Ferr(istat,fname,2)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine readLine_abcd(a,b,c,d,fname,iunit,buf)
    !! Interfaced with readLine()
  !====================================================================!
  ! reads a b c d on a single line
  real(r64), intent(out) :: a
    !! Number
  real(r64), intent(out) :: b
    !! Number
  real(r64), intent(out) :: c
    !! Number
  real(r64), intent(out) :: d
    !! Number
  character(len=*), intent(in) :: fname
    !! File name
  integer(i32), intent(in) :: iunit
    !! File id number to read from
  character(len=*), intent(in), optional :: buf
    !! Character string to read from instead of the line in the file
  integer(i32) :: istat
  if (present(buf)) then
    read(buf,*,iostat=istat)a,b,c,d
  else
    read(iunit,*,iostat=istat)a,b,c,d
  endif
  call Ferr(istat,fname,2)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine readLine_abcde(a,b,c,d,e,fname,iunit,buf)
    !! Interfaced with readLine()
  !====================================================================!
  ! reads a b c on a single line
  real(r64), intent(out) :: a
    !! Number
  real(r64), intent(out) :: b
    !! Number
  real(r64), intent(out) :: c
    !! Number
  real(r64), intent(out) :: d
    !! Number
  real(r64), intent(out) :: e
    !! Number
  character(len=*), intent(in) :: fname
    !! File name
  integer(i32), intent(in) :: iunit
    !! File id number to read from
  character(len=*), intent(in), optional :: buf
    !! Character string to read from instead of the line in the file
  integer(i32) :: istat
  if (present(buf)) then
    read(buf,*,iostat=istat) a,b,c,d,e
  else
    read(iunit,*,iostat=istat) a,b,c,d,e
  endif
  call Ferr(istat,fname,2)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine readLine_av(a,fname,iunit,buf)
    !! Interfaced with readLine()
  !====================================================================!
  ! reads a(1),...,a(Na) on a single line
  real(r64), intent(out) :: a(:)
    !! Vector
  character(len=*), intent(in) :: fname
    !! File name
  integer(i32), intent(in) :: iunit
    !! File id number to read from
  character(len=*), intent(in), optional :: buf
    !! Character string to read from instead of the line in the file
  integer(i32) :: i,Na,istat
  Na=size(a)
  if (present(buf)) then
    read(buf,*,iostat=istat) (a(i),i=1,Na)
  else
    read(iunit,*,iostat=istat) (a(i),i=1,Na)
  endif
  call Ferr(istat,fname,2)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine readLine_avbv(a,b,fname,iunit,buf)
    !! Interfaced with readLine()
  !====================================================================!
  ! reads a(1),...,a(Na),b(1),...,b(Nb) on a single line
  real(r64), intent(out) :: a(:)
    !! Vector
  real(r64), intent(out) :: b(:)
    !! Vector
  character(len=*), intent(in) :: fname
    !! File name
  integer(i32), intent(in) :: iunit
    !! File id number to read from
  character(len=*), intent(in), optional :: buf
    !! Character string to read from instead of the line in the file
  integer(i32) :: i,Na,Nb,istat
  Na=size(a);Nb=size(b)
  if (present(buf)) then
    read(buf,*,iostat=istat) (a(i),i=1,Na),(b(i),i=1,Nb)
  else
    read(iunit,*,iostat=istat) (a(i),i=1,Na),(b(i),i=1,Nb)
  endif
  call Ferr(istat,fname,2)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine readLine_avbvcv(a,b,c,fname,iunit,buf)
    !! Interfaced with readLine()
  !====================================================================!
  ! reads a(1),...,a(Na),b(1),...,b(Nb) on a single line
  real(r64), intent(out) :: a(:)
    !! Vector
  real(r64), intent(out) :: b(:)
    !! Vector
  real(r64), intent(out) :: c(:)
    !! Vector
  character(len=*), intent(in) :: fname
    !! File name
  integer(i32), intent(in) :: iunit
    !! File id number to read from
  character(len=*), intent(in), optional :: buf
    !! Character string to read from instead of the line in the file
  integer(i32) :: i,Na,Nb,Nc,istat
  Na=size(a);Nb=size(b);Nc=size(c)
  if (present(buf)) then
    read(buf,*,iostat=istat) (a(i),i=1,Na),(b(i),i=1,Nb),(c(i),i=1,Nc)
  else
    read(iunit,*,iostat=istat) (a(i),i=1,Na),(b(i),i=1,Nb),(c(i),i=1,Nc)
  endif
  call Ferr(istat,fname,2)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine readLine_abv(a,b,fname,iunit,buf)
    !! Interfaced with readLine()
  !====================================================================!
  ! reads a,b(1),...,b(Nb) on a single line
  real(r64), intent(out) :: a
    !! Number
  real(r64), intent(out) :: b(:)
    !! Vector
  character(len=*), intent(in) :: fname
    !! File name
  integer(i32), intent(in) :: iunit
    !! File id number to read from
  character(len=*), intent(in), optional :: buf
    !! Character string to read from instead of the line in the file
  integer(i32) :: i,Nb,istat
  Nb=size(b)
  if (present(buf)) then
    read(buf,*,iostat=istat) a,(b(i),i=1,Nb)
  else
    read(iunit,*,iostat=istat) a,(b(i),i=1,Nb)
  endif
  call Ferr(istat,fname,2)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine readLine_abvcv(a,b,c,fname,iunit,buf)
    !! Interfaced with readLine()
  !====================================================================!
  ! reads a,b(1),...,b(Nb),c(1),...,c(Nc) on a single line
  real(r64), intent(out) :: a
    !! Number
  real(r64), intent(out) :: b(:)
    !! Vector
  real(r64), intent(out) :: c(:)
    !! Vector
  character(len=*), intent(in) :: fname
    !! File name
  integer(i32), intent(in) :: iunit
    !! File id number to read from
  character(len=*), intent(in), optional :: buf
    !! Character string to read from instead of the line in the file
  integer(i32) :: i,Nb,Nc,istat
  Nb=size(b);Nc=size(c)
  if (present(buf)) then
    read(buf,*,iostat=istat) a,(b(i),i=1,Nb),(c(i),i=1,Nc)
  else
    read(iunit,*,iostat=istat) a,(b(i),i=1,Nb),(c(i),i=1,Nc)
  endif
  if (.not. isString(fname,''))call Ferr(istat,fname,2)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine readLine_abcv(a,b,c,fname,iunit,buf)
    !! Interfaced with readLine()
  !====================================================================!
  ! reads a,b,c(1),...,c(Nc) on a single line
  real(r64), intent(out) :: a
    !! Number
  real(r64), intent(out) :: b
    !! Number
  real(r64), intent(out) :: c(:)
    !! Vector
  character(len=*), intent(in) :: fname
    !! File name
  integer(i32), intent(in) :: iunit
    !! File id number to read from
  character(len=*), intent(in), optional :: buf
    !! Character string to read from instead of the line in the file
  integer(i32) :: i,Nc,istat
  Nc=size(c)
  if (present(buf)) then
    read(buf,*,iostat=istat) a,b,(c(i),i=1,Nc)
  else
    read(iunit,*,iostat=istat) a,b,(c(i),i=1,Nc)
  endif
  if (.not. isString(fname,'')) call Ferr(istat,fname,2)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine readLine_abcvdv(a,b,c,d,fname,iunit,buf)
    !! Interfaced with readLine()
  !====================================================================!
  ! reads a,b,c(1),...,c(Nc),d(1),...,d(Nd) on a single line
  real(r64), intent(out) :: a
    !! Number
  real(r64), intent(out) :: b
    !! Number
  real(r64), intent(out) :: c(:)
    !! Vector
  real(r64), intent(out) :: d(:)
    !! Vector
  character(len=*), intent(in) :: fname
    !! File name
  integer(i32), intent(in) :: iunit
    !! File id number to read from
  character(len=*), intent(in), optional :: buf
    !! Character string to read from instead of the line in the file
  integer(i32) :: i,Nc,Nd,istat
  Nc=size(c);Nd=size(d)
  if (present(buf)) then
    read(buf,*,iostat=istat) a,b,(c(i),i=1,Nc),(d(i),i=1,Nd)
  else
    read(iunit,*,iostat=istat) a,b,(c(i),i=1,Nc),(d(i),i=1,Nd)
  endif
  if (.not. isString(fname,''))call Ferr(istat,fname,2)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine readLine_abcdv(a,b,c,d,fname,iunit,buf)
    !! Interfaced with readLine()
  !====================================================================!
  ! reads a,b,c,d(1),...,d(Nd) on a single line
  real(r64), intent(out) :: a
    !! Number
  real(r64), intent(out) :: b
    !! Number
  real(r64), intent(out) :: c
    !! Number
  real(r64), intent(out) :: d(:)
    !! Vector
  character(len=*), intent(in) :: fname
    !! File name
  integer(i32), intent(in) :: iunit
    !! File id number to read from
  character(len=*), intent(in), optional :: buf
    !! Character string to read from instead of the line in the file
  integer(i32) :: i,Nd,istat
  Nd=size(d)
  if (present(buf)) then
    read(buf,*,iostat=istat) a,b,c,(d(i),i=1,Nd)
  else
    read(iunit,*,iostat=istat) a,b,c,(d(i),i=1,Nd)
  endif
  call Ferr(istat,fname,2)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine readLine_abcdvev(a,b,c,d,e,fname,iunit,buf)
    !! Interfaced with readLine()
  !====================================================================!
  ! reads a,b,c,d(1),...,d(Nd),e(1),...,e(Ne) on a single line
  real(r64), intent(out) :: a
    !! Number
  real(r64), intent(out) :: b
    !! Number
  real(r64), intent(out) :: c
    !! Number
  real(r64), intent(out) :: d(:)
    !! Vector
  real(r64), intent(out) :: e(:)
    !! Vector
  character(len=*), intent(in) :: fname
    !! File name
  integer(i32), intent(in) :: iunit
    !! File id number to read from
  character(len=*), intent(in), optional :: buf
    !! Character string to read from instead of the line in the file
  integer(i32) :: i,Nd,Ne,istat
  Nd=size(d);Ne=size(e)
  if (present(buf)) then
    read(buf,*,iostat=istat) a,b,c,(d(i),i=1,Nd),(e(i),i=1,Ne)
  else
    read(iunit,*,iostat=istat) a,b,c,(d(i),i=1,Nd),(e(i),i=1,Ne)
  endif
  call Ferr(istat,fname,2)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine readLine_abcdev(a,b,c,d,e,fname,iunit,buf)
    !! Interfaced with readLine()
  !====================================================================!
  ! reads a,b,c,d,e(1),...,e(Ne) on a single line
  real(r64), intent(out) :: a
    !! Number
  real(r64), intent(out) :: b
    !! Number
  real(r64), intent(out) :: c
    !! Number
  real(r64), intent(out) :: d
    !! Number
  real(r64), intent(out) :: e(:)
    !! Vector
  character(len=*), intent(in) :: fname
    !! File name
  integer(i32), intent(in) :: iunit
    !! File id number to read from
  character(len=*), intent(in), optional :: buf
    !! Character string to read from instead of the line in the file
  integer(i32) :: i,Ne,istat
  Ne=size(e)
  if (present(buf)) then
    read(buf,*,iostat=istat) a,b,c,d,(e(i),i=1,Ne)
  else
    read(iunit,*,iostat=istat) a,b,c,d,(e(i),i=1,Ne)
  endif
  call Ferr(istat,fname,2)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine readLine_abcdefv(a,b,c,d,e,f,fname,iunit,buf)
    !! Interfaced with readLine()
  !====================================================================!
  ! reads a,b,c,d,e,f(1),...,f(Nf) on a single line
  real(r64), intent(out) :: a
    !! Number
  real(r64), intent(out) :: b
    !! Number
  real(r64), intent(out) :: c
    !! Number
  real(r64), intent(out) :: d
    !! Number
  real(r64), intent(out) :: e
    !! Number
  real(r64), intent(out) :: f(:)
    !! Vector
  character(len=*), intent(in) :: fname
    !! File name
  integer(i32), intent(in) :: iunit
    !! File id number to read from
  character(len=*), intent(in), optional :: buf
    !! Character string to read from instead of the line in the file
  integer(i32) :: i,Nf,istat
  Nf=size(f)
  if (present(buf)) then
    read(buf,*,iostat=istat) a,b,c,d,e,(f(i),i=1,Nf)
  else
    read(iunit,*,iostat=istat) a,b,c,d,e,(f(i),i=1,Nf)
  endif
  call Ferr(istat,fname,2)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine readLine_abcdefvgv(a,b,c,d,e,f,g,fname,iunit,buf)
    !! Interfaced with readLine()
  !====================================================================!
  ! reads a,b,c,d,e,f(1),...,f(Nf),g(1),...,g(Ng) on a single line
  real(r64), intent(out) :: a
    !! Number
  real(r64), intent(out) :: b
    !! Number
  real(r64), intent(out) :: c
    !! Number
  real(r64), intent(out) :: d
    !! Number
  real(r64), intent(out) :: e
    !! Number
  real(r64), intent(out) :: f(:)
    !! Vector
  real(r64), intent(out) :: g(:)
    !! Vector
  character(len=*), intent(in) :: fname
    !! File name
  integer(i32), intent(in) :: iunit
    !! File id number to read from
  character(len=*), intent(in), optional :: buf
    !! Character string to read from instead of the line in the file
  integer(i32) :: i,Nf,Ng,istat
  Nf=size(f);Ng=size(g)
  if (present(buf)) then
    read(buf,*,iostat=istat) a,b,c,d,e,(f(i),i=1,Nf),(g(i),i=1,Ng)
  else
    read(iunit,*,iostat=istat) a,b,c,d,e,(f(i),i=1,Nf),(g(i),i=1,Ng)
  endif
  call Ferr(istat,fname,2)
  end subroutine
  !====================================================================!
end module
