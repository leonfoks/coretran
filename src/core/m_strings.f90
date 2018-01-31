  module m_strings
    !! Module provides string handling capabilities
  use iso_fortran_env, only: output_unit
  use variableKind
  use m_errors, only: wMsg, eMsg, Ferr, mErr
  use m_parameters, only: NaN, inf
  implicit none

  private

  public :: appendString
  public :: compact
  public :: countEntries
  public :: hasNentries
  public :: iachar1D
  public :: isString
  public :: lowercase
  public :: prependString
  public :: read1Dble
  public :: read1Integer
  public :: removeComments
  public :: readline
  public :: replacedelim
  public :: str
  public :: uppercase

  public :: printOptions

  interface str
    !! Interface to print a string representation of a number
    !! The output format options can be set using the printOptions class
    !!
    !! Example usage
    !!```fortran
    !!use variableKind
    !!use m_strings
    !!real(r64) :: arr(20)
    !!integer(i32) :: i
    !!integer(i32) :: j(5)
    !!arr = [(dble(i), i = 1,20)]
    !!i = 12
    !!j = [0, 1, 2, 3, 4]
    !!write(*,'(a)') str(i)//str(j)
    !!write(*,'(a)') str(j)//str(arr)
    !!write(*,'(a)') str(arr)
    !!printOptions%threshold = 0
    !!write(*,'(a)') str(arr)
    !!```
  module procedure :: str_r1,str_r1D
  module procedure :: str_d1,str_d1D,str_d2D
  module procedure :: str_i1,str_i1D,str_id1
  module procedure :: str_id1D
  module procedure :: str_s1,str_s1D
  module procedure :: str_1L
  end interface

  interface compactReal
    !! Returns a compact representation of a real number
    !! By default, truncates to 3 decimal places unless printOptions%precision is changed
  module procedure :: compactReal_d1!,compactReal_d1D
  end interface

  type, public :: c_printOptions
    !! Print options similar to numpy's print_options
    integer(i32) :: precision = 3
      !! Force this precision on the written number
    integer(i32) :: threshold = 10
      !! Omit the middle entries if the size is greater than threshold
    integer(i32) :: edgeitems = 3
      !! Only write the first and last threshold elements from rows and/or columns
    character(len=32) :: nanstr = 'nan'
      !! Print NAN as this
    character(len=32) :: infstr = 'inf'
      !! Print infinity as this
  end type

  type(c_printOptions) :: printOptions

  contains
  !====================================================================!
  subroutine ensure1Integer(N,fname,vName,iUnit)
    !! Forces the next line, read in from file contains a single integer
    !! Useful for ascii data files with a size specified in a header
  !====================================================================!
  integer(i32), intent(out) :: N
    !! Number
  character(len=*), intent(in) :: fname
    !! File name
  character(len=*), intent(in) :: vName
    !! Integer variable name for warning message
  integer(i32), intent(in) :: iunit
    !! File id number to read from
  character(len=cLen) :: buf
  integer(i32) :: istat

  ! Removes multiple spaces and tabs from the line
  ! Adjusts the string to the left
  call readline(iunit,buf,istat)
  call Ferr(istat,fname,2)

  call read1Integer(buf,N,istat)

  if (istat /= 0) then
    call eMsg('Reading a single integer to variable '//trim(vName)//' Value obtained = '//str(N))
  endif
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine read1Integer(buf,N,istat)
    !! Get a single integer from a line, returns an error code if there is more than one entry
  !====================================================================!
  character(len=*) :: buf
    !! String
  integer(i32) :: N
    !! Single integer
  integer(i32) :: istat
    !! istat > 0 if more than one entry is found
  integer(i32) :: i
  call compact(buf)
  ! Check if any spaces exist between 2 numbers(there should only be 1 number)
  i=scan(trim(buf),' ',.true.)
  read(buf,*,iostat=istat) N
  if (i > 2) istat=1
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine read1Dble(iunit,this,fname,vName,istat)
    !!TODO: CHECK THIS
  !====================================================================!
  ! Requires the line being read to contain ONLY one real number
  integer(i32), intent(in) :: iunit
  character(len=*) :: fname
  character(len=*) :: vName ! varvariable name for warning msg, if str='', no msg will write
  character(len=500) :: buf
  integer(i32) :: i,istat
  real(r64) :: this

  call readline(iunit,buf,istat)
  call Ferr(istat,fname,2)

  call compact(buf) !Need to do this to ensure that spaces after the number
  !Are not considered as multiple spaces, and to remove control
  !characters and tabs.

  ! Check if any spaces exist between 2 numbers(there should only be 1)
  i=scan(trim(buf),' ',.true.)

  read(buf,*,iostat=istat) this
  if (istat /= 0) then
    if (len_trim(vName)/=0) then
      call Wmsg(str(fname)//vName)
      write(output_unit,'(a)') trim(vName)//': ',this
    endif
  endif
  end subroutine
  !====================================================================!
  !====================================================================!
  function hasNentries(this,N) result(yes)
    !! Check that a string has N entries
  !====================================================================!
  character(len=*) :: this
  integer(i32) :: N
  logical :: yes
  yes=(countEntries(this)==N)
  end function
  !====================================================================!
  !====================================================================!
  function countEntries(this) result(N)
    !! Count the number of entries in a string
  !====================================================================!
  character(len=*) :: this
  integer(i32) :: N
  integer(i32) :: i,ich,lenstr
  character(len=1) :: c

  call compact(this)  ! Ensure only single spaces between items
  lenstr=len_trim(this)

  c=this(1:1)
  ich=iachar(c)
  if (ich >= 33) N=1

  do i=2,lenstr
    c=this(i:i)
    ich=iachar(c)
    if (ich==32) N=N+1
  enddo

  end function
  !====================================================================!
  !====================================================================!
  subroutine compact(this)
    !! Replace tabs and spaces with a single space
    !! str must be a variable and not an explicit 'string'. Otherwise adjustl will fail.
  !====================================================================!
  character(len=*):: this
  character(len=1):: s
  character(len=len_trim(this)) :: tmp
  integer(i32) :: i,iL,iRes
  integer(i32) :: N
  logical :: oneSpace

  N=len_trim(this)
  oneSpace = .true.
  iRes = 1
  tmp=''
  do i = 1, N
    s = this(i:i)
    iL = iachar(s)
    select case(iL)
    case(9,32) ! If tab or space, skip entries while they are tabs or spaces
      if (oneSpace) then
        tmp(iRes:iRes) = ' '
        oneSpace = .false.
        iRes = iRes + 1
      endif
    case(33:)
      tmp(iRes:iRes) = s
      oneSpace = .true.
      iRes = iRes + 1
    end select
  enddo
  this=tmp
  end subroutine
  !====================================================================!
  !====================================================================!
  function isNumeric(this) result(yes)
    !! Determine if the item in a string is numeric
  !====================================================================!
  character(len=*), intent(in) :: this
  logical :: yes
  real(r64) :: tmp
  integer(i32) :: istat
  read(this,*,iostat=istat) tmp
  yes=(istat==0)
  end function
  !====================================================================!
  !====================================================================!
  function compactReal_d1(this) result(res)
    !! Returns a compact representation of a real number
    !! By default, truncates to 3 decimal places unless dp is provided
  !====================================================================!
  real(r64), intent(in) :: this
    !! Double precision number
  character(len=:), allocatable :: res
    !! String
  character(len=1024) :: s
  character(len=9) :: ctmp
  real(r64) :: tmp
  integer(i32) :: p
  integer(i32) :: myP
  character(len=12) :: FMT
  ! Check for an NaN
  if (this /= this) then
    res = printOptions%nanstr
    return
  end if
  if (this > inf) then
    res = printOptions%infstr
    return
  end if
  myP = printOptions%precision
  tmp=dabs(this)
  select case(tmp < 1.d0)
  case(.true.)
    write(FMT,'("(es",i0,".",i0,")")') 7+myP,myP
    write(s,FMT) this
  case(.false.)
    p=floor(dlog10(tmp))
    if (p > printOptions%precision) then
      write(FMT,'("(es",i0,".",i0,")")') p+7+myP,myP
      write(s,FMT) this
    else
      if (this==0.d0) then
        s = '0.'
      else
        write(FMT,'("(f0.",i0,")")') myP
        write(s,FMT) this
        p=len_trim(s)
        if (all(iachar1D(s(p-myP+1:p))==48)) then
          ctmp=s(1:p-myP)
          s=''
          s=ctmp
        endif
      endif
    endif
  end select
  write(cTmp,1) 46,48,48,48,69,43,48,48
1 format(8(a1))
  if (verify(s,cTmp)==0) then ! Checks that zero isnt being written as 0.000e+00, and replaces with just 0
    s = '0.'
  endif
  s = adjustl(s)
  res = trim(s)
  end function
  !====================================================================!
  !====================================================================!
  subroutine replacedelim(this,dlim,dlimr)
  !! Replace a single character length delimiter in a string
  !====================================================================!
  character(len=*) :: this  !! Replace delim_miter in this
  character(len=*) :: dlim  !! Find this delim_miter
  character(len=*) :: dlimr !! Replace with this delim_miter
  integer(i32) :: i,it
  it=len(dlim)
  if (it /= len(dlimr)) call Emsg('replacedelim_m : un-equal length replacement')
  do i=1,len_trim(this)
    if (this(i:(i-1)+it)==dlim) this(i:(i-1)+it)=dlimr
  enddo
  end subroutine
  !====================================================================!
!  !====================================================================!
!  subroutine compactReal_d1D(this,res)
!    !! Use CompactReal on an r64 vector
!  !====================================================================!
!  real(r64) :: this(:)
!    !! 1D vector of numbers
!  character(len=*) :: res(:)
!    !! 1D vector of strings
!  integer(i32) :: i,N
!  N=size(this)
!  do i=1,N
!    call compactReal(this(i),res(i))
!  enddo
!  end subroutine
!  !====================================================================!
  !====================================================================!
  function str_r1(this,delim) result(res)
    !! Interfaced with str()
  !====================================================================!
  real(r32), intent(in) :: this
  character(len=*),optional, intent(in) :: delim
  character(len=:),allocatable :: res
  character(len=:),allocatable :: delim_
  delim_=' '
  if (present(delim)) delim_=delim
  res = compactReal(dble(this))//delim_
  end function
  !====================================================================!
  !====================================================================!
  function str_r1D(this,delim) result(res)
    !! Interfaced with str()
  !====================================================================!
  real(r32), intent(in) :: this(:)
  character(len=*),optional, intent(in) :: delim
  character(len=:),allocatable :: res
  character(len=:),allocatable :: delim_
  integer(i32) :: i,N
  N=size(this)
  res=''

  if (N < printOptions%threshold .or. printOptions%threshold == 0) then
    do i=1,N - 1
      res=res//str(this(i),delim)
    enddo
    res=res//str(this(N))
  else
    do i = 1, printOptions%edgeitems
      res = res // str(this(i),delim)
    end do
    delim_=' '
    if (present(delim)) delim_=delim
    res = res // '...'//delim_
    do i = N - printOptions%edgeitems + 1, N - 1
      res = res // str(this(i),delim)
    end do
    res=res//str(this(N))
  end if
  end function
  !====================================================================!
  !====================================================================!
  function str_d2D(this,delim) result(res)
    !! Interfaced with str()
  !====================================================================!
  real(r64), intent(in) :: this(:,:)
  character(len=*),optional, intent(in) :: delim
  character(len=:),allocatable :: res
  integer(i32) :: i,N
  N = size(this,1)
  res=''

  if (N < printOptions%threshold .or. printOptions%threshold == 0) then
    do i=1,N-1
      res = res // str_d1D(this(i,:),delim)//new_line('a')
    enddo
  else
    do i = 1, printOptions%edgeitems
      res = res // str_d1D(this(i,:),delim)//new_line('a')
    end do
    res = res //'...'//new_line('a')
    do i=N - printOptions%edgeitems + 1, N-1
      res = res // str_d1D(this(i,:),delim)//new_line('a')
    enddo
  endif
  res = res // str_d1D(this(N,:),delim)
  end function
  !====================================================================!
  !====================================================================!
  function str_d1D(this,delim) result(res)
    !! Interfaced with str()
  !====================================================================!
  real(r64), intent(in) :: this(:)
  character(len=*),optional, intent(in) :: delim
  character(len=:),allocatable :: res
  character(len=:),allocatable :: delim_
  integer(i32) :: i,N
  N=size(this)
  res=''

  if (N < printOptions%threshold .or. printOptions%threshold == 0) then
    do i=1,N - 1
      res=res//str(this(i),delim)
    enddo
    res=res//str(this(N))
  else
    do i = 1, printOptions%edgeitems
      res = res // str(this(i),delim)
    end do
    delim_=' '
    if (present(delim)) delim_=delim
    res = res // '...'//delim_
    do i = N - printOptions%edgeitems + 1, N - 1
      res = res // str(this(i),delim)
    end do
    res=res//str(this(N))
  end if
  end function
  !====================================================================!
  !====================================================================!
  function str_d1(this,delim) result(res)
    !! Interfaced with str()
  !====================================================================!
  real(r64), intent(in) :: this
  character(len=*),optional, intent(in) :: delim
  character(len=:),allocatable :: res
  character(len=:),allocatable :: delim_
  delim_=' '
  if (present(delim)) delim_=delim
  res = compactReal(this)//delim_
  end function
  !====================================================================!
  !====================================================================!
  function str_i1(this,delim) result(res)
    !! Interfaced with str()
  !====================================================================!
  integer(i32), intent(in) :: this
  character(len=*),optional, intent(in) :: delim
  character(len=cLen) :: tmp
  character(len=:),allocatable :: res
  character(len=:),allocatable :: delim_
  write(tmp,'(i0)') this
  delim_=' '
  if(present(delim)) delim_=delim
  res=trim(tmp)//delim_
  end function
  !====================================================================!
  !====================================================================!
  function str_id1(this,delim) result(res)
    !! Interfaced with str()
  !====================================================================!
  integer(i64), intent(in) :: this
  character(len=*),optional, intent(in) :: delim
  character(len=cLen) :: tmp
  character(len=:),allocatable :: res
  character(len=:),allocatable :: delim_
  write(tmp,'(i0)') this
  delim_=' '
  if(present(delim)) delim_=delim
  res=trim(tmp)//delim_
  end function
  !====================================================================!
  !====================================================================!
  function str_i2D(this,delim) result(res)
    !! Interfaced with str()
  !====================================================================!
  integer(i32), intent(in) :: this(:,:)
  character(len=*),optional, intent(in) :: delim
  character(len=:),allocatable :: res
  integer(i32) :: i,N
  N = size(this,1)
  res=''

  if (N < printOptions%threshold .or. printOptions%threshold == 0) then
    do i=1,N-1
      res = res // str_i1D(this(i,:),delim)//new_line('a')
    enddo
  else
    do i = 1, printOptions%edgeitems
      res = res // str_i1D(this(i,:),delim)//new_line('a')
    end do
    res = res // '...'//new_line('a')
    do i=N - printOptions%edgeitems + 1, N-1
      res = res // str_i1D(this(i,:),delim)//new_line('a')
    enddo
  endif
  res = res // str_i1D(this(N,:),delim)
  end function
  !====================================================================!
  !====================================================================!
  function str_i1D(this,delim) result(res)
    !! Interfaced with str()
  !====================================================================!
  integer(i32), intent(in) :: this(:)
  character(len=*),optional, intent(in) :: delim
  character(len=:),allocatable :: res
  character(len=:),allocatable :: delim_
  integer(i32) :: i,N
  N=size(this)
  res=''
  if (N < printOptions%threshold .or. printOptions%threshold == 0) then
    do i=1,N - 1
      res=res//str(this(i),delim)
    enddo
    res=res//str(this(N))
  else
    do i = 1, printOptions%edgeitems
      res = res // str(this(i),delim)
    end do
    delim_=' '
    if (present(delim)) delim_=delim
    res = res // '...'//delim_
    do i = N - printOptions%edgeitems + 1, N - 1
      res = res // str(this(i),delim)
    end do
    res=res//str(this(N))
  end if
  end function
  !====================================================================!
  !====================================================================!
  function str_id2D(this,delim) result(res)
    !! Interfaced with str()
  !====================================================================!
  integer(i64), intent(in) :: this(:,:)
  character(len=*),optional, intent(in) :: delim
  character(len=:),allocatable :: res
  integer(i32) :: i,N
  N = size(this,1)
  res=''

  if (N < printOptions%threshold .or. printOptions%threshold == 0) then
    do i=1,N-1
      res = res // str_id1D(this(i,:),delim)//new_line('a')
    enddo
  else
    do i = 1, printOptions%edgeitems
      res = res // str_id1D(this(i,:),delim)//new_line('a')
    end do
    res = res // '...'//new_line('a')
    do i=N - printOptions%edgeitems + 1, N-1
      res = res // str_id1D(this(i,:),delim)//new_line('a')
    enddo
  endif
  res = res // str_id1D(this(N,:),delim)
  end function
  !====================================================================!
  !====================================================================!
  function str_id1D(this,delim) result(res)
    !! Interfaced with str()
  !====================================================================!
  integer(i64), intent(in) :: this(:) !! 1D array
  character(len=*),optional, intent(in) :: delim
  character(len=:),allocatable :: res
  character(len=:),allocatable :: delim_
    !! String
  integer(i32) :: i,N
  N=size(this)
  res=''
  if (N < printOptions%threshold .or. printOptions%threshold == 0) then
    do i=1,N - 1
      res=res//str(this(i),delim)
    enddo
    res=res//str(this(N))
  else
    do i = 1, printOptions%edgeitems
      res = res // str(this(i),delim)
    end do
    delim_=' '
    if (present(delim)) delim_=delim
    res = res // '...'//delim_
    do i = N - printOptions%edgeitems + 1, N - 1
      res = res // str(this(i),delim)
    end do
    res=res//str(this(N))
  end if
  end function
  !====================================================================!
  !====================================================================!
  function str_s1(this,delim) result(res)
    !! Interfaced with str()
  !====================================================================!
  character(len=*), intent(in) :: this
  character(len=*),optional, intent(in) :: delim
  character(len=:),allocatable :: res
  character(len=:),allocatable :: delim_
  delim_=' '
  if(present(delim)) delim_=delim
  res = trim(this)//delim_
  end function
  !====================================================================!
  !====================================================================!
  function str_s1D(this,delim) result(res)
    !! Interfaced with str()
  !====================================================================!
  character(len=*), intent(in) :: this(:)
  character(len=*),optional, intent(in) :: delim
  character(len=:),allocatable :: res
  integer(i32) :: i,N
  N=size(this)
  res=''
  do i=1,N - 1
    res=res//str_s1(this(i),delim)
  enddo
  res=res//str(this(N))
  end function
  !====================================================================!
  !====================================================================!
  function str_1L(this,delim) result(res)
    !! Interfaced with str()
  !====================================================================!
  logical, intent(in) :: this
  character(len=*),optional, intent(in) :: delim
  character(len=:),allocatable :: res
  character(len=:),allocatable :: delim_
  delim_=' '
  if (present(delim)) delim_=delim
  if (this) then
    res='True'//delim_
  else
    res='False'//delim_
  endif
  end function
  !====================================================================!
  !====================================================================!
  function lowerCase(str) result(res)
    !! Convert a string to lowercase
  !====================================================================!
  character (len=*) :: str
  character(len=len_trim(str)) :: res
  character(len=1) :: s
  integer(i32) :: i,iSft
  integer(i32) :: N
  iSft = iachar('A') - iachar('a')
  N = len_trim(str)
  res = str
  do i = 1, N
    s = str(i:i)
    select case(s)
    case('A' : 'Z')
      res(i:i) = achar(iachar(s) - iSft)
    end select
  enddo

  end function lowercase
  !====================================================================!
  !====================================================================!
  function upperCase(str) result(res)
    !! Convert a string to uppercase
  !====================================================================!
  character (len=*) :: str
  character(len=len_trim(str)) :: res
  character(len=1) :: s
  integer(i32) :: i,iSft
  integer(i32) :: N
  iSft = iachar('A') - iachar('a')
  N = len_trim(str)
  res = str
  do i = 1, N
    s = res(i:i)
    select case(s)
    case('a' : 'z')
      res(i:i) = achar(iachar(s) + iSft)
    end select
  enddo

  end function upperCase
  !====================================================================!
  !====================================================================!
  function isString(tmp1,tmp2,exact_) result(yes)
    !! Match two string together
  !====================================================================!
  character(len=*) :: tmp1
    !! Compare this string
  character(len=*) :: tmp2
    !! Compare this string
  logical,optional :: exact_
    !! Optional logical, if true, the strings are not converted to lowercase before comparison
  logical :: yes,exact
  character(len=len_trim(tmp1)) :: this
  character(len=len_trim(tmp2)) :: that
  this='';that=''
  this(1:len_trim(tmp1))=trim(tmp1)
  that(1:len_trim(tmp2))=trim(tmp2)
  yes=.false.
  exact=.false.
  if (present(exact_))exact=exact_
  if (exact) then
    if (trim(this)==trim(that)) yes=.true.
  else
    if (lowercase(trim(this))==lowercase(trim(that))) yes=.true.
  endif
  end function
  !====================================================================!
  !====================================================================!
  subroutine readline(iUnit,line,istat)
    !! Reads a line from a file, ignoring any comments
  !====================================================================!
  integer(i32) :: iUnit !! File ID number
  character(len=*) :: line !! Character string to read the line into
  integer(i32) :: istat !! Error Status
  integer(i32) :: ipos

!  go = .true.
!  do while(go)
!    read(iUnit,'(a)',iostat=istat, end = 1) line
!    call removeBOM(line) ! Remove the byte order mark if present
!    i = index(line,'!') ! Get the location of the comment
!
!  enddo
  do
    read(iUnit,'(a)', iostat=istat, end=1) line      ! read input line
    call compact(line)
    if(istat /= 0) return
    call removeBOM(line)
    line=adjustl(line)
    ipos=index(line,'!')
    if(ipos == 1) cycle
    if(ipos /= 0) line=line(:ipos-1)
    if(len_trim(line) /= 0) exit
  end do
  return
1 istat=1
  end subroutine readline
  !====================================================================!
  !====================================================================!
  elemental subroutine removeBOM(this)
    !! Removes the byte order mark from the beginning of a string
  !====================================================================!
  character(len=*), intent(inout) :: this
  if (iachar(this(1:1))==239 .and. iachar(this(2:2))==187 .and. iachar(this(3:3))==191) this(1:3)='   '
  end subroutine
  !====================================================================!
  !====================================================================!
  elemental subroutine removeComments(this)
  !! Removes the text after the ! mark in a string
  !====================================================================!
  character(len=*), intent(inout) :: this
  character(len=:), allocatable :: tmp
  integer(i32) :: i,length
  length=len_trim(this)
  tmp=this;this=''
  i=index(tmp,'!')
  this(1:i-1) = tmp(1:i-1)
  end subroutine
  !====================================================================!
!====================================================================!
!! Replace a substring with another in a string
!====================================================================!
subroutine replace(this, sub1, sub2)
  character(len=*) :: this
  character(len=*) :: sub1
  character(len=*) :: sub2

end subroutine
!====================================================================!
  !====================================================================!
  function appendString(this,that,delim_) result(res)
    !! Append a string
  !====================================================================!
  character(len=*) :: this
    !! String to append to
  character(len=*) :: that
    !! String to append
  character(len=*),optional :: delim_
    !! Optional delimiter to separate the append
  character(len=:), allocatable :: res
    !! Appended String
  if (present(delim_)) then
    res = trim(this)//trim(delim_)//trim(that)
  else
    res = trim(this)//trim(that)
  endif
  end function
  !====================================================================!
  !====================================================================!
  function prependString(this,that,delim_) result(res)
    !! Prepend a string
  !====================================================================!
  character(len=*) :: this
    !! String to prepend to
  character(len=*) :: that
    !! String to prepend
  character(len=*),optional :: delim_
    !! Optional delimiter to separate the append
  character(len=:), allocatable :: res
    !! Prepended String
  if (present(delim_)) then
    res = trim(that)//trim(delim_)//trim(this)
  else
    res = trim(that)//trim(this)
  endif
  end function
  !====================================================================!
  !====================================================================!
  function iachar1D(this) result(res)
    !! Use iachar on a full string
    !!
    !! Cannot overload to intrinsic iachar because of ambiguity
  !====================================================================!
  character(len=*), intent(in) :: this
  integer(i32), allocatable :: res(:)
  integer(i32) :: i,istat
  integer(i32) :: N
  N = len_trim(this)
  allocate(res(N), stat=istat); call mErr(istat,'iachar:result',2)
  do i =1, N
    res(i) = iachar(this(i:i))
  end do
  end function
  !====================================================================!
  end module
