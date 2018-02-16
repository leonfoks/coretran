  module m_fileIO
    !! Contains functions and subroutines that inquire and operate on files
    !! including reading and writing multiple entries to a file
  use variableKind, only: i32,r64
  use m_strings, only: compact, lowercase, isString
  use m_errors, only: Emsg, Ferr
  implicit none

  interface isOpen
    module procedure :: isOpen_s, isOpen_i1
  end interface

  contains

  !====================================================================!
  function fileExists(fName) result(yes)
  !! Checks whether the file with name fName exists on disk
  !====================================================================!
  character(len=*), intent(in) :: fName
    !! File name to check
  logical :: yes
    !! Exists?
  character(len=len_trim(fName)) :: this
  this='';this=trim(fName)
  ! Inquire as to whether the file exists on disk or not
  inquire(FILE=trim(this),EXIST=yes)
  end function
  !====================================================================!
  !====================================================================!
  function hasExtension(fName,extension) result(yes)
  !====================================================================!
  !! Checks if a file 'fname' is of type 'extension'
  character(len=*),intent(in) :: fName
    !! File name
  character(len=3),intent(in) :: extension
    !! Extension to find
  logical :: yes
    !! Has this extension?
  ! Function Declarations
  integer(i32) :: i,ilen
  ilen=len_trim(fName)
  i=scan(fName,'.',.true.) ! Get the location of the dot
  ! If the three entries after the dot match, return yes
  yes = isString(fName(i+1:i+3),extension)
  end function
  !====================================================================!
  
  !====================================================================!
  subroutine checkIsOpen(fName)
    !! Checks whether a file is open with an error message if not
  !====================================================================!
  character(len=*),intent(in) :: fName
    !! File name
  if (.not.isOpen(fName)) call eMsg("File "//trim(fName)//" is not open.")
  end subroutine
  !====================================================================!

  !====================================================================!
  function isOpen_s(fname) result(yes)
    !! Is the file open or not
  !====================================================================!
  character(len=*),intent(in) :: fname
    !! File name
  logical :: yes
    !! Is the file open?
  inquire(file=trim(fname), opened=yes)
  end function
  !====================================================================!
  !====================================================================!
  function isOpen_i1(iUnit) result(yes)
    !! Is the file open or not
  !====================================================================!
  integer(i32), intent(in) :: iUnit
    !! File name
  logical :: yes
    !! Is the file open?
  inquire(unit=iUnit, opened=yes)
  end function
  !====================================================================!

  !====================================================================!
  subroutine openFile(fname, iunit, stat, istat, fixedUnit)
    !! Open a file and perform necessary checks for failure
    !! stat should be 'new','old','unknown','append'
  !====================================================================!
  character(len=*), intent(in) :: fname
    !! File name
  integer(i32), intent(out) :: iunit
    !! Unit number returned
  character(len=*), intent(in) :: stat
    !! Status of the file you are opening
  integer(i32),intent(out) :: istat
    !! Error flag
  logical, intent(in), optional :: fixedUnit

  character(len=len_trim(fname)) :: this
  this='';this=trim(fname)
  call compact(this)
  select case(lowercase(trim(stat)))
  case('new','old','unknown')
    open(newunit=iunit,file=trim(this),status=stat,iostat=istat)
  case('append')
    open(newunit=iunit,file=trim(this),access=stat,status='old',iostat=istat)
  case default
    call Emsg('openFile : Invalid status [new,old,unknown,append]')
  end select
  call Ferr(istat,this,1)
  end subroutine
  !====================================================================!

  !====================================================================!
  subroutine openBinaryFile(fname,iunit,stat,istat)
    !! Open an unformatted binary file
    !! stat should be 'new','old','unknown','append'
  !====================================================================!
  character(len=*), intent(in) :: fname
    !! File Name
  integer(i32), intent(out) :: iunit
    !! Unit number returned
  character(len=*), intent(in) :: stat
    !! Status of the file you are opening
  integer(i32), intent(out) :: istat
    !! Error Flag
  select case(lowercase(trim(stat)))
  case('new','old','unknown')
    open(newunit=iunit,file=trim(fname),form='unformatted',status=stat,iostat=istat)
  case('append')
    open(newunit=iunit,file=trim(fname),form='unformatted',access=stat,status='old',iostat=istat)
  case default
    call Emsg('openBinaryFile : Invalid status [new,old,unknown,append]')
  end select
  call Ferr(istat,fname,1)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine closeFile(fname,iunit,stat,istat)
    !! Close a file and perform any necessary checks
  !====================================================================!
  character(len=*), intent(in) :: fname
    !! File name
  integer(i32), intent(in) :: iunit
    !! Unit number returned
  character(len=*), intent(in) :: stat
    !! Status of the file you are closing
  integer(i32), intent(out) :: istat
    !! Error Flag

  character(len=len_trim(fname)) :: this

  this='';this=trim(fname)
  call compact(this)
  select case(lowercase(trim(stat)))
  case('delete')
    close(iunit,status='delete',iostat=istat)
    case default
    close(iunit,iostat=istat)
  end select
  call Ferr(istat,this,4)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine deleteFile(fname)
    !! Deletes a file on disk
  !====================================================================!
  character(len=*), intent(in) :: fname
    !! File name to delete
  integer(i32) :: u,istat
  open(newunit=u,file=fname,status='old',iostat=istat)
  if(istat==0) close(u,status='delete')
  end subroutine
  !====================================================================!
  !====================================================================!
  function getFileSize(fName) result(that)
    !! Get the file size in Bytes
  !====================================================================!
  character(len=*), intent(in) :: fName
    !! File name
  integer(i32) :: that
    !! Size of the file
  that = 0
  if (.not. fileExists(fName)) return
  inquire(file=trim(fName),size=that)
  end function
  !====================================================================!
  !====================================================================!
  function getNFileLines(fName,nHeader) result(N)
    !! Counts the number of lines in a file after the number of specified header lines
  !====================================================================!
  character(len=*), intent(in) :: fName
    !! File name
  integer(i32), intent(in), optional :: nHeader ! Skip Lines
    !! Skip this number of lines at the top of the file
  integer(i32) :: N
    !! Number of lines in the file
  integer(i32) :: iunit,istat
  call openFile(fName,iunit,'old',istat)
  if (present(nHeader)) then
    if (nHeader > 0) call skipFileLines(iunit,nHeader)
  endif
  N=0
  read(iunit,'(a)',iostat=istat)
  do while(istat == 0)
    N=N+1
    read(iunit,'(a)',iostat=istat)
  enddo
  call closeFile(fName,iunit,'',istat)
  end function
  !====================================================================!
  !====================================================================!
  subroutine skipFileLines(iunit,N)
    !! Skip N lines in a file
  !====================================================================!
  integer(i32), intent(in) :: iunit
    !! Unit number to skip
  integer(i32) , intent(in):: N
    !! Number of lines to skip
  integer(i32) :: i
  do i=1,N
    read(iunit,*)
  enddo
  end subroutine
  !====================================================================!
  !====================================================================!
  function getExtension(fName) result(that)
    !! Get the extension of a file
  !====================================================================!
  character(len=*), intent(in) :: fName
    !! File name
  character(len=:), allocatable :: that
    !! File extension
  integer(i32) :: i,N
  N=len_trim(fName)
  i=scan(fName,'.')
  if(i == 0) call Emsg('getExtension : Filename '//trim(fName)//' needs an extension (.txt?)')
  that=fName(i+1:N)
  end function
  !====================================================================!
  !====================================================================!
  function trimExtension(fName) result(that)
    !! Trims the extension of a filename
  !====================================================================!
  character(len=*), intent(in) :: fName
    !! File name
  character(len=:), allocatable :: that
    !! File name without the extension
  integer(i32) :: i
  i=scan(fName,'.'); if(i == 0) call Emsg('trimExtension : Filename '//trim(fName)//' needs an extension (.txt?)')
  that=fName(1:i-1)
  end function
  !====================================================================!
end module
