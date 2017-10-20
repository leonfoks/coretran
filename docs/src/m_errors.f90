  module m_errors
    !!Module contains error handling procedures
    use iso_fortran_env, only: output_unit
    use variableKind
    implicit none
    contains
    !====================================================================!
    subroutine mErr(istat, aMsg, alloc, iunit)
      !! Checks for successful (de)allocation.  Stops the code.
      !!
      !! Use this after an allocate/deallocate statement
      !! allocate(x(nz,ny,nx), stat=istat); call mErr(istat,'x',1)
      !! deallocate(x, stat=istat); call mErr(istat,'x',2)
    !====================================================================!
    integer(i32), intent(in) :: istat !! results of stat=istat in (de)allocate
    character(len=*), intent(in) :: aMsg !! Message associated with the (de)allocate
    integer(i32), intent(in) :: alloc !! 1 = allocate, 2 = deallocate
    integer(i32), intent(in), optional :: iunit !! Optional file id to write the message to
    character(len=:), allocatable :: tmp

    ! If istat is not zero, there is an error.
    tmp=''
    if (istat /= 0) then
      select case(alloc)
      case(1)
        tmp='Allocating Memory: '//trim(aMsg)
      case(2)
        tmp='Deallocating Memory: '//trim(aMsg)
      end select
      call eMsg(tmp,iunit)
    endif
    end subroutine
    !====================================================================!
    !====================================================================!
    subroutine msg(aMsg,iunit)
      !! Write a message
    !====================================================================!
    character(len=*), intent(in) :: aMsg !! Message to write
    integer(i32), intent(in), optional :: iunit !! file id to write the message to

    if (present(iunit)) then
      write(iunit,'(a)') trim(aMsg)
      write(output_unit,'(a)') trim(aMsg)
    else
      write(output_unit,'(a)') trim(aMsg)
    endif
    end subroutine
    !====================================================================!
    !====================================================================!
    subroutine wMsg(aMsg,iunit)
      !! Write a Warning message
    !====================================================================!
    character(len=*), intent(in) :: aMsg !! Message to write
    integer(i32), intent(in), optional :: iunit !! file id to write the message to
    call Msg('Warning:'//trim(aMsg),iunit)
    end subroutine
    !====================================================================!
    !====================================================================!
    subroutine eMsg(aMsg,iunit)
      !! Write an Error message. Stops the code
    !====================================================================!
    character(len=*), intent(in) :: aMsg !! Message to write
    integer(i32), intent(in), optional :: iunit !! file id to write the message to
    call Msg('Error:'//trim(aMsg),iunit)
    if (present(iunit))close(iunit)
    stop
    end subroutine
    !====================================================================!
    !====================================================================!
    subroutine fErr(istat,fname,flg,iunit)
      !! Checks for a file error
    !====================================================================!
    integer(i32), intent(in) :: istat !! Result of iostat=istat for open,read,write,close
    character(len=*), intent(in) :: fname !! Name of the file
    integer(i32), intent(in) :: flg !! 1=Open, 2=Read, 3=Write, 4=Close
    integer(i32), intent(in), optional :: iunit !! file id to write the error to
    character(len=:),allocatable :: Amsg
    Amsg=''
    if (istat /= 0) then
      select case(flg)
      case(1)
        Amsg='Opening file: '//trim(fname)
      case(2)
        Amsg='Reading from: '//trim(fname)
      case(3)
        Amsg='Writing to file: '//trim(fname)
      case(4)
        Amsg='Closing file: '//trim(fname)
      case default
        Amsg='Error:Invalid error flag [1-4]'
      end select
      call eMsg(aMsg,iunit)
    endif
    end subroutine
    !====================================================================!
  end module
