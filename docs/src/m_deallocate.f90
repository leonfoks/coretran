module m_deallocate
  !! Contains fundamental interface to deallocate allocatable arrays of different types and shapes.
  !!
  !! See [[deallocate]] for more information.
  use variableKind, only: r32, r64, i32, i64
  use m_errors, only: eMsg,mErr

  implicit none

  private

  public deallocate

  interface deallocate
    !! Deallocate an allocatable array.
    !!
    !! Contains fundamental routines to deallocate allocatable arrays of different types and shapes.
    !! Does not overload the intrinsic deallocate function.
    !!
    !! This way, calling deallocate makes the user aware that checks are being made and errors are handled with a message.
    !!
    !! Checks for an error during allocation, and will stop the code if there is one.
    !!
    !! Example usage
    !!```fortran
    !!use variableKind
    !!use m_allocate, only: allocate
    !!use m_deallocate, only: deallocate
    !! ! Could be other intrinsic types too, integer(i32), complex(r32), etc.
    !!real(r64),allocatable :: a1D(:), a2D(:,:), a3D(:,:,:)
    !!call allocate(a1D, 20)
    !!call allocate(a2D, [20,20])
    !!call allocate(a3D, [20,20,20])
    !!write(*,'(a)') 'Shape of a3D is [20,20,20]? '//all(shape(a3D) == [20,20,20])
    !!call deallocate(a1D)
    !!call deallocate(a2D)
    !!call deallocate(a3D)
    !!```
    !====================================================================!
    module subroutine deallocate_r1D(this)
      !! Interfaced with [[deallocate]]
    !====================================================================!
      real(r32), allocatable, intent(inout) :: this(:) !! 1D array
    end subroutine
    !====================================================================!
    !====================================================================!
    module subroutine deallocate_r2D(this)
      !! Interfaced with [[deallocate]]
    !====================================================================!
      real(r32), allocatable, intent(inout) :: this(:,:) !! 2D array
    end subroutine
    !====================================================================!
    !====================================================================!
    module subroutine deallocate_r3D(this)
      !! Interfaced with [[deallocate]]
    !====================================================================!
      real(r32), allocatable, intent(inout) :: this(:,:,:) !! 3D array
    end subroutine
    !====================================================================!

    !====================================================================!
    module subroutine deallocate_d1D(this)
      !! Interfaced with [[deallocate]]
    !====================================================================!
      real(r64), allocatable, intent(inout) :: this(:) !! 1D array
    end subroutine
    !====================================================================!
    !====================================================================!
    module subroutine deallocate_d2D(this)
      !! Interfaced with [[deallocate]]
    !====================================================================!
      real(r64), allocatable, intent(inout) :: this(:,:) !! 2D array
    end subroutine
    !====================================================================!
    !====================================================================!
    module subroutine deallocate_d3D(this)
      !! Interfaced with [[deallocate]]
    !====================================================================!
      real(r64), allocatable, intent(inout) :: this(:,:,:) !! 3D array
    end subroutine
    !====================================================================!

    !====================================================================!
    module subroutine deallocate_i1D(this)
      !! Interfaced with [[deallocate]]
    !====================================================================!
      integer(i32), allocatable, intent(inout) :: this(:) !! 1D array
    end subroutine
    !====================================================================!
    !====================================================================!
    module subroutine deallocate_i2D(this)
      !! Interfaced with [[deallocate]]
    !====================================================================!
      integer(i32), allocatable, intent(inout) :: this(:,:) !! 2D array
    end subroutine
    !====================================================================!
    !====================================================================!
    module subroutine deallocate_i3D(this)
      !! Interfaced with [[deallocate]]
    !====================================================================!
      integer(i32), allocatable, intent(inout) :: this(:,:,:) !! 3D array
    end subroutine
    !====================================================================!

    !====================================================================!
    module subroutine deallocate_id1D(this)
      !! Interfaced with [[deallocate]]
    !====================================================================!
      integer(i64), allocatable, intent(inout) :: this(:) !! 1D array
    end subroutine
    !====================================================================!
    !====================================================================!
    module subroutine deallocate_id2D(this)
      !! Interfaced with [[deallocate]]
    !====================================================================!
      integer(i64), allocatable, intent(inout) :: this(:,:) !! 2D array
    end subroutine
    !====================================================================!
    !====================================================================!
    module subroutine deallocate_id3D(this)
      !! Interfaced with [[deallocate]]
    !====================================================================!
      integer(i64), allocatable, intent(inout) :: this(:,:,:) !! 3D array
    end subroutine
    !====================================================================!

    !====================================================================!
    module subroutine deallocate_c1D(this)
      !! Interfaced with [[deallocate]]
    !====================================================================!
      complex(r32), allocatable, intent(inout) :: this(:) !! 1D array
    end subroutine
    !====================================================================!
    !====================================================================!
    module subroutine deallocate_c2D(this)
      !! Interfaced with [[deallocate]]
    !====================================================================!
      complex(r32), allocatable, intent(inout) :: this(:,:) !! 2D array
    end subroutine
    !====================================================================!
    !====================================================================!
    module subroutine deallocate_c3D(this)
      !! Interfaced with [[deallocate]]
    !====================================================================!
      complex(r32), allocatable, intent(inout) :: this(:,:,:) !! 3D array
    end subroutine
    !====================================================================!

    !====================================================================!
    module subroutine deallocate_z1D(this)
      !! Interfaced with [[deallocate]]
    !====================================================================!
      complex(r64), allocatable, intent(inout) :: this(:) !! 1D array
    end subroutine
    !====================================================================!
    !====================================================================!
    module subroutine deallocate_z2D(this)
      !! Interfaced with [[deallocate]]
    !====================================================================!
      complex(r64), allocatable, intent(inout) :: this(:,:) !! 2D array
    end subroutine
    !====================================================================!
    !====================================================================!
    module subroutine deallocate_z3D(this)
      !! Interfaced with [[deallocate]]
    !====================================================================!
      complex(r64), allocatable, intent(inout) :: this(:,:,:) !! 3D array
    end subroutine
    !====================================================================!

    !====================================================================!
    module subroutine deallocate_l1D(this)
      !! Interfaced with [[deallocate]]
    !====================================================================!
      logical, allocatable, intent(inout) :: this(:) !! 1D array
    end subroutine
    !====================================================================!
    !====================================================================!
    module subroutine deallocate_l2D(this)
      !! Interfaced with [[deallocate]]
    !====================================================================!
      logical, allocatable, intent(inout) :: this(:,:) !! 2D array
    end subroutine
    !====================================================================!
    !====================================================================!
    module subroutine deallocate_l3D(this)
      !! Interfaced with [[deallocate]]
    !====================================================================!
      logical, allocatable, intent(inout) :: this(:,:,:) !! 3D array
    end subroutine
    !====================================================================!
  end interface

  contains

  !====================================================================!
  module subroutine deallocate_r1D(this)
    !! Interfaced with [[deallocate]]
  !====================================================================!
    real(r32), allocatable, intent(inout) :: this(:) !! 1D array
    integer(i32) :: istat
    if(allocated(this)) then
      deallocate(this, stat=istat)
      call mErr(istat,'deallocate_r1D:this',2)
    endif
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine deallocate_r2D(this)
    !! Interfaced with [[deallocate]]
  !====================================================================!
    real(r32), allocatable, intent(inout) :: this(:,:) !! 2D array
    integer(i32) :: istat
    if(allocated(this)) then
      deallocate(this, stat=istat)
      call mErr(istat,'deallocate_r2D:this',2)
    endif
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine deallocate_r3D(this)
    !! Interfaced with [[deallocate]]
  !====================================================================!
    real(r32), allocatable, intent(inout) :: this(:,:,:) !! 3D array
    integer(i32) :: istat
    if(allocated(this)) then
      deallocate(this, stat=istat)
      call mErr(istat,'deallocate_r3D:this',2)
    endif
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine deallocate_d1D(this)
    !! Interfaced with [[deallocate]]
  !====================================================================!
    real(r64), allocatable, intent(inout) :: this(:) !! 1D array
    integer(i32) :: istat
    if(allocated(this)) then
      deallocate(this, stat=istat)
      call mErr(istat,'deallocate_d1D:this',2)
    endif
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine deallocate_d2D(this)
    !! Interfaced with [[deallocate]]
  !====================================================================!
    real(r64), allocatable, intent(inout) :: this(:,:) !! 2D array
    integer(i32) :: istat
    if(allocated(this)) then
      deallocate(this, stat=istat)
      call mErr(istat,'deallocate_d2D:this',2)
    endif
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine deallocate_d3D(this)
    !! Interfaced with [[deallocate]]
  !====================================================================!
    real(r64), allocatable, intent(inout) :: this(:,:,:) !! 3D array
    integer(i32) :: istat
    if(allocated(this)) then
      deallocate(this, stat=istat)
      call mErr(istat,'deallocate_d3D:this',2)
    endif
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine deallocate_i1D(this)
    !! Interfaced with [[deallocate]]
  !====================================================================!
    integer(i32), allocatable, intent(inout) :: this(:) !! 1D array
    integer(i32) :: istat
    if(allocated(this)) then
      deallocate(this, stat=istat)
      call mErr(istat,'deallocate_i1D:this',2)
    endif
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine deallocate_i2D(this)
    !! Interfaced with [[deallocate]]
  !====================================================================!
    integer(i32), allocatable, intent(inout) :: this(:,:) !! 2D array
    integer(i32) :: istat
    if(allocated(this)) then
      deallocate(this, stat=istat)
      call mErr(istat,'deallocate_i2D:this',2)
    endif
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine deallocate_i3D(this)
    !! Interfaced with [[deallocate]]
  !====================================================================!
    integer(i32), allocatable, intent(inout) :: this(:,:,:) !! 3D array
    integer(i32) :: istat
    if(allocated(this)) then
      deallocate(this, stat=istat)
      call mErr(istat,'deallocate_i3D:this',2)
    endif
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine deallocate_id1D(this)
    !! Interfaced with [[deallocate]]
  !====================================================================!
    integer(i64), allocatable, intent(inout) :: this(:) !! 1D array
    integer(i32) :: istat
    if(allocated(this)) then
      deallocate(this, stat=istat)
      call mErr(istat,'deallocate_id1D:this',2)
    endif
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine deallocate_id2D(this)
    !! Interfaced with [[deallocate]]
  !====================================================================!
    integer(i64), allocatable, intent(inout) :: this(:,:) !! 2D array
    integer(i32) :: istat
    if(allocated(this)) then
      deallocate(this, stat=istat)
      call mErr(istat,'deallocate_id2D:this',2)
    endif
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine deallocate_id3D(this)
    !! Interfaced with [[deallocate]]
  !====================================================================!
    integer(i64), allocatable, intent(inout) :: this(:,:,:) !! 3D array
    integer(i32) :: istat
    if(allocated(this)) then
      deallocate(this, stat=istat)
      call mErr(istat,'deallocate_id3D:this',2)
    endif
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine deallocate_c1D(this)
    !! Interfaced with [[deallocate]]
  !====================================================================!
    complex(r32), allocatable, intent(inout) :: this(:) !! 1D array
    integer(i32) :: istat
    if(allocated(this)) then
      deallocate(this, stat=istat)
      call mErr(istat,'deallocate_c1D:this',2)
    endif
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine deallocate_c2D(this)
    !! Interfaced with [[deallocate]]
  !====================================================================!
    complex(r32), allocatable, intent(inout) :: this(:,:) !! 2D array
    integer(i32) :: istat
    if(allocated(this)) then
      deallocate(this, stat=istat)
      call mErr(istat,'deallocate_c2D:this',2)
    endif
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine deallocate_c3D(this)
    !! Interfaced with [[deallocate]]
  !====================================================================!
    complex(r32), allocatable, intent(inout) :: this(:,:,:) !! 3D array
    integer(i32) :: istat
    if(allocated(this)) then
      deallocate(this, stat=istat)
      call mErr(istat,'deallocate_c3D:this',2)
    endif
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine deallocate_z1D(this)
    !! Interfaced with [[deallocate]]
  !====================================================================!
    complex(r64), allocatable, intent(inout) :: this(:) !! 1D array
    integer(i32) :: istat
    if(allocated(this)) then
      deallocate(this, stat=istat)
      call mErr(istat,'deallocate_z1D:this',2)
    endif
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine deallocate_z2D(this)
    !! Interfaced with [[deallocate]]
  !====================================================================!
    complex(r64), allocatable, intent(inout) :: this(:,:) !! 2D array
    integer(i32) :: istat
    if(allocated(this)) then
      deallocate(this, stat=istat)
      call mErr(istat,'deallocate_z2D:this',2)
    endif
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine deallocate_z3D(this)
    !! Interfaced with [[deallocate]]
  !====================================================================!
    complex(r64), allocatable, intent(inout) :: this(:,:,:) !! 3D array
    integer(i32) :: istat
    if(allocated(this)) then
      deallocate(this, stat=istat)
      call mErr(istat,'deallocate_z3D:this',2)
    endif
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine deallocate_l1D(this)
    !! Interfaced with [[deallocate]]
  !====================================================================!
    logical, allocatable, intent(inout) :: this(:) !! 1D array
    integer(i32) :: istat
    if(allocated(this)) then
      deallocate(this, stat=istat)
      call mErr(istat,'deallocate_l1D:this',2)
    endif
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine deallocate_l2D(this)
    !! Interfaced with [[deallocate]]
  !====================================================================!
    logical, allocatable, intent(inout) :: this(:,:) !! 2D array
    integer(i32) :: istat
    if(allocated(this)) then
      deallocate(this, stat=istat)
      call mErr(istat,'deallocate_l2D:this',2)
    endif
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine deallocate_l3D(this)
    !! Interfaced with [[deallocate]]
  !====================================================================!
    logical, allocatable, intent(inout) :: this(:,:,:) !! 3D array
    integer(i32) :: istat
    if(allocated(this)) then
      deallocate(this, stat=istat)
      call mErr(istat,'deallocate_l3D:this',2)
    endif
  end subroutine
  !====================================================================!
end module
