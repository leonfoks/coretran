module m_deallocate
  !! Contains fundamental routines to deallocate allocatable arrays of different types and shapes
  !! Does not overload the intrinsic deallocate function.
  !!
  !! This way, calling deallocate makes the user aware that checks are being made and errors are handled with a message.

  use variableKind
  use m_errors, only: eMsg,mErr

  implicit none

  private

  public deallocate

  interface deallocate
    !! Deallocate an allocatable array.
    !!
    !! Example usage
    !!```fortran
    !!use variableKind
    !!use m_allocate, only: allocate
    !!use m_deallocate, only: deallocate
    !!real(r64),allocatable :: a1D(:), a2D(:,:), a3D(:,:,:)
    !!call allocate(a1D, 20)
    !!call allocate(a2D, [20,20])
    !!call allocate(a3D, [20,20,20])
    !!write(*,'(a)') 'Shape of a3D is [20,20,20]? '//all(shape(a3D) == [20,20,20])
    !!call deallocate(a1D)
    !!call deallocate(a2D)
    !!call deallocate(a3D)
    !!```
    module procedure :: deallocate_r1D, deallocate_r2D, deallocate_r3D
    module procedure :: deallocate_d1D, deallocate_d2D, deallocate_d3D
    module procedure :: deallocate_i1D, deallocate_i2D, deallocate_i3D
    module procedure :: deallocate_id1D, deallocate_id2D, deallocate_id3D
    module procedure :: deallocate_c1D, deallocate_c2D, deallocate_c3D
    module procedure :: deallocate_z1D, deallocate_z2D, deallocate_z3D
    module procedure :: deallocate_l1D, deallocate_l2D, deallocate_l3D
  end interface

  contains

  !====================================================================!
  subroutine deallocate_r1D(this)
    !! Interfaced with deallocate()
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
  subroutine deallocate_r2D(this)
    !! Interfaced with deallocate()
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
  subroutine deallocate_r3D(this)
    !! Interfaced with deallocate()
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
  subroutine deallocate_d1D(this)
    !! Interfaced with deallocate()
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
  subroutine deallocate_d2D(this)
    !! Interfaced with deallocate()
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
  subroutine deallocate_d3D(this)
    !! Interfaced with deallocate()
  !====================================================================!
    real(r64), allocatable, intent(inout) :: this(:,:,:) !! 3D array
    integer(i32) :: istat
    if(allocated(this)) then
      deallocate(this, stat=istat)
      call mErr(istat,'deallocate_d3D:this',2)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine deallocate_i1D(this)
    !! Interfaced with deallocate()
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
  subroutine deallocate_i2D(this)
    !! Interfaced with deallocate()
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
  subroutine deallocate_i3D(this, n)
    !! Interfaced with deallocate()
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
  subroutine deallocate_id1D(this)
    !! Interfaced with deallocate()
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
  subroutine deallocate_id2D(this)
    !! Interfaced with deallocate()
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
  subroutine deallocate_id3D(this)
    !! Interfaced with deallocate()
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
  subroutine deallocate_c1D(this)
    !! Interfaced with deallocate()
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
  subroutine deallocate_c2D(this)
    !! Interfaced with deallocate()
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
  subroutine deallocate_c3D(this)
    !! Interfaced with deallocate()
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
  subroutine deallocate_z1D(this)
    !! Interfaced with deallocate()
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
  subroutine deallocate_z2D(this)
    !! Interfaced with deallocate()
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
  subroutine deallocate_z3D(this)
    !! Interfaced with deallocate()
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
  subroutine deallocate_l1D(this)
    !! Interfaced with deallocate()
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
  subroutine deallocate_l2D(this)
    !! Interfaced with deallocate()
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
  subroutine deallocate_l3D(this)
    !! Interfaced with deallocate()
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
