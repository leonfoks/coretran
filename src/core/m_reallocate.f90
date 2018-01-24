module m_reallocate
  !! Contains routines to re-allocate allocatable arrays
  !!
  !! Copies the original values after reallocation. If you don't need to copy the values, use 'call allocate()' instead
use variableKind
use m_allocate, only: allocate
use m_errors, only: eMsg, mErr

implicit none

private

public :: reallocate

interface reallocate
  !! Reallocate an allocatable array
  !!
  !! Example usage
  !!```fortran
  !!use variableKind
  !!use
  !!use m_reallocate
  !!real(r64),allocatable :: a1D(:), a2D(:,:), a3D(:,:,:)
  !!allocate(a1D(5))
  !!allocate(a2D(5,5))
  !!allocate(a3D(5,5,5))
  !!write(*,'(a)') 'Shape of a3D is [5,5,5]? '//all(shape(a3D) == [5,5,5])
  !!call reallocate(a1D, 20)
  !!call reallocate(a2D, [20,20])
  !!call reallocate(a3D, [20,20,20])
  !!write(*,'(a)') 'Shape of a3D is [20,20,20]? '//all(shape(a3D) == [20,20,20])
  !!```
  module procedure :: reallocate_r1D,reallocate_r2D,reallocate_r3D
  module procedure :: reallocate_d1D,reallocate_d2D,reallocate_d3D
  module procedure :: reallocate_i1D,reallocate_i2D,reallocate_i3D
  module procedure :: reallocate_id1D,reallocate_id2D,reallocate_id3D
  module procedure :: reallocate_c1D,reallocate_c2D,reallocate_c3D
  module procedure :: reallocate_z1D,reallocate_z2D,reallocate_z3D
  module procedure :: reallocate_l1D,reallocate_l2D,reallocate_l3D
end interface

contains
  !====================================================================!
  subroutine reallocate_r1D(this, n)
    !! Interfaced with reallocate()
  !====================================================================!
    real(r32), allocatable, intent(inout) :: this(:) !! 1D array
    integer(i32), intent(in) :: n !! New allocation size
    real(r32), allocatable :: tmp(:)
    integer(i32) :: istat
    integer(i32) :: n0, nTmp

    if (.not. allocated(this)) call eMsg('Cannot reallocate an unallocated array')
    n0 = size(this)
    if (n == n0) return ! Don't reallocate the same size

    allocate(tmp(n), stat=istat); call mErr(istat,'reallocate_r1D:tmp',1)
    tmp = 0.d0
    nTmp = min(n, n0)
    tmp(1:nTmp) = this(1:nTmp)
    deallocate(this)
    call move_alloc(from=tmp, to=this)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine reallocate_r2D(this, n)
    !! Interfaced with reallocate()
  !====================================================================!
    real(r32), allocatable, intent(inout) :: this(:,:) !! 2D array
    integer(i32), intent(in) :: n(2) !! New allocation shape
    real(r32), allocatable :: tmp(:,:)
    integer(i32) :: istat
    integer(i32) :: n0(2), nTmp(2)
    if (.not. allocated(this)) call eMsg('Cannot reallocate an unallocated array')
    n0 = shape(this)
    if (all(n == n0)) return ! Don't reallocate the same size
    allocate(tmp(n(1),n(2)), stat=istat); call mErr(istat,'reallocate_r2D:tmp',1)
    tmp = 0.d0
    nTmp=[min(n(1),n0(1)), min(n(2),n0(2))]
    tmp(1:nTmp(1), 1:nTmp(2)) = this(1:nTmp(1), 1:nTmp(2))
    call move_alloc(from=tmp, to=this)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine reallocate_r3D(this, n)
    !! Interfaced with reallocate()
  !====================================================================!
    real(r32), allocatable, intent(inout) :: this(:,:,:) !! 3D array
    integer(i32), intent(in) :: n(3) !! New allocation shape
    real(r32), allocatable :: tmp(:,:,:)
    integer(i32) :: istat
    integer(i32) :: n0(3), nTmp(3)
    if (.not. allocated(this)) call eMsg('Cannot reallocate an unallocated array')
    n0 = shape(this)
    if (all(n == n0)) return ! Don't reallocate the same size
    allocate(tmp(n(1),n(2), n(3)), stat=istat); call mErr(istat,'reallocate_r3D:tmp',1)
    tmp = 0.d0
    nTmp=[min(n(1),n0(1)), min(n(2),n0(2)), min(n(3),n0(3))]
    tmp(1:nTmp(1), 1:nTmp(2), 1:nTmp(3)) = this(1:nTmp(1), 1:nTmp(2), 1:nTmp(3))
    call move_alloc(from=tmp, to=this)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine reallocate_d1D(this, n)
    !! Interfaced with reallocate()
  !====================================================================!
    real(r64), allocatable, intent(inout) :: this(:) !! 1D array
    integer(i32), intent(in) :: n !! New allocation size
    real(r64), allocatable :: tmp(:)
    integer(i32) :: istat
    integer(i32) :: n0, nTmp

    if (.not. allocated(this)) call eMsg('Cannot reallocate an unallocated array')
    n0 = size(this)
    if (n == n0) return ! Don't reallocate the same size

    allocate(tmp(n), stat=istat); call mErr(istat,'reallocate_d1D:tmp',1)
    tmp = 0.d0
    nTmp = min(n, n0)
    tmp(1:nTmp) = this(1:nTmp)
    call move_alloc(from=tmp, to=this)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine reallocate_d2D(this, n)
    !! Interfaced with reallocate()
  !====================================================================!
    real(r64), allocatable, intent(inout) :: this(:,:) !! 2D array
    integer(i32), intent(in) :: n(2) !! New allocation shape
    real(r64), allocatable :: tmp(:,:)
    integer(i32) :: istat
    integer(i32) :: n0(2), nTmp(2)
    if (.not. allocated(this)) call eMsg('Cannot reallocate an unallocated array')
    n0 = shape(this)
    if (all(n == n0)) return ! Don't reallocate the same size
    allocate(tmp(n(1),n(2)), stat=istat); call mErr(istat,'reallocate_d2D:tmp',1)
    tmp = 0.d0
    nTmp=[min(n(1),n0(1)), min(n(2),n0(2))]
    tmp(1:nTmp(1), 1:nTmp(2)) = this(1:nTmp(1), 1:nTmp(2))
    call move_alloc(from=tmp, to=this)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine reallocate_d3D(this, n)
    !! Interfaced with reallocate()
  !====================================================================!
    real(r64), allocatable, intent(inout) :: this(:,:,:) !! 3D array
    integer(i32), intent(in) :: n(3) !! New allocation shape
    real(r64), allocatable :: tmp(:,:,:)
    integer(i32) :: istat
    integer(i32) :: n0(3), nTmp(3)
    if (.not. allocated(this)) call eMsg('Cannot reallocate an unallocated array')
    n0 = shape(this)
    if (all(n == n0)) return ! Don't reallocate the same size
    allocate(tmp(n(1),n(2), n(3)), stat=istat); call mErr(istat,'reallocate_d3D:tmp',1)
    tmp = 0.d0
    nTmp=[min(n(1),n0(1)), min(n(2),n0(2)), min(n(3),n0(3))]
    tmp(1:nTmp(1), 1:nTmp(2), 1:nTmp(3)) = this(1:nTmp(1), 1:nTmp(2), 1:nTmp(3))
    call move_alloc(from=tmp, to=this)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine reallocate_i1D(this, n)
    !! Interfaced with reallocate()
  !====================================================================!
    integer(i32), allocatable, intent(inout) :: this(:) !! 1D array
    integer(i32), intent(in) :: n !! New allocation size
    integer(i32), allocatable :: tmp(:)
    integer(i32) :: istat
    integer(i32) :: n0, nTmp

    if (.not. allocated(this)) call eMsg('Cannot reallocate an unallocated array')
    n0 = size(this)
    if (n == n0) return ! Don't reallocate the same size

    allocate(tmp(n), stat=istat); call mErr(istat,'reallocate_i1D:tmp',1)
    tmp = 0
    nTmp = min(n, n0)
    tmp(1:nTmp) = this(1:nTmp)
    call move_alloc(from=tmp, to=this)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine reallocate_i2D(this, n)
    !! Interfaced with reallocate()
  !====================================================================!
    integer(i32), allocatable, intent(inout) :: this(:,:) !! 2D array
    integer(i32), intent(in) :: n(2) !! New allocation shape
    integer(i32), allocatable :: tmp(:,:)
    integer(i32) :: istat
    integer(i32) :: n0(2), nTmp(2)
    if (.not. allocated(this)) call eMsg('Cannot reallocate an unallocated array')
    n0 = shape(this)
    if (all(n == n0)) return ! Don't reallocate the same size
    allocate(tmp(n(1),n(2)), stat=istat); call mErr(istat,'reallocate_i2D:tmp',1)
    tmp = 0
    nTmp=[min(n(1),n0(1)), min(n(2),n0(2))]
    tmp(1:nTmp(1), 1:nTmp(2)) = this(1:nTmp(1), 1:nTmp(2))
    call move_alloc(from=tmp, to=this)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine reallocate_i3D(this, n)
    !! Interfaced with reallocate()
  !====================================================================!
    integer(i32), allocatable, intent(inout) :: this(:,:,:) !! 3D array
    integer(i32), intent(in) :: n(3) !! New allocation shape
    integer(i32), allocatable :: tmp(:,:,:)
    integer(i32) :: istat
    integer(i32) :: n0(3), nTmp(3)
    if (.not. allocated(this)) call eMsg('Cannot reallocate an unallocated array')
    n0 = shape(this)
    if (all(n == n0)) return ! Don't reallocate the same size
    allocate(tmp(n(1),n(2), n(3)), stat=istat); call mErr(istat,'reallocate_i3D:tmp',1)
    tmp = 0
    nTmp=[min(n(1),n0(1)), min(n(2),n0(2)), min(n(3),n0(3))]
    tmp(1:nTmp(1), 1:nTmp(2), 1:nTmp(3)) = this(1:nTmp(1), 1:nTmp(2), 1:nTmp(3))
    call move_alloc(from=tmp, to=this)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine reallocate_id1D(this, n)
    !! Interfaced with reallocate()
  !====================================================================!
    integer(i64), allocatable, intent(inout) :: this(:) !! 1D array
    integer(i32), intent(in) :: n !! New allocation size
    integer(i64), allocatable :: tmp(:)
    integer(i32) :: istat
    integer(i32) :: n0, nTmp

    if (.not. allocated(this)) call eMsg('Cannot reallocate an unallocated array')
    n0 = size(this)
    if (n == n0) return ! Don't reallocate the same size

    allocate(tmp(n), stat=istat); call mErr(istat,'reallocate_id1D:tmp',1)
    tmp = 0
    nTmp = min(n, n0)
    tmp(1:nTmp) = this(1:nTmp)
    call move_alloc(from=tmp, to=this)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine reallocate_id2D(this, n)
    !! Interfaced with reallocate()
  !====================================================================!
    integer(i64), allocatable, intent(inout) :: this(:,:) !! 2D array
    integer(i32), intent(in) :: n(2) !! New allocation shape
    integer(i64), allocatable :: tmp(:,:)
    integer(i32) :: istat
    integer(i32) :: n0(2), nTmp(2)
    if (.not. allocated(this)) call eMsg('Cannot reallocate an unallocated array')
    n0 = shape(this)
    if (all(n == n0)) return ! Don't reallocate the same size
    allocate(tmp(n(1),n(2)), stat=istat); call mErr(istat,'reallocate_id2D:tmp',1)
    tmp = 0
    nTmp=[min(n(1),n0(1)), min(n(2),n0(2))]
    tmp(1:nTmp(1), 1:nTmp(2)) = this(1:nTmp(1), 1:nTmp(2))
    call move_alloc(from=tmp, to=this)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine reallocate_id3D(this, n)
    !! Interfaced with reallocate()
  !====================================================================!
    integer(i64), allocatable, intent(inout) :: this(:,:,:) !! 3D array
    integer(i32), intent(in) :: n(3) !! New allocation shape
    integer(i64), allocatable :: tmp(:,:,:)
    integer(i32) :: istat
    integer(i32) :: n0(3), nTmp(3)
    if (.not. allocated(this)) call eMsg('Cannot reallocate an unallocated array')
    n0 = shape(this)
    if (all(n == n0)) return ! Don't reallocate the same size
    allocate(tmp(n(1),n(2), n(3)), stat=istat); call mErr(istat,'reallocate_id3D:tmp',1)
    tmp = 0
    nTmp=[min(n(1),n0(1)), min(n(2),n0(2)), min(n(3),n0(3))]
    tmp(1:nTmp(1), 1:nTmp(2), 1:nTmp(3)) = this(1:nTmp(1), 1:nTmp(2), 1:nTmp(3))
    call move_alloc(from=tmp, to=this)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine reallocate_c1D(this, n)
    !! Interfaced with reallocate()
  !====================================================================!
    complex(r32), allocatable, intent(inout) :: this(:) !! 1D array
    integer(i32), intent(in) :: n !! New allocation size
    complex(r32), allocatable :: tmp(:)
    integer(i32) :: istat
    integer(i32) :: n0, nTmp

    if (.not. allocated(this)) call eMsg('Cannot reallocate an unallocated array')
    n0 = size(this)
    if (n == n0) return ! Don't reallocate the same size

    allocate(tmp(n), stat=istat); call mErr(istat,'reallocate_c1D:tmp',1)
    tmp = (0.d0,0.d0)
    nTmp = min(n, n0)
    tmp(1:nTmp) = this(1:nTmp)
    deallocate(this)
    call move_alloc(from=tmp, to=this)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine reallocate_c2D(this, n)
    !! Interfaced with reallocate()
  !====================================================================!
    complex(r32), allocatable, intent(inout) :: this(:,:) !! 2D array
    integer(i32), intent(in) :: n(2) !! New allocation shape
    complex(r32), allocatable :: tmp(:,:)
    integer(i32) :: istat
    integer(i32) :: n0(2), nTmp(2)
    if (.not. allocated(this)) call eMsg('Cannot reallocate an unallocated array')
    n0 = shape(this)
    if (all(n == n0)) return ! Don't reallocate the same size
    allocate(tmp(n(1),n(2)), stat=istat); call mErr(istat,'reallocate_c2D:tmp',1)
    tmp = (0.d0,0.d0)
    nTmp=[min(n(1),n0(1)), min(n(2),n0(2))]
    tmp(1:nTmp(1), 1:nTmp(2)) = this(1:nTmp(1), 1:nTmp(2))
    call move_alloc(from=tmp, to=this)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine reallocate_c3D(this, n)
    !! Interfaced with reallocate()
  !====================================================================!
    complex(r32), allocatable, intent(inout) :: this(:,:,:) !! 3D array
    integer(i32), intent(in) :: n(3) !! New allocation shape
    complex(r32), allocatable :: tmp(:,:,:)
    integer(i32) :: istat
    integer(i32) :: n0(3), nTmp(3)
    if (.not. allocated(this)) call eMsg('Cannot reallocate an unallocated array')
    n0 = shape(this)
    if (all(n == n0)) return ! Don't reallocate the same size
    allocate(tmp(n(1),n(2), n(3)), stat=istat); call mErr(istat,'reallocate_c3D:tmp',1)
    tmp = (0.d0,0.d0)
    nTmp=[min(n(1),n0(1)), min(n(2),n0(2)), min(n(3),n0(3))]
    tmp(1:nTmp(1), 1:nTmp(2), 1:nTmp(3)) = this(1:nTmp(1), 1:nTmp(2), 1:nTmp(3))
    call move_alloc(from=tmp, to=this)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine reallocate_z1D(this, n)
    !! Interfaced with reallocate()
  !====================================================================!
    complex(r64), allocatable, intent(inout) :: this(:) !! 1D array
    integer(i32), intent(in) :: n !! New allocation size
    complex(r64), allocatable :: tmp(:)
    integer(i32) :: istat
    integer(i32) :: n0, nTmp

    if (.not. allocated(this)) call eMsg('Cannot reallocate an unallocated array')
    n0 = size(this)
    if (n == n0) return ! Don't reallocate the same size

    allocate(tmp(n), stat=istat); call mErr(istat,'reallocate_z1D:tmp',1)
    tmp = (0.d0,0.d0)
    nTmp = min(n, n0)
    tmp(1:nTmp) = this(1:nTmp)
    deallocate(this)
    call move_alloc(from=tmp, to=this)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine reallocate_z2D(this, n)
    !! Interfaced with reallocate()
  !====================================================================!
    complex(r64), allocatable, intent(inout) :: this(:,:) !! 2D array
    integer(i32), intent(in) :: n(2) !! New allocation shape
    complex(r64), allocatable :: tmp(:,:)
    integer(i32) :: istat
    integer(i32) :: n0(2), nTmp(2)
    if (.not. allocated(this)) call eMsg('Cannot reallocate an unallocated array')
    n0 = shape(this)
    if (all(n == n0)) return ! Don't reallocate the same size
    allocate(tmp(n(1),n(2)), stat=istat); call mErr(istat,'reallocate_z2D:tmp',1)
    tmp = (0.d0,0.d0)
    nTmp=[min(n(1),n0(1)), min(n(2),n0(2))]
    tmp(1:nTmp(1), 1:nTmp(2)) = this(1:nTmp(1), 1:nTmp(2))
    call move_alloc(from=tmp, to=this)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine reallocate_z3D(this, n)
    !! Interfaced with reallocate()
  !====================================================================!
    complex(r64), allocatable, intent(inout) :: this(:,:,:) !! 3D array
    integer(i32), intent(in) :: n(3) !! New allocation shape
    complex(r64), allocatable :: tmp(:,:,:)
    integer(i32) :: istat
    integer(i32) :: n0(3), nTmp(3)
    if (.not. allocated(this)) call eMsg('Cannot reallocate an unallocated array')
    n0 = shape(this)
    if (all(n == n0)) return ! Don't reallocate the same size
    allocate(tmp(n(1),n(2), n(3)), stat=istat); call mErr(istat,'reallocate_z3D:tmp',1)
    tmp = (0.d0,0.d0)
    nTmp=[min(n(1),n0(1)), min(n(2),n0(2)), min(n(3),n0(3))]
    tmp(1:nTmp(1), 1:nTmp(2), 1:nTmp(3)) = this(1:nTmp(1), 1:nTmp(2), 1:nTmp(3))
    call move_alloc(from=tmp, to=this)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine reallocate_l1D(this, n)
    !! Interfaced with reallocate()
  !====================================================================!
    logical, allocatable, intent(inout) :: this(:) !! 1D array
    integer(i32), intent(in) :: n !! New allocation size
    logical, allocatable :: tmp(:)
    integer(i32) :: istat
    integer(i32) :: n0, nTmp

    if (.not. allocated(this)) call eMsg('Cannot reallocate an unallocated array')
    n0 = size(this)
    if (n == n0) return ! Don't reallocate the same size

    allocate(tmp(n), stat=istat); call mErr(istat,'reallocate_l1D:tmp',1)
    tmp = .false.
    nTmp = min(n, n0)
    tmp(1:nTmp) = this(1:nTmp)
    deallocate(this)
    call move_alloc(from=tmp, to=this)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine reallocate_l2D(this, n)
    !! Interfaced with reallocate()
  !====================================================================!
    logical, allocatable, intent(inout) :: this(:,:) !! 2D array
    integer(i32), intent(in) :: n(2) !! New allocation shape
    logical, allocatable :: tmp(:,:)
    integer(i32) :: istat
    integer(i32) :: n0(2), nTmp(2)
    if (.not. allocated(this)) call eMsg('Cannot reallocate an unallocated array')
    n0 = shape(this)
    if (all(n == n0)) return ! Don't reallocate the same size
    allocate(tmp(n(1),n(2)), stat=istat); call mErr(istat,'reallocate_l2D:tmp',1)
    tmp = .false.
    nTmp=[min(n(1),n0(1)), min(n(2),n0(2))]
    tmp(1:nTmp(1), 1:nTmp(2)) = this(1:nTmp(1), 1:nTmp(2))
    call move_alloc(from=tmp, to=this)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine reallocate_l3D(this, n)
    !! Interfaced with reallocate()
  !====================================================================!
    logical, allocatable, intent(inout) :: this(:,:,:) !! 3D array
    integer(i32), intent(in) :: n(3) !! New allocation shape
    logical, allocatable :: tmp(:,:,:)
    integer(i32) :: istat
    integer(i32) :: n0(3), nTmp(3)
    if (.not. allocated(this)) call eMsg('Cannot reallocate an unallocated array')
    n0 = shape(this)
    if (all(n == n0)) return ! Don't reallocate the same size
    allocate(tmp(n(1),n(2), n(3)), stat=istat); call mErr(istat,'reallocate_l3D:tmp',1)
    tmp = .false.
    nTmp=[min(n(1),n0(1)), min(n(2),n0(2)), min(n(3),n0(3))]
    tmp(1:nTmp(1), 1:nTmp(2), 1:nTmp(3)) = this(1:nTmp(1), 1:nTmp(2), 1:nTmp(3))
    call move_alloc(from=tmp, to=this)
  end subroutine
  !====================================================================!
end module
