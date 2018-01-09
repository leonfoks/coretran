module m_reallocate
  !! Contains routines to re-allocate allocatable arrays
  !!
  !! Copies the original values after reallocation. If you don't need to copy the values, use 'call allocate()' instead
use variableKind
use m_allocate, only: allocate
use m_errors, only: eMsg, mErr, msg
use m_unitTester, only: tester

implicit none

private

public :: reallocate_test

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
  !====================================================================!
  subroutine reallocate_test(test)
    !! graph: false
  !====================================================================!
  class(tester) :: test

  real(r32), allocatable :: ar1D(:), ar2D(:,:), ar3D(:,:,:)
  real(r64), allocatable :: a1D(:), a2D(:,:), a3D(:,:,:)
  integer(i32), allocatable :: ia1D(:), ia2D(:,:), ia3D(:,:,:)
  integer(i64), allocatable :: iad1D(:), iad2D(:,:), iad3D(:,:,:)
  complex(r32), allocatable :: z1D(:), z2D(:,:), z3D(:,:,:)
  complex(r64), allocatable :: zz1D(:), zz2D(:,:), zz3D(:,:,:)
  logical, allocatable :: la1D(:), la2D(:,:), la3D(:,:,:)

  call Msg('==========================')
  call Msg('Testing : Reallocate')
  call Msg('==========================')

  call allocate(ar1D, 10)
  ar1D = 1.0
  call reallocate(ar1D,50)
  call test%test(size(ar1D) == 50 .and. all(ar1D(1:10) == 1.0) .and. all(ar1D(11:50) == 0.0),'reallocate_r1D')

  call allocate(ar2D,[10,10])
  ar2D = 2.0
  call reallocate(ar2D,[5,7])
  call test%test(all(shape(ar2D) == [5,7]) .and. all(ar2D == 2.0),'reallocate_r2D')

  call allocate(ar3D,[10,10,10])
  ar3D = 3.0
  call reallocate(ar3D,[2,20,3])
  call test%test(all(shape(ar3D) == [2,20,3]) .and. all(ar3D(:,1:10,:) == 3.0) .and. all(ar3D(:,11:20,:) == 0.0),'reallocate_r3D')

  call allocate(a1D,100)
  a1D = 1.d0
  call reallocate(a1D,50)
  call test%test(size(a1D) == 50 .and. all(a1D == 1.d0),'reallocate_d1D')

  call allocate(a2D,[10,10])
  a2D = 2.d0
  call reallocate(a2D,[20,20])
  call test%test(all(shape(a2D) == [20,20]) .and. all(a2D(1:10,1:10) == 2.d0) .and. all(a2D(11:20,11:20) == 0.d0),'reallocate_d2D')

  call allocate(a3D,[10,10,10])
  a3D = 3.d0
  call reallocate(a3D,[20,20,20])
  call test%test(all(shape(a3D) == [20,20,20]) .and. all(a3D(1:10,1:10,1:10) == 3.d0) .and. all(a3D(11:20,11:20,11:20) == 0.d0),'reallocate_d3D')

  call allocate(ia1D,100)
  ia1D = 1
  call reallocate(ia1D,50)
  call test%test(size(ia1D) == 50 .and. all(ia1d == 1),'reallocate_i1D')

  call allocate(ia2D,[10,10])
  ia2D = 2
  call reallocate(ia2D,[20,20])
  call test%test(all(shape(ia2D) == [20,20]) .and. all(ia2d(1:10,1:10) == 2) .and. all(ia2D(11:20,11:20) == 0),'reallocate_i2D')

  call allocate(ia3D,[10,10,10])
  ia3D = 3
  call reallocate(ia3D,[20,20,20])
  call test%test(all(shape(ia3D) == [20,20,20]) .and. all(ia3d(1:10,1:10,1:10) == 3) .and. all(ia3D(11:20,11:20,11:20) == 0),'reallocate_i3D')

  call allocate(iad1D,100)
  iad1D = 1
  call reallocate(iad1D,50)
  call test%test(size(iad1D) == 50 .and. all(iad1d == 1),'reallocate_id1D')

  call allocate(iad2D,[10,10])
  iad2d = 2
  call reallocate(iad2D,[20,20])
  call test%test(all(shape(iad2D) == [20,20]) .and. all(iad2d(1:10,1:10) == 2) .and. all(iad2D(11:20,11:20) == 0),'reallocate_id2D')

  call allocate(iad3D,[10,10,10])
  iad3d = 3
  call reallocate(iad3D,[20,20,20])
  call test%test(all(shape(iad3D) == [20,20,20]) .and. all(iad3d(1:10,1:10,1:10) == 3)  .and. all(iad3D(11:20,11:20,11:20) == 0),'reallocate_id3D')

  call allocate(z1D,100)
  z1D = (1.d0, 0.d0)
  call reallocate(z1D,50)
  call test%test(size(z1D) == 50 .and. all(z1D == (1.d0,0.d0)),'reallocate_c1D')

  call allocate(z2D,[10,10])
  z2D = (2.d0, 0.d0)
  call reallocate(z2D,[20,20])
  call test%test(all(shape(z2D) == [20,20]) .and. all(z2D(1:10,1:10) == (2.d0, 0.d0)) .and. all(z2D(11:20,11:20) == (0.d0, 0.d0)),'reallocate_c2D')

  call allocate(z3D,[10,10,10])
  z3D = (3.d0, 0.d0)
  call reallocate(z3D,[20,20,20])
  call test%test(all(shape(z3D) == [20,20,20]) .and. all(z3D(1:10,1:10,1:10) == (3.d0, 0.d0)) .and. all(z3D(11:20,11:20,11:20) == (0.d0, 0.d0)),'reallocate_c3D')

  call allocate(zz1D,100)
  zz1D = (1.d0, 0.d0)
  call reallocate(zz1D,50)
  call test%test(size(zz1D) == 50 .and. all(zz1D == (1.d0,0.d0)),'reallocate_z1D')

  call allocate(zz2D,[10,10])
  zz2D = (2.d0, 0.d0)
  call reallocate(zz2D,[20,20])
  call test%test(all(shape(zz2D) == [20,20]) .and. all(zz2D(1:10,1:10) == (2.d0, 0.d0)) .and. all(zz2D(11:20,11:20) == (0.d0, 0.d0)),'reallocate_z2D')

  call allocate(zz3D,[10,10,10])
  zz3D = (3.d0, 0.d0)
  call reallocate(zz3D,[20,20,20])
  call test%test(all(shape(zz3D) == [20,20,20]) .and. all(zz3D(1:10,1:10,1:10) == (3.d0, 0.d0)) .and. all(zz3D(11:20,11:20,11:20) == (0.d0, 0.d0)),'reallocate_z3D')

  call allocate(la1D,100)
  la1D = .true.
  call reallocate(la1D,50)
  call test%test(size(la1D) == 50 .and. all(la1D .eqv. .true.),'reallocate_l1D')

  call allocate(la2D,[10,10])
  la2D = .true.
  call reallocate(la2D,[20,20])
  call test%test(all(shape(la2D) == [20,20]) .and. all(la2D(1:10,1:10) .eqv. .true.) .and. all(la2D(11:20,11:20) .eqv. .false.),'reallocate_l2D')

  call allocate(la3D,[10,10,10])
  la3D = .true.
  call reallocate(la3D,[20,20,20])
  call test%test(all(shape(la3D) == [20,20,20]) .and. all(la3D(1:10,1:10,1:10) .eqv. .true.) .and. all(la3D(11:20,11:20,11:20) .eqv. .false.),'reallocate_l3D')

  deallocate(ar1D,ar2D,ar3D,a1D,a2D,a3D,ia1D,ia2D,ia3D)
  deallocate(iad1D,iad2D,iad3D,z1D,z2D,z3D,zz1D,zz2D,zz3D)
  deallocate(la1D,la2D,la3D)

  end subroutine
  !====================================================================!
end module
