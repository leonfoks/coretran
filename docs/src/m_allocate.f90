module m_allocate
  !! Contains fundamental interface to allocate allocatable arrays of different types and shapes
  !!
  !! See [[allocate]] for more information.

  use variableKind
  use m_unitTester, only: tester
  use m_errors, only: eMsg, mErr, msg

  implicit none

  private

  public :: allocate
  public :: allocate_test

  interface allocate
    !! Allocate an allocatable array. If the array is already allocated, memory is reallocated to the given size, unless no size change will occur.
    !! The allocated memory is NOT initialized to any value, so be sure to do so afterwards if you call allocate.
    !!
    !! Does not overload the intrinsic allocate function.
    !!
    !! This way, calling allocate makes the user aware that checks are being made, and memory will be re-allocated.
    !!
    !! Checks for an error during allocation, and will stop the code if there is one.
    !!
    !! Example usage
    !!```fortran
    !!use variableKind
    !!use m_allocate, only: allocate
    !!real(r64),allocatable :: a1D(:), a2D(:,:), a3D(:,:,:)
    !!call allocate(a1D, 20)
    !!call allocate(a2D, [20,20])
    !!call allocate(a3D, [20,20,20])
    !!write(*,'(a)') 'Shape of a3D is [20,20,20]? '//all(shape(a3D) == [20,20,20])
    !!```
    !====================================================================!
    module subroutine allocate_r1D(this, n)
    !====================================================================!
      !!Interfaced with [[allocate]]
      real(r32), allocatable, intent(inout) :: this(:) !! 1D array
      integer(i32), intent(in) :: n !! Allocation size
    end subroutine
    !====================================================================!
    !====================================================================!
    module subroutine allocate_r2D(this, n)
    !====================================================================!
      !!Interfaced with  [[allocate]]
      real(r32), allocatable, intent(inout) :: this(:,:) !! 1D array
      integer(i32), intent(in) :: n(2) !! Allocation size
    end subroutine
    !====================================================================!
    !====================================================================!
    module subroutine allocate_r3D(this, n)
    !====================================================================!
      !!Interfaced with  [[allocate]]
      real(r32), allocatable, intent(inout) :: this(:,:,:) !! 1D array
      integer(i32), intent(in) :: n(3) !! Allocation size
    end subroutine
    !====================================================================!

    !====================================================================!
    module subroutine allocate_d1D(this, n)
    !====================================================================!
      !!Interfaced with  [[allocate]]
      real(r64), allocatable, intent(inout) :: this(:) !! 1D array
      integer(i32), intent(in) :: n !! Allocation size
    end subroutine
    !====================================================================!
    !====================================================================!
    module subroutine allocate_d2D(this, n)
    !====================================================================!
      !!Interfaced with  [[allocate]]
      real(r64), allocatable, intent(inout) :: this(:,:) !! 1D array
      integer(i32), intent(in) :: n(2) !! Allocation size
    end subroutine
    !====================================================================!
    !====================================================================!
    module subroutine allocate_d3D(this, n)
    !====================================================================!
      !!Interfaced with  [[allocate]]
      real(r64), allocatable, intent(inout) :: this(:,:,:) !! 1D array
      integer(i32), intent(in) :: n(3) !! Allocation size
    end subroutine
    !====================================================================!

    !====================================================================!
    module subroutine allocate_i1D(this, n)
    !====================================================================!
      !!Interfaced with  [[allocate]]
      integer(i32), allocatable, intent(inout) :: this(:) !! 1D array
      integer(i32), intent(in) :: n !! Allocation size
    end subroutine
    !====================================================================!
    !====================================================================!
    module subroutine allocate_i2D(this, n)
    !====================================================================!
      !!Interfaced with  [[allocate]]
      integer(i32), allocatable, intent(inout) :: this(:,:) !! 1D array
      integer(i32), intent(in) :: n(2) !! Allocation size
    end subroutine
    !====================================================================!
    !====================================================================!
    module subroutine allocate_i3D(this, n)
    !====================================================================!
      !!Interfaced with  [[allocate]]
      integer(i32), allocatable, intent(inout) :: this(:,:,:) !! 1D array
      integer(i32), intent(in) :: n(3) !! Allocation size
    end subroutine
    !====================================================================!

    !====================================================================!
    module subroutine allocate_id1D(this, n)
    !====================================================================!
      !!Interfaced with  [[allocate]]
      integer(i64), allocatable, intent(inout) :: this(:) !! 1D array
      integer(i32), intent(in) :: n !! Allocation size
    end subroutine
    !====================================================================!
    !====================================================================!
    module subroutine allocate_id2D(this, n)
    !====================================================================!
      !!Interfaced with  [[allocate]]
      integer(i64), allocatable, intent(inout) :: this(:,:) !! 1D array
      integer(i32), intent(in) :: n(2) !! Allocation size
    end subroutine
    !====================================================================!
    !====================================================================!
    module subroutine allocate_id3D(this, n)
    !====================================================================!
      !!Interfaced with  [[allocate]]
      integer(i64), allocatable, intent(inout) :: this(:,:,:) !! 1D array
      integer(i32), intent(in) :: n(3) !! Allocation size
    end subroutine
    !====================================================================!

    !====================================================================!
    module subroutine allocate_c1D(this, n)
    !====================================================================!
      !!Interfaced with  [[allocate]]
      complex(r32), allocatable, intent(inout) :: this(:) !! 1D array
      integer(i32), intent(in) :: n !! Allocation size
    end subroutine
    !====================================================================!
    !====================================================================!
    module subroutine allocate_c2D(this, n)
    !====================================================================!
      !!Interfaced with  [[allocate]]
      complex(r32), allocatable, intent(inout) :: this(:,:) !! 1D array
      integer(i32), intent(in) :: n(2) !! Allocation size
    end subroutine
    !====================================================================!
    !====================================================================!
    module subroutine allocate_c3D(this, n)
    !====================================================================!
      !!Interfaced with  [[allocate]]
      complex(r32), allocatable, intent(inout) :: this(:,:,:) !! 1D array
      integer(i32), intent(in) :: n(3) !! Allocation size
    end subroutine
    !====================================================================!

    !====================================================================!
    module subroutine allocate_z1D(this, n)
    !====================================================================!
      !!Interfaced with  [[allocate]]
      complex(r64), allocatable, intent(inout) :: this(:) !! 1D array
      integer(i32), intent(in) :: n !! Allocation size
    end subroutine
    !====================================================================!
    !====================================================================!
    module subroutine allocate_z2D(this, n)
    !====================================================================!
      !!Interfaced with  [[allocate]]
      complex(r64), allocatable, intent(inout) :: this(:,:) !! 1D array
      integer(i32), intent(in) :: n(2) !! Allocation size
    end subroutine
    !====================================================================!
    !====================================================================!
    module subroutine allocate_z3D(this, n)
    !====================================================================!
      !!Interfaced with  [[allocate]]
      complex(r64), allocatable, intent(inout) :: this(:,:,:) !! 1D array
      integer(i32), intent(in) :: n(3) !! Allocation size
    end subroutine
    !====================================================================!

    !====================================================================!
    module subroutine allocate_l1D(this, n)
    !====================================================================!
      !!Interfaced with  [[allocate]]
      logical, allocatable, intent(inout) :: this(:) !! 1D array
      integer(i32), intent(in) :: n !! Allocation size
    end subroutine
    !====================================================================!
    !====================================================================!
    module subroutine allocate_l2D(this, n)
    !====================================================================!
      !!Interfaced with  [[allocate]]
      logical, allocatable, intent(inout) :: this(:,:) !! 1D array
      integer(i32), intent(in) :: n(2) !! Allocation size
    end subroutine
    !====================================================================!
    !====================================================================!
    module subroutine allocate_l3D(this, n)
    !====================================================================!
      !!Interfaced with  [[allocate]]
      logical, allocatable, intent(inout) :: this(:,:,:) !! 1D array
      integer(i32), intent(in) :: n(3) !! Allocation size
    end subroutine
    !====================================================================!
  end interface

  contains

  !====================================================================!
  module subroutine allocate_r1D(this, n)
    !!Interfaced with  [[allocate]]
  !====================================================================!
  real(r32), allocatable, intent(inout) :: this(:) !! 1D array
  integer(i32), intent(in) :: n !! Allocation size
  integer(i32) :: istat

  if(allocated(this)) then
    if (n == size(this)) then
      return ! Don't need to allocate the same size
    else
      deallocate(this) ! No need to duplicate memory
    end if
  end if
  allocate(this(n), stat=istat); call mErr(istat,'allocate_r1D:this',1)
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine allocate_r2D(this, n)
    !! Interfaced with [[allocate]]
  !====================================================================!
    real(r32), allocatable, intent(inout) :: this(:,:) !! 2D array
    integer(i32), intent(in) :: n(2) !! New allocation shape
    integer(i32) :: istat

    if(allocated(this)) then
      if (all(n == shape(this))) then
        return ! Don't need to allocate the same size
      else
        deallocate(this)
      end if
    end if
    allocate(this(n(1),n(2)), stat=istat); call mErr(istat,'allocate_r2D:this',1)
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine allocate_r3D(this, n)
    !! Interfaced with [[allocate]]
  !====================================================================!
    real(r32), allocatable, intent(inout) :: this(:,:,:) !! 3D array
    integer(i32), intent(in) :: n(3) !! New allocation shape
    integer(i32) :: istat

    if(allocated(this)) then
      if (all(n == shape(this))) then
        return ! Don't need to allocate the same size
      else
        deallocate(this)
      end if
    end if
    allocate(this(n(1),n(2),n(3)), stat=istat); call mErr(istat,'allocate_r3D:this',1)
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine allocate_d1D(this, n)
    !! Interfaced with [[allocate]]
  !====================================================================!
    real(r64), allocatable, intent(inout) :: this(:) !! 1D array
    integer(i32), intent(in) :: n !! Allocation size
    integer(i32) :: istat

    if(allocated(this)) then
      if (n == size(this)) then
        return ! Don't need to allocate the same size
      else
        deallocate(this) ! No need to duplicate memory
      end if
    end if
    allocate(this(n), stat=istat); call mErr(istat,'allocate_d1D:this',1)
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine allocate_d2D(this, n)
    !! Interfaced with [[allocate]]
  !====================================================================!
    real(r64), allocatable, intent(inout) :: this(:,:) !! 2D array
    integer(i32), intent(in) :: n(2) !! New allocation shape
    integer(i32) :: istat

    if(allocated(this)) then
      if (all(n == shape(this))) then
        return ! Don't need to allocate the same size
      else
        deallocate(this)
      end if
    end if
    allocate(this(n(1),n(2)), stat=istat); call mErr(istat,'allocate_d2D:this',1)
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine allocate_d3D(this, n)
    !! Interfaced with [[allocate]]
  !====================================================================!
    real(r64), allocatable, intent(inout) :: this(:,:,:) !! 3D array
    integer(i32), intent(in) :: n(3) !! New allocation shape
    integer(i32) :: istat

    if(allocated(this)) then
      if (all(n == shape(this))) then
        return ! Don't need to allocate the same size
      else
        deallocate(this)
      end if
    end if
    allocate(this(n(1),n(2),n(3)), stat=istat); call mErr(istat,'allocate_d3D:this',1)
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine allocate_i1D(this, n)
    !! Interfaced with [[allocate]]
  !====================================================================!
    integer(i32), allocatable, intent(inout) :: this(:) !! 1D array
    integer(i32), intent(in) :: n !! Allocation size
    integer(i32) :: istat

    if(allocated(this)) then
      if (n == size(this)) then
        return ! Don't need to allocate the same size
      else
        deallocate(this) ! No need to duplicate memory
      end if
    end if
    allocate(this(n), stat=istat); call mErr(istat,'allocate_i1D:this',1)
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine allocate_i2D(this, n)
    !! Interfaced with [[allocate]]
  !====================================================================!
    integer(i32), allocatable, intent(inout) :: this(:,:) !! 2D array
    integer(i32), intent(in) :: n(2) !! New allocation shape
    integer(i32) :: istat

    if(allocated(this)) then
      if (all(n == shape(this))) then
        return ! Don't need to allocate the same size
      else
        deallocate(this)
      end if
    end if
    allocate(this(n(1),n(2)), stat=istat); call mErr(istat,'allocate_i2D:this',1)
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine allocate_i3D(this, n)
    !! Interfaced with [[allocate]]
  !====================================================================!
    integer(i32), allocatable, intent(inout) :: this(:,:,:) !! 3D array
    integer(i32), intent(in) :: n(3) !! New allocation shape
    integer(i32) :: istat

    if(allocated(this)) then
      if (all(n == shape(this))) then
        return ! Don't need to allocate the same size
      else
        deallocate(this)
      end if
    end if
    allocate(this(n(1),n(2),n(3)), stat=istat); call mErr(istat,'allocate_i3D:this',1)
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine allocate_id1D(this, n)
    !! Interfaced with [[allocate]]
  !====================================================================!
    integer(i64), allocatable, intent(inout) :: this(:) !! 1D array
    integer(i32), intent(in) :: n !! Allocation size
    integer(i32) :: istat

    if(allocated(this)) then
      if (n == size(this)) then
        return ! Don't need to allocate the same size
      else
        deallocate(this) ! No need to duplicate memory
      end if
    end if
    allocate(this(n), stat=istat); call mErr(istat,'allocate_id1D:this',1)
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine allocate_id2D(this, n)
    !! Interfaced with [[allocate]]
  !====================================================================!
    integer(i64), allocatable, intent(inout) :: this(:,:) !! 2D array
    integer(i32), intent(in) :: n(2) !! New allocation shape
    integer(i32) :: istat

    if(allocated(this)) then
      if (all(n == shape(this))) then
        return ! Don't need to allocate the same size
      else
        deallocate(this)
      end if
    end if
    allocate(this(n(1),n(2)), stat=istat); call mErr(istat,'allocate_id2D:this',1)
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine allocate_id3D(this, n)
    !! Interfaced with [[allocate]]
  !====================================================================!
    integer(i64), allocatable, intent(inout) :: this(:,:,:) !! 3D array
    integer(i32), intent(in) :: n(3) !! New allocation shape
    integer(i32) :: istat

    if(allocated(this)) then
      if (all(n == shape(this))) then
        return ! Don't need to allocate the same size
      else
        deallocate(this)
      end if
    end if
    allocate(this(n(1),n(2),n(3)), stat=istat); call mErr(istat,'allocate_id3D:this',1)
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine allocate_c1D(this, n)
    !! Interfaced with [[allocate]]
  !====================================================================!
    complex(r32), allocatable, intent(inout) :: this(:) !! 1D array
    integer(i32), intent(in) :: n !! Allocation size
    integer(i32) :: istat

    if(allocated(this)) then
      if (n == size(this)) then
        return ! Don't need to allocate the same size
      else
        deallocate(this) ! No need to duplicate memory
      end if
    end if
    allocate(this(n), stat=istat); call mErr(istat,'allocate_c1D:this',1)
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine allocate_c2D(this, n)
    !! Interfaced with [[allocate]]
  !====================================================================!
    complex(r32), allocatable, intent(inout) :: this(:,:) !! 2D array
    integer(i32), intent(in) :: n(2) !! New allocation shape
    integer(i32) :: istat

    if(allocated(this)) then
      if (all(n == shape(this))) then
        return ! Don't need to allocate the same size
      else
        deallocate(this)
      end if
    end if
    allocate(this(n(1),n(2)), stat=istat); call mErr(istat,'allocate_c2D:this',1)
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine allocate_c3D(this, n)
    !! Interfaced with [[allocate]]
  !====================================================================!
    complex(r32), allocatable, intent(inout) :: this(:,:,:) !! 3D array
    integer(i32), intent(in) :: n(3) !! New allocation shape
    integer(i32) :: istat

    if(allocated(this)) then
      if (all(n == shape(this))) then
        return ! Don't need to allocate the same size
      else
        deallocate(this)
      end if
    end if
    allocate(this(n(1),n(2),n(3)), stat=istat); call mErr(istat,'allocate_c3D:this',1)
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine allocate_z1D(this, n)
    !! Interfaced with [[allocate]]
  !====================================================================!
    complex(r64), allocatable, intent(inout) :: this(:) !! 1D array
    integer(i32), intent(in) :: n !! Allocation size
    integer(i32) :: istat

    if(allocated(this)) then
      if (n == size(this)) then
        return ! Don't need to allocate the same size
      else
        deallocate(this) ! No need to duplicate memory
      end if
    end if
    allocate(this(n), stat=istat); call mErr(istat,'allocate_z1D:this',1)
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine allocate_z2D(this, n)
    !! Interfaced with [[allocate]]
  !====================================================================!
    complex(r64), allocatable, intent(inout) :: this(:,:) !! 2D array
    integer(i32), intent(in) :: n(2) !! New allocation shape
    integer(i32) :: istat

    if(allocated(this)) then
      if (all(n == shape(this))) then
        return ! Don't need to allocate the same size
      else
        deallocate(this)
      end if
    end if
    allocate(this(n(1),n(2)), stat=istat); call mErr(istat,'allocate_z2D:this',1)
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine allocate_z3D(this, n)
    !! Interfaced with [[allocate]]
  !====================================================================!
    complex(r64), allocatable, intent(inout) :: this(:,:,:) !! 3D array
    integer(i32), intent(in) :: n(3) !! New allocation shape
    integer(i32) :: istat

    if(allocated(this)) then
      if (all(n == shape(this))) then
        return ! Don't need to allocate the same size
      else
        deallocate(this)
      end if
    end if
    allocate(this(n(1),n(2),n(3)), stat=istat); call mErr(istat,'allocate_z3D:this',1)
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine allocate_l1D(this, n)
    !! Interfaced with [[allocate]]
  !====================================================================!
    logical, allocatable, intent(inout) :: this(:) !! 1D array
    integer(i32), intent(in) :: n !! Allocation size
    integer(i32) :: istat

    if(allocated(this)) then
      if (n == size(this)) then
        return ! Don't need to allocate the same size
      else
        deallocate(this) ! No need to duplicate memory
      end if
    end if
    allocate(this(n), stat=istat); call mErr(istat,'allocate_l1D:this',1)
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine allocate_l2D(this, n)
    !! Interfaced with [[allocate]]
  !====================================================================!
    logical, allocatable, intent(inout) :: this(:,:) !! 2D array
    integer(i32), intent(in) :: n(2) !! New allocation shape
    integer(i32) :: istat

    if(allocated(this)) then
      if (all(n == shape(this))) then
        return ! Don't need to allocate the same size
      else
        deallocate(this)
      end if
    end if
    allocate(this(n(1),n(2)), stat=istat); call mErr(istat,'allocate_l2D:this',1)
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine allocate_l3D(this, n)
    !! Interfaced with [[allocate]]
  !====================================================================!
    logical, allocatable, intent(inout) :: this(:,:,:) !! 3D array
    integer(i32), intent(in) :: n(3) !! New allocation shape
    integer(i32) :: istat

    if(allocated(this)) then
      if (all(n == shape(this))) then
        return ! Don't need to allocate the same size
      else
        deallocate(this)
      end if
    end if
    allocate(this(n(1),n(2),n(3)), stat=istat); call mErr(istat,'allocate_l3D:this',1)
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine allocate_test(test)
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
  call Msg('Testing : Allocate')
  call Msg('==========================')

  call allocate(ar1D, 100)
  call test%test(size(ar1D) == 100,'allocate_r1D')
  call allocate(ar2D, [5,6])
  call test%test(all(shape(ar2D) == [5,6]),'allocate_r2D')
  call allocate(ar3D, [10,9,8])
  call test%test(all(shape(ar3D) == [10,9,8]),'allocate_r3D')
  call allocate(a1D, 100)
  call test%test(size(a1D) == 100,'allocate_d1D')
  call allocate(a2D, [5,6])
  call test%test(all(shape(a2D) == [5,6]),'allocate_d2D')
  call allocate(a3D, [10,9,8])
  call test%test(all(shape(a3D) == [10,9,8]),'allocate_d3D')
  call allocate(ia1D, 100)
  call test%test(size(ia1D) == 100,'allocate_i1D')
  call allocate(ia2D, [5,6])
  call test%test(all(shape(ia2D) == [5,6]),'allocate_i2D')
  call allocate(ia3D, [10,9,8])
  call test%test(all(shape(ia3D) == [10,9,8]),'allocate_i3D')
  call allocate(iad1D, 100)
  call test%test(size(iad1D) == 100,'allocate_id1D')
  call allocate(iad2D, [5,6])
  call test%test(all(shape(iad2D) == [5,6]),'allocate_id2D')
  call allocate(iad3D, [10,9,8])
  call test%test(all(shape(iad3D) == [10,9,8]),'allocate_id3D')
  call allocate(z1D, 100)
  call test%test(size(z1D) == 100,'allocate_c1D')
  call allocate(z2D, [5,6])
  call test%test(all(shape(z2D) == [5,6]),'allocate_c2D')
  call allocate(z3D, [10,9,8])
  call test%test(all(shape(z3D) == [10,9,8]),'allocate_c3D')
  call allocate(zz1D, 100)
  call test%test(size(zz1D) == 100,'allocate_z1D')
  call allocate(zz2D, [5,6])
  call test%test(all(shape(zz2D) == [5,6]),'allocate_z2D')
  call allocate(zz3D, [10,9,8])
  call test%test(all(shape(zz3D) == [10,9,8]),'allocate_z3D')
  call allocate(la1D, 100)
  call test%test(size(la1D) == 100,'allocate_l1D')
  call allocate(la2D, [5,6])
  call test%test(all(shape(la2D) == [5,6]),'allocate_l2D')
  call allocate(la3D, [10,9,8])
  call test%test(all(shape(la3D) == [10,9,8]),'allocate_l3D')
  deallocate(ar1D,ar2D,ar3D,a1D,a2D,a3D,ia1D,ia2D,ia3D)
  deallocate(iad1D,iad2D,iad3D,z1D,z2D,z3D,zz1D,zz2D,zz3D)
  deallocate(la1D,la2D,la3D)
  end subroutine
end module
