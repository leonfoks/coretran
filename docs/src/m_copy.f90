module m_copy
  !! Contains fundamental routines to copy arrays to new memory locations while maintaining both copies
  !!
  !! See [[copy]] for more information.
  use variableKind
  use m_errors, only: mErr, eMsg, msg
  use m_allocate, only: allocate
  use m_unitTester, only: tester

  private

  public :: copy_test

  public :: copy
  interface copy
    !! Copies an array to new memory (no pointers), The output array size will be changed to match the copy.
    !!
    !! Example usage
    !!```fortran
    !!use variableKind, only: r64
    !!use m_allocate, only: allocate
    !!use m_copy, only: copy
    !!
    !!real(r64),allocatable :: a1D(:), a2D(:,:), a3D(:,:,:)
    !!real(r64),allocatable :: b1D(:), b2D(:,:), b3D(:,:,:)
    !!call allocate(a1D, 20)
    !!call allocate(a2D, [20,20])
    !!call allocate(a3D, [20,20,20])
    !!a1D = 1.d0
    !!a2D = 2.d0
    !!a3D = 3.d0
    !!call copy(a1D, b1D)
    !!call copy(a2D, b2D)
    !!call copy(a3D, b3D)
    !!write(*,'(a)') 'a1D equals b1D: '//str(all(a1D == b1D))
    !!write(*,'(a)') 'a2D equals b2D: '//str(all(a2D == b2D))
    !!write(*,'(a)') 'a3D equals b3D: '//str(all(a3D == b3D))
    !!call deallocate(a1D)
    !!call deallocate(a2D)
    !!call deallocate(a3D)
    !!call deallocate(b1D)
    !!call deallocate(b2D)
    !!call deallocate(b3D)
    !!```
    !====================================================================!
    module subroutine copy_r1D(this,that)
    !====================================================================!
      !! Interfaced with [[copy]]
      real(r32), allocatable, intent(in) :: this(:) !! Copy this array
      real(r32), allocatable, intent(inout)  :: that(:) !! Copy of this
    end subroutine
    !====================================================================!
    !====================================================================!
    module subroutine copy_r2D(this,that)
    !====================================================================!
      !! Interfaced with [[copy]]
      real(r32), allocatable, intent(in) :: this(:,:) !! Copy this array
      real(r32), allocatable, intent(inout) :: that(:,:) !! Copy of this
    end subroutine
    !====================================================================!
    !====================================================================!
    module subroutine copy_r3D(this,that)
    !====================================================================!
      !! Interfaced with [[copy]]
      real(r32), allocatable, intent(in) :: this(:,:,:) !! Copy this array
      real(r32), allocatable, intent(inout) :: that(:,:,:) !! Copy of this
    end subroutine
    !====================================================================!

    !====================================================================!
    module subroutine copy_d1D(this,that)
    !====================================================================!
      !! Interfaced with [[copy]]
      real(r64), allocatable, intent(in) :: this(:) !! Copy this array
      real(r64), allocatable, intent(inout)  :: that(:) !! Copy of this
    end subroutine
    !====================================================================!
    !====================================================================!
    module subroutine copy_d2D(this,that)
    !====================================================================!
      !! Interfaced with [[copy]]
      real(r64), allocatable, intent(in) :: this(:,:) !! Copy this array
      real(r64), allocatable, intent(inout) :: that(:,:) !! Copy of this
    end subroutine
    !====================================================================!
    !====================================================================!
    module subroutine copy_d3D(this,that)
    !====================================================================!
      !! Interfaced with [[copy]]
      real(r64), allocatable, intent(in) :: this(:,:,:) !! Copy this array
      real(r64), allocatable, intent(inout) :: that(:,:,:) !! Copy of this
    end subroutine
    !====================================================================!

    !====================================================================!
    module subroutine copy_i1D(this,that)
    !====================================================================!
      !! Interfaced with [[copy]]
      integer(i32), allocatable, intent(in) :: this(:) !! Copy this array
      integer(i32), allocatable, intent(inout)  :: that(:) !! Copy of this
    end subroutine
    !====================================================================!
    !====================================================================!
    module subroutine copy_i2D(this,that)
    !====================================================================!
      !! Interfaced with [[copy]]
      integer(i32), allocatable, intent(in) :: this(:,:) !! Copy this array
      integer(i32), allocatable, intent(inout) :: that(:,:) !! Copy of this
    end subroutine
    !====================================================================!
    !====================================================================!
    module subroutine copy_i3D(this,that)
    !====================================================================!
      !! Interfaced with [[copy]]
      integer(i32), allocatable, intent(in) :: this(:,:,:) !! Copy this array
      integer(i32), allocatable, intent(inout) :: that(:,:,:) !! Copy of this
    end subroutine
    !====================================================================!

    !====================================================================!
    module subroutine copy_id1D(this,that)
    !====================================================================!
      !! Interfaced with [[copy]]
      integer(i64), allocatable, intent(in) :: this(:) !! Copy this array
      integer(i64), allocatable, intent(inout)  :: that(:) !! Copy of this
    end subroutine
    !====================================================================!
    !====================================================================!
    module subroutine copy_id2D(this,that)
    !====================================================================!
      !! Interfaced with [[copy]]
      integer(i64), allocatable, intent(in) :: this(:,:) !! Copy this array
      integer(i64), allocatable, intent(inout) :: that(:,:) !! Copy of this
    end subroutine
    !====================================================================!
    !====================================================================!
    module subroutine copy_id3D(this,that)
    !====================================================================!
      !! Interfaced with [[copy]]
      integer(i64), allocatable, intent(in) :: this(:,:,:) !! Copy this array
      integer(i64), allocatable, intent(inout) :: that(:,:,:) !! Copy of this
    end subroutine
    !====================================================================!

    !====================================================================!
    module subroutine copy_c1D(this,that)
    !====================================================================!
      !! Interfaced with [[copy]]
      complex(r32), allocatable, intent(in) :: this(:) !! Copy this array
      complex(r32), allocatable, intent(inout)  :: that(:) !! Copy of this
    end subroutine
    !====================================================================!
    !====================================================================!
    module subroutine copy_c2D(this,that)
    !====================================================================!
      !! Interfaced with [[copy]]
      complex(r32), allocatable, intent(in) :: this(:,:) !! Copy this array
      complex(r32), allocatable, intent(inout) :: that(:,:) !! Copy of this
    end subroutine
    !====================================================================!
    !====================================================================!
    module subroutine copy_c3D(this,that)
    !====================================================================!
      !! Interfaced with [[copy]]
      complex(r32), allocatable, intent(in) :: this(:,:,:) !! Copy this array
      complex(r32), allocatable, intent(inout) :: that(:,:,:) !! Copy of this
    end subroutine
    !====================================================================!

    !====================================================================!
    module subroutine copy_z1D(this,that)
    !====================================================================!
      !! Interfaced with [[copy]]
      complex(r64), allocatable, intent(in) :: this(:) !! Copy this array
      complex(r64), allocatable, intent(inout)  :: that(:) !! Copy of this
    end subroutine
    !====================================================================!
    !====================================================================!
    module subroutine copy_z2D(this,that)
    !====================================================================!
      !! Interfaced with [[copy]]
      complex(r64), allocatable, intent(in) :: this(:,:) !! Copy this array
      complex(r64), allocatable, intent(inout) :: that(:,:) !! Copy of this
    end subroutine
    !====================================================================!
    !====================================================================!
    module subroutine copy_z3D(this,that)
    !====================================================================!
      !! Interfaced with [[copy]]
      complex(r64), allocatable, intent(in) :: this(:,:,:) !! Copy this array
      complex(r64), allocatable, intent(inout) :: that(:,:,:) !! Copy of this
    end subroutine
    !====================================================================!

    !====================================================================!
    module subroutine copy_l1D(this,that)
    !====================================================================!
      !! Interfaced with [[copy]]
      logical, allocatable, intent(in) :: this(:) !! Copy this array
      logical, allocatable, intent(inout)  :: that(:) !! Copy of this
    end subroutine
    !====================================================================!
    !====================================================================!
    module subroutine copy_l2D(this,that)
    !====================================================================!
      !! Interfaced with [[copy]]
      logical, allocatable, intent(in) :: this(:,:) !! Copy this array
      logical, allocatable, intent(inout) :: that(:,:) !! Copy of this
    end subroutine
    !====================================================================!
    !====================================================================!
    module subroutine copy_l3D(this,that)
    !====================================================================!
      !! Interfaced with [[copy]]
      logical, allocatable, intent(in) :: this(:,:,:) !! Copy this array
      logical, allocatable, intent(inout) :: that(:,:,:) !! Copy of this
    end subroutine
    !====================================================================!

  end interface

contains
  !====================================================================!
  module subroutine copy_r1D(this,that)
  !====================================================================!
    !! Interfaced with [[copy]]
    real(r32), allocatable, intent(in) :: this(:) !! Copy this array
    real(r32), allocatable, intent(inout)  :: that(:) !! Copy of this
    if (.not. allocated(this)) call eMsg('copy_r1D:Array to be copied is not allocated')
    call allocate(that,size(this))
    that = this
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine copy_r2D(this,that)
  !====================================================================!
    !! Interfaced with [[copy]]
    real(r32), allocatable, intent(in) :: this(:,:) !! Copy this array
    real(r32), allocatable, intent(inout) :: that(:,:) !! Copy of this
    if (.not. allocated(this)) call eMsg('copy_r2D:Array to be copied is not allocated')
    call allocate(that,shape(this))
    that = this
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine copy_r3D(this,that)
  !====================================================================!
    !! Interfaced with [[copy]]
    real(r32), allocatable, intent(in) :: this(:,:,:) !! Copy this array
    real(r32), allocatable, intent(inout) :: that(:,:,:) !! Copy of this
    if (.not. allocated(this)) call eMsg('copy_r3D:Array to be copied is not allocated')
    call allocate(that,shape(this))
    that = this
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine copy_d1D(this,that)
  !====================================================================!
    !! Interfaced with [[copy]]
    real(r64), allocatable, intent(in) :: this(:) !! Copy this array
    real(r64), allocatable, intent(inout) :: that(:) !! Copy of this
    if (.not. allocated(this)) call eMsg('copy_d1D:Array to be copied is not allocated')
    call allocate(that,size(this))
    that = this
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine copy_d2D(this,that)
  !====================================================================!
    !! Interfaced with [[copy]]
    real(r64), allocatable, intent(in) :: this(:,:) !! Copy this array
    real(r64), allocatable, intent(inout) :: that(:,:) !! Copy of this
    if (.not. allocated(this)) call eMsg('copy_d2D:Array to be copied is not allocated')
    call allocate(that,shape(this))
    that = this
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine copy_d3D(this,that)
  !====================================================================!
    !! Interfaced with [[copy]]
    real(r64), allocatable, intent(in) :: this(:,:,:) !! Copy this array
    real(r64), allocatable, intent(inout) :: that(:,:,:) !! Copy of this
    if (.not. allocated(this)) call eMsg('copy_d3D:Array to be copied is not allocated')
    call allocate(that,shape(this))
    that = this
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine copy_i1D(this,that)
  !====================================================================!
    !! Interfaced with [[copy]]
    integer(i32), allocatable, intent(in) :: this(:) !! Copy this array
    integer(i32), allocatable, intent(inout) :: that(:) !! Copy of this
    if (.not. allocated(this)) call eMsg('copy_i1D:Array to be copied is not allocated')
    call allocate(that,size(this))
    that = this
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine copy_i2D(this,that)
  !====================================================================!
    !! Interfaced with [[copy]]
    integer(i32), allocatable, intent(in) :: this(:,:) !! Copy this array
    integer(i32), allocatable, intent(inout) :: that(:,:) !! Copy of this
    if (.not. allocated(this)) call eMsg('copy_i2D:Array to be copied is not allocated')
    call allocate(that,shape(this))
    that = this
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine copy_i3D(this,that)
  !====================================================================!
    !! Interfaced with [[copy]]
    integer(i32), allocatable, intent(in) :: this(:,:,:) !! Copy this array
    integer(i32), allocatable, intent(inout) :: that(:,:,:) !! Copy of this
    if (.not. allocated(this)) call eMsg('copy_i3D:Array to be copied is not allocated')
    call allocate(that,shape(this))
    that = this
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine copy_id1D(this,that)
  !====================================================================!
    !! Interfaced with [[copy]]
    integer(i64), allocatable, intent(in) :: this(:) !! Copy this array
    integer(i64), allocatable, intent(inout) :: that(:) !! Copy of this
    if (.not. allocated(this)) call eMsg('copy_id1D:Array to be copied is not allocated')
    call allocate(that,size(this))
    that = this
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine copy_id2D(this,that)
  !====================================================================!
    !! Interfaced with [[copy]]
    integer(i64), allocatable, intent(in) :: this(:,:) !! Copy this array
    integer(i64), allocatable, intent(inout) :: that(:,:) !! Copy of this
    if (.not. allocated(this)) call eMsg('copy_id2D:Array to be copied is not allocated')
    call allocate(that,shape(this))
    that = this
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine copy_id3D(this,that)
  !====================================================================!
    !! Interfaced with [[copy]]
    integer(i64), allocatable, intent(in) :: this(:,:,:) !! Copy this array
    integer(i64), allocatable, intent(inout) :: that(:,:,:) !! Copy of this
    if (.not. allocated(this)) call eMsg('copy_id3D:Array to be copied is not allocated')
    call allocate(that,shape(this))
    that = this
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine copy_c1D(this,that)
  !====================================================================!
    !! Interfaced with [[copy]]
    complex(r32), allocatable, intent(in) :: this(:) !! Copy this array
    complex(r32), allocatable, intent(inout)  :: that(:) !! Copy of this
    if (.not. allocated(this)) call eMsg('copy_c1D:Array to be copied is not allocated')
    call allocate(that,size(this))
    that = this
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine copy_c2D(this,that)
  !====================================================================!
    !! Interfaced with [[copy]]
    complex(r32), allocatable, intent(in) :: this(:,:) !! Copy this array
    complex(r32), allocatable, intent(inout) :: that(:,:) !! Copy of this
    if (.not. allocated(this)) call eMsg('copy_c2D:Array to be copied is not allocated')
    call allocate(that,shape(this))
    that = this
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine copy_c3D(this,that)
  !====================================================================!
    !! Interfaced with [[copy]]
    complex(r32), allocatable, intent(in) :: this(:,:,:) !! Copy this array
    complex(r32), allocatable, intent(inout) :: that(:,:,:) !! Copy of this
    if (.not. allocated(this)) call eMsg('copy_c3D:Array to be copied is not allocated')
    call allocate(that,shape(this))
    that = this
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine copy_z1D(this,that)
  !====================================================================!
    !! Interfaced with [[copy]]
    complex(r64), allocatable, intent(in) :: this(:) !! Copy this array
    complex(r64), allocatable, intent(inout)  :: that(:) !! Copy of this
    if (.not. allocated(this)) call eMsg('copy_z1D:Array to be copied is not allocated')
    call allocate(that,size(this))
    that = this
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine copy_z2D(this,that)
  !====================================================================!
    !! Interfaced with [[copy]]
    complex(r64), allocatable, intent(in) :: this(:,:) !! Copy this array
    complex(r64), allocatable, intent(inout) :: that(:,:) !! Copy of this
    if (.not. allocated(this)) call eMsg('copy_z2D:Array to be copied is not allocated')
    call allocate(that,shape(this))
    that = this
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine copy_z3D(this,that)
  !====================================================================!
    !! Interfaced with [[copy]]
    complex(r64), allocatable, intent(in) :: this(:,:,:) !! Copy this array
    complex(r64), allocatable, intent(inout) :: that(:,:,:) !! Copy of this
    if (.not. allocated(this)) call eMsg('copy_z3D:Array to be copied is not allocated')
    call allocate(that,shape(this))
    that = this
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine copy_l1D(this,that)
  !====================================================================!
    !! Interfaced with [[copy]]
    logical, allocatable, intent(in) :: this(:) !! Copy this array
    logical, allocatable, intent(inout)  :: that(:) !! Copy of this
    if (.not. allocated(this)) call eMsg('copy_l1D:Array to be copied is not allocated')
    call allocate(that,size(this))
    that = this
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine copy_l2D(this,that)
  !====================================================================!
    !! Interfaced with [[copy]]
    logical, allocatable, intent(in) :: this(:,:) !! Copy this array
    logical, allocatable, intent(inout) :: that(:,:) !! Copy of this
    if (.not. allocated(this)) call eMsg('copy_l2D:Array to be copied is not allocated')
    call allocate(that,shape(this))
    that = this
  end subroutine
  !====================================================================!
  !====================================================================!
  module subroutine copy_l3D(this,that)
  !====================================================================!
    !! Interfaced with [[copy]]
    logical, allocatable, intent(in) :: this(:,:,:) !! Copy this array
    logical, allocatable, intent(inout) :: that(:,:,:) !! Copy of this
    if (.not. allocated(this)) call eMsg('copy_l3D:Array to be copied is not allocated')
    call allocate(that,shape(this))
    that = this
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine copy_test(test)
  !====================================================================!
  class(tester) :: test

  real(r32), allocatable :: ar1D(:), ar2D(:,:), ar3D(:,:,:)
  real(r64), allocatable :: a1D(:), a2D(:,:), a3D(:,:,:)
  integer(i32), allocatable :: ia1D(:), ia2D(:,:), ia3D(:,:,:)
  integer(i64), allocatable :: iad1D(:), iad2D(:,:), iad3D(:,:,:)
  complex(r32), allocatable :: za1D(:), za2D(:,:), za3D(:,:,:)
  complex(r64), allocatable :: zza1D(:), zza2D(:,:), zza3D(:,:,:)
  logical, allocatable :: la1D(:), la2D(:,:), la3D(:,:,:)

  real(r32), allocatable :: br1D(:), br2D(:,:), br3D(:,:,:)
  real(r64), allocatable :: b1D(:), b2D(:,:), b3D(:,:,:)
  integer(i32), allocatable :: ib1D(:), ib2D(:,:), ib3D(:,:,:)
  integer(i64), allocatable :: ibd1D(:), ibd2D(:,:), ibd3D(:,:,:)
  complex(r32), allocatable :: zb1D(:), zb2D(:,:), zb3D(:,:,:)
  complex(r64), allocatable :: zzb1D(:), zzb2D(:,:), zzb3D(:,:,:)
  logical, allocatable :: lb1D(:), lb2D(:,:), lb3D(:,:,:)

  call Msg('==========================')
  call Msg('Testing : Copy')
  call Msg('==========================')

  call allocate(ar1D, 10); ar1D = 1.0
  call allocate(ar2D, [5,6]); ar2D = 2.0
  call allocate(ar3D, [10,9,8]); ar3D = 3.0
  call allocate(a1D, 10); a1D = 1.d0
  call allocate(a2D, [5,6]); a2D = 2.d0
  call allocate(a3D, [10,9,8]); a3D = 3.d0
  call allocate(ia1D, 10); ia1D = 1
  call allocate(ia2D, [5,6]); ia2D = 2
  call allocate(ia3D, [10,9,8]); ia3D = 3
  call allocate(iad1D, 10); iad1D = 1_i64
  call allocate(iad2D, [5,6]); iad2D = 2_i64
  call allocate(iad3D, [10,9,8]); iad3D = 3_i64
  call allocate(za1D, 10); za1D = (1.0, 1.0)
  call allocate(za2D, [5,6]); za2D = (2.0, 1.0)
  call allocate(za3D, [10,9,8]); za3D = (3.0, 1.0)
  call allocate(zza1D, 10); zza1D = (1.d0, 1.d0)
  call allocate(zza2D, [5,6]); zza2D = (2.d0, 1.d0)
  call allocate(zza3D, [10,9,8]); zza3D = (3.d0, 1.d0)
  call allocate(la1D, 10); la1D = .true.
  call allocate(la2D, [5,6]); la2D = .true.
  call allocate(la3D, [10,9,8]); la3D = .true.

  call copy(ar1D,br1D)
  call test%test(all(ar1D == br1D),'copy_r1D')
  call copy(ar2D,br2D)
  call test%test(all(ar2D == br2D),'copy_r2D')
  call copy(ar3D,br3D)
  call test%test(all(ar3D == br3D),'copy_r3D')
  call copy(a1D,b1D)
  call test%test(all(a1D == b1D),'copy_d1D')
  call copy(a2D,b2D)
  call test%test(all(a2D == b2D),'copy_d2D')
  call copy(a3D,b3D)
  call test%test(all(a3D == b3D),'copy_d3D')
  call copy(ia1D,ib1D)
  call test%test(all(ia1D == ib1D),'copy_i1D')
  call copy(ia2D,ib2D)
  call test%test(all(ia2D == ib2D),'copy_i2D')
  call copy(ia3D,ib3D)
  call test%test(all(ia3D == ib3D),'copy_i3D')
  call copy(iad1D,ibd1D)
  call test%test(all(iad1D == ibd1D),'copy_id1D')
  call copy(iad2D,ibd2D)
  call test%test(all(iad2D == ibd2D),'copy_id2D')
  call copy(iad3D,ibd3D)
  call test%test(all(iad3D == ibd3D),'copy_id3D')
  call copy(za1D,zb1D)
  call test%test(all(za1D == zb1D),'copy_c1D')
  call copy(za2D,zb2D)
  call test%test(all(za2D == zb2D),'copy_c2D')
  call copy(za3D,zb3D)
  call test%test(all(za3D == zb3D),'copy_c3D')
  call copy(zza1D,zzb1D)
  call test%test(all(zza1D == zzb1D),'copy_z1D')
  call copy(zza2D,zzb2D)
  call test%test(all(zza2D == zzb2D),'copy_z2D')
  call copy(zza3D,zzb3D)
  call test%test(all(zza3D == zzb3D),'copy_z3D')
  call copy(la1D,lb1D)
  call test%test(all(la1D .eqv. lb1D),'copy_l1D')
  call copy(la2D,lb2D)
  call test%test(all(la2D .eqv. lb2D),'copy_l2D')
  call copy(la3D,lb3D)
  call test%test(all(la3D .eqv. lb3D),'copy_l3D')

  deallocate(ar1D,ar2D,ar3D,a1D,a2D,a3D,ia1D,ia2D,ia3D)
  deallocate(iad1D,iad2D,iad3D,za1D,za2D,za3D,zza1D,zza2D,zza3D)
  deallocate(la1D,la2D,la3D)

  deallocate(br1D,br2D,br3D,b1D,b2D,b3D,ib1D,ib2D,ib3D)
  deallocate(ibd1D,ibd2D,ibd3D,zb1D,zb2D,zb3D,zzb1D,zzb2D,zzb3D)
  deallocate(lb1D,lb2D,lb3D)
  end subroutine
end module
