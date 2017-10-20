module m_swap
  !! Handles a simple swapping of two elements

  use variableKind

  implicit none

  private

  public :: swap

  interface swap
    !! Swap the values of two variables
    !!
    !!Example usage
    !!```fortran
    !!use variableKind
    !!use m_strings, only: str
    !!use m_swap, only: swap
    !!real(r64) :: a,b
    !!a = 10.d0
    !!b = 100.d0
    !!call swap(a,b)
    !!write(*,'(a)') 'Values were swapped? '//str(a == 100.d0 .and. b == 10.d0)
    !!```
    module procedure :: swap_i1, swap_id1, swap_r1, swap_d1, swap_c1, swap_z1, swap_l1
  end interface

  contains

  !====================================================================!
  subroutine swap_i1(this,that)
    !! Interfaced with swap()
  !====================================================================!
  integer(i32), intent(inout) :: this
  integer(i32), intent(inout) :: that
  integer(i32) :: tmp
  tmp = this ; this = that ; that = tmp
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine swap_id1(this,that)
    !! Interfaced with swap()
  !====================================================================!
  integer(i64), intent(inout) :: this
  integer(i64), intent(inout) :: that
  integer(i64) :: tmp
  tmp = this ; this = that ; that = tmp
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine swap_r1(this,that)
    !! Interfaced with swap()
  !====================================================================!
  real(r32), intent(inout) :: this
  real(r32), intent(inout) :: that
  real(r32) :: tmp
  tmp = this ; this = that ; that = tmp
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine swap_d1(this,that)
    !! Interfaced with swap()
  !====================================================================!
  real(r64), intent(inout) :: this
  real(r64), intent(inout) :: that
  real(r64) :: tmp
  tmp = this ; this = that ; that = tmp
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine swap_c1(this,that)
    !! Interfaced with swap()
  !====================================================================!
  complex(r32) :: this
  complex(r32) :: that
  complex(r32) :: tmp
  tmp = this ; this = that ; that = tmp
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine swap_z1(this,that)
    !! Interfaced with swap()
  !====================================================================!
  complex(r64) :: this
  complex(r64) :: that
  complex(r64) :: tmp
  tmp = this ; this = that ; that = tmp
  end subroutine
  !====================================================================!
  !====================================================================!
  subroutine swap_l1(this,that)
    !! Interfaced with swap()
  !====================================================================!
  logical :: this
  logical :: that
  logical :: tmp
  tmp = this ; this = that ; that = tmp
  end subroutine
  !====================================================================!
end module
