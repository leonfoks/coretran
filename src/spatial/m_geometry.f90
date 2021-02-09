module m_geometry
  !! See Shewchuk 1997 Adaptive Precision Floating-Point Arithmetic and Fast Robust Geometric Predicates

  use variableKind, only: i32, i64, r32, r64
  use m_unitTester, only: tester

  private

  public :: orient2D
  public :: orient3D
  public :: geometryTest


  interface
    !====================================================================!
    module function inCircle(ax, ay, bx, by, cx, cy, dx, dy) result(determinant)
      !! Determines whether the point d is inside the circumcircle of the triangle formed by a-b-c
      !! Returns a positive value if d is inside.
      !! Returns a negative value if d is outside.
      !! Returns a zero if the four points are cocircular.
      !!
      !! Uses an adaptive floating method by Shewchuk. Only exact computations
      !! are carried out when needed.
    !====================================================================!
    real(r64), intent(in) :: ax
      !! x co-ordinate of the first point
    real(r64), intent(in) :: ay
      !! y co-ordinate of the first point
    real(r64), intent(in) :: bx
      !! x co-ordinate of the second point
    real(r64), intent(in) :: by
      !! y co-ordinate of the second point
    real(r64), intent(in) :: cx
      !! x co-ordinate of the third point
    real(r64), intent(in) :: cy
      !! y co-ordinate of the third point
    real(r64), intent(in) :: dx
      !! x co-ordinate of the fourth point
    real(r64), intent(in) :: dy
      !! y co-ordinate of the fourth point
    real(r64) :: determinant
      !! [-ve, 0, +ve] for point d [outside, on, inside] the circumcircle of a -> b -> c
    end function
  end interface

  interface
    !====================================================================!
    module function inSphere(ax, ay, az, bx, by, bz, cx, cy, cz, dx, dy, dz, ex, ey, ez) result(determinant)
      !! Determines whether the point e is inside the circumsphere of the tetrahedron formed by a-b-c-d
      !! Returns a positive value if e is inside.
      !! Returns a negative value if e is outside.
      !! Returns a zero if the five points are cospherical.
      !! a -> b -> c -> d must be ordered in a clockwise manner as defined by orient3D
      !!
      !! Uses an adaptive floating method by Shewchuk. Only exact computations
      !! are carried out when needed.
    !====================================================================!
    real(r64), intent(in) :: ax
      !! x co-ordinate of the first point
    real(r64), intent(in) :: ay
      !! y co-ordinate of the first point
    real(r64), intent(in) :: az
      !! z co-ordinate of the first point
    real(r64), intent(in) :: bx
      !! x co-ordinate of the second point
    real(r64), intent(in) :: by
      !! y co-ordinate of the second point
    real(r64), intent(in) :: bz
      !! z co-ordinate of the second point
    real(r64), intent(in) :: cx
      !! x co-ordinate of the third point
    real(r64), intent(in) :: cy
      !! y co-ordinate of the third point
    real(r64), intent(in) :: cz
      !! z co-ordinate of the third point
    real(r64), intent(in) :: dx
      !! x co-ordinate of the fourth point
    real(r64), intent(in) :: dy
      !! y co-ordinate of the fourth point
    real(r64), intent(in) :: dz
      !! z co-ordinate of the fourth point
    real(r64), intent(in) :: ex
      !! x co-ordinate of the fifth point
    real(r64), intent(in) :: ey
      !! y co-ordinate of the fifth point
    real(r64), intent(in) :: ez
      !! z co-ordinate of the fifth point
    real(r64) :: determinant
      !! [-ve, 0, +ve] for point e [outside, on, inside] the circumsphere defined by points a -> b -> c -> d
    end function
    !====================================================================!
  end interface

  interface
    !====================================================================!
    module function orient2D(ax, ay, bx, by, cx, cy) result(determinant)
      !! Determines whether three points a, b, c are in clockwise order.
      !! i.e. is the point c to the left or right of line a-b?
      !! Returns a positive value if a-b-c are in an anticlockwise order.
      !! Returns a negative value if a-b-c are in clockwise order.
      !! Returns a zero if the points are colinear.
      !!
      !! Uses an adaptive floating method by Shewchuk. Only exact computations
      !! are carried out when needed.
    !====================================================================!
    real(r64), intent(in) :: ax
      !! x co-ordinate of the first point
    real(r64), intent(in) :: ay
      !! y co-ordinate of the first point
    real(r64), intent(in) :: bx
      !! x co-ordinate of the second point
    real(r64), intent(in) :: by
      !! y co-ordinate of the second point
    real(r64), intent(in) :: cx
      !! x co-ordinate of the third point
    real(r64), intent(in) :: cy
      !! y co-ordinate of the third point
    real(r64) :: determinant
      !! [-ve, 0, +ve] for [clockwise, colinear, anticlockwise] points a -> b -> c
    end function
  end interface
  
  interface
    !====================================================================!
    module function orient3D(ax, ay, az, bx, by, bz, cx, cy, cz, dx, dy, dz) result(determinant)
      !! Determines whether the points a, b, c, d defining a polyhedron are ordered
      !! in an anticlockwise manner.
      !! Returns a positive value if a-b-c-d are in an anticlockwise order.
      !! Returns a negative value if a-b-c-d are in clockwise order.
      !! Returns zero if they are coplanar.
      !! Clockwise is defined when viewed from above the plane defined by a-b-c.
      !!
      !! Uses an adaptive floating method by Shewchuk. Only exact computations
      !! are carried out when needed.
    !====================================================================!
    real(r64), intent(in) :: ax
      !! x co-ordinate of the first point
    real(r64), intent(in) :: ay
      !! y co-ordinate of the first point
    real(r64), intent(in) :: az
      !! z co-ordinate of the first point
    real(r64), intent(in) :: bx
      !! x co-ordinate of the second point
    real(r64), intent(in) :: by
      !! y co-ordinate of the second point
    real(r64), intent(in) :: bz
      !! z co-ordinate of the second point
    real(r64), intent(in) :: cx
      !! x co-ordinate of the third point
    real(r64), intent(in) :: cy
      !! y co-ordinate of the third point
    real(r64), intent(in) :: cz
      !! z co-ordinate of the third point
    real(r64), intent(in) :: dx
      !! x co-ordinate of the fourth point
    real(r64), intent(in) :: dy
      !! y co-ordinate of the fourth point
    real(r64), intent(in) :: dz
      !! z co-ordinate of the fourth point
    real(r64) :: determinant
      !! [-ve, 0, +ve] for point d [right, on, left] of the plane defined by points a -> b -> c
    end function
  end interface

  interface
    !====================================================================!
    module subroutine geometryTest(test)
    !====================================================================!
    class(tester) :: test
    end subroutine
  end interface
  !====================================================================!


end module