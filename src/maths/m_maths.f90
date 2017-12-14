module m_maths
  !! Math routines
  use variableKind
  use m_allocate, only: allocate
  use m_deallocate, only: deallocate
  use m_errors, only:eMsg, msg
  use m_sort, only: argsort
  use m_select, only: argSelect
  use m_array1D, only: arange
  use m_unitTester, only: tester
  implicit none

  private

  public :: maths_test

  public :: crossproduct
  interface crossproduct
    !! Compute the cross product between two arrays of length 2 or 3
    module function crossproduct_r1D(a,b) result(res)
      !! Interfaced with crossproduct()
      real(r32),intent(in) :: a(3) !! 1D Array
      real(r32),intent(in) :: b(3) !! 1D Array
      real(r32) :: res(3) !! cross product
    end function
    module function crossproduct_d1D(a,b) result(res)
      !! Interfaced with crossproduct()
      real(r64),intent(in) :: a(3) !! 1D Array
      real(r64),intent(in) :: b(3) !! 1D Array
      real(r64) :: res(3) !! cross product
    end function
  end interface

  public :: cumprod
  interface cumprod
    !! Compute the variance of an array
    module function cumprod_r1D(this) result(res)
      !! Interfaced with cumprod()
      real(r32),intent(in) :: this(:) !! 1D array
      real(r32) :: res(size(this)) !! Cumulative product
    end function
    module function cumprod_d1D(this) result(res)
      !! Interfaced with cumprod()
      real(r64),intent(in) :: this(:) !! 1D array
      real(r64) :: res(size(this)) !! Cumulative product
    end function
    module function cumprod_i1D(this) result(res)
      !! Interfaced with cumprod()
      integer(i32),intent(in) :: this(:) !! 1D array
      integer(i32) :: res(size(this)) !! Cumulative product
    end function
    module function cumprod_id1D(this) result(res)
      !! Interfaced with cumprod()
      integer(i64),intent(in) :: this(:) !! 1D array
      integer(i64) :: res(size(this)) !! Cumulative product
    end function
  end interface

  public :: cumsum
  interface cumsum
    !! Compute the variance of an array
    module function cumsum_r1D(this) result(res)
      !! Interfaced with cumsum()
      real(r32),intent(in) :: this(:) !! 1D array
      real(r32) :: res(size(this)) !! Cumulative sum
    end function
    module function cumsum_d1D(this) result(res)
      !! Interfaced with cumsum()
      real(r64),intent(in) :: this(:) !! 1D array
      real(r64) :: res(size(this)) !! Cumulative sum
    end function
    module function cumsum_i1D(this) result(res)
      !! Interfaced with cumsum()
      integer(i32),intent(in) :: this(:) !! 1D array
      integer(i32) :: res(size(this)) !! Cumulative sum
    end function
    module function cumsum_id1D(this) result(res)
      !! Interfaced with cumsum()
      integer(i64),intent(in) :: this(:) !! 1D array
      integer(i64) :: res(size(this)) !! Cumulative sum
    end function
  end interface

  public :: fastTwoDiff
  interface fastTwoDiff
    !! Compute the difference two numbers and compute the numerical round-off error. See Shewchuk 1997 Adaptive Precision Floating-Point Arithmetic and Fast Robust Geometric Predicates
    !! This should only be used if you know that the magnitude of a is greater than or equal to b, otherwise, you should use the slower twoDiff routine
    module function fastTwoDiff_r(a,b) result(res)
      !! Interfaced with fastTwoDiff()
      real(r32), intent(in) :: a !! First number
      real(r32), intent(in) :: b !! Second number
      real(r32) :: res(2) !! Result and its error
    end function
    module function fastTwoDiff_d(a,b) result(res)
      !! Interfaced with fastTwoDiff()
      real(r64), intent(in) :: a !! First number
      real(r64), intent(in) :: b !! Second number
      real(r64) :: res(2) !! Result and its error
    end function
  end interface

  public :: fastTwoSum
  interface fastTwoSum
    !! Compute the sum of two numbers and compute the numerical round-off error. See Shewchuk 1997 Adaptive Precision Floating-Point Arithmetic and Fast Robust Geometric Predicates
    !! This should only be used if you know that the magnitude of a is greater than or equal to b, otherwise, you should use the slower twoSum routine
    module function fastTwoSum_r(a,b) result(res)
      !! Interfaced with fastTwoSum()
      real(r32), intent(in) :: a !! First number
      real(r32), intent(in) :: b !! Second number
      real(r32) :: res(2) !! Result and its error
    end function
    module function fastTwoSum_d(a,b) result(res)
      !! Interfaced with fastTwoSum()
      real(r64), intent(in) :: a !! First number
      real(r64), intent(in) :: b !! Second number
      real(r64) :: res(2) !! Result and its error
    end function
  end interface

  public :: geometricMean
  interface geometricMean
    !! Compute the geometric mean of a vector
    module function geometricMean_r1D(this) result(res)
      !! Interfaced with geometricMean()
      real(r32),intent(in) :: this(:)
      real(r64) :: res
    end function
    module function geometricMean_d1D(this) result(res)
      !! Interfaced with geometricMean()
      real(r64),intent(in) :: this(:)
      real(r64) :: res
    end function
    module function geometricMean_i1D(this) result(res)
      !! Interfaced with geometricMean()
      integer(i32),intent(in) :: this(:)
      real(r64) :: res
    end function
    module function geometricMean_id1D(this) result(res)
      !! Interfaced with geometricMean()
      integer(i64),intent(in) :: this(:)
      real(r64) :: res
    end function
  end interface

  public :: mean
  interface mean
    !! Compute the mean
    module function mean_r1D(this) result(res)
      !! Interfaced with mean()
      real(r32), intent(in) :: this(:) !! 1D array
      real(r64) :: res !! mean
    end function
    module function mean_d1D(this) result(res)
      !! Interfaced with mean()
      real(r64), intent(in) :: this(:) !! 1D array
      real(r64) :: res !! mean
    end function
    module function mean_i1D(this) result(res)
        !! Interfaced with mean()
      integer(i32), intent(in) :: this(:) !! 1D array
      real(r64) :: res !! mean
    end function
    module function mean_id1D(this) result(res)
      !! Interfaced with mean()
      integer(i64), intent(in) :: this(:) !! 1D array
      real(r64) :: res !! mean
    end function
  end interface

  public :: median
  interface median
    !! Compute the median of a set of numbers
    module function median_r1D(this) result(res)
      !! Interfaced with median()
      real(r32), intent(in) :: this(:) !! 1D array
      real(r32) :: res !! median
    end function
    module function median_d1D(this) result(res)
      !! Interfaced with median()
      real(r64), intent(in) :: this(:) !! 1D array
      real(r64) :: res !! median
    end function
    module function median_i1D(this) result(res)
      !! Interfaced with median()
      integer(i32), intent(in) :: this(:) !! 1D array
      real(r64) :: res !! median
    end function
    module function median_id1D(this) result(res)
      !! Interfaced with median()
      integer(i64), intent(in) :: this(:) !! 1D array
      real(r64) :: res !! median
    end function
  end interface

  public :: norm1
  interface norm1
    !! Compute the L1 norm of a set of numbers
    module function norm1_r1D(this) result(res)
      !! Interfaced with norm1()
      real(r32), intent(in) :: this(:) !! 1D array
      real(r32) :: res !! L1 norm
    end function
    module function norm1_d1D(this) result(res)
      !! Interfaced with norm1()
      real(r64), intent(in) :: this(:) !! 1D array
      real(r64) :: res !! L1 norm
    end function
    module function norm1_i1D(this) result(res)
      !! Interfaced with norm1()
      integer(i32), intent(in) :: this(:) !! 1D array
      real(r64) :: res !! L1 norm
    end function
    module function norm1_id1D(this) result(res)
      !! Interfaced with norm1()
      integer(i64), intent(in) :: this(:) !! 1D array
      real(r64) :: res !! L1 norm
    end function
  end interface

  public :: normI
  interface normI
    !! Compute the Linfinity norm of a set of numbers
    module function normI_r1D(this) result(res)
      !! Interfaced with normI()
      real(r32), intent(in) :: this(:) !! 1D array
      real(r32) :: res !! Linfinity norm
    end function
    module function normI_d1D(this) result(res)
      !! Interfaced with normI()
      real(r64), intent(in) :: this(:) !! 1D array
      real(r64) :: res !! Linfinity norm
    end function
    module function normI_i1D(this) result(res)
      !! Interfaced with normI()
      integer(i32), intent(in) :: this(:) !! 1D array
      integer(i32) :: res !! Linfinity norm
    end function
    module function normI_id1D(this) result(res)
      !! Interfaced with normI()
      integer(i64), intent(in) :: this(:) !! 1D array
      integer(i64) :: res !! Linfinity norm
    end function
  end interface

  public :: project
  interface project
    !! Project a vector a onto vector b
    module function project_r1D(a,b) result(c)
      !! Interfaced with project()
      real(r32),intent(in) :: a(:) !! 1D array
      real(r32),intent(in) :: b(size(a)) !! 1D array
      real(r32) :: c(size(a)) !! 1D array
    end function
    module function project_d1D(a,b) result(c)
      !! Interfaced with project()
      real(r64),intent(in) :: a(:) !! 1D array
      real(r64),intent(in) :: b(size(a)) !! 1D array
      real(r64) :: c(size(a)) !! 1D array
    end function
  end interface

  public :: trimmedmean
  interface trimmedmean
    !! Compute the Trimmed mean of an array,  alpha is a percent value to trim from either end
    module function trimmedmean_r1D(this,alpha) result(res)
      !! Interfaced with trimmedmean()
      real(r32), intent(in) :: this(:) !! 1D array
      real(r32), intent(in) :: alpha !! Percentage to trim off each end
      real(r64) :: res !! trimmedmean
    end function
    module function trimmedmean_d1D(this,alpha) result(res)
      !! Interfaced with trimmedmean()
      real(r64), intent(in) :: this(:) !! 1D array
      real(r64), intent(in) :: alpha !! Percentage to trim off each end
      real(r64) :: res !! trimmedmean
    end function
    module function trimmedmean_i1D(this,alpha) result(res)
      !! Interfaced with trimmedmean()
      integer(i32), intent(in) :: this(:) !! 1D array
      real(r64), intent(in) :: alpha !! Percentage to trim off each end
      real(r64) :: res !! trimmedmean
    end function
    module function trimmedmean_id1D(this,alpha) result(res)
      !! Interfaced with trimmedmean()
      integer(i64), intent(in) :: this(:) !! 1D array
      real(r64), intent(in) :: alpha !! Percentage to trim off each end
      real(r64) :: res !! trimmedmean
    end function
  end interface

  public :: twoDiff
  interface twoDiff
    !! Compute the difference between two numbers and compute the numerical round-off error. See Shewchuk 1997 Adaptive Precision Floating-Point Arithmetic and Fast Robust Geometric Predicates
    !! If you know that the magnitude of a is greater than or equal to b, use fastTwoDiff
    module function twoDiff_r(a,b) result(res)
      !! Interfaced with twoDiff()
      real(r32), intent(in) :: a !! First number
      real(r32), intent(in) :: b !! Second number
      real(r32) :: res(2) !! Result and its error
    end function
    module function twoDiff_d(a,b) result(res)
      !! Interfaced with twoDiff()
      real(r64), intent(in) :: a !! First number
      real(r64), intent(in) :: b !! Second number
      real(r64) :: res(2) !! Result and its error
    end function
  end interface

  public :: twoSum
  interface twoSum
    !! Compute the sum of two numbers and compute the numerical round-off error. See Shewchuk 1997 Adaptive Precision Floating-Point Arithmetic and Fast Robust Geometric Predicates
    !! If you know that the magnitude of a is greater than or equal to b, use fastTwoSum
    module function twoSum_r(a,b) result(res)
      !! Interfaced with twoSum()
      real(r32), intent(in) :: a !! First number in sum
      real(r32), intent(in) :: b !! Second number in sum
      real(r32) :: res(2) !! The sum and its error
    end function
    module function twoSum_d(a,b) result(res)
      !! Interfaced with twoSum()
      real(r64), intent(in) :: a !! First number in sum
      real(r64), intent(in) :: b !! Second number in sum
      real(r64) :: res(2) !! The sum and its error
    end function
  end interface

  public :: std
  interface std
    !! Compute the standard deviation of an array
    module function std_r1D(this) result(res)
      !! Interfaced with std()
      real(r32), intent(in) :: this(:) !! 1D array
      real(r64) :: res !! standard deviation
    end function
    module function std_d1D(this) result(res)
      !! Interfaced with std()
      real(r64), intent(in) :: this(:) !! 1D array
      real(r64) :: res !! standard deviation
    end function
    module function std_i1D(this) result(res)
      !! Interfaced with std()
      integer(i32), intent(in) :: this(:) !! 1D array
      real(r64) :: res !! standard deviation
    end function
    module function std_id1D(this) result(res)
      !! Interfaced with std()
      integer(i64), intent(in) :: this(:) !! 1D array
      real(r64) :: res !! standard deviation
    end function
  end interface

  public :: variance
  interface variance
    !! Compute the variance of an array
    module function variance_r1D(this) result(res)
      !! Interfaced with variance()
      real(r32), intent(in) :: this(:) !! 1D array
      real(r64) :: res !! variance
    end function
    module function variance_d1D(this) result(res)
      !! Interfaced with variance()
      real(r64), intent(in) :: this(:) !! 1D array
      real(r64) :: res !! variance
    end function
    module function variance_i1D(this) result(res)
      !! Interfaced with variance()
      integer(i32), intent(in) :: this(:) !! 1D array
      real(r64) :: res !! variance
    end function
    module function variance_id1D(this) result(res)
      !! Interfaced with variance()
      integer(i64), intent(in) :: this(:) !! 1D array
      real(r64) :: res !! variance
    end function
  end interface

contains

  !====================================================================!
  subroutine maths_test(test)
  !====================================================================!
  class(tester) :: test

  real(r32) :: ar, br
  real(r32), allocatable :: ar1D(:), br1D(:), cr1D(:)
  real(r64) :: a, b
  real(r64), allocatable :: a1D(:), b1D(:), c1D(:)
  integer(i32) :: ia, ib, ic
  integer(i32), allocatable :: ia1D(:), ib1D(:)
  integer(i64) :: iad
  integer(i64), allocatable :: iad1D(:), ibd1D(:)

  call Msg('==========================')
  call Msg('Testing : Maths')
  call Msg('==========================')


  call allocate(ar1D, 3)
  call allocate(a1D, 3)
  call allocate(ia1D, 3)
  call allocate(iad1D, 3)
  call allocate(br1D, 2)
  call allocate(b1D, 2)
  call allocate(ib1D, 2)
  call allocate(ibd1D, 2)

  call arange(ar1D,1.0, 3.0, 1.0)
  call arange(a1D,1.d0, 3.d0, 1.d0)
  call arange(ia1D,1, 3, 1)
  call arange(iad1D,1_i64, 3_i64, 1_i64)


  br1D = [5.0,6.0,7.0]
  b1D = [5.d0,6.d0,7.d0]
  ib1D = [5,6,7]
  ibd1D = [5_i64, 6_i64, 7_i64]

  cr1D=crossproduct(ar1D, br1D)
  call test%test(all(cr1D==[-4.0,8.0,-4.0]),'crossproduct_r1D')
  c1D=crossproduct(a1D,b1D)
  call test%test(all(cr1D==[-4.d0,8.d0,-4.d0]),'crossproduct_d1D')

  cr1D=cumprod(ar1D)
  call test%test(all(cr1D==[1.0,2.0,6.0]),'cumprod_r1D')
  c1D=cumprod(a1D)
  call test%test(all(c1D==[1.d0,2.d0,6.d0]),'cumprod_d1D')
  ib1D=cumprod(ia1D)
  call test%test(all(ib1D==[1,2,6]),'cumprod_i1D')
  ibd1D=cumprod(iad1D)
  call test%test(all(ibd1D==[1,2,6]),'cumprod_id1D')

  cr1D=cumsum(ar1D)
  call test%test(all(cr1D==[1.0,3.0,6.0]),'cumsum_r1D')
  c1D=cumsum(a1D)
  call test%test(all(c1D==[1.d0,3.d0,6.d0]),'cumsum_d1D')
  ib1D=cumsum(ia1D)
  call test%test(all(ib1D==[1,3,6]),'cumsum_i1D')
  ibd1D=cumsum(iad1D)
  call test%test(all(ibd1D==[1,3,6]),'cumsum_id1D')

  ar1D=[1.0,2.0,3.0]
  br1D=[0.0,0.5,0.0]
  a1D=[1.d0,2.d0,3.d0]
  b1D=[0.d0,0.5d0,0.d0]

  cr1D=project(ar1D,br1D)
  call test%test(all(cr1D==[0.0,2.0,0.0]),'project_r1D')
  c1D=project(a1D,b1D)
  call test%test(all(c1D==[0.d0,2.d0,0.d0]),'project_d1D')

  a=mean(ar1D)
  call test%test(a==2.d0,'mean_r1D')
  a=mean(a1D)
  call test%test(a==2.d0,'mean_d1D')
  a=mean(ia1D)
  call test%test(a==2.d0,'mean_i1D')
  a=mean(iad1D)
  call test%test(a==2.d0,'mean_id1D')

  a=norm1(ar1D)
  call test%test(a==6.d0,'norm1_r1D')
  a=norm1(a1D)
  call test%test(a==6.d0,'norm1_d1D')
  a=norm1(a1D)
  call test%test(a==6.d0,'norm1_i1D')
  a=norm1(a1D)
  call test%test(a==6.d0,'norm1_id1D')

  ar=normI(ar1D)
  call test%test(ar==3.0,'normI_r1D')
  a=normI(a1D)
  call test%test(a==3.d0,'normI_d1D')
  ia=normI(ia1D)
  call test%test(a==3,'normI_i1D')
  iad=normI(iad1D)
  call test%test(a==3,'normI_id1D')

  a=geometricMean(ar1D)
  call test%test(a==216.d0,'geometricMean_r1D')
  a=geometricMean(a1D)
  call test%test(a==216.d0,'geometricMean_d1D')
  a=geometricMean(ia1D)
  call test%test(a==216.d0,'geometricMean_i1D')
  a=geometricMean(iad1D)
  call test%test(a==216.d0,'geometricMean_id1D')


  call allocate(a1D,2)
  call allocate(ar1D,2)
  ar = 0.2
  br = 0.1
  a = 0.2d0
  b = 0.1d0
  ar1D = twoSum(ar,br)
  call test%test(abs(ar1D(2)) < 1.d-7,'twoSum_r')
  a1D = twoSum(a,b)
  call test%test(abs(a1D(2)) < 1.d-15,'twoSum_d')
  ar1D = fastTwoSum(ar,br)
  call test%test(abs(ar1D(2)) < 1.d-7,'fastTwoSum_r')
  a1D = fastTwoSum(a,b)
  call test%test(abs(a1D(2)) < 1.d-15,'fastTwoSum_d')

  ar1D=[1.0,2.0,3.0]
  br1D=[0.0,0.5,0.0]
  a1D=[1.d0,2.d0,3.d0]
  b1D=[0.d0,0.5d0,0.d0]

  a=std(ar1D)
  call test%test(a==1.d0,'std_r1D')
  a=std(a1D)
  call test%test(a==1.d0,'std_d1D')
  a=std(ia1D)
  call test%test(a==1.d0,'std_i1D')
  a=std(iad1D)
  call test%test(a==1.d0,'std_id1D')

  a=variance(ar1D)
  call test%test(a==1.d0,'variance_r1D')
  a=variance(a1D)
  call test%test(a==1.d0,'variance_d1D')
  a=variance(ia1D)
  call test%test(a==1.d0,'variance_i1D')
  a=variance(iad1D)
  call test%test(a==1.d0,'variance_id1D')

  a=median(ar1D)
  call test%test(a==2.d0,'median_r1D')
  a=median(a1D)
  call test%test(a==2.d0,'median_d1D')
  a=median(ia1D)
  call test%test(a==2.d0,'median_i1D')
  a=median(iad1D)
  call test%test(a==2.d0,'median_id1D')

  call deallocate(ar1D)
  call deallocate(a1D)
  call deallocate(ia1D)
  call deallocate(iad1D)
  call deallocate(br1D)
  call deallocate(b1D)
  call deallocate(ib1D)
  call deallocate(ibd1D)
  call deallocate(cr1D)
  call deallocate(c1D)

  end subroutine

end module
