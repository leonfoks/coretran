module m_maths
  !! Math routines
  use variableKind
  use m_allocate, only: allocate
  use m_deallocate, only: deallocate
  use m_errors, only: eMsg, msg
  use m_sort, only: argsort
  use m_select, only: argSelect
  use m_array1D, only: arange
  use m_unitTester, only: tester
  implicit none

  private

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
end module
