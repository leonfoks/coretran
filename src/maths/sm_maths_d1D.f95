submodule (m_maths) sm_maths_d1d
  !! Implement math routines for double precision arrays
use variableKind
use m_allocate, only: allocate
use m_errors, only:eMsg,mErr
use m_sort, only: argsort
use m_select, only: argSelect
use m_array1D, only: arange
implicit none

contains
  !====================================================================!
  module function crossProduct_d1D(a,b) result(res)
    !! Interfaced with crossproduct()
  !====================================================================!
  real(r64),intent(in) :: a(3) !! 1D Array
  real(r64),intent(in) :: b(3) !! 1D Array
  real(r64) :: res(3) !! cross product
  res(1)=a(2)*b(3)-a(3)*b(2)
  res(2)=a(3)*b(1)-a(1)*b(3)
  res(3)=a(1)*b(2)-a(2)*b(1)
  end function
  !====================================================================!
  !====================================================================!
  module function cumprod_d1D(this) result(res)
    !! Interfaced with cumprod()
  !====================================================================!
  real(r64),intent(in) :: this(:) !! 1D array
  real(r64) :: res(size(this)) !! Cumulative product
  integer(i32) :: i
  integer(i32) :: N
  N=size(this)
  res(1) = this(1)
  do i=2,N
    res(i) = res(i-1) * this(i)
  end do
  end function
  !====================================================================!
  !====================================================================!
  module function cumsum_d1D(this) result(res)
    !! Interfaced with cumsum()
  !====================================================================!
  real(r64),intent(in) :: this(:) !! 1D array
  real(r64) :: res(size(this)) !! Cumulative sum
  integer(i32) :: i
  integer(i32) :: N
  N=size(this)
  res(1) = this(1)
  do i=2,N
    res(i) = res(i-1) + this(i)  ! Round off error?
  end do
  end function
  !====================================================================!
  !====================================================================!
  module function fastTwoDiff_d(a,b) result(res)
    !! Interfaced with fastTwoDiff()
  !====================================================================!
    real(r64), intent(in) :: a !! First number
    real(r64), intent(in) :: b !! Second number
    real(r64) :: res(2) !! Result and its error
    real(r64) :: bVirtual
    res(1) = a - b
    bVirtual = a - res(1)
    res(2) = bVirtual - b
  end function
  !====================================================================!
  !====================================================================!
  module function fastTwoSum_d(a,b) result(res)
    !! Interfaced with fastTwoSum()
  !====================================================================!
    real(r64), intent(in) :: a !! First number in sum
    real(r64), intent(in) :: b !! Second number in sum
    real(r64) :: res(2) !! The sum and its error
    real(r64) :: bVirtual
    res(1) = a + b
    bVirtual = res(1) - a
    res(2) = b - bVirtual
  end function
  !====================================================================!
  !====================================================================!
  module function geometricMean_d1D(this) result(res)
    !! Interfaced with geometricMean()
  !====================================================================!
  real(r64),intent(in) :: this(:)
  real(r64) :: res
  res=product(this)
  res=res**(dble(size(this)))
  end function
  !====================================================================!
  !====================================================================!
  module procedure Mean_d1D
    !! interface with mean()
  !====================================================================!
  !module function mean_d1D(this) result(res)
  !real(r64) :: this(:)
  !real(r64) :: res
  res=sum(this)/dble(size(this))
  end procedure
  !====================================================================!
  !====================================================================!
  module function median_d1D(this) result(res)
  !====================================================================!
    !! Interfaced with median()
  real(r64), intent(in) :: this(:) !! 1D array
  real(r64) :: res !! median
  integer(i32), allocatable :: i(:)
  integer(i32) :: iMed
  integer(i32) :: N
  N=size(this)
  call allocate(i,N)
  call arange(i,1,N)

  if (mod(N,2)==0) then
    res=argSelect(this,i,N/2)
    call arange(i,1,N)
    res=0.5d0*(res+argSelect(this,i,(N/2)+1))
  else
    iMed=N/2+1
    res=argSelect(this,i,iMed)
  end if
  deallocate(i)
  end function
  !====================================================================!
  !====================================================================!
  module procedure norm1_d1D
    !! interface with norm1()
  !====================================================================!
  !module function norm1_d1D(this) result(res)
  !real(r64) :: this(:)
  !real(r64) :: res
  res=sum(abs(this))
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure normI_d1D
    !! interface with normI()
  !====================================================================!
  !module function normI_d1D(this) result(res)
  !real(r64) :: this(:)
  !real(r64) :: res
  res=maxval(abs(this))
  end procedure
  !====================================================================!
  !====================================================================!
  module function project_d1D(a,b) result(c)
  !====================================================================!
    !! Interfaced with project()
    real(r64),intent(in) :: a(:) !! 1D array
    real(r64),intent(in) :: b(size(a)) !! 1D array
    real(r64) :: c(size(a)) !! 1D array
    real(r64) :: c1
    ! Magnitude of b^2
    c1=norm2(b)**2.d0
    if (c1 == 0.0) then
      c=0.d0
      return
    end if
    c=b/c1
    ! Dot a onto b hat
    c1=dot_product(a,b)
    ! Multiply projected length by b hat
    c=c1*c
  end function
  !====================================================================!
  !====================================================================!
  module procedure trimmedmean_d1D
  !====================================================================!
  !function trimmedmean_d1D(this,alpha) result(res)
  !real(r64) :: this(:)
  !real(r64) :: alpha
  !real(r64) :: res
  integer(i32) :: istat
  integer(i32) :: j
  integer(i32) :: N
  integer(i32) :: tmp
  integer(i32), allocatable :: i(:)
  real(r64) :: alpha_
  N=size(this)
  alpha_=alpha*0.01d0
  ! Test the percentage
  if (alpha_ <= 0.d0) then
    res=Mean(this)
    return
  elseif (alpha_ >= 0.5d0) then
    call eMsg('trimmedmean:alpha >= 50% does not make sense')
  endif
  ! Calculate the number of integers that make up the trimmed percentage
  tmp=idnint(alpha_*dble(N))

  ! Set the indices into the vector
  allocate(i(N),stat=istat); call mErr(istat,'trimmedmean:i',1)
  i=[(j, j=1, N)]

  ! Sort the vector
  call argSort(this,i)

  res=mean(this(i(tmp+1:N-tmp)))
  deallocate(i,stat=istat) ; call mErr(istat,'trimmedmean:i',2)
  end procedure
  !====================================================================!
  !====================================================================!
  module function twoDiff_d(a,b) result(res)
  !====================================================================!
    !! Interfaced with twoDiff()
  real(r64), intent(in) :: a !! First number
  real(r64), intent(in) :: b !! Second number
  real(r64) :: res(2) !! Result and its error
  real(r64) :: aVirtual, bVirtual
  real(r64) :: aRoundoff, bRoundoff
  res(1) = a - b
  bVirtual = a - res(1)
  aVirtual = res(1) + bVirtual
  bRoundoff = bVirtual - b
  aRoundoff = a - aVirtual
  res(2) = aRoundoff + bRoundoff
  end function
  !====================================================================!
  !====================================================================!
  module function twoSum_d(a,b) result(res)
    !! Interfaced with twoSum()
  !====================================================================!
    real(r64), intent(in) :: a !! First number in sum
    real(r64), intent(in) :: b !! Second number in sum
    real(r64) :: res(2) !! The sum and its error
    real(r64) :: aVirtual, bVirtual
    real(r64) :: aRoundoff, bRoundoff
    res(1) = a + b
    bVirtual = res(1) - a
    aVirtual = res(1) - bVirtual
    bRoundoff = b - bVirtual
    aRoundoff = a - aVirtual
    res(2) = aRoundoff + bRoundoff
  end function
  !====================================================================!
  !====================================================================!
  module procedure std_d1D
    !! Interfaced with std()
  !====================================================================!
  !real(r64) :: this(:)
  !real(r64) :: res
  res=dsqrt(Variance(this))
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure variance_d1D
    !! Interfaced with variance()
  !====================================================================!
  !real(r64) :: this(:)
  !real(r64) :: res
  real(r64) :: tmp
  tmp=Mean(this)
  res=sum((this-tmp)**2.d0)/dble(size(this)-1)
  end procedure
  !====================================================================!
end submodule
