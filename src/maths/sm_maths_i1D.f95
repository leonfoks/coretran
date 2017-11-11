submodule (m_maths) sm_maths_i1D

use variableKind
use m_allocate, only: allocate
use m_errors, only:eMsg,mErr
use m_sort, only: argsort
use m_select, only: argSelect
use m_array1D, only: arange
implicit none

contains
  !====================================================================!
  module function cumprod_i1D(this) result(res)
    !! Interfaced with cumprod()
  !====================================================================!
  integer(i32),intent(in) :: this(:) !! 1D array
  integer(i32) :: res(size(this)) !! Cumulative product
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
  module function cumsum_i1D(this) result(res)
    !! Interfaced with cumsum()
  !====================================================================!
  integer(i32), intent(in) :: this(:) !! 1D array
  integer(i32) :: res(size(this)) !! Cumulative sum
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
  module function geometricMean_i1D(this) result(res)
    !! Interfaced with geometricMean()
  !====================================================================!
  integer(i32),intent(in) :: this(:)
  real(r64) :: res
  integer(i32) :: tmp
  tmp=product(this)
  res=dble(tmp)**(dble(size(this)))
  end function
  !====================================================================!
  !====================================================================!
  module procedure Mean_i1D
    !! interface with mean()
  !====================================================================!
  !module function mean_i1D(this) result(res)
  !integer(i32) :: this(:)
  !real(r64) :: res
  res=dble(sum(this))/dble(size(this))
  end procedure
  !====================================================================!
  !====================================================================!
  module function median_i1D(this) result(res)
  !====================================================================!
    !! Interfaced with median()
  integer(i32), intent(in) :: this(:) !! 1D array
  real(r64) :: res !! median
  integer(i32), allocatable :: i(:)
  integer(i32) :: iMed
  integer(i32) :: N
  N=size(this)
  call allocate(i,N)
  call arange(i,1,N)

  if (mod(N,2)==0) then
    res=this(argSelect(this,i,N/2))
    call arange(i,1,N)
    res=0.5d0*(res+this(argSelect(this,i,(N/2)+1)))
  else
    iMed=N/2+1
    res=argSelect(this,i,iMed)
  end if
  deallocate(i)
  end function
  !====================================================================!
  !====================================================================!
  module procedure norm1_i1D
    !! interface with norm1()
  !====================================================================!
  !module function norm1_i1D(this) result(res)
  !integer(i32) :: this(:)
  !integer(i32) :: res
  res=sum(abs(this))
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure normI_i1D
    !! interface with normI()
  !====================================================================!
  !module function normI_i1D(this) result(res)
  !integer(i32) :: this(:)
  !integer(i32) :: res
  res=maxval(abs(this))
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure trimmedmean_i1D
  !====================================================================!
  !function trimmedmean_io1D(this,alpha) result(res)
  !integer(i32) :: this(:)
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
  module procedure std_i1D
    !! Interfaced with std()
  !====================================================================!
  !integer(i32) :: this(:)
  !real(r64) :: res
  res=dsqrt(Variance(this))
  end procedure
  !====================================================================!

  !====================================================================!
  module procedure Variance_i1D
  !====================================================================!
  !integer(i32) :: this(:)
  !real(r64) :: res
  real(r64) :: tmp
  tmp=Mean(this)
  res=sum(dble(this-tmp)**2.d0)/dble(size(this)-1)
  end procedure
  !====================================================================!
end submodule
