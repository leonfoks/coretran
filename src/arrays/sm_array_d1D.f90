  submodule (m_array1D) m_Array_d1D
    !! Routines for double precision arrays
  use variableKind
  use m_errors, only: mErr, eMsg
  use m_sort, only: argSort
  use m_strings, only: str
  implicit none

  contains
  !====================================================================!
  module procedure arange_d1D
    !! Interfaced with [[arange]]
  !====================================================================!
  !module function arange_d1D(start,stp,_step) result(this)
  !real(r64) :: start !! Start from here
  !real(r64) :: stp !! Stop here
  !real(r64) :: step !! Step size
  !real(r64), allocatable :: res(:)
  integer(i32) :: i
  integer(i32) :: N
  real(r64) :: step_
  step_=1.d0
  if (present(step)) step_ = step
  N=int((stp-start)/step_)+1
  if (size(res) /= N) call eMsg('arange_d1D:1D Array must be size '//str(N))
  if (step_ == 1.d0) then
      do i = 1, N
          res(i) = start + real(i-1, kind=r64)
      enddo
  else
      do i = 1, N
          res(i) = start + real(i-1, kind=r64)*step_
      enddo
  endif
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure diff_d1D
    !! Interfaced [[diff]]
  !====================================================================!
!  real(r64), intent(in) :: this(:) !! 1D array
!  real(r64) :: res(size(this)-1) !! Difference along array
  integer(i32) :: i
  integer(i32) :: N
  N=size(this)
  if (size(res) /= N-1) call eMsg('diff_d1D:Result must be size '//str(N-1))
  do i=1,N-1
    res(i) = this(i+1) - this(i)
  end do
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure isSorted_d1D
    !! Interfaced with [[isSorted]]
  !====================================================================!
  !module function isSorted_d1D(this) result(yes)
  !real(r64):: this(:) !! 1D array
  !logical :: yes !! isSorted
  integer :: i,N
  N=size(this)
  yes=.true.
  do i=2,N
    if (this(i) < this(i-1)) then
      yes=.false.
      return
    end if
  end do
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure isSorted_d1Di1D
    !! Interfaced with [[isSorted]]
  !====================================================================!
  !module function isSorted_d1D(this) result(yes)
  !real(r64):: this(:) !! 1D array
  !integer(i32) :: indx(:)
  !logical :: yes !! isSorted
  integer :: i,N
  N=size(this)
  yes=.true.
  do i=2,N
    if (this(indx(i)) < this(indx(i-1))) then
      yes=.false.
      return
    end if
  end do
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure repeat_d1D
    !! Interfaced with [[repeat]]
  !====================================================================!
!  real(r64) :: this(:) !! 1D array
!  integer(i32) :: nRepeats !! Number of times each element should be repeated
!  real(r64) :: res(size(this)*nRepeats)
  integer(i32) :: i,k,N,nTmp
  N = size(this)
  nTmp = N*nRepeats
  call allocate(res, nTmp)
  !if (size(res) /= nTmp) call eMsg('repeat_d1D:Result must be size '//str(nTmp))
  k=1
  do i = 1, N
    res(k:k+nRepeats-1) = this(i) ! Repeat the element
    k = k + nRepeats
  end do
  end procedure
  !====================================================================!
!  !====================================================================!
!  function calcInternalAngle_Vector(this,that) result(theta)
!  !====================================================================!
!  real(r64) :: this(:),that(:)
!  real(r64) :: theta
!  theta=dacos(dot_product(this,that)/(magnitude_Vector(this)*magnitude_Vector(that)))
!  end function
!  !====================================================================!
!  !====================================================================!
!  function outerproduct(a,b) result(c)
!  !====================================================================!
!  real(r64) :: a(:),b(:)
!  integer :: nB
!  real(r64) :: c(size(a),size(b))
!  integer :: i
!  nB=size(b)
!  do i=1,nB
!    c(:,i)=a*b(i)
!  enddo
!  end function
!  !====================================================================!
!  !====================================================================!
!  function normalize_Vector(this) result(that)
!  !====================================================================!
!  real(r64) :: this(:)
!  real(r64) :: that(size(this))
!  real(r64) :: magnitude
!
!  that=this
!  magnitude=magnitude_Vector(this)
!  if (magnitude /= 0.d0) that=that/magnitude
!
!  end function
!  !====================================================================!
!  !====================================================================!
!  function ContraharmonicMean(this,order) result(that)
!  !====================================================================!
!  ! Input
!  real(r64) :: this(:)
!  real(r64) :: order
!  ! Output
!  real(r64) :: that
!  ! Specific
!  real(r64) :: tmp
!  !====================================================================!
!  ! Compute the Contraharmonic mean of a vector
!  ! Digital Image Processing|Gonzalez and Woods|2011|3rd Edition
!  !====================================================================!
!  tmp=sum(this)
!  that=tmp**(order+1.d0)
!  that=that/(tmp**order)
!  return
!  end function
!  !====================================================================!
!  !====================================================================!
!  function HarmonicMean(this) result(that)
!  !====================================================================!
!  real(r64) :: this(:)
!  real(r64) :: that
!  real(r64) :: N
!  !====================================================================!
!  ! Compute the Harmonic mean of a vector
!  ! Digital Image Processing|Gonzalez and Woods|2011|3rd Edition
!  !====================================================================!
!  N=dble(size(this))
!  that=N/(sum(1.d0/this))
!  return
!  end function
!  !====================================================================!

!  !====================================================================!
!  function isConstant_DV(this) result(yes)
!  !====================================================================!
!  ! Checks whether a vector contains the same value throughout
!  real(r64) :: this(:)
!  logical :: yes
!  real(r64) :: tmp
!  yes=.true.
!  tmp=this(1)
!  if (any(this/=tmp)) yes=.false.
!  end function
!  !====================================================================!
!  !====================================================================!
!  function isConstantIncrement_DV(this,val) result(yes)
!  !====================================================================!
!  ! Checks whether a vector contains the same value throughout
!  real(r64) :: this(:)
!  real(r64) :: val
!  logical :: yes
!  integer :: i,N
!  N=size(this)
!  yes=.true.
!  do i=2,N
!    if ((this(i)-this(i-1))/=val) then
!      yes=.false.
!      return
!    endif
!  enddo
!  end function
!  !====================================================================!
!  !====================================================================!
!  function isInside1D_I1(this,x0,sortme) result(yes)
!  !====================================================================!
!  integer :: this(:)
!  integer :: x0
!  integer :: n
!  logical :: sortme
!  logical :: yes
!  !====================================================================!
!  ! Determine if x0 is within this
!  !====================================================================!
!  n=size(this)
!  if (sortme) call sort(this)
!
!  yes=.true.
!  if (x0 < this(1)) yes=.false.
!  if (x0 > this(n)) yes=.false.
!
!  end function
!  !====================================================================!
!  !====================================================================!
!  function isFactor2Vector(this) result(yes)
!  !====================================================================!
!  real(r64) :: this(:)
!  logical :: yes
!  integer :: ii,N
!  real(r64) :: tmp1,tmp2
!  N=size(this)
!  yes=.true.
!  tmp1=dabs(this(2)-this(1))
!  do ii=3,N
!    tmp2=dabs(this(ii)-this(ii-1))
!    if (tmp2==tmp1) cycle
!    if (tmp2 /= 2.d0*tmp1 .and. tmp2/=0.5d0*tmp1) then
!      yes=.false.
!      return
!    endif
!    tmp1=tmp2
!  enddo
!  end function
!  !====================================================================!
!  !====================================================================!
!  function getBin1D_I1(this,x0,sortme) result(i)
!  !====================================================================!
!  integer :: this(:)
!  integer :: x0
!  integer :: x1,x2,x3
!  logical :: sortme
!  integer :: i
!  integer :: i1,i2,i3,n
!  !====================================================================!
!  ! Determine the bin in this containing x0 using bisection
!  ! Forces the return index to lie within the vector "this"
!  !====================================================================!
!  n=size(this)
!  if (sortme) call sort(this)
!
!  if (x0.le.this(2)  ) then
!    i=1
!    return
!  endif
!  if (x0.gt.this(n-1)) then
!    i=n-1
!    return
!  endif
!
!  i1=2;
!  x1=this(i1)  ! Left value
!  i3=n-2;
!  x3=this(i3)  ! Right value
!  i2=(i3+i1)/2;
!  x2=this(i2)  ! Central value
!
!  do while ( (i3-i1) /= 1)
!    if     (x0 <= x2) then;
!      i3=i2;
!      x3=x2
!    elseif (x0 >  x2) then;
!      i1=i2;
!      x1=x2;
!    endif
!    i2=(i3+i1)/2;
!    x2=this(i2)  ! Central value
!  enddo
!  i=i1
!  end function
!  !====================================================================!
!  !====================================================================!
!  function isInside1D_D1(this,x0,sortme) result(yes)
!  !====================================================================!
!  real(r64) :: this(:)
!  real(r64) :: x0
!  integer :: n
!  logical :: sortme
!  logical :: yes
!  !====================================================================!
!  ! Determine if x0 is within this
!  !====================================================================!
!  n=size(this)
!  if (sortme) call sort(this)
!
!  yes=.true.
!  if (x0 < this(1)) yes=.false.
!  if (x0 > this(n)) yes=.false.
!
!  end function
!  !====================================================================!
!  !====================================================================!
!  function getBin1D_D1(this,x0,sortme) result(i)
!  !====================================================================!
!  real(r64) :: this(:)
!  real(r64) :: x0
!  real(r64) :: x1,x2,x3
!  logical :: sortme
!  integer :: i
!  integer :: i1,i2,i3,n
!  !====================================================================!
!  ! Determine the bin in this containing x0 using bisection
!  ! Forces the return index to lie within the vector "this"
!  ! Do NOT use an unsorted vector!
!  !====================================================================!
!  n=size(this)
!  if (sortme) call sort(this)
!
!  if (x0 <= this(2)) then
!    i=1
!    return
!  endif
!  if (x0 > this(n-1)) then
!    i=n-1;
!    return;
!  endif
!
!  i1=2;
!  x1=this(i1)  ! Left value
!  i3=n-2;
!  x3=this(i3)  ! Right value
!  i2=(i3+i1)/2;
!  x2=this(i2)  ! Central value
!  ! Bisection Method to obtain the bin
!  do while ( (i3-i1) /= 1)
!    if     (x0 <= x2) then;
!      i3=i2;
!      x3=x2
!    elseif (x0 >  x2) then;
!      i1=i2;
!      x1=x2;
!    endif
!    i2=(i3+i1)/2;
!    x2=this(i2)  ! Central value
!  enddo
!  i=i1
!  end function
!  !====================================================================!
!  !====================================================================!
!  function getSmallestIncrement(this) result(inc)
!  !====================================================================!
!  real(r64) :: this(:)
!  real(r64) :: inc
!  integer :: ii,N
!  real(r64) :: tmp1
!  N=size(this)
!  tmp1=dabs(this(2)-this(1))
!  inc=tmp1
!  do ii=2,N
!    tmp1=dabs(this(ii+1)-this(ii))
!    if (tmp1 < inc) inc=tmp1
!  enddo
!  end function
!  !====================================================================!
!  !====================================================================!
!  subroutine unitize_1D(this)
!  !====================================================================!
!  real(r64) :: this(:)
!  ! Normalize the input vector
!  this=this-minval(this)
!  this=this/maxval(this)
!  end subroutine
!  !====================================================================!
!  !====================================================================!
!  subroutine scaleVector(this,xmin,xmax)
!  !====================================================================!
!  ! Scales a vector to [xmin xmax]
!  real(r64) :: this(:),xmin,xmax
!  ! Normalize the input vector
!  call unitize(this)
!  ! Map to new limits
!  this=(this*(xmax-xmin))+xmin
!  end subroutine
!  !====================================================================!
!  !====================================================================!
!  subroutine integerizeVector(this,x0,inc)
!  !====================================================================!
!  ! Takes a vector and maps it to integer increments for easy nearest neighbour
!  ! operations for grids, e.g. if x0=10, inc=5 then
!  ! (-50,-12.5,5,24.3) = (-11,-3.5,0,3.86) Only the 4th point is inside a grid.
!  real(r64) :: this(:),inc,x0
!  this=((this-x0)/inc)+1.d0
!  end subroutine
!  !====================================================================!
!  !====================================================================!
!  subroutine deintegerizeVector(this,x0,inc)
!  !====================================================================!
!  ! Takes an integerized vector and maps it to non-integerized format.
!  real(r64) :: this(:),inc,x0
!  this=((this-1.d0)*inc)+x0
!  end subroutine
!  !====================================================================!
!  !====================================================================!
!  subroutine removeItem(this,i)
!  !====================================================================!
!  real(r64) :: this(:)
!  integer :: i,j
!  integer :: N
!  N=size(this)
!  if (i < 1 .or. i > N) call Emsg('removeItem','index is outside the array')
!  do j=i,N-1
!    this(j)=this(j+1)
!  enddo
!  this(N)=tiny(0.d0)
!  end subroutine
!  !====================================================================!
!  !====================================================================!
!  subroutine insertItem(this,i,val)
!  !====================================================================!
!  real(r64) :: this(:)
!  integer :: i,j
!  real(r64) :: val
!  integer :: N
!  N=size(this)
!  if (i < 1 .or. i > N) call Emsg('insertItem','index is outside the array')
!  do j=N,i+1,-1
!    this(j)=this(j-1)
!  enddo
!  this(i)=val
!  end subroutine
!  !====================================================================!
!  !====================================================================!
!  subroutine writeToFile_D1D(this,fname)
!  !====================================================================!
!  real(r64) :: this(:)
!  character(len=*) :: fname
!  integer :: i,istat,iunit,N
!  N=size(this)
!  call openFile(fname,iunit,'unknown',istat)
!  do i=1,N
!    write(iunit,'(a)') str(this(i),7)
!  enddo
!  call closeFile(fname,iunit,'unknown',istat)
!  end subroutine
!  !====================================================================!
end submodule
