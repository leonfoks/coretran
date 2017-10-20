submodule (m_Sort) sm_MergeSort
  !! Merge sort and arg Merge sort routines
use variableKind
use m_errors, only: mErr

contains
!====================================================================!
module procedure mergeSort_d1D
  !! Interfaced with mergeSort()
!====================================================================!
!module subroutine mergeSort_d1D
!real(r64) :: this(:)
real(r64),allocatable :: aux(:)
integer(i32) :: iLeft,iRight
iLeft=1;iRight=size(this)
allocate(aux(iRight),stat=istat);call mErr(istat,'MergeSort:Auxiliary Array',1)
aux=0.d0
call SplitSort_d1D(this,aux,iLeft,iRight)
deallocate(aux)
end procedure
!====================================================================!
!====================================================================!
recursive subroutine SplitSort_d1D(this,aux,iLeft,iRight)
!====================================================================!
real(r64) :: this(:)
real(r64) :: aux(:)
integer(i32) :: iLeft,iRight
integer(i32) :: iMid
if (iRight <= iLeft) return
if (iRight <= iLeft + 19) then
  call InsertionSort(this,iLeft,iRight)
  return
end if
iMid=iLeft+(iRight-iLeft)/2
call SplitSort_d1D(this,aux,iLeft,iMid)
call SplitSort_d1D(this,aux,iMid+1,iRight)
call MergeSorted_d1D(this,aux,iLeft,iMid,iRight)
end subroutine
!====================================================================!
!====================================================================!
subroutine MergeSorted_d1D(this,aux,iLeft,iMid,iRight)
!====================================================================!
real(r64) :: this(:)
real(r64) :: aux(:)
integer(i32) :: iLeft,iMid,iRight
integer(i32) :: i,j,k

aux(iLeft:iRight)=this(iLeft:iRight)
i=iLeft;j=iMid+1
do k=iLeft,iRight
  if (i > iMid) then
    this(k)=aux(j);j=j+1
  elseif (j > iRight) then
    this(k)=aux(i);i=i+1
  elseif (aux(j) < aux(i)) then
    this(k)=aux(j);j=j+1
  else
    this(k)=aux(i);i=i+1
  end if
enddo
end subroutine
!====================================================================!
!====================================================================!
module procedure mergeSort_r1D
  !! Interfaced with mergeSort()
!====================================================================!
!module subroutine mergeSort_r1D
!real(r32) :: this(:)
real(r32),allocatable :: aux(:)
integer(i32) :: iLeft,iRight
iLeft=1;iRight=size(this)
allocate(aux(iRight),stat=istat);call Merr(istat,'MergeSort:Auxiliary Array',1)
aux=0.d0
call SplitSort_r1D(this,aux,iLeft,iRight)
deallocate(aux)
end procedure
!====================================================================!
!====================================================================!
recursive subroutine SplitSort_r1D(this,aux,iLeft,iRight)
!====================================================================!
real(r32) :: this(:)
real(r32) :: aux(:)
integer(i32) :: iLeft,iRight
integer(i32) :: iMid
if (iRight <= iLeft) return
if (iRight <= iLeft + 19) then
  call InsertionSort(this,iLeft,iRight)
  return
end if
iMid=iLeft+(iRight-iLeft)/2
call SplitSort_r1D(this,aux,iLeft,iMid)
call SplitSort_r1D(this,aux,iMid+1,iRight)
call MergeSorted_r1D(this,aux,iLeft,iMid,iRight)
end subroutine
!====================================================================!
!====================================================================!
subroutine MergeSorted_r1D(this,aux,iLeft,iMid,iRight)
!====================================================================!
real(r32) :: this(:)
real(r32) :: aux(:)
integer(i32) :: iLeft,iMid,iRight
integer(i32) :: i,j,k

aux(iLeft:iRight)=this(iLeft:iRight)
i=iLeft;j=iMid+1
do k=iLeft,iRight
  if (i > iMid) then
    this(k)=aux(j);j=j+1
  elseif (j > iRight) then
    this(k)=aux(i);i=i+1
  elseif (aux(j) < aux(i)) then
    this(k)=aux(j);j=j+1
  else
    this(k)=aux(i);i=i+1
  end if
enddo
end subroutine
!====================================================================!
!====================================================================!
module procedure MergeSort_i1D
  !! Interfaced with mergeSort()
!====================================================================!
!module subroutine mergeSort_i1D
!integer(i32) :: this(:)
integer(i32),allocatable :: aux(:)
integer(i32) :: iLeft,iRight
iLeft=1;iRight=size(this)
allocate(aux(iRight),stat=istat);call Merr(istat,'MergeSort:Auxiliary Array',1)
aux=0
call SplitSort_i1D(this,aux,iLeft,iRight)
deallocate(aux)
end procedure
!====================================================================!
!====================================================================!
recursive subroutine SplitSort_i1D(this,aux,iLeft,iRight)
!====================================================================!
integer(i32) :: this(:)
integer(i32) :: aux(:)
integer(i32) :: iLeft,iRight
integer(i32) :: iMid
if (iRight <= iLeft) return
if (iRight <= iLeft + 32) then
  call InsertionSort(this,iLeft,iRight)
  return
end if
iMid=iLeft+(iRight-iLeft)/2
call SplitSort_i1D(this,aux,iLeft,iMid)
call SplitSort_i1D(this,aux,iMid+1,iRight)
call MergeSorted_i1D(this,aux,iLeft,iMid,iRight)
end subroutine
!====================================================================!
!====================================================================!
subroutine MergeSorted_i1D(this,aux,iLeft,iMid,iRight)
!====================================================================!
integer(i32) :: this(:)
integer(i32) :: aux(:)
integer(i32) :: iLeft,iMid,iRight
integer(i32) :: i,j,k

do k=iLeft,iRight
  aux(k)=this(k)
end do
i=iLeft;j=iMid+1
do k=iLeft,iRight
  if (i > iMid) then
    this(k)=aux(j);j=j+1
  elseif (j > iRight) then
    this(k)=aux(i);i=i+1
  elseif (aux(j) < aux(i)) then
    this(k)=aux(j);j=j+1
  else
    this(k)=aux(i);i=i+1
  end if
enddo
end subroutine
!====================================================================!
!====================================================================!
module procedure MergeSort_id1D
  !! Interfaced with mergeSort()
!====================================================================!
!module subroutine mergeSort_id1D
!integer(i64) :: this(:)
integer(i64),allocatable :: aux(:)
integer(i32) :: iLeft,iRight
iLeft=1;iRight=size(this)
allocate(aux(iRight),stat=istat);call Merr(istat,'MergeSort:Auxiliary Array',1)
aux=0
call SplitSort_id1D(this,aux,iLeft,iRight)
deallocate(aux)
end procedure
!====================================================================!
!====================================================================!
recursive subroutine SplitSort_id1D(this,aux,iLeft,iRight)
!====================================================================!
integer(i64) :: this(:)
integer(i64) :: aux(:)
integer(i32) :: iLeft,iRight
integer(i32) :: iMid
if (iRight <= iLeft) return
if (iRight <= iLeft + 32) then
  call InsertionSort(this,iLeft,iRight)
  return
end if
iMid=iLeft+(iRight-iLeft)/2
call SplitSort_id1D(this,aux,iLeft,iMid)
call SplitSort_id1D(this,aux,iMid+1,iRight)
call MergeSorted_id1D(this,aux,iLeft,iMid,iRight)
end subroutine
!====================================================================!
!====================================================================!
subroutine MergeSorted_id1D(this,aux,iLeft,iMid,iRight)
!====================================================================!
integer(i64) :: this(:)
integer(i64) :: aux(:)
integer(i32) :: iLeft,iMid,iRight
integer(i32) :: i,j,k

do k=iLeft,iRight
  aux(k)=this(k)
end do
i=iLeft;j=iMid+1
do k=iLeft,iRight
  if (i > iMid) then
    this(k)=aux(j);j=j+1
  elseif (j > iRight) then
    this(k)=aux(i);i=i+1
  elseif (aux(j) < aux(i)) then
    this(k)=aux(j);j=j+1
  else
    this(k)=aux(i);i=i+1
  end if
enddo
end subroutine
!====================================================================!
!====================================================================!
module procedure argMergeSort_d1D
  !! Interfaced with argMergeSort()
!====================================================================!
!module subroutine argMergeSort_d1D
!real(r64) :: this(:)
!integer(i32) :: i(:)
integer(i32),allocatable :: aux(:)
integer(i32) :: iLeft,iRight
iLeft=1;iRight=size(this)
allocate(aux(iRight),stat=istat);call Merr(istat,'MergeSort:Auxiliary Array',1)
aux=0
call argSplitSort_d1D(this,i,aux,iLeft,iRight)
deallocate(aux)
end procedure
!====================================================================!
!====================================================================!
recursive subroutine argSplitSort_d1D(this,indx,aux,iLeft,iRight)
!====================================================================!
real(r64) :: this(:)
integer(i32) :: indx(:)
integer(i32) :: aux(:)
integer(i32) :: iLeft,iRight
integer(i32) :: iMid
if (iRight <= iLeft) return
if (iRight <= iLeft + 32) then
  call argInsertionSort(this,indx,iLeft,iRight)
  return
end if
iMid=iLeft+(iRight-iLeft)/2
call argSplitSort_d1D(this,indx,aux,iLeft,iMid)
call argSplitSort_d1D(this,indx,aux,iMid+1,iRight)
call argMergeSorted_d1D(this,indx,aux,iLeft,iMid,iRight)
end subroutine
!====================================================================!
!====================================================================!
subroutine argMergeSorted_d1D(this,indx,aux,iLeft,iMid,iRight)
!====================================================================!
real(r64) :: this(:)
integer(i32) :: indx(:)
integer(i32) :: aux(:)
integer(i32) :: iLeft,iMid,iRight
integer(i32) :: i,j,k

aux(iLeft:iRight)=indx(iLeft:iRight)
i=iLeft;j=iMid+1
do k=iLeft,iRight
  if (i > iMid) then
    indx(k)=aux(j);j=j+1
  elseif (j > iRight) then
    indx(k)=aux(i);i=i+1
  elseif (this(aux(j)) < this(aux(i))) then
    indx(k)=aux(j);j=j+1
  else
    indx(k)=aux(i);i=i+1
  end if
enddo
end subroutine
!====================================================================!
!====================================================================!
module procedure argMergeSort_r1D
  !! Interfaced with argMergeSort()
!====================================================================!
!module subroutine argMergeSort_r1D
!real(r32) :: this(:)
!integer(i32) :: i(:)
integer(i32),allocatable :: aux(:)
integer(i32) :: iLeft,iRight
iLeft=1;iRight=size(this)
allocate(aux(iRight),stat=istat);call Merr(istat,'MergeSort:Auxiliary Array',1)
aux=0
call argSplitSort_r1D(this,i,aux,iLeft,iRight)
deallocate(aux)
end procedure
!====================================================================!
!====================================================================!
recursive subroutine argSplitSort_r1D(this,indx,aux,iLeft,iRight)
!====================================================================!
real(r32) :: this(:)
integer(i32) :: indx(:)
integer(i32) :: aux(:)
integer(i32) :: iLeft,iRight
integer(i32) :: iMid
if (iRight <= iLeft) return
if (iRight <= iLeft + 32) then
  call argInsertionSort(this,indx,iLeft,iRight)
  return
end if
iMid=iLeft+(iRight-iLeft)/2
call argSplitSort_r1D(this,indx,aux,iLeft,iMid)
call argSplitSort_r1D(this,indx,aux,iMid+1,iRight)
call argMergeSorted_r1D(this,indx,aux,iLeft,iMid,iRight)
end subroutine
!====================================================================!
!====================================================================!
subroutine argMergeSorted_r1D(this,indx,aux,iLeft,iMid,iRight)
!====================================================================!
real(r32) :: this(:)
integer(i32) :: indx(:)
integer(i32) :: aux(:)
integer(i32) :: iLeft,iMid,iRight
integer(i32) :: i,j,k

aux(iLeft:iRight)=indx(iLeft:iRight)
i=iLeft;j=iMid+1
do k=iLeft,iRight
  if (i > iMid) then
    indx(k)=aux(j);j=j+1
  elseif (j > iRight) then
    indx(k)=aux(i);i=i+1
  elseif (this(aux(j)) < this(aux(i))) then
    indx(k)=aux(j);j=j+1
  else
    indx(k)=aux(i);i=i+1
  end if
enddo
end subroutine
!====================================================================!
!====================================================================!
module procedure argMergeSort_i1D
  !! Interfaced with argMergeSort()
!====================================================================!
!module subroutine argMergeSort_i1D
!integer(i32) :: this(:)
!integer(i32) :: i(:)
integer(i32),allocatable :: aux(:)
integer(i32) :: iLeft,iRight
iLeft=1;iRight=size(this)
allocate(aux(iRight),stat=istat);call Merr(istat,'MergeSort:Auxiliary Array',1)
aux=0
call argSplitSort_i1D(this,i,aux,iLeft,iRight)
deallocate(aux)
end procedure
!====================================================================!
!====================================================================!
recursive subroutine argSplitSort_i1D(this,indx,aux,iLeft,iRight)
!====================================================================!
integer(i32) :: this(:)
integer(i32) :: indx(:)
integer(i32) :: aux(:)
integer(i32) :: iLeft,iRight
integer(i32) :: iMid
if (iRight <= iLeft) return
if (iRight <= iLeft + 19) then
  call argInsertionSort(this,indx,iLeft,iRight)
  return
end if
iMid=iLeft+(iRight-iLeft)/2
call argSplitSort_i1D(this,indx,aux,iLeft,iMid)
call argSplitSort_i1D(this,indx,aux,iMid+1,iRight)
call argMergeSorted_i1D(this,indx,aux,iLeft,iMid,iRight)
end subroutine
!====================================================================!
!====================================================================!
subroutine argMergeSorted_i1D(this,indx,aux,iLeft,iMid,iRight)
!====================================================================!
integer(i32) :: this(:)
integer(i32) :: indx(:)
integer(i32) :: aux(:)
integer(i32) :: iLeft,iMid,iRight
integer(i32) :: i,j,k

aux(iLeft:iRight)=indx(iLeft:iRight)
i=iLeft;j=iMid+1
do k=iLeft,iRight
  if (i > iMid) then
    indx(k)=aux(j);j=j+1
  elseif (j > iRight) then
    indx(k)=aux(i);i=i+1
  elseif (this(aux(j)) < this(aux(i))) then
    indx(k)=aux(j);j=j+1
  else
    indx(k)=aux(i);i=i+1
  end if
enddo
end subroutine
!====================================================================!
!====================================================================!
module procedure argMergeSort_id1D
  !! Interfaced with argMergeSort()
!====================================================================!
!module subroutine argMergeSort_i1D
!integer(i64) :: this(:)
!integer(i32) :: i(:)
integer(i32),allocatable :: aux(:)
integer(i32) :: iLeft,iRight
iLeft=1;iRight=size(this)
allocate(aux(iRight),stat=istat);call Merr(istat,'MergeSort:Auxiliary Array',1)
aux=0
call argSplitSort_id1D(this,i,aux,iLeft,iRight)
deallocate(aux)
end procedure
!====================================================================!
!====================================================================!
recursive subroutine argSplitSort_id1D(this,indx,aux,iLeft,iRight)
!====================================================================!
integer(i64) :: this(:)
integer(i32) :: indx(:)
integer(i32) :: aux(:)
integer(i32) :: iLeft,iRight
integer(i32) :: iMid
if (iRight <= iLeft) return
if (iRight <= iLeft + 19) then
  call argInsertionSort(this,indx,iLeft,iRight)
  return
end if
iMid=iLeft+(iRight-iLeft)/2
call argSplitSort_id1D(this,indx,aux,iLeft,iMid)
call argSplitSort_id1D(this,indx,aux,iMid+1,iRight)
call argMergeSorted_id1D(this,indx,aux,iLeft,iMid,iRight)
end subroutine
!====================================================================!
!====================================================================!
subroutine argMergeSorted_id1D(this,indx,aux,iLeft,iMid,iRight)
!====================================================================!
integer(i64) :: this(:)
integer(i32) :: indx(:)
integer(i32) :: aux(:)
integer(i32) :: iLeft,iMid,iRight
integer(i32) :: i,j,k

aux(iLeft:iRight)=indx(iLeft:iRight)
i=iLeft;j=iMid+1
do k=iLeft,iRight
  if (i > iMid) then
    indx(k)=aux(j);j=j+1
  elseif (j > iRight) then
    indx(k)=aux(i);i=i+1
  elseif (this(aux(j)) < this(aux(i))) then
    indx(k)=aux(j);j=j+1
  else
    indx(k)=aux(i);i=i+1
  end if
enddo
end subroutine
!====================================================================!
end submodule
