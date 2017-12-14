submodule (m_Sort) sm_introsort
  !! introsort and argIntrosort routines

  use variableKind
  use m_swap, only: swap
  use m_partition, only: partition, argPartition
  use m_medianOf3, only: medianOf3, argMedianOf3

  implicit none

  contains
  !====================================================================!
  module procedure introsort_r1D
    !! Interfaced with introsort()
  !====================================================================!
  integer(i32) :: left,right
  integer(i32) :: maxDepth
  left=1
  right=size(this)
  maxDepth = 2*idnint(log(dble(right)))
  call r_introsort_r1D(this,left,right,maxDepth)
  end procedure
  !====================================================================!
  !====================================================================!
  recursive subroutine r_introsort_r1D(this,left,right,maxDepth)
  !====================================================================!
  real(r32) :: this(:)
  integer(i32) :: left,right
  integer(i32) :: maxDepth
  integer(i32) :: imid,iPivot
  integer(i32) :: N
  N=right-left
  if (N < 16) then
    call InsertionSort(this,left,right)
    return
  end if
  if (maxDepth == 0) then
    call heapsort_r1D(this(left:right+1))
    return
  end if
  imid = left + N/2
  call medianOf3(this, left, imid, right)
  call swap(this(left), this(imid))
  call partition(this,left,right,iPivot)
  call r_introsort_r1D(this,left,iPivot-1,maxDepth-1)
  call r_introsort_r1D(this,iPivot+1,right,maxDepth-1)
  end subroutine
  !====================================================================!
  !====================================================================!
  module procedure introsort_d1D
    !! Interfaced with introsort()
  !====================================================================!
  integer(i32) :: left,right,maxDepth
  left=1
  right=size(this)
  maxDepth = 2*idnint(log(dble(right-left)))
  call r_introsort_d1D(this,left,right,maxDepth)
  end procedure
  !====================================================================!
  !====================================================================!
  recursive subroutine r_introsort_d1D(this,left,right,maxDepth)
  !====================================================================!
  real(r64) :: this(:)
  integer(i32) :: left,right
  integer(i32) :: maxDepth
  integer(i32) :: imid,iPivot
  integer(i32) :: N
  N = right - left
  if (N < 16) then
    call InsertionSort(this,left,right)
    return
  end if
  if (maxDepth == 0) then
    call heapsort_d1D(this(left:right+1))
    return
  end if
  imid = left + N/2
  call medianOf3(this, left, imid, right)
  call swap(this(left), this(imid))
  call partition(this,left,right,iPivot)
  call r_introsort_d1D(this,left,iPivot-1, maxDepth-1)
  call r_introsort_d1D(this,iPivot+1,right, maxDepth-1)
  end subroutine
  !====================================================================!
  !====================================================================!
  module procedure introsort_i1D
    !! Interfaced with introsort()
  !====================================================================!
  integer(i32) :: left,right
  integer(i32) :: maxDepth
  left=1
  right=size(this)
  maxDepth=2*idnint(log(dble(right)))
  call r_introsort_i1D(this,left,right,maxDepth)
  end procedure
  !====================================================================!
  !====================================================================!
  recursive subroutine r_introsort_i1D(this,left,right,maxDepth)
  !====================================================================!
    integer(i32) :: this(:)
    integer(i32) :: left,right
    integer(i32) :: maxDepth
    integer(i32) :: imid,iPivot
    integer(i32) :: N
    N=right-left
    if (N < 16) then
      call InsertionSort(this,left,right)
      return
    end if
    if (maxDepth == 0) then
      call heapsort_i1D(this(left:right+1))
      return
    end if
    imid = left + N/2
    call medianOf3(this, left, imid, right)
    call swap(this(left), this(imid))
    call partition(this,left,right,iPivot)
    call r_introsort_i1D(this,left,iPivot-1,maxDepth-1)
    call r_introsort_i1D(this,iPivot+1,right,maxDepth-1)
  end subroutine
  !====================================================================!
  !====================================================================!
  module procedure introsort_id1D
    !! Interfaced with introsort()
  !====================================================================!
  integer(i32) :: left,right
  integer(i32) :: maxDepth
  left=1
  right=size(this)
  maxDepth=2*idnint(log(dble(right)))
  call r_introsort_id1D(this,left,right,maxDepth)
  end procedure
  !====================================================================!
  !====================================================================!
  recursive subroutine r_introsort_id1D(this,left,right,maxDepth)
  !====================================================================!
    integer(i64) :: this(:)
    integer(i32) :: left,right
    integer(i32) :: maxDepth
    integer(i32) :: imid,iPivot
    integer(i32) :: N
    N=right-left
    if (N < 16) then
      call InsertionSort(this,left,right)
      return
    end if
    if (maxDepth == 0) then
      call heapsort_id1D(this(left:right+1))
      return
    end if
    imid = left + N/2
    call medianOf3(this, left, imid, right)
    call swap(this(left), this(imid))
    call partition(this,left,right,iPivot)
    call r_introsort_id1D(this,left,iPivot-1,maxDepth-1)
    call r_introsort_id1D(this,iPivot+1,right,maxDepth-1)
  end subroutine
  !====================================================================!
  !====================================================================!
  module procedure argIntrosort_r1D
    !! Interfaced with argIntrosort()
  !====================================================================!
  integer(i32) :: left,right
  integer(i32) :: maxDepth
  left=1
  right=size(this)
  maxDepth=2*idnint(log(dble(right)))
  call r_argIntrosort_r1D(this,i,left,right,maxDepth)
  end procedure
  !====================================================================!
  !====================================================================!
  recursive subroutine r_argIntrosort_r1D(this,idx,left,right,maxDepth)
  !====================================================================!
  real(r32) :: this(:)
  integer(i32) :: idx(:)
  integer(i32) :: left,right
  integer(i32) :: maxDepth
  integer(i32) :: imid,iPivot
  integer(i32) :: N
  N=right-left
  if (N < 16) then
    call argInsertionSort(this,idx,left,right)
    return
  end if
  if (maxDepth == 0) then
    call argHeapsort_r1D(this, idx(left:right+1))
    return
  end if
  imid = left + N/2
  call argMedianOf3(this, idx, left, imid, right)
  call swap(idx(left), idx(imid))
  call argPartition(this,idx,left,right,iPivot)
  call r_argIntrosort_r1D(this,idx,left,iPivot-1,maxDepth-1)
  call r_argIntrosort_r1D(this,idx,iPivot+1,right,maxDepth-1)
  end subroutine
  !====================================================================!
  !====================================================================!
  module procedure argIntrosort_d1D
    !! Interfaced with argIntrosort()
  !====================================================================!
  integer(i32) :: left,right
  integer(i32) :: maxDepth
  left=1
  right=size(this)
  maxDepth=2*idnint(log(dble(right)))
  call r_argIntrosort_d1D(this,i,left,right,maxDepth)
  end procedure
  !====================================================================!
  !====================================================================!
  recursive subroutine r_argIntrosort_d1D(this,idx,left,right,maxDepth)
  !====================================================================!
  real(r64) :: this(:)
  integer(i32) :: idx(:)
  integer(i32) :: left,right
  integer(i32) :: maxDepth
  integer(i32) :: imid,iPivot
  integer(i32) :: N
  N=right-left
  if (N < 16) then
    call argInsertionSort(this,idx,left,right)
    return
  end if
  if (maxDepth == 0) then
    call argHeapsort_d1D(this, idx(left:right+1))
    return
  end if
  imid = left + N/2
  call argMedianOf3(this, idx, left, imid, right)
  call swap(idx(left), idx(imid))
  call argPartition(this,idx,left,right,iPivot)
  call r_argIntrosort_d1D(this,idx,left,iPivot-1,maxDepth-1)
  call r_argIntrosort_d1D(this,idx,iPivot+1,right,maxDepth-1)
  end subroutine
  !====================================================================!
  !====================================================================!
  module procedure argIntrosort_i1D
    !! Interfaced with argIntrosort()
  !====================================================================!
  integer(i32) :: left,right
  integer(i32) :: maxDepth
  left=1
  right=size(this)
  maxDepth=2*idnint(log(dble(right)))
  call r_argIntrosort_i1D(this,i,left,right,maxDepth)
  end procedure
  !====================================================================!
  !====================================================================!
  recursive subroutine r_argIntrosort_i1D(this,idx,left,right,maxDepth)
  !====================================================================!
  integer(i32) :: this(:)
  integer(i32) :: idx(:)
  integer(i32) :: left,right
  integer(i32) :: maxDepth
  integer(i32) :: imid,iPivot
  integer(i32) :: N
  N=right-left
  if (N < 16) then
    call argInsertionSort(this,idx,left,right)
    return
  end if
  if (maxDepth == 0) then
    call argHeapsort_i1D(this, idx(left:right+1))
    return
  end if
  imid = left + N/2
  call argMedianOf3(this, idx, left, imid, right)
  call swap(idx(left), idx(imid))
  call argPartition(this,idx,left,right,iPivot)
  call r_argIntrosort_i1D(this,idx,left,iPivot-1,maxDepth-1)
  call r_argIntrosort_i1D(this,idx,iPivot+1,right,maxDepth-1)
  end subroutine
  !====================================================================!
  !====================================================================!
  module procedure argIntrosort_id1D
    !! Interfaced with argIntrosort()
  !====================================================================!
  integer(i32) :: left,right
  integer(i32) :: maxDepth
  left=1
  right=size(this)
  maxDepth=2*idnint(log(dble(right)))
  call r_argIntrosort_id1D(this,i,left,right,maxDepth)
  end procedure
  !====================================================================!
  !====================================================================!
  recursive subroutine r_argIntrosort_id1D(this,idx,left,right,maxDepth)
  !====================================================================!
  integer(i64) :: this(:)
  integer(i32) :: idx(:)
  integer(i32) :: left,right
  integer(i32) :: maxDepth
  integer(i32) :: imid,iPivot
  integer(i32) :: N
  N=right-left
  if (N < 16) then
    call argInsertionSort(this,idx,left,right)
    return
  end if
  if (maxDepth == 0) then
    call argHeapsort_id1D(this, idx(left:right+1))
    return
  end if
  imid = left + N/2
  call argMedianOf3(this, idx, left, imid, right)
  call swap(idx(left), idx(imid))
  call argPartition(this,idx,left,right,iPivot)
  call r_argIntrosort_id1D(this,idx,left,iPivot-1,maxDepth-1)
  call r_argIntrosort_id1D(this,idx,iPivot+1,right,maxDepth-1)
  end subroutine
  !====================================================================!
end submodule
