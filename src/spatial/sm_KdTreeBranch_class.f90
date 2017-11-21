submodule (m_KdTree) sm_KdTreeBranch_class

implicit none

contains

  !====================================================================!
  module procedure init_branch
    !! Overloaded Type bound procedure KdTreeBranch%init()
  !====================================================================!
  !class(KdTreebranch) :: this
  !integer(i32), intent(in) :: left
  !integer(i32), intent(in) :: right
  this%left = left
  this%right = right
  this%median = 0.d0
  this%splitAlong = 1
  this%buds => null()
  end procedure
  !====================================================================!
  !====================================================================!
  module procedure deallocate_branch
  !====================================================================!
  !class(KdTreebranch) :: this
  if (associated(this%buds)) then
    call this%buds(1)%deallocate()
    call this%buds(2)%deallocate()
    deallocate(this%buds)
  endif
  end procedure
  !====================================================================!

end submodule
