program test_coretran
  !! Test program for the coretran library
  use m_unitTester
  use iso_fortran_env
  use variableKind
  use m_random
  use m_tests

  implicit none

  type(tester) :: test

  character(len=cLen) :: sa
  integer(i32) :: i, iTest,istat, N, nIterations
  

! Get an integer from command line argument
  i = command_argument_count()
  if (i < 2) then
    write(*,'(a)') 'Error with input options'
    write(*,'(a)') 'Usage: coretranTest size iterations'
    write(*,'(a)') '  size : Size of the array to run tests on'
    write(*,'(a)') '  iterations : Number of times to run a test that is being timed'
    stop
  end if
  call get_command_argument(1, sa)
  read(sa,*) N

  if (N < 15) call eMsg('Please use size >= 15')

  call get_command_argument(2, sa)
  read(sa,*) nIterations

  test = tester()

  call Msg('==========================')
  call Msg('LibFortran Testing')
  call Msg('==========================')

  call setPrng(big = .true., display = .true.)

  call strings_test(test)
  
  call fileIO_test(test)
  
  call random_test(test, .false.)

  call Prng_test(test)

  call time_test(test, nIterations)
  
  call indexing_test(test)
  
  call allocate_test(test)
  
  call reallocate_test(test)
  
  call copy_test(test)
  
  call sorting_test(test, N)
  
  call select_test(test, N)
  
  call array1D_test(test)
  
  call maths_test(test)

  call KdTree_test(test, N)
  

  call Msg('==========================')
  call Msg('Testing : Dynamic Arrays')
  call Msg('==========================')
  call rDynamicArray_test(test)
  call dDynamicArray_test(test)
  call iDynamicArray_test(test)
  call idDynamicArray_test(test)
  
  call Msg('==========================')
  call Msg('Testing : ArgDynamic Arrays')
  call Msg('==========================')
  call rArgDynamicArray_test(test)
  call dArgDynamicArray_test(test)
  call iArgDynamicArray_test(test)
  call idArgDynamicArray_test(test)

  call test%summary()

  stop
1 format(a)

end program



