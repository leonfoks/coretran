module m_rngLib
!! Random number generators.
!! Original code was distributed under LGPL v3 as evidenced by their link to (this license)[https://people.sc.fsu.edu/~jburkardt/txt/gnu_lgpl.txt]
!! which was referenced by the link on (this page)[https://people.sc.fsu.edu/~jburkardt/f_src/rnglib/rnglib.html] on November 23rd 2017,
!! and can be used with no issue in this GPL v3 or later library as stated (here)[https://www.gnu.org/licenses/gpl-faq.html#AllCompatibility]
!! Author:
!!
!!    Original Pascal version by Pierre L'Ecuyer, Serge Cote.
!!    FORTRAN90 version by John Burkardt.
!!
!! Modified with an object oriented class for generating random numbers

implicit none

contains


subroutine advance_state ( k )

!*****************************************************************************80
!
!! ADVANCE_STATE advances the state of the current generator.
!
!  Discussion:
!
!    This procedure advances the state of the current generator by 2^K
!    values and resets the initial seed to that value.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 March 2013
!
!  Author:
!
!    Original Pascal version by Pierre L'Ecuyer, Serge Cote.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Pierre LEcuyer, Serge Cote,
!    Implementing a Random Number Package with Splitting Facilities,
!    ACM Transactions on Mathematical Software,
!    Volume 17, Number 1, March 1991, pages 98-111.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) K, indicates that the generator is to be
!    advanced by 2^K values.
!    0 <= K.
!
  implicit none

  integer ( kind = 4 ), parameter :: a1 = 40014
  integer ( kind = 4 ), parameter :: a2 = 40692
  integer ( kind = 4 ) b1
  integer ( kind = 4 ) b2
  integer ( kind = 4 ) cg1
  integer ( kind = 4 ) cg2
  integer ( kind = 4 ) cgn_get
  integer ( kind = 4 ) g
  integer ( kind = 4 ) i
  logical initialized_get
  integer ( kind = 4 ) k
  integer ( kind = 4 ), parameter :: m1 = 2147483563
  integer ( kind = 4 ), parameter :: m2 = 2147483399
  integer ( kind = 4 ) multmod

  if ( k < 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'ADVANCE_STATE - Fatal error!'
    write ( *, '(a)' ) '  Input exponent K is out of bounds.'
    stop 1
  end if
!
!  Check whether the package must be initialized.
!
  if ( .not. initialized_get ( ) ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'ADVANCE_STATE - Note:'
    write ( *, '(a)' ) '  Initializing RNGLIB package.'
    call initialize ( )
  end if
!
!  Get the current generator index.
!
  g = cgn_get ( )

  b1 = a1
  b2 = a2

  do i = 1, k
    b1 = multmod ( b1, b1, m1 )
    b2 = multmod ( b2, b2, m2 )
  end do

  call cg_get ( g, cg1, cg2 )
  cg1 = multmod ( b1, cg1, m1 )
  cg2 = multmod ( b2, cg2, m2 )
  call cg_set ( g, cg1, cg2 )

  return
end
function antithetic_get ( )

!*****************************************************************************80
!
!! ANTITHETIC_GET queries the antithetic value for a given generator.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 March 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, logical ANTITHETIC_GET, is TRUE if generator G is antithetic.
!
  implicit none

  logical antithetic_get
  integer ( kind = 4 ) i
  logical value

  i = -1
  call antithetic_memory ( i, value )

  antithetic_get = value

  return
end
subroutine antithetic_memory ( i, value )

!*****************************************************************************80
!
!! ANTITHETIC_MEMORY stores the antithetic value for all generators.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 March 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) I, the desired action.
!    -1, get a value.
!    0, initialize all values.
!    1, set a value.
!
!    Input/output, logical VALUE.  For I = -1, VALUE is an output
!    quantity, for I = +1, an input quantity.
!
  implicit none

  integer ( kind = 4 ), parameter :: g_max = 32

  logical a_save(g_max)
  integer ( kind = 4 ) cgn_get
  integer ( kind = 4 ) g
  integer ( kind = 4 ) i
  logical value

  save a_save

  data a_save / 32 * .false. /

  if ( i < 0 ) then
    g = cgn_get ( )
    value = a_save(g)
  else if ( i == 0 ) then
    a_save(1:g_max) = .false.
  else if ( 0 < i ) then
    g = cgn_get ( )
    a_save(g) = value
  end if

  return
end
subroutine antithetic_set ( value )

!*****************************************************************************80
!
!! ANTITHETIC_SET sets the antithetic value for a given generator.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 March 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, logical VALUE, is TRUE if generator G is to be antithetic.
!
  implicit none

  integer ( kind = 4 ) i
  logical value

  i = +1
  call antithetic_memory ( i, value )

  return
end
subroutine cg_get ( g, cg1, cg2 )

!*****************************************************************************80
!
!! CG_GET queries the CG values for a given generator.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 March 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) G, the index of the generator.
!    1 <= G <= 32.
!
!    Output, integer ( kind = 4 ) CG1, CG2, the CG values for generator G.
!
  implicit none

  integer ( kind = 4 ) cg1
  integer ( kind = 4 ) cg2
  integer ( kind = 4 ) g
  integer ( kind = 4 ) i

  i = -1
  call cg_memory ( i, g, cg1, cg2 )

  return
end
subroutine cg_memory ( i, g, cg1, cg2 )

!*****************************************************************************80
!
!! CG_MEMORY stores the CG values for all generators.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 March 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) I, the desired action.
!    -1, get a value.
!    0, initialize all values.
!    1, set a value.
!
!    Input, integer ( kind = 4 ) G, for I = -1 or +1, the index of
!    the generator, with 1 <= G <= 32.
!
!    Input/output, integer ( kind = 4 ) CG1, CG2.  For I = -1,
!    these are output, for I = +1, these are input, for I = 0,
!    these arguments are ignored.  When used, the arguments are
!    old or new values of the CG parameter for generator G.
!
  implicit none

  integer ( kind = 4 ), parameter :: g_max = 32

  integer ( kind = 4 ) cg1
  integer ( kind = 4 ) cg1_save(g_max)
  integer ( kind = 4 ) cg2
  integer ( kind = 4 ) cg2_save(g_max)
  integer ( kind = 4 ) g
  integer ( kind = 4 ) i

  save cg1_save
  save cg2_save

  data cg1_save / 32 * 0 /
  data cg2_save / 32 * 0 /

  if ( g < 1 .or. g_max < g ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CG_MEMORY - Fatal error!'
    write ( *, '(a)' ) '  Input generator index G is out of bounds.'
    stop 1
  end if

  if ( i < 0 ) then
    cg1 = cg1_save(g)
    cg2 = cg2_save(g)
  else if ( i == 0 ) then
    cg1_save(1:g_max) = 0
    cg2_save(1:g_max) = 0
  else if ( 0 < i ) then
    cg1_save(g) = cg1
    cg2_save(g) = cg2
  end if

  return
end
subroutine cg_set ( g, cg1, cg2 )

!*****************************************************************************80
!
!! CG_SET sets the CG values for a given generator.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 March 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) G, the index of the generator.
!    1 <= G <= 32.
!
!    Input, integer ( kind = 4 ) CG1, CG2, the CG values for generator G.
!
  implicit none

  integer ( kind = 4 ) cg1
  integer ( kind = 4 ) cg2
  integer ( kind = 4 ) g
  integer ( kind = 4 ) i

  i = +1
  call cg_memory ( i, g, cg1, cg2 )

  return
end
function cgn_get ( )

!*****************************************************************************80
!
!! CGN_GET gets the current generator index.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 March 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) CGN_GET, the current generator index.
!    1 <= CGN_GET <= 32.
!
  implicit none

  integer ( kind = 4 ) cgn_get
  integer ( kind = 4 ) g
  integer ( kind = 4 ) i

  i = -1
  call cgn_memory ( i, g )

  cgn_get = g

  return
end
subroutine cgn_memory ( i, g )

!*****************************************************************************80
!
!! CGN_MEMORY stores the current generator index.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 March 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) I, the desired action.
!    -1, get the value.
!    0, initialize the value.
!    1, set the value.
!
!    Input/output, integer ( kind = 4 ) G.  For I = -1 or 0,
!    this is output, for I = +1, this is input.
!
  implicit none

  integer ( kind = 4 ), parameter :: g_max = 32

  integer ( kind = 4 ) g
  integer ( kind = 4 ) g_save
  integer ( kind = 4 ) i

  save g_save

  data g_save / 1 /

  if ( i < 0 ) then

    g = g_save

  else if ( i == 0 ) then

    g_save = 1
    g = g_save

  else if ( 0 < i ) then

    if ( g < 1 .or. g_max < g ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CGN_MEMORY - Fatal error!'
      write ( *, '(a)' ) '  Generator index G is out of bounds.'
      stop 1
    end if

    g_save = g

  end if

  return
end
subroutine cgn_set ( g )

!*****************************************************************************80
!
!! CGN_SET sets the current generator index.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 March 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) G, the index of the generator.
!    1 <= G <= 32.
!
  implicit none

  integer ( kind = 4 ) g
  integer ( kind = 4 ) i

  i = +1
  call cgn_memory ( i, g )

  return
end
subroutine get_state ( cg1, cg2 )

!*****************************************************************************80
!
!! GET_STATE returns the state of the current generator.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 March 2013
!
!  Author:
!
!    Original Pascal version by Pierre L'Ecuyer, Serge Cote.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Pierre LEcuyer, Serge Cote,
!    Implementing a Random Number Package with Splitting Facilities,
!    ACM Transactions on Mathematical Software,
!    Volume 17, Number 1, March 1991, pages 98-111.
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) CG1, CG2, the CG values for the
!    current generator.
!
  implicit none

  integer ( kind = 4 ) cg1
  integer ( kind = 4 ) cg2
  integer ( kind = 4 ) cgn_get
  integer ( kind = 4 ) g
  logical initialized_get
!
!  Check whether the package must be initialized.
!
  if ( .not. initialized_get ( ) ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'GET_STATE - Note:'
    write ( *, '(a)' ) '  Initializing RNGLIB package.'
    call initialize ( )
  end if
!
!  Get the current generator index.
!
  g = cgn_get ( )
!
!  Retrieve the seed values for this generator.
!
  call cg_get ( g, cg1, cg2 )

  return
end
function i4_uni ( )

!*****************************************************************************80
!
!! I4_UNI generates a random positive integer.
!
!  Discussion:
!
!    This procedure returns a random integer following a uniform distribution
!    over (1, 2147483562) using the current generator.
!
!    The original name of this function was "random()", but this conflicts
!    with a standard library function name in C.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 August 2013
!
!  Author:
!
!    Original Pascal version by Pierre L'Ecuyer, Serge Cote.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Pierre LEcuyer, Serge Cote,
!    Implementing a Random Number Package with Splitting Facilities,
!    ACM Transactions on Mathematical Software,
!    Volume 17, Number 1, March 1991, pages 98-111.
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) I4_UNI, the random integer.
!
  implicit none

  integer ( kind = 4 ), parameter :: a1 = 40014
  integer ( kind = 4 ), parameter :: a2 = 40692
  logical antithetic_get
  integer ( kind = 4 ) cg1
  integer ( kind = 4 ) cg2
  integer ( kind = 4 ) cgn_get
  integer ( kind = 4 ) g
  integer ( kind = 4 ) i4_uni
  logical initialized_get
  integer ( kind = 4 ) k
  integer ( kind = 4 ), parameter :: m1 = 2147483563
  integer ( kind = 4 ), parameter :: m2 = 2147483399
  logical value
  integer ( kind = 4 ) z
!
!  Check whether the package must be initialized.
!
  if ( .not. initialized_get ( ) ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'I4_UNI - Note:'
    write ( *, '(a)' ) '  Initializing RNGLIB package.'
    call initialize ( )
  end if
!
!  Get the current generator index.
!
  g = cgn_get ( )
!
!  Retrieve the seeds for the current generator.
!
  call cg_get ( g, cg1, cg2 )
!
!  Update the seeds.
!
  k = cg1 / 53668
  cg1 = a1 * ( cg1 - k * 53668 ) - k * 12211

  if ( cg1 < 0 ) then
    cg1 = cg1 + m1
  end if

  k = cg2 / 52774
  cg2 = a2 * ( cg2 - k * 52774 ) - k * 3791

  if ( cg2 < 0 ) then
    cg2 = cg2 + m2
  end if
!
!  Store the updated seeds.
!
  call cg_set ( g, cg1, cg2 )
!
!  Construct the random integer from the seeds.
!
  z = cg1 - cg2

  if ( z < 1 ) then
    z = z + m1 - 1
  end if
!
!  If the generator is in antithetic mode, we must reflect the value.
!
  value = antithetic_get ( )

  if ( value ) then
    z = m1 - z
  end if

  i4_uni = z

  return
end
subroutine ig_get ( g, ig1, ig2 )

!*****************************************************************************80
!
!! IG_GET queries the IG values for a given generator.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 March 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) G, the index of the generator.
!    1 <= G <= 32.
!
!    Output, integer ( kind = 4 ) IG1, IG2, the IG values for generator G.
!
  implicit none

  integer ( kind = 4 ) g
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ig1
  integer ( kind = 4 ) ig2

  i = -1
  call ig_memory ( i, g, ig1, ig2 )

  return
end
subroutine ig_memory ( i, g, ig1, ig2 )

!*****************************************************************************80
!
!! IG_MEMORY stores the IG values for all generators.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 March 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) I, the desired action.
!    -1, get a value.
!    0, initialize all values.
!    1, set a value.
!
!    Input, integer ( kind = 4 ) G, for I = -1 or +1, the index of
!    the generator, with 1 <= G <= 32.
!
!    Input/output, integer ( kind = 4 ) IG1, IG2.  For I = -1,
!    these are output, for I = +1, these are input, for I = 0,
!    these arguments are ignored.  When used, the arguments are
!    old or new values of the IG parameter for generator G.
!
  implicit none

  integer ( kind = 4 ), parameter :: g_max = 32

  integer ( kind = 4 ) g
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ig1
  integer ( kind = 4 ) ig1_save(g_max)
  integer ( kind = 4 ) ig2
  integer ( kind = 4 ) ig2_save(g_max)

  save ig1_save
  save ig2_save

  data ig1_save / 32 * 0 /
  data ig2_save / 32 * 0 /

  if ( g < 1 .or. g_max < g ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'IG_MEMORY - Fatal error!'
    write ( *, '(a)' ) '  Input generator index G is out of bounds.'
    stop 1
  end if

  if ( i < 0 ) then
    ig1 = ig1_save(g)
    ig2 = ig2_save(g)
  else if ( i == 0 ) then
    ig1_save(1:g_max) = 0
    ig2_save(1:g_max) = 0
  else if ( 0 < i ) then
    ig1_save(g) = ig1
    ig2_save(g) = ig2
  end if

  return
end
subroutine ig_set ( g, ig1, ig2 )

!*****************************************************************************80
!
!! IG_SET sets the IG values for a given generator.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 March 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) G, the index of the generator.
!    1 <= G <= 32.
!
!    Input, integer ( kind = 4 ) IG1, IG2, the IG values for generator G.
!
  implicit none

  integer ( kind = 4 ) g
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ig1
  integer ( kind = 4 ) ig2

  i = +1
  call ig_memory ( i, g, ig1, ig2 )

  return
end
subroutine init_generator ( t )

!*****************************************************************************80
!
!! INIT_GENERATOR sets the current generator to initial, last or new seed.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 March 2013
!
!  Author:
!
!    Original Pascal version by Pierre L'Ecuyer, Serge Cote.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Pierre LEcuyer, Serge Cote,
!    Implementing a Random Number Package with Splitting Facilities,
!    ACM Transactions on Mathematical Software,
!    Volume 17, Number 1, March 1991, pages 98-111.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) T, the seed type:
!    0, use the seed chosen at initialization time.
!    1, use the last seed.
!    2, use a new seed set 2^30 values away.
!
  implicit none

  integer ( kind = 4 ), parameter :: a1_w = 1033780774
  integer ( kind = 4 ), parameter :: a2_w = 1494757890
  integer ( kind = 4 ) cg1
  integer ( kind = 4 ) cg2
  integer ( kind = 4 ) cgn_get
  integer ( kind = 4 ) g
  integer ( kind = 4 ) ig1
  integer ( kind = 4 ) ig2
  logical initialized_get
  integer ( kind = 4 ) lg1
  integer ( kind = 4 ) lg2
  integer ( kind = 4 ), parameter :: m1 = 2147483563
  integer ( kind = 4 ), parameter :: m2 = 2147483399
  integer ( kind = 4 ) multmod
  integer ( kind = 4 ) t
!
!  Check whether the package must be initialized.
!
  if ( .not. initialized_get ( ) ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'INIT_GENERATOR - Note:'
    write ( *, '(a)' ) '  Initializing RNGLIB package.'
    call initialize ( )
  end if
!
!  Get the current generator index.
!
  g = cgn_get ( )
!
!  0: restore the initial seed.
!
  if ( t == 0 ) then

    call ig_get ( g, ig1, ig2 )
    lg1 = ig1
    lg2 = ig2
    call lg_set ( g, lg1, lg2 )
!
!  1: restore the last seed.
!
  else if ( t == 1 ) then

    call lg_get ( g, lg1, lg2 )
!
!  2: advance to a new seed.
!
  else if ( t == 2 ) then

    call lg_get ( g, lg1, lg2 )
    lg1 = multmod ( a1_w, lg1, m1 )
    lg2 = multmod ( a2_w, lg2, m2 )
    call lg_set ( g, lg1, lg2 )

  else

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'INIT_GENERATOR - Fatal error!'
    write ( *, '(a)' ) '  Input parameter T out of bounds.'
    stop 1

  end if
!
!  Store the new seed.
!
  cg1 = lg1
  cg2 = lg2
  call cg_set ( g, cg1, cg2 )

  return
end
subroutine initialize ( )

!*****************************************************************************80
!
!! INITIALIZE initializes the random number generator library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 March 2013
!
!  Author:
!
!    Original Pascal version by Pierre L'Ecuyer, Serge Cote.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Pierre LEcuyer, Serge Cote,
!    Implementing a Random Number Package with Splitting Facilities,
!    ACM Transactions on Mathematical Software,
!    Volume 17, Number 1, March 1991, pages 98-111.
!
!  Parameters:
!
!    None
!
  implicit none

  integer ( kind = 4 ) g
  integer ( kind = 4 ), parameter :: g_max = 32
  integer ( kind = 4 ) ig1
  integer ( kind = 4 ) ig2
  logical value
!
!  Remember that we have called INITIALIZE().
!
  call initialized_set ( )
!
!  Initialize all generators to have FALSE antithetic value.
!
  value = .false.
  do g = 1, g_max
    call cgn_set ( g )
    call antithetic_set ( value )
  end do
!
!  Set the initial seeds.
!
  ig1 = 1234567890
  ig2 = 123456789
  call set_initial_seed ( ig1, ig2 )
!
!  Initialize the current generator index to the first one.
!
  g = 1
  call cgn_set ( g )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'INITIALIZE - Note:'
  write ( *, '(a)' ) '  The RNGLIB package has been initialized.'

  return
end
function initialized_get ( )

!*****************************************************************************80
!
!! INITIALIZED_GET queries the INITIALIZED value.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 March 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, logical INITIALIZED_GET, is TRUE if the package has
!    been initialized.
!
  implicit none

  integer ( kind = 4 ) i
  logical initialized
  logical initialized_get

  i = -1
  call initialized_memory ( i, initialized )

  initialized_get = initialized

  return
end
subroutine initialized_memory ( i, initialized )

!*****************************************************************************80
!
!! INITIALIZED_MEMORY stores the INITIALIZED value for the package.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 March 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) I, the desired action.
!    -1, get the value.
!    0, initialize the value.
!    1, set the value.
!
!    Input/output, logical INITIALIZED.  For I = -1,
!    this is output, for I = +1, this is input, for I = 0,
!    this argument is ignored.
!
  implicit none

  integer ( kind = 4 ) i
  logical initialized
  logical initialized_save

  save initialized_save

  data initialized_save / .false. /

  if ( i < 0 ) then
    initialized = initialized_save
  else if ( i == 0 ) then
    initialized_save = .false.
  else if ( 0 < i ) then
    initialized_save = initialized
  end if

  return
end
subroutine initialized_set ( )

!*****************************************************************************80
!
!! INITIALIZED_SET sets the INITIALIZED value true.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 March 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    None
!
  implicit none

  integer ( kind = 4 ) i
  logical initialized

  i = +1
  initialized = .true.
  call initialized_memory ( i, initialized )

  return
end
subroutine lg_get ( g, lg1, lg2 )

!*****************************************************************************80
!
!! LG_GET queries the LG values for a given generator.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 March 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) G, the index of the generator.
!    1 <= G <= 32.
!
!    Output, integer ( kind = 4 ) LG1, LG2, the LG values for generator G.
!
  implicit none

  integer ( kind = 4 ) g
  integer ( kind = 4 ) i
  integer ( kind = 4 ) lg1
  integer ( kind = 4 ) lg2

  i = -1
  call lg_memory ( i, g, lg1, lg2 )

  return
end
subroutine lg_memory ( i, g, lg1, lg2 )

!*****************************************************************************80
!
!! LG_MEMORY stores the LG values for all generators.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 March 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) I, the desired action.
!    -1, get a value.
!    0, initialize all values.
!    1, set a value.
!
!    Input, integer ( kind = 4 ) G, for I = -1 or +1, the index of
!    the generator, with 1 <= G <= 32.
!
!    Input/output, integer ( kind = 4 ) LG1, LG2.  For I = -1,
!    these are output, for I = +1, these are input, for I = 0,
!    these arguments are ignored.  When used, the arguments are
!    old or new values of the LG parameter for generator G.
!
  implicit none

  integer ( kind = 4 ), parameter :: g_max = 32

  integer ( kind = 4 ) g
  integer ( kind = 4 ) i
  integer ( kind = 4 ) lg1
  integer ( kind = 4 ) lg1_save(g_max)
  integer ( kind = 4 ) lg2
  integer ( kind = 4 ) lg2_save(g_max)

  save lg1_save
  save lg2_save

  data lg1_save / 32 * 0 /
  data lg2_save / 32 * 0 /

  if ( g < 1 .or. g_max < g ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'LG_MEMORY - Fatal error!'
    write ( *, '(a)' ) '  Input generator index G is out of bounds.'
    stop 1
  end if

  if ( i < 0 ) then
    lg1 = lg1_save(g)
    lg2 = lg2_save(g)
  else if ( i == 0 ) then
    lg1_save(1:g_max) = 0
    lg2_save(1:g_max) = 0
  else if ( 0 < i ) then
    lg1_save(g) = lg1
    lg2_save(g) = lg2
  end if

  return
end
subroutine lg_set ( g, lg1, lg2 )

!*****************************************************************************80
!
!! LG_SET sets the LG values for a given generator.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 March 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) G, the index of the generator.
!    1 <= G <= 32.
!
!    Input, integer ( kind = 4 ) LG1, LG2, the LG values for generator G.
!
  implicit none

  integer ( kind = 4 ) g
  integer ( kind = 4 ) i
  integer ( kind = 4 ) lg1
  integer ( kind = 4 ) lg2

  i = +1
  call lg_memory ( i, g, lg1, lg2 )

  return
end
function multmod ( a, s, m )

!*****************************************************************************80
!
!! MULTMOD carries out modular multiplication.
!
!  Discussion:
!
!    This procedure returns
!
!      ( A * S ) mod M
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 March 2013
!
!  Author:
!
!    Original Pascal version by Pierre L'Ecuyer, Serge Cote.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Pierre LEcuyer, Serge Cote,
!    Implementing a Random Number Package with Splitting Facilities,
!    ACM Transactions on Mathematical Software,
!    Volume 17, Number 1, March 1991, pages 98-111.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, S, M, the arguments.
!
!    Output, integer ( kind = 4 ) MULTMOD, the value of the product of A and S,
!    modulo M.
!
  implicit none

  integer ( kind = 4 ) a
  integer ( kind = 4 ) a0
  integer ( kind = 4 ) a1
  integer ( kind = 4 ), parameter :: h = 32768
  integer ( kind = 4 ) k
  integer ( kind = 4 ) m
  integer ( kind = 4 ) multmod
  integer ( kind = 4 ) p
  integer ( kind = 4 ) q
  integer ( kind = 4 ) qh
  integer ( kind = 4 ) rh
  integer ( kind = 4 ) s

  if ( a <= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'MULTMOD - Fatal error!'
    write ( *, '(a)' ) '  A <= 0.'
    stop 1
  end if

  if ( m <= a ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'MULTMOD - Fatal error!'
    write ( *, '(a)' ) '  M <= A.'
    stop 1
  end if

  if ( s <= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'MULTMOD - Fatal error!'
    write ( *, '(a)' ) '  S <= 0.'
    stop 1
  end if

  if ( m <= s ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'MULTMOD - Fatal error!'
    write ( *, '(a)' ) '  M <= S.'
    stop 1
  end if

  if ( a < h ) then

    a0 = a
    p = 0

  else

    a1 = a / h
    a0 = a - h * a1
    qh = m / h
    rh = m - h * qh

    if ( h <= a1 ) then

      a1 = a1 - h
      k = s / qh
      p = h * ( s - k * qh ) - k * rh

      do while ( p < 0 )
        p = p + m
      end do

    else

      p = 0

    end if

    if ( a1 /= 0 ) then

      q = m / a1
      k = s / q
      p = p - k * ( m - a1 * q )

      if ( 0 < p ) then
        p = p - m
      end if

      p = p + a1 * ( s - k * q )

      do while ( p < 0 )
        p = p + m
      end do

    end if

    k = p / qh
    p = h * ( p - k * qh ) - k * rh

    do while ( p < 0 )
      p = p + m
    end do

  end if

  if ( a0 /= 0 ) then

    q = m / a0
    k = s / q
    p = p - k * ( m - a0 * q )

    if ( 0 < p ) then
      p = p - m
    end if

    p = p + a0 * ( s - k * q )

    do while ( p < 0 )
      p = p + m
    end do

  end if

  multmod = p

  return
end
function r4_uni_01 ( )

!*****************************************************************************80
!
!! R4_UNI_01 returns a uniform random real number in [0,1].
!
!  Discussion:
!
!    This procedure returns a random floating point number from a uniform
!    distribution over (0,1), not including the endpoint values, using the
!    current random number generator.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 August 2013
!
!  Author:
!
!    Original Pascal version by Pierre L'Ecuyer, Serge Cote.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Pierre LEcuyer, Serge Cote,
!    Implementing a Random Number Package with Splitting Facilities,
!    ACM Transactions on Mathematical Software,
!    Volume 17, Number 1, March 1991, pages 98-111.
!
!  Parameters:
!
!    Output, real ( kind = 4 ) R4_UNI_01, a uniform random value in [0,1].
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_uni
  logical initialized_get
  real ( kind = 4 ) r4_uni_01
!
!  Check whether the package must be initialized.
!
  if ( .not. initialized_get ( ) ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R4_UNI_01 - Note:'
    write ( *, '(a)' ) '  Initializing RNGLIB package.'
    call initialize ( )
  end if
!
!  Get a random positive integer.
!
  i = i4_uni ( )
!
!  Scale it to a random real in [0,1].
!
  r4_uni_01 = real ( i, kind = 4 ) * 4.656613057E-10

  return
end
function r8_uni_01 ( )

!*****************************************************************************80
!
!! R8_UNI_01 returns a uniform random double precision number in [0,1].
!
!  Discussion:
!
!    This procedure returns a random floating point number from a uniform
!    distribution over (0,1), not including the endpoint values, using the
!    current random number generator.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 August 2013
!
!  Author:
!
!    Original Pascal version by Pierre L'Ecuyer, Serge Cote.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Pierre LEcuyer, Serge Cote,
!    Implementing a Random Number Package with Splitting Facilities,
!    ACM Transactions on Mathematical Software,
!    Volume 17, Number 1, March 1991, pages 98-111.
!
!  Parameters:
!
!    Output, real ( kind = 8 ) R8_UNI_01, a uniform random value in [0,1].
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_uni
  logical initialized_get
  real ( kind = 8 ) r8_uni_01
!
!  Check whether the package must be initialized.
!
  if ( .not. initialized_get ( ) ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_UNI_01 - Note:'
    write ( *, '(a)' ) '  Initializing RNGLIB package.'
    call initialize ( )
  end if
!
!  Get a random positive integer.
!
  i = i4_uni ( )
!
!  Scale it to a random real in [0,1].
!
  r8_uni_01 = real ( i, kind = 8 ) * 4.656613057D-10

  return
end
subroutine set_initial_seed ( ig1, ig2 )

!*****************************************************************************80
!
!! SET_INITIAL_SEED resets the initial seed and state for all generators.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 March 2013
!
!  Author:
!
!    Original Pascal version by Pierre L'Ecuyer, Serge Cote.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Pierre LEcuyer, Serge Cote,
!    Implementing a Random Number Package with Splitting Facilities,
!    ACM Transactions on Mathematical Software,
!    Volume 17, Number 1, March 1991, pages 98-111.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) IG1, IG2, the initial seed values
!    for the first generator.
!    1 <= IG1 < 2147483563
!    1 <= IG2 < 2147483399
!
  implicit none

  integer ( kind = 4 ), parameter :: a1_vw = 2082007225
  integer ( kind = 4 ), parameter :: a2_vw = 784306273
  integer ( kind = 4 ) g
  integer ( kind = 4 ), parameter :: g_max = 32
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ig1
  integer ( kind = 4 ) ig2
  logical initialized_get
  integer ( kind = 4 ), parameter :: m1 = 2147483563
  integer ( kind = 4 ), parameter :: m2 = 2147483399
  integer ( kind = 4 ) multmod
  integer ( kind = 4 ) t

  if ( ig1 < 1 .or. m1 <= ig1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'SET_INITIAL_SEED - Fatal error!'
    write ( *, '(a)' ) '  Input parameter IG1 out of bounds.'
    stop 1
  end if

  if ( ig2 < 1 .or. m2 <= ig2 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'SET_INITIAL_SEED - Fatal error!'
    write ( *, '(a)' ) '  Input parameter IG2 out of bounds.'
    stop 1
  end if
!
!  Because INITIALIZE calls SET_INITIAL_SEED, it's not easy to correct
!  the error that arises if SET_INITIAL_SEED is called before INITIALIZE.
!  So don't bother trying.
!
  if ( .not. initialized_get ( ) ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'SET_INITIAL_SEED - Fatal error!'
    write ( *, '(a)' ) '  The RNGLIB package has not been initialized.'
    stop 1
  end if
!
!  Set the initial seed, then initialize the first generator.
!
  g = 1
  call cgn_set ( g )

  call ig_set ( g, ig1, ig2 )

  t = 0
  call init_generator ( t )
!
!  Now do similar operations for the other generators.
!
  do g = 2, g_max

    call cgn_set ( g )
    ig1 = multmod ( a1_vw, ig1, m1 )
    ig2 = multmod ( a2_vw, ig2, m2 )
    call ig_set ( g, ig1, ig2 )
    call init_generator ( t )

  end do
!
!  Now choose the first generator.
!
  g = 1
  call cgn_set ( g )

  return
end
subroutine set_seed ( cg1, cg2 )

!*****************************************************************************80
!
!! SET_SEED resets the initial seed and state of the current generator.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 March 2013
!
!  Author:
!
!    Original Pascal version by Pierre L'Ecuyer, Serge Cote.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Pierre LEcuyer, Serge Cote,
!    Implementing a Random Number Package with Splitting Facilities,
!    ACM Transactions on Mathematical Software,
!    Volume 17, Number 1, March 1991, pages 98-111.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) CG1, CG2, the CG values for generator G.
!    1 <= CG1 < 2147483563
!    1 <= CG2 < 2147483399
!
  implicit none

  integer ( kind = 4 ) cg1
  integer ( kind = 4 ) cg2
  integer ( kind = 4 ) cgn_get
  integer ( kind = 4 ) g
  integer ( kind = 4 ) i
  logical initialized_get
  integer ( kind = 4 ), parameter :: m1 = 2147483563
  integer ( kind = 4 ), parameter :: m2 = 2147483399
  integer ( kind = 4 ) t

  if ( cg1 < 1 .or. m1 <= cg1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'SET_SEED - Fatal error!'
    write ( *, '(a)' ) '  Input parameter CG1 out of bounds.'
    stop 1
  end if

  if ( cg2 < 1 .or. m2 <= cg2 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'SET_SEED - Fatal error!'
    write ( *, '(a)' ) '  Input parameter CG2 out of bounds.'
    stop 1
  end if
!
!  Check whether the package must be initialized.
!
  if ( .not. initialized_get ( ) ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'SET_SEED - Note:'
    write ( *, '(a)' ) '  Initializing RNGLIB package.'
    call initialize ( )
  end if
!
!  Retrieve the current generator index.
!
  g = cgn_get ( )
!
!  Set the seeds.
!
  call cg_set ( g, cg1, cg2 )
!
!  Initialize the generator.
!
  t = 0
  call init_generator ( t )

  return
end
subroutine timestamp ( )

!*****************************************************************************80
!
!! TIMESTAMP prints the current YMDHMS date as a time stamp.
!
!  Example:
!
!    31 May 2001   9:45:54.872 AM
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 May 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    None
!
  implicit none

  character ( len = 8 ) ampm
  integer ( kind = 4 ) d
  integer ( kind = 4 ) h
  integer ( kind = 4 ) m
  integer ( kind = 4 ) mm
  character ( len = 9 ), parameter, dimension(12) :: month = (/ &
    'January  ', 'February ', 'March    ', 'April    ', &
    'May      ', 'June     ', 'July     ', 'August   ', &
    'September', 'October  ', 'November ', 'December ' /)
  integer ( kind = 4 ) n
  integer ( kind = 4 ) s
  integer ( kind = 4 ) values(8)
  integer ( kind = 4 ) y

  call date_and_time ( values = values )

  y = values(1)
  m = values(2)
  d = values(3)
  h = values(5)
  n = values(6)
  s = values(7)
  mm = values(8)

  if ( h < 12 ) then
    ampm = 'AM'
  else if ( h == 12 ) then
    if ( n == 0 .and. s == 0 ) then
      ampm = 'Noon'
    else
      ampm = 'PM'
    end if
  else
    h = h - 12
    if ( h < 12 ) then
      ampm = 'PM'
    else if ( h == 12 ) then
      if ( n == 0 .and. s == 0 ) then
        ampm = 'Midnight'
      else
        ampm = 'AM'
      end if
    end if
  end if

  write ( *, '(i2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
    d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, trim ( ampm )

  return
end

end module
