
!ccccccccccccccc

module Constants
  use iso_fortran_env, only: error_unit, dp => real64
  implicit none
  real (dp), parameter :: Pi    = 3.141592653589793_dp , ExpEuler = 1.781072417990198_dp,&
                          Zeta3 = 1.2020569031595942   , Zeta2 = 1.6449340668482262_dp,  &
                          Euler = 0.5772156649015329_dp, l2  = 0.6931471805599453_dp,    &
                          Pio2  = 1.5707963267948966_dp, Pi2 = 9.869604401089358_dp ,    &
                          sqrt2   = 1.4142135623730951_dp, sqrt3 = 1.7320508075688772_dp

    !**************************************************************
    !>
    !  Machine constants (replaces the old SLATEC [D1MACH](http://www.netlib.org/slatec/src/d1mach.f) function)
    !
    !  The traditional D1MACH constants are:
    !  * _dpD1MACH( 1) = B**(EMIN-1)_dp,           the smallest positive magnitude.
    !  * _dpD1MACH( 2) = B**EMAX*(1 - B**(-T))_dp, the largest magnitude.
    !  * _dpD1MACH( 3) = B**(-T)_dp,               the smallest relative spacing.
    !  * _dpD1MACH( 4) = B**(1-T)_dp,              the largest relative spacing.
    !  * _dpD1MACH( 5) = LOG10(B)_dp

    !**************************************************************

  real (dp), dimension(5), parameter :: d1mach = [  tiny(1.0_dp), huge(1.0_dp), &
           real(radix(1.0_dp),dp)**(-digits(1.0_dp)), epsilon(1.0_dp), &
           log10(real(radix(1.0_dp),dp)) ]

end module Constants

!ccccccccccccccc

module Legendre
  use Constants, only: dp
contains
  function LegendreList(n,x) result(list)
    integer      , intent(in) :: n
    real (dp)    , intent(in) :: x
    real (dp), dimension(0:n) :: list
    integer                   :: i

    list (:1) = [ 1._dp, x ]

    do i = 2, n
      list(i) = ( (2 * i - 1) * x * list(i - 1) - (i - 1) * list(i - 2) )/i
    end do

  end function LegendreList

end module Legendre

!ccccccccccccccc

subroutine f90compass_search ( function_handle, m, x0, delta_tol, delta_init, &
  k_max, x, fx, k )

!*****************************************************************************80
!
!! COMPASS_SEARCH carries out a direct search minimization algorithm.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 January 2012
!
!   Edited by Vicent Mateu, 15-06-2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Tamara Kolda, Robert Michael Lewis, Virginia Torczon,
!    Optimization by Direct Search: New Perspectives on Some Classical
!    and Modern Methods,
!    SIAM Review,
!    Volume 45, Number 3, 2003, pages 385-482.
!
!  Parameters:
!
!    Input, external real (dp) FUNCTION_HANDLE, the name of
!    a FORTRAN90 function which evaluates the function to be minimized, of the
!    form FUNCTION FUNCTION_HANDLE ( M, X ).
!
!    Input, integer M, the number of variables.
!
!    Input, real (dp) X0(M), a starting estimate for the minimizer.
!
!    Input, real (dp) DELTA_TOL, the smallest step size that is allowed.
!
!    Input, real (dp) DELTA_INIT, the starting stepsize.
!
!    Input, integer K_MAX, the maximum number of steps allowed.
!
!    Output, real (dp) X(M), the estimated minimizer.
!
!    Output, real (dp) FX, the function value at X.
!
!    Output, integer K, the number of steps taken.

  use Constants, only: dp; implicit none

  logical                              :: decrease
  integer                , intent(in)  :: m, k_max
  integer                , intent(out) :: k
  real (dp)              , intent(in)  :: delta_init, delta_tol
  real (dp)              , intent(out) :: fx
  real (dp), dimension(m), intent(in)  :: x0
  real (dp), dimension(m), intent(out) :: x
  real (dp), external                  :: function_handle
  real (dp), dimension(m)              :: xd
  integer                              :: i, ii
  real (dp)                            :: delta, fxd, s

  k = 0; x = x0; fx = function_handle ( m, x )

  if ( delta_tol <= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'COMPASS_SEARCH - Fatal error!'
    write ( *, '(a)' ) '  DELTA_TOL <= 0.0.'
    write ( *, '(a,g14.6)' ) '  DELTA_TOL = ', delta_tol
    stop
  end if

  if ( delta_init <= delta_tol ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'COMPASS_SEARCH - Fatal error!'
    write ( *, '(a)' ) '  DELTA_INIT < DELTA_TOL.'
    write ( *, '(a,g14.6)' ) '  DELTA_INIT = ', delta_init
    write ( *, '(a,g14.6)' ) '  DELTA_TOL = ', delta_tol
    stop
  end if

  delta = delta_init

  do while ( k < k_max )

    k = k + 1

!  For each coordinate direction I, seek a lower function value
!  by increasing or decreasing X(I) by DELTA.

    decrease = .false. ;  s = 1;  i = 1

    do ii = 1, 2 * m

      xd = x;  xd(i) = xd(i) + s * delta;  fxd = function_handle ( m, xd )

!  As soon as a decrease is noticed, accept the new point.

      if ( fxd < fx ) then
        x = xd;  fx = fxd;  decrease = .true.
        exit
      end if

      s = - s
      if ( s == 1 ) i = i + 1

    end do

!  If no decrease occurred, reduce DELTA.

    if ( .not. decrease ) then
      delta = delta / 2
      if ( delta < delta_tol ) exit
    end if

  end do

end subroutine f90compass_search

!ccccccccccccccc
