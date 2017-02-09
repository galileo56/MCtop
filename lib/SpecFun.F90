module Constants
  use iso_fortran_env, only: error_unit, dp => real64
  implicit none
  real (dp), parameter :: Pi    = 3.141592653589793_dp , ExpEuler = 1.781072417990198_dp,&
                          Zeta3 = 1.2020569031595942   , Zeta2 = 1.6449340668482262_dp,  &
                          Euler = 0.5772156649015329_dp, l2  = 0.6931471805599453_dp,    &
                          Pio2  = 1.5707963267948966_dp, Pi2 = 9.869604401089358_dp ,    &
                          sr2   = 1.4142135623730951_dp, prec = 1e-10_dp

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
