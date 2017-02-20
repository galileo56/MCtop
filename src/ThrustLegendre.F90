
program ThrustLegendre
  use constants, only: dp; use MatrixElementsClass; use MCtopClass; implicit none

  real (dp)                              :: mt, mb, mW, Q
  integer                                :: n, Nevent, Niter, i
  character (len = 8)                    :: spin, current, method
  real (dp)                              :: moQ, Tmax
  real (dp), dimension(2)                :: delta
  real (dp), dimension(:,:), allocatable :: list

  read*, mt, mb, mW, Q;  read*, n, Nevent, Niter;  read*, spin, current, method

  allocate( list(0:n,2) ); moQ = (mt/Q)**2; Tmax = 1 - sqrt(1 - 4 * moQ )

  write( *, '(5F18.6)' ) mt, mW, Q, mt/Q, Tmax

  call f90VegasThrust(n, mt, mb, mW, Q, method, spin, 'unstable', current, &
  0._dp, Tmax, Nevent, Niter, list, delta)

  print*, 'delta coefficient'; print*,

  write( *, '(2F18.6)' ) delta; print*, 'Legendre coefficients'; print*,

  do i = 0, n
    write( *, '(I2,2F18.6)' ) i, list(i,:)
  end do

  deallocate(list)

 end program ThrustLegendre
