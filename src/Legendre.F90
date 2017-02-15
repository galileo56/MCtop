
program Legendre
  use constants, only: dp; use MatrixElementsClass; use MCtopClass; implicit none

  real (dp)                              :: mt, mW, Q
  integer                                :: n, Nevent, Niter, i
  character (len = 8)                    :: spin, current, method
  real (dp)                              :: moQ, Cmax
  real (dp), dimension(:,:), allocatable :: list

  read*, mt, mW, Q
  read*, n, Nevent, Niter
  read*, spin, current, method

  allocate( list(2,0:n) ); moQ = (mt/Q)**2; Cmax = 12 * moQ * (1 - 3 * moQ)

  print*, mt, mW, Q, mt/Q; print*,

  call f90CparamLegendre(n, mt, 0._dp, mW, Q, 'noexpand', method, spin, 'unstable', &
                         current, 0._dp, Cmax, Nevent, Niter, list)
  do i = 0, n
    write( *, '(I2,5F18.6)' ) i, list(:,i)
  end do

  deallocate(list)

 end program Legendre
