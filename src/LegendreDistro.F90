
program LegendreDistro
  use constants, only: dp; use MatrixElementsClass; use MCtopClass; implicit none

  real (dp)                              :: mt, mW, Q
  integer                                :: n, Nbins, Nevent, Niter, i
  character (len = 8)                    :: spin, current, method
  real (dp)                              :: moQ, Cmax
  real (dp), dimension(:,:), allocatable :: list, list2

  read*, mt, mW, Q
  read*, n, Nbins, Nevent, Niter
  read*, spin, current, method

  allocate( list(Nbins,3), list2(0:n, 2) )
  moQ = (mt/Q)**2; Cmax = 12 * moQ * (1 - 3 * moQ)

  print*, mt, mW, Q; print*,; print*, 'Distribution'

  call f90CparamLegendreDistro(mt, 0._dp, mW, Q, 'noexpand', method, spin, 'unstable',  &
                        current, 0._dp, Cmax, n, Nbins, Nevent, Niter, list, list2)

  do i = 1, Nbins
    print*, list(i,:)
  end do

  print*, 'Legendre Coefficients'

  do i = 0, n
    write( *, '(I2,5F18.6)' ) i, list2(:,i)
  end do

  deallocate(list, list2)

 end program LegendreDistro
