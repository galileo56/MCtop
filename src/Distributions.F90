
program Legendre
  use constants, only: dp; use MatrixElementsClass; use MCtopClass; implicit none

  real (dp)                              :: mt, mW, Q
  integer                                :: Nbins, Nevent, Niter, i
  character (len = 8)                    :: spin, current, method
  real (dp)                              :: moQ, Cmax
  real (dp), dimension(:,:), allocatable :: list

  read*, mt, mW, Q
  read*, Nbins, Nevent, Niter
  read*, spin, current, method

  allocate( list(Nbins,3) ); moQ = (mt/Q)**2; Cmax = 12 * moQ * (1 - 3 * moQ)

  print*, mt, mW, Q; print*,

  call f90CparamDistribution(mt, 0._dp, mW, Q, 'noexpand', method, spin, 'unstable',  &
                                current, 0._dp, Cmax, Nbins, Nevent, Niter, list)
  do i = 1, Nbins
    print*, list(i,:)
  end do

  deallocate(list)

 end program Legendre
