
program ThrustDistro
  use constants, only: dp; use MatrixElementsClass; use MCtopClass; implicit none

  real (dp)                              :: mt, mb, mW, Q
  integer                                :: Nbins, Nevent, Niter, i
  character (len = 8)                    :: spin, current, method
  real (dp)                              :: moQ, Tmax
  real (dp), dimension(2)                :: delta
  real (dp), dimension(:,:), allocatable :: list

  read*, mt, mb, mW, Q;  read*, Nbins, Nevent, Niter;  read*, spin, current, method

  allocate( list(Nbins,3) ); moQ = (mt/Q)**2; Tmax = 1 - sqrt(1 - 4 * moQ )

  write( *, '(5F18.6)' ) mt, mW, Q, mt/Q, Tmax

  call f90DistroThrust(mt, mb, mW, Q, method, spin, 'unstable', current, &
  0._dp, Tmax, Nbins, Nevent, Niter, list, delta)

  print*, ; print*, 'delta coefficient'; print*,

  write( *, '(2F18.6)' ) delta; print*; print*, 'Interpolation'; print*,

  do i = 1, Nbins
    write( *, '(3F18.6)' ) list(i,:)
  end do

  deallocate(list)

 end program ThrustDistro
