
program LegendreDistro
  use constants, only: dp; use MatrixElementsClass; use MCtopClass; implicit none

  real (dp)                              :: mt, mW, Q
  integer                                :: n, Nbins, Nevent, Niter, i
  character (len = 13)                   :: spin, current, method, operation
  real (dp)                              :: moQ, Cmax
  real (dp), dimension(:,:), allocatable :: list, list2

  read*, mt, mW, Q
  read*, n, Nbins, Nevent, Niter
  read*, spin, current, method

  if ( operation(:8) == 'legendre' ) then
    Nbins = 1
  else if ( operation(:13) == 'distributions' ) then
    n = 0
  end if

  allocate( list(Nbins,3), list2(0:n,2) )
  moQ = (mt/Q)**2; Cmax = 12 * moQ * (1 - 3 * moQ)

  write( *, '(4F18.6)' ) mt, mW, Q, mt/Q; print*,

  call f90CparamLegendreDistro(mt, 0._dp, mW, Q, 'noexpand', method(:5), spin(:8), &
   'unstable', current(:8), 0._dp, Cmax, n, Nbins, Nevent, Niter, list, list2)

  if ( operation(:8) /= 'legendre' ) then
    print*, 'Distribution'; print*,

    do i = 1, Nbins
      write( *, '(3F18.6)' ) list(i,:)
    end do

  end if

  if ( operation(:13) /= 'distributions' ) then

    print*, ; print*, 'Legendre Coefficients'; print*,

    do i = 0, n
      write( *, '(I2,2F18.6)' ) i, list2(i,:)
    end do

  end if

  deallocate(list, list2)

 end program LegendreDistro
