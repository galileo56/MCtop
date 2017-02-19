
program Stable
  use constants, only: dp; use MCtopClass; implicit none

  character (len = 13)                     :: oriented, method, operation
  integer                                  :: i, k, power
  real (dp), allocatable, dimension(:,:,:) :: distLin, distLog
  real (dp)                                :: mt, Q
  integer                                  :: Nevent, Niter, Nlin, Nlog

  read *, oriented, method, operation
  read *, mt, Q
  read *, Nevent, Niter, Nlin, Nlog ! Nlog will be ignored if operation /= 'distributions'

  if ( operation(:13) == 'distributions' ) then

    allocate( distLin(Nlin, 16, 5), distLog(Nlog, 16, 5) )

    call f90StableDistributions(mt, Q, oriented(:3), method(:5), power, Nlin, Nlog, &
                                      Nevent, Niter, distLin, distLog)

    print*, Nevent, Niter, Nlin, Nlog, 16, mt; print*, ; print*, 'linear binning'
    write(*,'(I8,4I4,F8.3)') Nevent, Niter, Nlin, Nlog, 16, mt
    print*, ; print*, 'linear binning'

    do k = 1, 16

      print*, ; print*, StableNames(k); print*,

      do i = 1, Nlin
        write(*,'(5F18.6)') distLin(i, k, :)
      end do

    end do

    print*, 'log binning'

    do k = 1, 16

      print*, ; print*, StableNames(k); print*,

      do i = 1, Nlog
        write(*,'(5F18.6)') distLog(i, k, :)
      end do

    end do

    deallocate(distLin, distLog)

  else

    allocate( distLin(0:Nlin, 16, 4) )

    call f90LegendreStable(mt, Q, oriented, method, Nlin, Nevent, Niter, distLin)

    print*, Nevent, Niter, Nlin, 16, mt; print*, ; print*, 'Legendre coefficients'

    do k = 1, 16

      print*, ; print*, StableNames(k); print*,

      do i = 1, Nlin
        write(*,'(5F18.6)') distLin(i, k, :)
      end do

    end do

    deallocate(distLin)

  end if

 end program Stable
