
program Event1Run
  use constants, only: dp; use MCtopClass, only: StableNames; implicit none

  character (len = 5)                      :: oriented, method
  integer                                  :: i, k, power
  real (dp), allocatable, dimension(:,:,:) :: distLin, distLog
  real (dp)                                :: mt, Q
  integer                                  :: Nevent, Niter, Nlin, Nlog

  read *, oriented
  read *, mt, Q
  read *, Nevent, Niter, Nlin, Nlog

  allocate( distLin(Nlin, 16, 5), distLog(Nlog, 16, 5) )

  call f90StableDistributions(mt, Q, oriented(:3), method(:5), power, Nlin, Nlog, &
                                    Nevent, Niter, distLin, distLog)

  print*, Nevent, Niter, Nlin, Nlog, 16, mt; print*, ; print*, 'linear binning'

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

 end program Event1Run
