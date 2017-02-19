
program Event1Run
  use MatrixElementsClass; use Event1Class; implicit none

  character (len = 2)        , dimension(6)      :: oriented
  integer                                        :: i, k
  real (kind = 8), allocatable, dimension(:,:,:) :: distLin, distLog
  real (kind = 8)                                :: m
  integer                                        :: Nevent, Niter, Nlin, Nlog

  type (MatrixElements)                          :: MatEl
  type (Event1        )                          :: MC

  read *, arg
  read *, m, Q
  read *, Nevent, Niter, Nlin, Nlog

  allocate( distLin(Nlin, 16, 5), distLog(Nlog, 16, 5) )

  MatEl = MatrixElements(arg(6)(:3), m);  MC = Event1(MatEl, Nlin, Nlog, Nevent, Niter)

  distLin = MC%ListLin();  distLog = MC%ListLog()

  print*, Nevent, Niter, Nlin, Nlog, 16, m; print*, ; print*, 'linear binning'

  do k = 1, 16

    print*, ; print*, ESNames(k); print*,

    do i = 1, Nlin

      write(*,'(5F18.6)') distLin(i, k, :)

    end do
  end do

  print*, 'log binning'

  do k = 1, 16

    print*, ; print*, ESNames(k); print*,

    do i = 1, Nlog

      write(*,'(5F18.6)') distLog(i, k, :)

    end do
  end do

  deallocate(distLin, distLog)

 end program Event1Run
