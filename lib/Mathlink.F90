
!ccccccccccccccc

subroutine f90ESList(mt, mb, mW, Q, ESmax, Nbins, list)
  use constants, only: dp; use MatrixElementsClass; use MCtopClass; implicit none
  real (dp)                     , intent(in)  :: mt, mW, mb, Q
  integer                       , intent(in)  :: Nbins
  real (dp), dimension(8)       , intent(in)  :: ESmax
  real (dp), dimension(Nbins, 8), intent(out) :: list
  type (MCtop)                                :: MC
  type (MatrixElements4)                      :: MatEl

  MatEl = MatrixElements4(mt, mb, mW, Q)
  MC    = MCtop(MatEl, 'uncorr', 'vector', ESmax, Nbins, 0, 0)
  list  = MC%ESlist()

end subroutine f90ESList

!ccccccccccccccc

subroutine f90CparamList(mt, mb, mW, Q, Cmax, Nbins, list)
  use constants, only: dp; use MatrixElementsClass; use MCtopClass; implicit none
  real (dp)                  , intent(in)  :: mt, mW, mb, Q
  integer                    , intent(in)  :: Nbins
  real (dp)                  , intent(in)  :: Cmax
  real (dp), dimension(Nbins), intent(out) :: list
  type (MCtop)                             :: MC
  type (MatrixElements4)                   :: MatEl

  MatEl = MatrixElements4(mt, mb, mW, Q)
  MC    = MCtop(MatEl, 'uncorr', 'vector', [1,1,1,1,1,1,1,1] * Cmax, Nbins, 0, 0)
  list  = MC%Cparamlist()

end subroutine f90CparamList

!ccccccccccccccc

subroutine f90ESDistributions(mt, mb, mW, Q, Spin, decay, current, ESmax, Nbins, &
                              Nevent, Niter, list)
  use constants, only: dp; use MatrixElementsClass; use MCtopClass; implicit none
  real (dp)                        , intent(in)  :: mt, mW, mb, Q
  integer                          , intent(in)  :: Nbins, Nevent, Niter
  character (len = *)              , intent(in)  :: Spin, decay, current
  real (dp), dimension(8)          , intent(in)  :: ESmax
  real (dp), dimension(Nbins, 8, 3), intent(out) :: list
  type (MCtop)                                   :: MC
  class (MatrixElements), allocatable            :: MatEl

  if ( decay(:6) == 'stable') then
    allocate( MatrixElements4 :: MatEl )
    select type (MatEl)
    type is (MatrixElements4);  MatEl = MatrixElements4(mt, mb, mW, Q)
    end select
  else
    allocate( MatrixElements6 :: MatEl )
    select type (MatEl)
    type is (MatrixElements6);  MatEl = MatrixElements6(mt, mb, mW, Q)
    end select
  end if

  MC   = MCtop(MatEl, Spin, current, ESmax, Nbins, Nevent, Niter)
  list = MC%list()

end subroutine f90ESDistributions

!ccccccccccccccc

subroutine f90CparamDistribution(mt, mb, mW, Q, Spin, decay, current, Cmax, &
                                 Nbins, Nevent, Niter, list)
  use constants, only: dp; use MatrixElementsClass; use MCtopClass; implicit none
  real (dp)                     , intent(in)  :: mt, mW, mb, Q
  integer                       , intent(in)  :: Nbins, Nevent, Niter
  character (len = *)           , intent(in)  :: Spin, decay, current
  real (dp)                     , intent(in)  :: Cmax
  real (dp), dimension(Nbins, 3), intent(out) :: list
  type (MCtop)                                :: MC
  class (MatrixElements), allocatable         :: MatEl

  if ( decay(:6) == 'stable') then
    allocate( MatrixElements4 :: MatEl )
    select type (MatEl)
    type is (MatrixElements4);  MatEl = MatrixElements4(mt, mb, mW, Q)
    end select
  else
    allocate( MatrixElements6 :: MatEl )
    select type (MatEl)
    type is (MatrixElements6);  MatEl = MatrixElements6(mt, mb, mW, Q)
    end select
  end if

  MC   = MCtop(MatEl, Spin, current, [1,1,1,1,1,1,1,1] * Cmax, Nbins, Nevent, Niter)
  list = MC%ListCparam()

end subroutine f90CparamDistribution

!ccccccccccccccc

subroutine f90CparamComputer(p, len, ES)
  use constants, only: dp; use MatrixElementsClass, only: Cparam; implicit none
  integer                     , intent(in)  :: len
  real (dp), dimension(len, 4), intent(in)  :: p
  real (dp)                   , intent(out) :: ES

  ES = Cparam(p)

end subroutine f90CparamComputer

!ccccccccccccccc

subroutine f90EScomputer(p, len, ES)
  use constants, only: dp; use MatrixElementsClass, only: EScomputer; implicit none
  integer                     , intent(in) :: len
  real (dp), dimension(len, 4), intent(in) :: p
  real (dp), dimension(8)    , intent(out) :: ES

  ES = EScomputer(p)

end subroutine f90EScomputer

!ccccccccccccccc

subroutine f90CparamMinMax4(n, mt, mb, mW, Q, res)
  use constants, only: dp; use MatrixElementsClass; implicit none
  integer                , intent(in)  :: n
  real (dp)              , intent(in)  :: mt, mW, mb, Q
  real (dp), dimension(2), intent(out) :: res
  type (MatrixElements4)               :: MatEl

  MatEl = MatrixElements4(mt, mb, mW, Q)
  res   = MatEl%CparamMinMax(n)

end subroutine f90CparamMinMax4

!ccccccccccccccc

subroutine f90CparamMinMax6(n, mt, mb, mW, Q, res)
  use constants, only: dp; use MatrixElementsClass; implicit none
  integer                , intent(in)  :: n
  real (dp)              , intent(in)  :: mt, mW, mb, Q
  real (dp), dimension(2), intent(out) :: res
  type (MatrixElements6)               :: MatEl

  MatEl = MatrixElements6(mt, mb, mW, Q)
  res   = MatEl%CparamMinMax(n)

end subroutine f90CparamMinMax6

!ccccccccccccccc

subroutine f90ESMinMax4(n, mt, mb, mW, Q, res)
  use constants, only: dp; use MatrixElementsClass; implicit none
  integer                  , intent(in)  :: n
  real (dp)                , intent(in)  :: mt, mW, mb, Q
  real (dp), dimension(2,8), intent(out) :: res
  type (MatrixElements4)                 :: MatEl

  MatEl = MatrixElements4(mt, mb, mW, Q)
  res   = MatEl%ESMinMax(n)

end subroutine f90ESMinMax4

!ccccccccccccccc

subroutine f90ESMinMax6(n, mt, mb, mW, Q, res)
  use constants, only: dp; use MatrixElementsClass; implicit none
  integer                  , intent(in)  :: n
  real (dp)                , intent(in)  :: mt, mW, mb, Q
  real (dp), dimension(2,8), intent(out) :: res
  type (MatrixElements6)                 :: MatEl

  MatEl = MatrixElements6(mt, mb, mW, Q)
  res   = MatEl%ESMinMax(n)

end subroutine f90ESMinMax6

!ccccccccccccccc

subroutine f90Vectors4(x, mt, mb, mW, Q, p)
  use constants, only: dp; use MatrixElementsClass; implicit none
  real (dp), dimension(3)  , intent(in)  :: x
  real (dp)                , intent(in)  :: mt, mW, mb, Q
  real (dp), dimension(4,4), intent(out) :: p
  type (MatrixElements4)                 :: MatEl

  MatEl = MatrixElements4(mt, mb, mW, Q)
  p     = MatEl%GenerateVectors(x)

end subroutine f90Vectors4

!ccccccccccccccc

subroutine f90Vectors6(x, mt, mb, mW, Q, p)
  use constants, only: dp; use MatrixElementsClass; implicit none
  real (dp), dimension(7)  , intent(in)  :: x
  real (dp)                , intent(in)  :: mt, mW, mb, Q
  real (dp), dimension(6,4), intent(out) :: p
  type (MatrixElements6)                  :: MatEl

  MatEl = MatrixElements6(mt, mb, mW, Q)
  p     = MatEl%GenerateVectors(x)

end subroutine f90Vectors6

!ccccccccccccccc
