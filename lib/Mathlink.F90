
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
  type (MatrixElements)                          :: MatEl

  MatEl = MatrixElements(mt, mb, mW, Q)
  MC    = MCtop(MatEl, Spin, decay, current, ESmax, Nbins, Nevent, Niter)

  list = MC%list()

end subroutine f90ESDistributions

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
  type (MatrixElements)                :: MatEl

  MatEl = MatrixElements(mt, mb, mW, Q)
  res   = MatEl%CparamMinMax4(n)

end subroutine f90CparamMinMax4

!ccccccccccccccc

subroutine f90CparamMinMax6(n, mt, mb, mW, Q, res)
  use constants, only: dp; use MatrixElementsClass; implicit none
  integer                , intent(in)  :: n
  real (dp)              , intent(in)  :: mt, mW, mb, Q
  real (dp), dimension(2), intent(out) :: res
  type (MatrixElements)                :: MatEl

  MatEl = MatrixElements(mt, mb, mW, Q)
  res   = MatEl%CparamMinMax6(n)

end subroutine f90CparamMinMax6

!ccccccccccccccc

subroutine f90ESMinMax4(n, mt, mb, mW, Q, res)
  use constants, only: dp; use MatrixElementsClass; implicit none
  integer                  , intent(in)  :: n
  real (dp)                , intent(in)  :: mt, mW, mb, Q
  real (dp), dimension(2,8), intent(out) :: res
  type (MatrixElements)                  :: MatEl

  MatEl = MatrixElements(mt, mb, mW, Q)
  res   = MatEl%ESMinMax4(n)

end subroutine f90ESMinMax4

!ccccccccccccccc

subroutine f90ESMinMax6(n, mt, mb, mW, Q, res)
  use constants, only: dp; use MatrixElementsClass; implicit none
  integer                  , intent(in)  :: n
  real (dp)                , intent(in)  :: mt, mW, mb, Q
  real (dp), dimension(2,8), intent(out) :: res
  type (MatrixElements)                  :: MatEl

  MatEl = MatrixElements(mt, mb, mW, Q)
  res   = MatEl%ESMinMax6(n)

end subroutine f90ESMinMax6

!ccccccccccccccc

subroutine f90Vectors4(x, mt, mb, mW, Q, p)
  use constants, only: dp; use MatrixElementsClass; implicit none
  real (dp), dimension(3)  , intent(in)  :: x
  real (dp)                , intent(in)  :: mt, mW, mb, Q
  real (dp), dimension(4,4), intent(out) :: p
  type (MatrixElements)                  :: MatEl

  MatEl = MatrixElements(mt, mb, mW, Q)
  p     = MatEl%GenerateVectors4(x)

end subroutine f90Vectors4

!ccccccccccccccc

subroutine f90Vectors6(x, mt, mb, mW, Q, p)
  use constants, only: dp; use MatrixElementsClass; implicit none
  real (dp), dimension(7)  , intent(in)  :: x
  real (dp)                , intent(in)  :: mt, mW, mb, Q
  real (dp), dimension(6,4), intent(out) :: p
  type (MatrixElements)                  :: MatEl

  MatEl = MatrixElements(mt, mb, mW, Q)
  p     = MatEl%GenerateVectors6(x)

end subroutine f90Vectors6

!ccccccccccccccc
