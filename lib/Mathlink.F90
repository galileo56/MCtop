
!ccccccccccccccc

subroutine f90StableDistributions(mt, Q, oriented, method, Nlin, Nlog, Nevent, &
                                  Niter, listLin, listLog)
  use constants, only: dp; use MatrixElementsClass; use MCtopClass; implicit none
  real (dp)                        , intent(in)  :: mt, Q
  integer                          , intent(in)  :: Nlin, Nlog, Nevent, Niter
  character (len = *)              , intent(in)  :: oriented, method
  real (dp), dimension(Nlin, 8, 5) , intent(out) :: listlin
  real (dp), dimension(Nlog, 8, 5) , intent(out) :: listlog
  type (MCStable)                                :: MC
  type (MatrixStable)                            :: MatEl

  MatEl = MatrixStable(oriented(:3), mt, Q)

  MC   = MCStable(MatEl, Nlin, Nlog, Nevent, Niter)
  call MC%callVegasStable(method(:5), listLin, listLog)

end subroutine f90StableDistributions

!ccccccccccccccc

subroutine f90MatrixElements(m, Q, h1, h2, oriented, ME)
  use constants, only: dp; use MatrixElementsClass; implicit none
  real (dp)              , intent(in ) :: m, Q, h1, h2
  character (len = *)    , intent(in ) :: oriented
  real (dp), dimension(2), intent(out) :: ME
  real (dp), dimension(16)             :: ES
  type (MatrixStable)                  :: MatEl

  MatEl = MatrixStable(oriented(:3), m, Q)

  call MatEl%MatElComputer(h1, h2, ME, ES)

end subroutine f90MatrixElements

!ccccccccccccccc

subroutine f90EShape(m, Q, h1, h2, ES)
  use constants, only: dp; use MatrixElementsClass; implicit none
  real (dp)               , intent(in ) :: m, Q, h1, h2
  real (dp), dimension(16), intent(out) :: ES
  real (dp), dimension(2)               :: ME
  type (MatrixStable)                   :: MatEl

  MatEl = MatrixStable('no', m, Q)

  call MatEl%MatElComputer(h1, h2, ME, ES)

end subroutine f90EShape

!ccccccccccccccc

subroutine f90ESMax(m, Q, ES)
  use constants, only: dp; use MatrixElementsClass; implicit none
  real (dp)               , intent(in ) :: m, Q
  real (dp), dimension(16), intent(out) :: ES
  type (MatrixStable)                   :: MatEl

  MatEl = MatrixStable('no', m, Q);  ES = MatEl%ESMax()

end subroutine f90ESMax

!ccccccccccccccc

subroutine f90ESMin(m, Q, ES)
  use constants, only: dp; use MatrixElementsClass; implicit none
  real (dp)               , intent(in ) :: m, Q
  real (dp), dimension(16), intent(out) :: ES
  type (MatrixStable)                   :: MatEl

  MatEl = MatrixStable('no', m, Q);  ES = MatEl%ESMin()

end subroutine f90ESMin

!ccccccccccccccc

subroutine f90LegendreList(n, x, res)
  use constants, only: dp; use Legendre
  integer                  , intent(in)  :: n
  real (dp)                , intent(in)  :: x
  real (dp), dimension(0:n), intent(out) :: res

  res = LegendreList(n,x)

end subroutine f90LegendreList

!ccccccccccccccc

subroutine f90ESList(mt, mb, mW, Q, ESmin, ESmax, Nbins, list)
  use constants, only: dp; use MatrixElementsClass; use MCtopClass; implicit none
  real (dp)                     , intent(in)  :: mt, mW, mb, Q
  integer                       , intent(in)  :: Nbins
  real (dp), dimension(8)       , intent(in)  :: ESmin, ESmax
  real (dp), dimension(Nbins, 8), intent(out) :: list
  type (MCUnstable)                        :: MC
  type (MatrixElements4)                      :: MatEl

  MatEl = MatrixElements4(mt, mb, mW, Q)
  MC    = MCUnstable(MatEl, 'uncorr', 'vector', ESmin, ESmax, Nbins, 0, 0)
  list  = MC%ESlist()

end subroutine f90ESList

!ccccccccccccccc

subroutine f90ESListStable(mt, Q, Nbins, list)
  use constants, only: dp; use MatrixElementsClass; use MCtopClass; implicit none
  real (dp)                      , intent(in)  :: mt, Q
  integer                        , intent(in)  :: Nbins
  real (dp), dimension(Nbins, 16), intent(out) :: list
  type (MCStable)                              :: MC
  type (MatrixStable)                          :: MatEl

  MatEl = MatrixStable('no', mt, Q)
  MC    = MCStable(MatEl, Nbins, 1, 0, 0)
  list  = MC%ESlist()

end subroutine f90ESListStable

!ccccccccccccccc

subroutine f90CparamList(mt, mb, mW, Q, Cmin, Cmax, Nbins, list)
  use constants, only: dp; use MatrixElementsClass; use MCtopClass; implicit none
  real (dp)                  , intent(in)  :: mt, mW, mb, Q
  integer                    , intent(in)  :: Nbins
  real (dp)                  , intent(in)  :: Cmin, Cmax
  real (dp), dimension(Nbins), intent(out) :: list
  type (MCUnstable)                     :: MC
  type (MatrixElements4)                   :: MatEl

  MatEl = MatrixElements4(mt, mb, mW, Q)
  MC    = MCUnstable(MatEl, 'uncorr', 'vector', [1,1,1,1,1,1,1,1] * Cmin, &
                [1,1,1,1,1,1,1,1] * Cmax, Nbins, 0, 0)
  list  = MC%Cparamlist()

end subroutine f90CparamList

!ccccccccccccccc

subroutine f90ESDistributions(mt, mb, mW, Q, method, Spin, decay, current, ESmin, &
                              ESmax, Nbins, Nevent, Niter, list)
  use constants, only: dp; use MatrixElementsClass; use MCtopClass; implicit none
  real (dp)                        , intent(in)  :: mt, mW, mb, Q
  integer                          , intent(in)  :: Nbins, Nevent, Niter
  character (len = *)              , intent(in)  :: Spin, decay, method, current
  real (dp), dimension(8)          , intent(in)  :: ESmin, ESmax
  real (dp), dimension(Nbins, 8, 3), intent(out) :: list
  type (MCUnstable)                           :: MC
  class (MatrixUnstable), allocatable            :: MatEl

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

  MC   = MCUnstable(MatEl, Spin(:8), current(:8), ESmin, ESmax, Nbins, Nevent, Niter)
  list = MC%list( method(:5) )

end subroutine f90ESDistributions

!ccccccccccccccc

subroutine f90ESLegendre(mt, mb, mW, Q, method, Spin, decay, current, ESmin, &
                         ESmax, n, Nevent, Niter, list)
  use constants, only: dp; use MatrixElementsClass; use MCtopClass; implicit none
  real (dp)                        , intent(in)  :: mt, mW, mb, Q
  integer                          , intent(in)  :: n, Nevent, Niter
  character (len = *)              , intent(in)  :: Spin, decay, method, current
  real (dp), dimension(8)          , intent(in)  :: ESmin, ESmax
  real (dp), dimension(0:n, 8, 2)  , intent(out) :: list
  real (dp), dimension(1  , 8, 3)                :: list2
  type (MCUnstable)                           :: MC
  class (MatrixUnstable), allocatable            :: MatEl

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

  MC   = MCUnstable(MatEl, Spin(:8), current(:8), ESmin, ESmax, 1, Nevent, Niter)
  call MC%callVegas( n, method(:5), list2, list )

end subroutine f90ESLegendre

!ccccccccccccccc

subroutine f90CparamDistribution(mt, mb, mW, Q, expand, method, spin, decay, current, &
                                 Cmin, Cmax, Nbins, Nevent, Niter, list)
  use constants, only: dp; use MatrixElementsClass; use MCtopClass; implicit none
  real (dp)                     , intent(in)  :: mt, mW, mb, Q
  integer                       , intent(in)  :: Nbins, Nevent, Niter
  character (len = *)           , intent(in)  :: spin, decay, current, method, expand
  real (dp)                     , intent(in)  :: Cmin, Cmax
  real (dp), dimension(Nbins, 3), intent(out) :: list
  type (MCUnstable)                                :: MC
  class (MatrixUnstable), allocatable         :: MatEl

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

  MC   = MCUnstable(MatEl, spin(:8), current(:8), [1,1,1,1,1,1,1,1] * Cmin, &
               [1,1,1,1,1,1,1,1] * Cmax, Nbins, Nevent, Niter)
  list = MC%ListCparam( expand(:6), method(:5) )

end subroutine f90CparamDistribution

!ccccccccccccccc

subroutine f90CparamLegendreDistro(mt, mb, mW, Q, expand, method, spin, decay, current, &
                                 Cmin, Cmax, n, Nbins, Nevent, Niter, list, list2)
  use constants, only: dp; use MatrixElementsClass; use MCtopClass; implicit none
  real (dp)                     , intent(in)  :: mt, mW, mb, Q
  integer                       , intent(in)  :: Nbins, Nevent, Niter, n
  character (len = *)           , intent(in)  :: spin, decay, current, method, expand
  real (dp)                     , intent(in)  :: Cmin, Cmax
  real (dp), dimension(Nbins, 3), intent(out) :: list
  real (dp), dimension(0:n  , 2), intent(out) :: list2
  type (MCUnstable)                        :: MC
  class (MatrixUnstable), allocatable         :: MatEl

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

  MC   = MCUnstable(MatEl, spin(:8), current(:8), [1,1,1,1,1,1,1,1] * Cmin, &
               [1,1,1,1,1,1,1,1] * Cmax, Nbins, Nevent, Niter)
  call MC%callVegasCparam( n, expand(:6), method(:5), list, list2 )

end subroutine f90CparamLegendreDistro

!ccccccccccccccc

subroutine f90CparamLegendre(n, mt, mb, mW, Q, expand, method, spin, decay, current, &
                             Cmin, Cmax, Nevent, Niter, list)
  use constants, only: dp; use MatrixElementsClass; use MCtopClass; implicit none
  real (dp)                  , intent(in)  :: mt, mW, mb, Q
  integer                    , intent(in)  :: n, Nevent, Niter
  character (len = *)        , intent(in)  :: spin, decay, current, method, expand
  real (dp)                  , intent(in)  :: Cmin, Cmax
  real (dp), dimension(2,0:n), intent(out) :: list
  real (dp), dimension(1,3)                :: list2
  type (MCUnstable)                     :: MC
  class (MatrixUnstable), allocatable      :: MatEl

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

  MC   = MCUnstable(MatEl, spin(:8), current(:8), [1,1,1,1,1,1,1,1] * Cmin, &
               [1,1,1,1,1,1,1,1] * Cmax, 1, Nevent, Niter)
  call MC%callVegasCparam( n, expand(:6), method(:5), list2, list )

end subroutine f90CparamLegendre

!ccccccccccccccc

subroutine f90CparamComputer(p, len, ES)
  use constants, only: dp; use MatrixElementsClass, only: Cparam; implicit none
  integer                     , intent(in)  :: len
  real (dp), dimension(len, 4), intent(in)  :: p
  real (dp)                   , intent(out) :: ES

  ES = Cparam(p)

end subroutine f90CparamComputer

!ccccccccccccccc

subroutine f90CparamBeta4(x, mt, mb, mW, Q, ES)
  use constants, only: dp; use MatrixElementsClass; implicit none
  real (dp),               intent(in)  :: mt, mb, mW, Q
  real (dp), dimension(3), intent(in)  :: x
  real (dp)              , intent(out) :: ES
  type (MatrixElements4)               :: MatEl

  MatEl = MatrixElements4(mt, mb, mW, Q)
  ES    = MatEl%CparamBeta(x)

end subroutine f90CparamBeta4

!ccccccccccccccc

subroutine f90Cparam4(x, mt, mb, mW, Q, ES)
  use constants, only: dp; use MatrixElementsClass; implicit none
  real (dp),               intent(in)  :: mt, mb, mW, Q
  real (dp), dimension(3), intent(in)  :: x
  real (dp), dimension(4,0:3)          :: p
  real (dp)              , intent(out) :: ES
  type (MatrixElements4)               :: MatEl

  MatEl = MatrixElements4(mt, mb, mW, Q)
  p     = MatEl%GenerateVectors(x)
  ES    = Cparam(p)

end subroutine f90Cparam4

!ccccccccccccccc

subroutine f90Cparam6(x, mt, mb, mW, Q, ES)
  use constants, only: dp; use MatrixElementsClass; implicit none
  real (dp),               intent(in)  :: mt, mb, mW, Q
  real (dp), dimension(7), intent(in)  :: x
  real (dp), dimension(6,0:3)          :: p
  real (dp)              , intent(out) :: ES
  type (MatrixElements6)               :: MatEl

  MatEl = MatrixElements6(mt, mb, mW, Q)
  p     = MatEl%GenerateVectors(x)
  ES    = Cparam(p)

end subroutine f90Cparam6

!ccccccccccccccc

subroutine f90CparamBeta6(x, mt, mb, mW, Q, ES)
  use constants, only: dp; use MatrixElementsClass; implicit none
  real (dp),               intent(in)  :: mt, mb, mW, Q
  real (dp), dimension(7), intent(in)  :: x
  real (dp)              , intent(out) :: ES
  type (MatrixElements6)               :: MatEl

  MatEl = MatrixElements6(mt, mb, mW, Q)
  ES    = MatEl%CparamBeta(x)

end subroutine f90CparamBeta6

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
  integer                  , intent(in)  :: n
  real (dp)                , intent(in)  :: mt, mW, mb, Q
  real (dp), dimension(4,2), intent(out) :: res
  type (MatrixElements4)                 :: MatEl

  MatEl = MatrixElements4(mt, mb, mW, Q)
  res   = MatEl%CparamMinMax(n)

end subroutine f90CparamMinMax4

!ccccccccccccccc

subroutine f90CparamMaxMin4(eps, mt, mb, mW, Q, res)
  use constants, only: dp; use MatrixElementsClass; implicit none
  real (dp)                , intent(in)  :: mt, mW, mb, Q, eps
  real (dp), dimension(4,2), intent(out) :: res
  type (MatrixElements4)                 :: MatEl

  MatEl = MatrixElements4(mt, mb, mW, Q)
  res   = MatEl%CparamMaxMin(eps)

end subroutine f90CparamMaxMin4

!ccccccccccccccc

subroutine f90ESMaxMin4(eps, mt, mb, mW, Q, res)
  use constants, only: dp; use MatrixElementsClass; implicit none
  real (dp)                  , intent(in)  :: mt, mW, mb, Q, eps
  real (dp), dimension(4,8,2), intent(out) :: res
  type (MatrixElements4)                   :: MatEl

  MatEl = MatrixElements4(mt, mb, mW, Q)
  res   = MatEl%ESMaxMin(eps)

end subroutine f90ESMaxMin4

!ccccccccccccccc

subroutine f90ESMaxMin6(eps, mt, mb, mW, Q, res)
  use constants, only: dp; use MatrixElementsClass; implicit none
  real (dp)                  , intent(in)  :: mt, mW, mb, Q, eps
  real (dp), dimension(8,8,2), intent(out) :: res
  type (MatrixElements6)                   :: MatEl

  MatEl = MatrixElements6(mt, mb, mW, Q)
  res   = MatEl%ESMaxMin(eps)

end subroutine f90ESMaxMin6

!ccccccccccccccc

subroutine f90CparamMaxMin6(eps, mt, mb, mW, Q, res)
  use constants, only: dp; use MatrixElementsClass; implicit none
  real (dp)                , intent(in)  :: mt, mW, mb, Q, eps
  real (dp), dimension(8,2), intent(out) :: res
  type (MatrixElements6)                 :: MatEl

  MatEl = MatrixElements6(mt, mb, mW, Q)
  res   = MatEl%CparamMaxMin(eps)

end subroutine f90CparamMaxMin6

!ccccccccccccccc

subroutine f90CparamMinMax6(n, mt, mb, mW, Q, res)
  use constants, only: dp; use MatrixElementsClass; implicit none
  integer                  , intent(in)  :: n
  real (dp)                , intent(in)  :: mt, mW, mb, Q
  real (dp), dimension(8,2), intent(out) :: res
  type (MatrixElements6)                 :: MatEl

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
  type (MatrixElements6)                 :: MatEl

  MatEl = MatrixElements6(mt, mb, mW, Q)
  p     = MatEl%GenerateVectors(x)

end subroutine f90Vectors6

!ccccccccccccccc

subroutine f90RestVectors4(x, mt, mb, mW, Q, p)
  use constants, only: dp; use MatrixElementsClass; implicit none
  real (dp), dimension(3)  , intent(in)  :: x
  real (dp)                , intent(in)  :: mt, mW, mb, Q
  real (dp), dimension(4,4), intent(out) :: p
  type (MatrixElements4)                 :: MatEl

  MatEl = MatrixElements4(mt, mb, mW, Q)
  p     = MatEl%GenerateRestVectors(x)

end subroutine f90RestVectors4

!ccccccccccccccc

subroutine f90RestVectors6(x, mt, mb, mW, Q, p)
  use constants, only: dp; use MatrixElementsClass; implicit none
  real (dp), dimension(7)  , intent(in)  :: x
  real (dp)                , intent(in)  :: mt, mW, mb, Q
  real (dp), dimension(6,4), intent(out) :: p
  type (MatrixElements6)                 :: MatEl

  MatEl = MatrixElements6(mt, mb, mW, Q)
  p     = MatEl%GenerateRestVectors(x)

end subroutine f90RestVectors6

!ccccccccccccccc
