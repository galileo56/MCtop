
module MCtopClass
  use MatrixElementsClass; use constants, only: dp, d1mach; use MC_VEGAS
  use Legendre;  implicit none;  private

  public                                      :: MCtop
  character (len = 11), dimension(8) , public :: ESNames
  character (len = 11), dimension(16), public :: StableNames

  data ESNames     /'tau', 'HJM' , 'LJM', 'SJM', 'C-parameter', 'B', 'B-wide', 'B-narrow'/
  data StableNames /'tau', 'tauQ', 'tauE', 'tauJ', 'CJ', 'CE', 'CQ', 'CP', &
  'rho', 'rhoE', 'rhoQ', 'rhoP', 'rhoSum', 'Broadening ', 'BroadeningQ', 'BroadeningE' /

!ccccccccccccccc

  type, abstract, public :: MCtop
    integer                    , private              :: Nbins, Nevent, Niter
    class (MatrixElements)     , private, allocatable :: MatEl
    real (dp), dimension(:,:  ), private, allocatable :: ES
    real (dp), dimension(:)    , private, allocatable :: ESmax, ESmin, delta
    integer                    , private              :: dimX, dimP, dimES

  contains

    procedure, private                                :: callVegas
    procedure, public                                 :: List, ESlist

  end type MCtop

!ccccccccccccccc

  type, extends (MCtop), public :: MCtopUnstable
    private
    character (len = 8)        , private              :: spin, current

  contains

    final                                             :: delete_object
    procedure, public                                 :: LegendreDistro, CparamList, ListCparam

  end type MCtopUnstable

!ccccccccccccccc

  type, extends (MCtop), public :: MCStable
    private
    integer                            , private    :: Nlog
    real (dp), dimension(:,:), private, allocatable :: ESlog
    real (dp), dimension(16 ), private              :: logMin, DeltaLog, logMax

   contains

    final                                                   :: delete_stable
    ! procedure                                               :: ListLinLog

  end type MCStable

!ccccccccccccccc

  interface MCtopUnstable
    module procedure InMCtop
  end interface MCtopUnstable

!ccccccccccccccc

  interface MCStable
    module procedure InitEvent1
  end interface MCStable

  contains

!ccccccccccccccc

   subroutine delete_object(this)
     type (MCtopUnstable) :: this

     if ( allocated(this%ES   ) ) deallocate(this%ES  )
     if ( allocated(this%ESMin) ) deallocate(this%ESMin  )
     if ( allocated(this%ESMax) ) deallocate(this%ESMax  )
     if ( allocated(this%MatEl) ) deallocate(this%MatEl  )
     if ( allocated(this%delta) ) deallocate(this%delta  )

   end subroutine delete_object

!ccccccccccccccc

   subroutine delete_stable(this)
     type (MCStable) :: this

     if ( allocated(this%ES   ) ) deallocate(this%ES  )
     if ( allocated(this%ESMin) ) deallocate(this%ESMin  )
     if ( allocated(this%ESMax) ) deallocate(this%ESMax  )
     if ( allocated(this%MatEl) ) deallocate(this%MatEl  )
     if ( allocated(this%delta) ) deallocate(this%delta  )
     if ( allocated(this%ESlog) ) deallocate(this%ESlog  )

   end subroutine delete_stable

!ccccccccccccccc

   type (MCStable) function InitEvent1(MatEl, Nbins, Nlog, Nevent, Niter)
     integer            , intent(in) :: Nbins, Nlog, Nevent, Niter
     type (MatrixStable), intent(in) :: MatEl
     real (dp)       , dimension(16) :: DeltaES, DeltaLog, ESMin, ESMax, LogMin, LogMax, delta
     integer                         :: i

     InitEvent1%Nbins  = Nbins   ; InitEvent1%Nlog  = Nlog; InitEvent1%dimES = 16
     InitEvent1%Nevent = Nevent  ; InitEvent1%Niter = Niter

     allocate( MatrixStable :: InitEvent1%MatEl )
     select type (selector => InitEvent1%MatEl)
       type is (MatrixStable);  selector = MatEl
     end select

     allocate( InitEvent1%ES(Nbins, 16), InitEvent1%ESlog(Nlog, 16) )
     allocate( InitEvent1%ESMin(16), InitEvent1%ESMax(16), InitEvent1%delta(16) )

     ESMin  = MatEl%ESMin();  ESmax  = MatEl%ESMax();  Delta    = (ESmax  - ESmin )/Nbins
     LogMin = - 5          ;  LogMax = 1            ;  DeltaLog = (LogMax - LogMin)/Nlog

     InitEvent1%Delta = Delta;  InitEvent1%DeltaLog = DeltaLog

     do i = 1, Nbins
       InitEvent1%ES(i,:) = ESMin + Delta * (2 * i - 1)/2
     end do

     do i = 1, Nlog
       InitEvent1%ESlog(i,:) = LogMin + DeltaLog * (2 * i - 1)/2
     end do

     InitEvent1%ESMin  = ESMin ; InitEvent1%ESmax  = ESmax
     InitEvent1%LogMin = - 5   ; InitEvent1%LogMax = 1

   end function InitEvent1

!ccccccccccccccc

   type (MCtopUnstable) function InMCtop(MatEl, Spin, current, ESmin, ESmax, Nbins, Nevent, Niter)
     class (MatrixElements) , intent(in) :: MatEl
     character (len = *)    , intent(in) :: Spin, current
     integer                , intent(in) :: Nbins, Nevent, Niter
     real (dp), dimension(8), intent(in) :: ESmin, ESmax
     real (dp), dimension(8)             :: delta
     integer                             :: i

     allocate( InMCtop%ES(Nbins, 8),  InMCtop%ESMin(8), InMCtop%ESMax(8), InMCtop%delta(8) )

     InMCtop%Nbins = Nbins ; InMCtop%Nevent  = Nevent ; InMCtop%Niter = Niter
     InMCtop%Spin  = Spin  ; InMCtop%current = current; InMCtop%ESmin = ESmin

     select type (MatEl)
     type is (MatrixElements6)
       allocate( MatrixElements6 :: InMCtop%MatEl )
       select type (selector => InMCtop%MatEl)
         type is (MatrixElements6);  selector = MatEl
       end select
     type is (MatrixElements4)
       allocate( MatrixElements4 :: InMCtop%MatEl )
       select type (selector => InMCtop%MatEl)
       type is (MatrixElements4);  selector = MatEl
       end select
     end select

     delta = (ESmax - ESmin)/Nbins; InMCtop%delta = delta;  InMCtop%ESmax = ESmax
     InMCtop%dimX = MatEl%dimX(); InMCtop%dimP = MatEl%dimP()

     do i = 1, Nbins
       InMCtop%ES(i,:) = ESmin + Delta * (2 * i - 1)/2
     end do

     InMCtop%dimES = 8

   end function InMCtop

!ccccccccccccccc

  subroutine callVegas(self, method, dist)
    class (MCtop), intent(in)                           :: self
    character (len = *)                   , intent(in)  :: method
    real (dp), dimension(self%Nbins, 8, 2), intent(out) :: dist
    real (dp), dimension(self%Nbins, 8, self%Niter)     :: distTot , distTot2
    real (dp), dimension(self%dimX)                     :: y
    real (dp)                                           :: AVGI, SD, CHI2A
    integer                                             :: i, j, n, iter

    NPRN = - 1; ITMX = 1; NCall = self%Nevent; iter = 1
    if (self%dimX <= 3) iter = 0; distTot = 0; distTot2 = 0

    do j = 1, self%Niter

      dist = 0

      if ( method(:5) == 'vegas' ) then
        if (j > 1 .and. self%dimX > 3) iter = 2
        call VEGAS(self%dimX, FunMatEl, AVGI, SD, CHI2A, iter)
      else
        do n = 1, NCall
          call Random_number(y)
          AVGI = AVGI + FunMatEl(y, 1._dp)
        end do
        AVGI = AVGI/self%Nevent; dist = dist/self%Nevent
      end if

      do i = 1, 8

        distTot(:,i,j) = dist(:,i,1)/self%Delta(i)

        distTot2(:,i,j) = sqrt(  ( dist(:,i,2)/self%Delta(i)**2 - &
                                distTot(:,i,j)**2 )/self%Nevent  )
      end do
    end do

    dist = 0;  distTot2 = 1/distTot2**2

    do j = 1, self%Niter
      dist(:,:,1) = dist(:,:,1) + distTot (:,:,j) * distTot2(:,:,j)
      dist(:,:,2) = dist(:,:,2) + distTot2(:,:,j)
    end do

    dist(:,:,2) = 1/dist(:,:,2);  dist(:,:,1) = dist(:,:,1) * dist(:,:,2)
    dist(:,:,2) = sqrt(dist(:,:,2))

    do i = 1, 8
      do j = 1, self%Nbins
        if ( dist(j,i,2) <= tiny(1._dp) ) dist(j,i,1) = 0
      enddo
    end do

    contains

!ccccccccccccccc

    real (dp) function FunMatEl(x, wgt)
      real (dp), dimension(self%dimX), intent(in) :: x
      real (dp)                      , intent(in) :: wgt
      real (dp), dimension(8)                     :: ES
      integer  , dimension(8)                     :: k
      real (dp), dimension(self%dimP,4)           :: p
      integer                                     :: l

      select type (self)
      type is (MCtopUnstable)
        p = self%MatEl%GenerateVectors(x); ES = EScomputer(p)
        FunMatEl = self%MatEl%SpinWeight(self%spin, self%current, p)
      end select

      k = Ceiling( self%Nbins * (ES - self%ESmin )/(self%ESmax - self%ESmin ) )

      do l = 1, 8

        if ( k(l) <= 0          ) k(l) = 1
        if ( k(l) >  self%Nbins ) k(l) = self%Nbins

        if ( k(l) > 0 ) then
          dist( k(l), l, 1 ) = dist( k(l), l, 1 ) + wgt * FunMatEl
          dist( k(l), l, 2 ) = dist( k(l), l, 2 ) + wgt * FunMatEl**2
        end if

      end do

    end function FunMatEl

!ccccccccccccccc

  end subroutine callVegas

!ccccccccccccccc

 subroutine LegendreDistro(self, n, expand, method, dist, list)
    class (MCtopUnstable)             , intent(in)  :: self
    integer                           , intent(in)  :: n
    character (len = *)               , intent(in)  :: expand, method
    real (dp), dimension(self%Nbins,3), intent(out) :: dist
    real (dp), dimension(0:n       ,2), intent(out) :: list
    real (dp), dimension(self%Nbins, self%Niter,2)  :: distTot
    real (dp), dimension(0:n       , self%Niter,2)  :: listTot
    real (dp), dimension(self%dimX)                 :: y
    real (dp)                                       :: AVGI, SD, CHI2A
    integer                                         :: i, j, iter

    NPRN = - 1; ITMX = 1; NCall = self%Nevent; iter = 1; list = 0
    if (self%dimX <= 3) iter = 0; distTot = 0
    dist(:,1) = self%ES(:,5)

    do j = 1, self%Niter

      list = 0; dist(:,2:) = 0

      if ( method(:5) == 'vegas' ) then
        if (j > 1 .and. self%dimX > 3) iter = 2
        call VEGAS(self%dimX, FunMatEl, AVGI, SD, CHI2A, iter)
      else
        do i = 1, NCall
          call Random_number(y)
          AVGI = AVGI + FunMatEl(y, 1._dp)
        end do
        AVGI = AVGI/self%Nevent; list  = list/self%Nevent
        dist(:,2:) = dist(:,2:)/self%Nevent
      end if

      listTot(:,j,1) = list(:,1);  distTot(:,j,1) = dist(:,2)/self%Delta(5)

      distTot(:,j,2) = sqrt(  ( dist(:,3)/self%Delta(5)**2 - &
                                            distTot(:,j,1)**2 )/self%Nevent  )
      listTot(:,j,2) = sqrt(  ( list(:,2) - listTot(:,j,1)**2 )/self%Nevent  )

    end do

    list       = 0;  listTot(:,:,2) = 1/listTot(:,:,2)**2
    dist(:,2:) = 0;  distTot(:,:,2) = 1/distTot(:,:,2)**2

    do j = 1, self%Niter
      list(1:,1) = list(1:,1) + listTot(1:,j,1) * listTot(1:,j,2)
      list(1:,2) = list(1:,2) + listTot(1:,j,2)
      dist(:,2)  = dist(:,2)  + distTot(:,j,1) * distTot(:,j,2)
      dist(:,3)  = dist(:,3)  + distTot(:,j,2)
    end do

    dist(:,3)  = 1/dist(:,3) ;  dist(:,2)  = dist(:,2) * dist(:,3)
    list(1:,2) = 1/list(1:,2);  list(1:,1) = list(1:,1) * list(1:,2)
    list(1:,2) = sqrt( list(1:,2) );  dist(:,3) = sqrt( dist(:,3) )

    list(0,:) = [ sum( listTot(0,:,1) )/self%Niter, 1/sqrt( sum( listTot(0,:,2) ) ) ]

    if ( method(:5) == 'vegas' ) list(0,1) = AVGI

    list = list/(self%ESmax(5) - self%ESmin(5) )

    do i = 0, n
      list(i,:) = (2 * i + 1) * list(i,:)
    end do

    if (  ISNAN( list(0,2) )  ) list(0,2) = 0

    do j = 1, self%Nbins
      if ( dist(j,3) <= d1mach(1) ) dist(j,2) = 0
    enddo

  contains

!ccccccccccccccc

    real (dp) function FunMatEl(x, wgt)
      real (dp), dimension(self%dimX), intent(in) :: x
      real (dp)                      , intent(in) :: wgt
      real (dp)                                   :: ES
      real (dp), dimension(0:n)                   :: ESLeg
      real (dp), dimension(self%dimP,4)           :: p
      integer                                     :: k

      if ( expand(:6) == 'expand' ) then
        ES = self%MatEl%CparamBeta(x); FunMatEl = 1
      else
        p = self%MatEl%GenerateVectors(x); ES = Cparam(p)
        FunMatEl = self%MatEl%SpinWeight(self%spin, self%current, p)
      end if

      ESLeg = LegendreList(  n, 2 * ( Cparam(p) - self%ESmin(5) )/&
                                ( self%ESmax(5) - self%ESmin(5) ) - 1  )

      if ( ES < self%ESmin(5) .or. ES > self%ESmax(5) ) return

      list(:,1) = list(:,1) + wgt *  FunMatEl * ESLeg
      list(:,2) = list(:,2) + wgt * (FunMatEl * ESLeg)**2

      k = Ceiling( self%Nbins * (ES - self%ESmin(5) )/(self%ESmax(5) - self%ESmin(5) ) )

      if ( k <= 0          ) k = 1
      if ( k >  self%Nbins ) k = self%Nbins

      if ( k > 0 ) then
        dist(k,2) = dist(k,2) + wgt * FunMatEl
        dist(k,3) = dist(k,3) + wgt * FunMatEl**2
      end if

    end function FunMatEl

 end subroutine LegendreDistro

!ccccccccccccccc

  function ESList(self) result(dist)
    class (MCtop)                  , intent(in)  :: self
    real (dp), dimension(self%Nbins, self%dimES) :: dist

    dist = self%ES

  end function ESList

!ccccccccccccccc

  function CparamList(self) result(dist)
    class (MCtopUnstable) , intent(in) :: self
    real (dp), dimension(self%Nbins)   :: dist

    dist = self%ES(:,5)

  end function CparamList

!ccccccccccccccc

  function List(self, method) result(dist)
    class (MCtop)                      , intent(in) :: self
    character (len = *)                , intent(in) :: method
    real (dp), dimension(self%Nbins, self%dimES, 3) :: dist

    dist(:,:,1  ) = self%ES
    call self%callVegas( method, dist(:,:,2:3)  )

  end function List

!ccccccccccccccc

  function ListCparam(self, expand, method) result(dist)
    class (MCtopUnstable)  , intent(in) :: self
    character (len = *)    , intent(in) :: expand, method
    real (dp), dimension(self%Nbins, 3) :: dist
    real (dp), dimension(1,2)           :: list

    call self%LegendreDistro( 0, expand, method, dist, list  )

  end function ListCparam

!ccccccccccccccc

end module MCtopClass
