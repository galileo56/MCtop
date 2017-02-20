
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

  type, abstract, public                            :: MCtop
    private
    integer                  , private              :: Nbin, Nevent, Niter
    class (MatrixElements)   , private, allocatable :: MatEl
    real (dp), dimension(:,:), private, allocatable :: ES
    real (dp), dimension(:)  , private, allocatable :: ESmax, ESmin, delta
    integer                  , private              :: dimX, dimP, dimES

  contains

    procedure, pass (self), public                  :: List, ESlist, callVegas

  end type MCtop

!ccccccccccccccc

  type, extends (MCtop), public                     :: MCUnstable
    private
    character (len = 8)    , private                :: spin, current
    real (dp), dimension(8), private                :: deltaPos

  contains

    final                                           :: delete_object
    procedure, public, pass (self)                  :: callVegasCparam, CparamList, &
                     ListCparam, callVegasThrust, callVegasDelta, callDistroThrust
  end type MCUnstable

!ccccccccccccccc

  type, extends (MCtop), public :: MCStable
    private
    integer                  , private              :: Nlog
    real (dp), dimension(:,:), private, allocatable :: ESlog
    real (dp), dimension(16 ), private              :: logMin, DeltaLog, logMax
    real (dp), dimension(2  ), private              :: B1
    character (len = 3)      , private              :: orient

   contains

    final                                           :: delete_stable
    procedure, pass (self)                          :: callVegasStable, LegendreStable

  end type MCStable

!ccccccccccccccc

  interface MCUnstable
    module procedure InMCtop
  end interface MCUnstable

!ccccccccccccccc

  interface MCStable
    module procedure InitStable
  end interface MCStable

  contains

!ccccccccccccccc

   subroutine delete_object(this)
     type (MCUnstable) :: this

     if ( allocated(this%ES   ) ) deallocate(this%ES   )
     if ( allocated(this%ESMin) ) deallocate(this%ESMin)
     if ( allocated(this%ESMax) ) deallocate(this%ESMax)
     if ( allocated(this%MatEl) ) deallocate(this%MatEl)
     if ( allocated(this%delta) ) deallocate(this%delta)

   end subroutine delete_object

!ccccccccccccccc

   subroutine delete_stable(this)
     type (MCStable) :: this

     if ( allocated(this%ES   ) ) deallocate(this%ES   )
     if ( allocated(this%ESMin) ) deallocate(this%ESMin)
     if ( allocated(this%ESMax) ) deallocate(this%ESMax)
     if ( allocated(this%MatEl) ) deallocate(this%MatEl)
     if ( allocated(this%delta) ) deallocate(this%delta)
     if ( allocated(this%ESlog) ) deallocate(this%ESlog)

   end subroutine delete_stable

!ccccccccccccccc

   type (MCStable) function InitStable(MatEl, Nbin, Nlog, Nevent, Niter)
     integer            , intent(in) :: Nbin, Nlog, Nevent, Niter
     type (MatrixStable), intent(in) :: MatEl
     real (dp)       , dimension(16) :: DeltaLog, ESMin, ESMax, LogMin, LogMax, delta
     integer                         :: i

     InitStable%Nbin   = Nbin   ; InitStable%Nlog  = Nlog; InitStable%dimES = 16
     InitStable%Nevent = Nevent ; InitStable%Niter = Niter

     allocate( MatrixStable :: InitStable%MatEl )
     select type (selector => InitStable%MatEl)
     type is (MatrixStable);  selector = MatEl; InitStable%B1 = selector%B1()
       InitStable%orient = selector%orientation()
     end select

     allocate( InitStable%ES(Nbin, 16), InitStable%ESlog(Nlog, 16) )
     allocate( InitStable%ESMin(16), InitStable%ESMax(16), InitStable%delta(16) )

     ESMin  = MatEl%ESMin();  ESmax  = MatEl%ESMax();  Delta    = (ESmax  - ESmin )/Nbin
     LogMin = - 5          ;  LogMax = 1            ;  DeltaLog = (LogMax - LogMin)/Nlog

     InitStable%Delta = Delta;  InitStable%DeltaLog = DeltaLog

     do i = 1, Nbin
       InitStable%ES(i,:) = ESMin + Delta * (2 * i - 1)/2
     end do

     do i = 1, Nlog
       InitStable%ESlog(i,:) = LogMin + DeltaLog * (2 * i - 1)/2
     end do

     InitStable%ESMin  = ESMin ; InitStable%ESmax  = ESmax
     InitStable%LogMin = - 5   ; InitStable%LogMax = 1

   end function InitStable

!ccccccccccccccc

   type (MCUnstable) function InMCtop(MatEl, Spin, current, ESmin, ESmax, Nbin, Nevent, Niter)
     class (MatrixUnstable) , intent(in) :: MatEl
     character (len = *)    , intent(in) :: Spin, current
     integer                , intent(in) :: Nbin, Nevent, Niter
     real (dp), dimension(8), intent(in) :: ESmin, ESmax
     real (dp), dimension(8)             :: delta, deltaPos
     integer                             :: i

     allocate( InMCtop%ES(Nbin, 8),  InMCtop%ESMin(8), InMCtop%ESMax(8), InMCtop%delta(8) )

     InMCtop%Nbin = Nbin  ; InMCtop%Nevent  = Nevent ; InMCtop%Niter = Niter
     InMCtop%Spin  = Spin ; InMCtop%current = current; InMCtop%ESmin = ESmin

     select type (MatEl)
     type is (MatrixElements6)
       allocate( MatrixElements6 :: InMCtop%MatEl )
       select type (selector => InMCtop%MatEl)
         type is (MatrixElements6);  selector = MatEl
         deltaPos = MatEl%deltaPos()
       end select
     type is (MatrixElements4)
       allocate( MatrixElements4 :: InMCtop%MatEl )
       select type (selector => InMCtop%MatEl)
       type is (MatrixElements4);  selector = MatEl
         deltaPos = MatEl%deltaPos()
       end select
     end select

     delta = (ESmax - ESmin)/Nbin; InMCtop%delta = delta;  InMCtop%ESmax = ESmax
     InMCtop%dimX = MatEl%dimX(); InMCtop%dimP = MatEl%dimP()

     do i = 1, Nbin
       InMCtop%ES(i,:) = ESmin + Delta * (2 * i - 1)/2
     end do

     InMCtop%dimES = 8

   end function InMCtop

!ccccccccccccccc

  function LegendreStable(self, m, method) result(list)
    class (MCStable)                  , intent(in)  :: self
    character (len = *)               , intent(in)  :: method
    integer                           , intent(in)  :: m
    real (dp), dimension(0:m, 16, 4)                :: list
    real (dp), dimension(0:m, 16, self%Niter, 4)    :: listTot
    real (dp), dimension(2)                         :: y
    real (dp)                                       :: AVGI, SD, CHI2A
    integer                                         :: i, j, n

    NPRN = - 1; ITMX = 1; Ncall = self%Nevent; listTot = 0

    do j = 1, self%Niter

      list = 0

      if ( method(:5) == 'vegas' ) then
        call VEGAS(2, FunMatEl, AVGI, SD, CHI2A)
      else

        do n = 1, NCall
          call Random_number(y);  AVGI = AVGI + FunMatEl(y, 1._dp)
        end do

        AVGI = AVGI/self%Nevent; list = list/self%Nevent

      end if

      do i = 1, 16

        listTot(:,i,j,1:3:2)  = list(:,i,1:3:2)
        listTot(:,i,j,2:4:2)  = sqrt(  ( list(:,i,2:4:2) - listTot(:,i,j,1:3:2)**2 )/self%Nevent  )

      end do
    end do

    list = 0;  listTot(:,:,:,2:4:2) = 1/listTot(:,:,:,2:4:2)**2

    do j = 1, self%Niter

      list(:,:,1:3:2) = list(:,:,1:3:2) + listTot(:,:,j,1:3:2) * listTot(:,:,j,2:4:2)
      list(:,:,2:4:2) = list(:,:,2:4:2) + listTot(:,:,j,2:4:2)

    end do

    list(:,:,2:4:2) = 1/list(:,:,2:4:2);  list(:,:,1:3:2) = list(:,:,1:3:2) * list(:,:,2:4:2)
    list(:,:,2:4:2) = sqrt( list(:,:,2:4:2) )

    do i = 0, m
      list(i,:,:) = (2 * i + 1) * list(i,:,:)
    end do

    do i = 1, 16
      list(:,i,:) = list(:,i,:)/(self%ESmax(i) - self%ESmin(i) )
    end do

  contains

!ccccccccccccccc

    real (dp) function FunMatEl(x, wgt)
      real (dp), dimension( 2), intent(in) :: x
      real (dp)               , intent(in) :: wgt
      real (dp), dimension( 2)             :: MatEl, factor
      real (dp), dimension(0:m)            :: ESLeg
      real (dp), dimension(16)             :: ES, ESNorm
      integer                              :: l

      select type (selector => self%MatEl)
      type is (MatrixStable)
        call selector%MatElComputer( x(1), x(2), MatEl, ES )
      end select

      FunMatEl = MatEl(1); ESNorm = (ES - self%ESmin)/(self%ESmax - self%ESmin)

      do l = 1, 16

        if ( ES(l) < self%ESmin(l) .or. ES(l) > self%ESmax(l) ) cycle
        if ( ISNAN( ES(l) ) .or. ES(l) + 1 == ES(l) )           cycle

        ESLeg = LegendreList(  m, 2 * ESNorm(l) - 1  ); factor = MatEl

        if ( self%orient(:2) == 'no') factor = factor * ( ES(l) - self%ESmin(l) )

        list(:,l,1) = list(:,l,1) + wgt * factor(1) * ESLeg
        list(:,l,3) = list(:,l,3) + wgt * factor(2) * ESLeg

        list(:,l,2) = list(:,l,2) + wgt * ( factor(1) * ESLeg )**2
        list(:,l,4) = list(:,l,4) + wgt * ( factor(2) * ESLeg )**2

      end do

    end function FunMatEl

  end function LegendreStable

!ccccccccccccccc

  subroutine callVegasStable(self, method, power, distLin, distLog)
    class (MCStable)                         , intent(in) :: self
    character (len = *)                      , intent(in) :: method
    integer                                  , intent(in) :: power
    real (dp), dimension(self%Nbin, 16, 5)  , intent(out) :: distLin
    real (dp), dimension(self%Nlog, 16, 5)  , intent(out) :: distLog
    real (dp), dimension(self%Nbin, 16, 2, self%Niter, 2) :: distTot
    real (dp), dimension(self%Nlog, 16, 2, self%Niter, 2) :: distTotL
    real (dp)                                             :: AVGI, SD, CHI2A
    real (dp), dimension(2)                               :: y
    integer                                               :: i, j, n

    NPRN = - 1; ITMX = 1; Ncall = self%Nevent; distTot = 0;  distTotL = 0
    distLin(:,:,1) = self%ES; distLog(:,:,1) = self%ESlog

    do j = 1, self%Niter

      distLin(:,:,2:) = 0;  distLog(:,:,2:) = 0

      if ( method(:5) == 'vegas') then
        call VEGAS(2, FunMatEl, AVGI, SD, CHI2A)
      else

        do n = 1, NCall
          call Random_number(y);  AVGI = AVGI + FunMatEl(y, 1._dp)
        end do

        AVGI = AVGI/self%Nevent; distLin(:,:,2:) = distLin(:,:,2:)/self%Nevent
        distLog(:,:,2:) = distLog(:,:,2:)/self%Nevent
      end if

      do i = 1, 16

        distTot (:,i,:,j,1)  = distLin(:,i,2:4:2)/self%Delta(i)
        distTotL(:,i,:,j,1)  = distLog(:,i,2:4:2)/self%DeltaLog(i)

        distTot(:,i,:,j,2)  = sqrt(  ( distLin(:,i,3:5:2)/self%Delta(i)**2 - &
                               distTot(:,i,:,j,1)**2 )/self%Nevent  )

        distTotL(:,i,:,j,2) = sqrt(  ( distLog(:,i,3:5:2)/self%DeltaLog(i)**2 - &
                               distTotL(:,i,:,j,1)**2 )/self%Nevent  )

      end do
    end do

    distLin(:,:,2:) = 0; distLog(:,:,2:) = 0

    distTot (:,:,:,:,2) = 1/distTot (:,:,:,:,2)**2
    distTotL(:,:,:,:,2) = 1/distTotL(:,:,:,:,2)**2

    do j = 1, self%Niter

      distLin(:,:,2:4:2) = distLin(:,:,2:4:2) + distTot (:,:,:,j,1) * distTot (:,:,:,j,2)
      distLog(:,:,2:4:2) = distLog(:,:,2:4:2) + distTotL(:,:,:,j,2) * distTotL(:,:,:,j,2)

      distLin(:,:,3:5:2) = distLin(:,:,3:5:2) + distTot (:,:,:,j,2)
      distLog(:,:,3:5:2) = distLog(:,:,3:5:2) + distTotL(:,:,:,j,2)

    end do

    distLin(:,:,3:5:2) = 1/distLin(:,:,3:5:2)
    distLin(:,:,2:4:2) = distLin(:,:,2:4:2) * distLin(:,:,3:5:2)
    distLin(:,:,3:5:2) = sqrt( distLin(:,:,3:5:2) )

    distLog(:,:,3:5:2) = 1/distLog(:,:,3:5:2)
    distLog(:,:,2:4:2) = distLog(:,:,2:4:2) * distLog(:,:,3:5:2)
    distLog(:,:,3:5:2) = sqrt( distLog(:,:,3:5:2) )

    do i = 1, 16
      do j = 1, self%Nbin

        if ( distLin(j,i,3) <= d1mach(1) ) distLin(j,i,2) = 0
        if ( distLin(j,i,5) <= d1mach(1) ) distLin(j,i,4) = 0

      enddo

      do j = 1, self%Nlog

        if ( distLog(j,i,3) <= d1mach(1) ) distLog(j,i,2) = 0
        if ( distLog(j,i,5) <= d1mach(1) ) distLog(j,i,4) = 0

      enddo
    end do

    contains

!ccccccccccccccc

    real (dp) function FunMatEl(x, wgt)
      real (dp), dimension( 2), intent(in) :: x
      real (dp)               , intent(in) :: wgt
      real (dp), dimension( 2)             :: MatEl, factor
      real (dp), dimension(16)             :: ES, ESlog
      integer  , dimension(16)             :: k, klog
      integer                              :: i

      select type (selector => self%MatEl)
      type is (MatrixStable)
        call selector%MatElComputer( x(1), x(2), MatEl, ES )
      end select

      FunMatEl = MatEl(1)
      ESlog    = log10( ES - self%ESMin )

      k        = Ceiling( self%Nbin * (ES    - self%ESmin )/(self%ESmax  - self%ESmin ) )
      klog     = Ceiling( self%Nlog * (ESlog - self%Logmin)/(self%Logmax - self%Logmin) )

      do i = 1, 16

        if ( k(i)    <= 0         ) k(i)    = 1
        if ( k(i)    >  self%Nbin ) k(i)    = self%Nbin
        if ( klog(i) <= 0         ) klog(i) = 1
        if ( klog(i) >  self%Nlog ) klog(i) = self%Nlog

        factor = MatEl * ( ES(i) - self%ESMin(i) )**power

        if ( k(i) > 0 ) then

          distLin( k(i), i, 2:4:2 ) = distLin( k(i), i, 2:4:2 ) + wgt * factor
          distLin( k(i), i, 3:5:2 ) = distLin( k(i), i, 3:5:2 ) + wgt * factor**2

        end if

        if ( klog(i) > 0 ) then

          distLog( klog(i), i, 2:4:2 ) = distLog( klog(i), i, 2:4:2 ) + wgt * factor
          distLog( klog(i), i, 3:5:2 ) = distLog( klog(i), i, 3:5:2 ) + wgt * factor**2

        end if
      end do

    end function FunMatEl

!ccccccccccccccc

  end subroutine callVegasStable

!ccccccccccccccc

  subroutine callDistroThrust(self, method, dist, delta)
    class (MCUnstable)          , intent(in)  :: self
    character (len = *)         , intent(in)  :: method
    real (dp), dimension(2)     , intent(out) :: delta
    real (dp), dimension(self%Nbin, 3), intent(out) :: dist
    real (dp), dimension(self%Nbin, self%Niter, 2)  :: distTot
    real (dp), dimension(self%Niter, 2)       :: deltaTot
    real (dp), dimension(self%dimX)           :: y
    real (dp)                                 :: AVGI, SD, CHI2A
    integer                                   :: i, j, iter

    NPRN = - 1; ITMX = 1; NCall = self%Nevent; iter = 1; dist(:,1) = self%ES(:,1)
    if (self%dimX <= 3) iter = 0; distTot = 0; deltaTot = 0

    do j = 1, self%Niter

      dist = 0; delta = 0

      if ( method(:5) == 'vegas' ) then
        if (j > 1 .and. self%dimX > 3) iter = 2
        call VEGAS(self%dimX, FunMatEl, AVGI, SD, CHI2A, iter)
      else
        do i = 1, NCall
          call Random_number(y);  AVGI = AVGI + FunMatEl(y, 1._dp)
        end do
        AVGI = AVGI/self%Nevent; dist(:,2:) = dist(:,2:)/self%Nevent;  delta = delta/self%Nevent
      end if

      distTot(:,j,1) = dist(:,2)/self%Delta(1);  deltaTot(j,1) = delta(1)

      distTot(:,j,2) = sqrt(  ( dist(:,3)/self%Delta(1)**2 - distTot(:,j,1)**2 )/self%Nevent  )
      deltaTot( j,2) = sqrt(  ( delta( 2) - deltaTot( j,1)**2 )/self%Nevent  )

    end do

    dist(:,2:) = 0;  distTot(:,:,2) = 1/distTot(:,:,2)**2
    delta      = 0;  deltaTot( :,2) = 1/deltaTot( :,2)**2

    do j = 1, self%Niter
      dist(:,2) = dist(:,2) + distTot(:,j,1) * distTot(:,j,2)
      dist(:,3) = dist(:,3) + distTot(:,j,2)
      delta(1)  = delta(1)  + deltaTot(j, 1) * deltaTot(j, 2)
      delta(2)  = delta(2)  + deltaTot(j, 2)
    end do

    dist(:,3) = 1/dist(:,3);  dist(:,2) = dist(:,2) * dist(:,3)
    dist(:,3) = sqrt( dist(:,3) ); delta(2) = 1/delta(2)
    delta(1)  = delta(1) * delta(2); delta(2) = sqrt( delta(2) )

    do i = 1, self%Nbin
      if ( dist(i,3) < d1mach(1) ) dist(i,2:) = 0
    end do

  contains

!ccccccccccccccc

    real (dp) function FunMatEl(x, wgt)
      real (dp), dimension(self%dimX), intent(in) :: x
      real (dp)                      , intent(in) :: wgt
      real (dp), dimension(8)                     :: ES
      real (dp)                                   :: ESNorm
      real (dp), dimension(self%dimP,4)           :: p
      integer                                     :: k

      select type (self)
      type is (MCUnstable)
        select type (selector => self%MatEl)
        class is (MatrixUnstable)
          p = selector%GenerateVectors(x); ES = EScomputer(p)
          FunMatEl = selector%SpinWeight(self%spin, self%current, p)
        end select
      end select

      if (  abs( ES(1) - self%deltaPos(1) ) <= 1e-9_dp  ) then
        delta = delta + wgt * FunMatEl**[1,2]
      else
        ESNorm = ( ES(1) - self%ESmin(1) )/( self%ESmax(1) - self%ESmin(1) )
        k = Ceiling( self%Nbin * ESNorm )
        if ( k <= 0          ) k = 1
        if ( k >  self%Nbin  ) k = self%Nbin

        if ( k > 0 ) then
          dist(k,2:) = dist(k,2:) + wgt * FunMatEl**[1,2]
        end if

      end if

    end function FunMatEl

  end subroutine callDistroThrust

!ccccccccccccccc

  subroutine callVegasThrust(self, m, method, list, delta)
    class (MCUnstable)          , intent(in)  :: self
    integer                     , intent(in)  :: m
    character (len = *)         , intent(in)  :: method
    real (dp), dimension(2)     , intent(out) :: delta
    real (dp), dimension(0:m, 2), intent(out) :: list
    real (dp), dimension(0:m, self%Niter, 2)  :: listTot
    real (dp), dimension(self%Niter, 2)       :: deltaTot
    real (dp), dimension(self%dimX)           :: y
    real (dp)                                 :: AVGI, SD, CHI2A
    integer                                   :: i, j, iter

    NPRN = - 1; ITMX = 1; NCall = self%Nevent; iter = 1
    if (self%dimX <= 3) iter = 0; listTot = 0; deltaTot = 0

    do j = 1, self%Niter

      list = 0; delta = 0

      if ( method(:5) == 'vegas' ) then
        if (j > 1 .and. self%dimX > 3) iter = 2
        call VEGAS(self%dimX, FunMatEl, AVGI, SD, CHI2A, iter)
      else
        do i = 1, NCall
          call Random_number(y);  AVGI = AVGI + FunMatEl(y, 1._dp)
        end do
        AVGI = AVGI/self%Nevent; list = list/self%Nevent;  delta = delta/self%Nevent
      end if

      listTot(:,j,1) = list(:,1);  deltaTot(j,1) = delta(1)

      listTot(:,j,2) = sqrt(  ( list(:,2) - listTot(:,j,1)**2 )/self%Nevent  )
      deltaTot( j,2) = sqrt(  ( delta( 2) - deltaTot( j,1)**2 )/self%Nevent  )

    end do

    list  = 0;  listTot(:,:,2) = 1/listTot(:,:,2)**2
    delta = 0;  deltaTot( :,2) = 1/deltaTot( :,2)**2

    do j = 1, self%Niter
      list(:,1) = list(:,1) + listTot(:,j,1) * listTot(:,j,2)
      list(:,2) = list(:,2) + listTot(:,j,2)
      delta(1)  = delta(1)  + deltaTot(j, 1) * deltaTot(j, 2)
      delta(2)  = delta(2)  + deltaTot(j, 2)
    end do

    list(:,2) = 1/list(:,2);  list(:,1) = list(:,1) * list(:,2)
    list(:,2) = sqrt( list(:,2) ); delta(2) = 1/delta(2)
    delta(1)  = delta(1) * delta(2); delta(2) = sqrt( delta(2) )

    list = list/(self%ESmax(1) - self%ESmin(1) )

    do i = 0, m
      list(i,:) = (2 * i + 1) * list(i,:)
      if (list(i,2) < d1mach(1) ) list(i,:) = 0
    end do

  contains

!ccccccccccccccc

    real (dp) function FunMatEl(x, wgt)
      real (dp), dimension(self%dimX), intent(in) :: x
      real (dp)                      , intent(in) :: wgt
      real (dp), dimension(8)                     :: ES
      real (dp)                                   :: ESNorm
      real (dp), dimension(0:m)                   :: ESLeg
      real (dp), dimension(self%dimP,4)           :: p

      select type (self)
      type is (MCUnstable)
        select type (selector => self%MatEl)
        class is (MatrixUnstable)
          p = selector%GenerateVectors(x); ES = EScomputer(p)
          FunMatEl = selector%SpinWeight(self%spin, self%current, p)
        end select
      end select

      if (  abs( ES(1) - self%deltaPos(1) ) <= 1e-9_dp  ) then
        delta = delta + wgt * FunMatEl**[1,2]
      else
        ESNorm = ( ES(1) - self%ESmin(1) )/( self%ESmax(1) - self%ESmin(1) )
        ESLeg = LegendreList(  m, 2 * ESNorm - 1  )
        list(:,1) = list(:,1) + wgt *  FunMatEl * ESLeg
        list(:,2) = list(:,2) + wgt * (FunMatEl * ESLeg)**2
      end if

    end function FunMatEl

  end subroutine callVegasThrust

!ccccccccccccccc

  subroutine callVegasDelta(self, m, method, list, delta)
    class (MCUnstable)             , intent(in)  :: self
    integer                        , intent(in)  :: m
    character (len = *)            , intent(in)  :: method
    real (dp), dimension(3,2)      , intent(out) :: delta
    real (dp), dimension(0:m, 3, 2), intent(out) :: list
    real (dp), dimension(0:m, 3, self%Niter, 2)  :: listTot
    real (dp), dimension(     3, self%Niter, 2)  :: deltaTot
    real (dp), dimension(self%dimX)              :: y
    real (dp)                                    :: AVGI, SD, CHI2A
    integer                                      :: i, j, iter
    integer, dimension(3), parameter             :: intk = [1,2,4]

    NPRN = - 1; ITMX = 1; NCall = self%Nevent; iter = 1; list = 0; delta = 0
    if (self%dimX <= 3) iter = 0; listTot = 0; deltaTot = 0

    do j = 1, self%Niter

      list = 0; delta = 0

      if ( method(:5) == 'vegas' ) then
        if (j > 1 .and. self%dimX > 3) iter = 2
        call VEGAS(self%dimX, FunMatEl, AVGI, SD, CHI2A, iter)
      else
        do i = 1, NCall
          call Random_number(y);  AVGI = AVGI + FunMatEl(y, 1._dp)
        end do
        AVGI = AVGI/self%Nevent; list = list/self%Nevent;  delta = delta/self%Nevent
      end if

      listTot(:,:,j,1) = list(:,:,1);  deltaTot(:,j,1) = delta(:,1)

      listTot(:,:,j,2) = sqrt(  ( list(:,:,2) - listTot(:,:,j,1)**2 )/self%Nevent  )
      deltaTot( :,j,2) = sqrt(  ( delta( :,2) - deltaTot( :,j,1)**2 )/self%Nevent  )

    end do

    list  = 0;  listTot(:,:,:,2) = 1/listTot(:,:,:,2)**2
    delta = 0;  deltaTot( :,:,2) = 1/deltaTot( :,:,2)**2

    do j = 1, self%Niter
      list(:,:,1) = list(:,:,1) + listTot(:,:,j,1) * listTot(:,:,j,2)
      list(:,:,2) = list(:,:,2) + listTot(:,:,j,2)
      delta(:,1)  = delta(:,1)  + deltaTot( :,j,1) * deltaTot(:,j, 2)
      delta(:,2)  = delta(:,2)  + deltaTot( :,j,2)
    end do

    list(:,:,2) = 1/list(:,:,2);  list(:,:,1) = list(:,:,1) * list(:,:,2)
    list(:,:,2) = sqrt( list(:,:,2) ); delta(:,2) = 1/delta(:,2)
    delta(:,1)  = delta(:,1) * delta(:,2); delta(:,2) = sqrt( delta(:,2) )

    do i = 1, 3
      j = intk(i);  list(:,i,:) = list(:,i,:)/(self%ESmax(j) - self%ESmin(j) )
    end do

    do i = 0, m
      list(:,i,:) = (2 * i + 1) * list(:,i,:)
      do j = 1, 3
        if (list(j,i,2) < d1mach(1) ) list(j,i,:) = 0
      end do
    end do

  contains

!ccccccccccccccc

    real (dp) function FunMatEl(x, wgt)
      real (dp), dimension(self%dimX), intent(in) :: x
      real (dp)                      , intent(in) :: wgt
      real (dp), dimension(8)                     :: ES
      real (dp)                                   :: ESNorm
      real (dp), dimension(0:m)                   :: ESLeg
      real (dp), dimension(self%dimP,4)           :: p
      integer                                     :: k, l

      select type (self)
      type is (MCUnstable)
        select type (selector => self%MatEl)
        class is (MatrixUnstable)
          p = selector%GenerateVectors(x); ES = EScomputer(p)
          FunMatEl = selector%SpinWeight(self%spin, self%current, p)
        end select
      end select

      do k = 1, 3
        l = intk(l)

        if (  abs( ES(l) - self%deltaPos(l) ) <= 1e-9_dp  ) then
          delta(k,1) = delta(k,1) + wgt * FunMatEl
          delta(k,2) = delta(k,2) + wgt * FunMatEl**2
        else
          ESNorm = ( ES(l) - self%ESmin(l) )/( self%ESmax(l) - self%ESmin(l) )
          ESLeg = LegendreList(  m, 2 * ESNorm - 1  )
          list(k,:,1) = list(k,:,1) + wgt *  FunMatEl * ESLeg
          list(k,:,2) = list(k,:,2) + wgt * (FunMatEl * ESLeg)**2
        end if

      end do

    end function FunMatEl

  end subroutine callVegasDelta

!ccccccccccccccc

  subroutine callVegas(self, m, method, dist, list)
    class (MCtop)                         , intent(in) :: self
    integer                               , intent(in) :: m
    character (len = *)                   , intent(in) :: method
    real (dp), dimension(self%Nbin, 8, 3), intent(out) :: dist
    real (dp), dimension(0:m      , 8, 2), intent(out) :: list
    real (dp), dimension(self%Nbin, 8, self%Niter, 2)  :: distTot
    real (dp), dimension(0:m      , 8, self%Niter, 2)  :: listTot
    real (dp), dimension(self%dimX)                    :: y
    real (dp)                                          :: AVGI, SD, CHI2A
    integer                                            :: i, j, n, iter

    NPRN = - 1; ITMX = 1; NCall = self%Nevent; iter = 1; listTot = 0
    if (self%dimX <= 3) iter = 0; distTot = 0; dist(:,:,1) = self%ES

    do j = 1, self%Niter

      dist(:,:,2:) = 0

      if ( method(:5) == 'vegas' ) then
        if (j > 1 .and. self%dimX > 3) iter = 2
        call VEGAS(self%dimX, FunMatEl, AVGI, SD, CHI2A, iter)
      else
        do n = 1, NCall
          call Random_number(y);  AVGI = AVGI + FunMatEl(y, 1._dp)
        end do
        AVGI = AVGI/self%Nevent; dist(:,:,2:) = dist(:,:,2:)/self%Nevent
        list = list/self%Nevent
      end if

      listTot(:,:,j,1) = list(:,:,1)
      listTot(:,:,j,2) = sqrt(  ( list(:,:,2) - listTot(:,:,j,1)**2 )/self%Nevent  )

      do i = 1, 8

        distTot(:,i,j,1) = dist(:,i,2)/self%Delta(i)

        distTot(:,i,j,2) = sqrt(  ( dist(:,i,3)/self%Delta(i)**2 - &
                                distTot(:,i,j,1)**2 )/self%Nevent  )
      end do
    end do

    dist(:,:,2:) = 0;  distTot(:,:,:,2) = 1/distTot(:,:,:,2)**2
    list         = 0;  listTot(:,:,:,2) = 1/listTot(:,:,:,2)**2

    do j = 1, self%Niter
      list(1:,:,1) = list(1:,:,1) + listTot(1:,:,j,1) * listTot(1:,:,j,2)
      list(1:,:,2) = list(1:,:,2) + listTot(1:,:,j,2)
      dist( :,:,2) = dist( :,:,2) + distTot( :,:,j,1) * distTot(:,:,j,2)
      dist( :,:,3) = dist( :,:,3) + distTot( :,:,j,2)
    end do

    dist(:,:,3) = 1/dist(:,:,3);  dist(:,:,2) = dist(:,:,2) * dist(:,:,3)
    dist(:,:,3) = sqrt( dist(:,:,3) ); list(1:,:,2) = 1/list(1:,:,2)

    if ( method(:5) == 'vegas' ) then
      list(0,:,1) = AVGI; list(1:,:,1) = list(1:,:,1) * list(1:,:,2)
      list(1:,:,2) = sqrt( list(1:,:,2) )
    else
      do j = 1, 8
        do i = 0, m
          list(i,j,:) = [ sum( listTot(i,j,:,1) )/self%Niter, &
                  1/sqrt( sum( listTot(i,j,:,2),  mask = listTot(i,j,:,2) > 0 ) ) ]
          if ( IsNAN(list(i,j,2)) .or. list(i,j,2) + 1 == list(i,j,2) ) list(i,j,2) = 0
        end do
      end do
    end if

    do i = 0, m
      list(i,:,:) = (2 * i + 1) * list(i,:,:)
    end do

    do i = 1, 8
      list(:,i,:) = list(:,i,:)/(self%ESmax(i) - self%ESmin(i) )
      if (  ISNAN( list(0,i,2) ) .or. list(0,i,2) + 1 == list(0,i,2)  ) list(0,i,2) = 0
    end do

    do i = 1, 8
      do j = 1, self%Nbin
        if ( dist(j,i,3) <= d1mach(1) ) dist(j,i,2) = 0
      enddo
    end do

    contains

!ccccccccccccccc

    real (dp) function FunMatEl(x, wgt)
      real (dp), dimension(self%dimX), intent(in) :: x
      real (dp)                      , intent(in) :: wgt
      real (dp), dimension(8)                     :: ES, ESNorm
      integer  , dimension(8)                     :: k
      real (dp), dimension(0:m)                   :: ESLeg
      real (dp), dimension(self%dimP,4)           :: p
      integer                                     :: l

      select type (self)
      type is (MCUnstable)
        select type (selector => self%MatEl)
        class is (MatrixUnstable)
          p = selector%GenerateVectors(x); ES = EScomputer(p)
          FunMatEl = selector%SpinWeight(self%spin, self%current, p)
        end select
      end select

      ESNorm = (ES - self%ESmin)/(self%ESmax - self%ESmin)

      k = Ceiling( self%Nbin * ESNorm )

      ESloop: do l = 1, 8

        if ( ES(l) < self%ESmin(l) .or. ES(l) > self%ESmax(l) ) cycle ESloop
        if ( ISNAN( ES(l) ) .or. ES(l) + 1 == ES(l) )           cycle ESloop

        ESLeg = LegendreList(  m, 2 * ESNorm(l) - 1  )

        list(:,l,1) = list(:,l,1) + wgt *  FunMatEl * ESLeg
        list(:,l,2) = list(:,l,2) + wgt * (FunMatEl * ESLeg)**2

        if ( k(l) <= 0          ) k(l) = 1
        if ( k(l) >  self%Nbin ) k(l) = self%Nbin

        if ( k(l) > 0 ) then
          dist( k(l), l, 2: ) = dist( k(l), l, 2: ) + wgt * FunMatEl**[1,2]
        end if

      end do ESloop

    end function FunMatEl

!ccccccccccccccc

  end subroutine callVegas

!ccccccccccccccc

 subroutine callVegasCparam(self, n, expand, method, dist, list)
    class (MCUnstable)                , intent(in)  :: self
    integer                           , intent(in)  :: n
    character (len = *)               , intent(in)  :: expand, method
    real (dp), dimension(self%Nbin,3) , intent(out) :: dist
    real (dp), dimension(0:n       ,2), intent(out) :: list
    real (dp), dimension(self%Nbin, self%Niter,2)   :: distTot
    real (dp), dimension(0:n       , self%Niter,2)  :: listTot
    real (dp), dimension(self%dimX)                 :: y
    real (dp)                                       :: AVGI, SD, CHI2A
    integer                                         :: i, j, iter

    NPRN = - 1; ITMX = 1; NCall = self%Nevent; iter = 1; list = 0; listTot = 0
    if (self%dimX <= 3) iter = 0; distTot = 0; dist(:,1) = self%ES(:,5)

    do j = 1, self%Niter

      list = 0; dist(:,2:) = 0

      if ( method(:5) == 'vegas' ) then
        if (j > 1 .and. self%dimX > 3) iter = 2
        call VEGAS(self%dimX, FunMatEl, AVGI, SD, CHI2A, iter)
      else
        do i = 1, NCall
          call Random_number(y);  AVGI = AVGI + FunMatEl(y, 1._dp)
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
      dist( :,2) = dist( :,2) + distTot( :,j,1) * distTot(:,j,2)
      dist( :,3) = dist( :,3) + distTot( :,j,2)
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

    do j = 1, self%Nbin
      if ( dist(j,3) <= d1mach(1) ) dist(j,2) = 0
    enddo

  contains

!ccccccccccccccc

    real (dp) function FunMatEl(x, wgt)
      real (dp), dimension(self%dimX), intent(in) :: x
      real (dp)                      , intent(in) :: wgt
      real (dp)                                   :: ES, ESNorm
      real (dp), dimension(0:n)                   :: ESLeg
      real (dp), dimension(self%dimP,4)           :: p
      integer                                     :: k

      select type (self)
      type is (MCUnstable)
        select type (selector => self%MatEl)
        class is (MatrixUnstable)
          if ( expand(:6) == 'expand' ) then
            ES = selector%CparamBeta(x); FunMatEl = 1
          else
            p = selector%GenerateVectors(x); ES = Cparam(p)
            FunMatEl = selector%SpinWeight(self%spin, self%current, p)
          end if
        end select
      end select

      ESNorm = ( ES - self%ESmin(5) )/( self%ESmax(5) - self%ESmin(5) )

      ESLeg = LegendreList(  n, 2 * ESNorm - 1  )

      if ( ES < self%ESmin(5) .or. ES > self%ESmax(5) ) return

      list(:,1) = list(:,1) + wgt *  FunMatEl * ESLeg
      list(:,2) = list(:,2) + wgt * (FunMatEl * ESLeg)**2

      k = Ceiling( self%Nbin * ESNorm )

      if ( k <= 0          ) k = 1
      if ( k >  self%Nbin ) k = self%Nbin

      if ( k > 0 ) then
        dist(k,2:) = dist(k,2:) + wgt * FunMatEl**[1,2]
      end if

    end function FunMatEl

 end subroutine callVegasCparam

!ccccccccccccccc

  function ESList(self) result(dist)
    class (MCtop)                  , intent(in)  :: self
    real (dp), dimension(self%Nbin, self%dimES) :: dist

    dist = self%ES

  end function ESList

!ccccccccccccccc

  function CparamList(self) result(dist)
    class (MCUnstable) , intent(in) :: self
    real (dp), dimension(self%Nbin) :: dist

    dist = self%ES(:,5)

  end function CparamList

!ccccccccccccccc

  function List(self, method) result(dist)
    class (MCtop)                     , intent(in) :: self
    character (len = *)               , intent(in) :: method
    real (dp), dimension(self%Nbin, self%dimES, 3) :: dist
    real (dp), dimension(1,8,2)                    :: lista

    call self%callVegas( 0, method, dist, lista )

  end function List

!ccccccccccccccc

  function ListCparam(self, expand, method) result(dist)
    class (MCUnstable)  , intent(in) :: self
    character (len = *)    , intent(in) :: expand, method
    real (dp), dimension(self%Nbin, 3) :: dist
    real (dp), dimension(1,2)           :: list

    call self%callVegasCparam( 0, expand, method, dist, list  )

  end function ListCparam

!ccccccccccccccc

end module MCtopClass
