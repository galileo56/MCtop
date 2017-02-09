
module MCtopClass
  use MatrixElementsClass; use constants, only: dp; use MC_VEGAS; implicit none
  private

  public                                     :: MCtop
  character (len = 11), dimension(8), public :: ESNames
  data ESNames /'tau', 'HJM', 'LJM', 'SJM', 'C-parameter', 'B', 'B-wide', 'B-narrow'/

!ccccccccccccccc

  type MCtop
    integer                    , private              :: Nbins, Nevent, Niter
    class (MatrixElements)     , private, allocatable :: MatEl
    real (dp), dimension(:,:  ), private, allocatable :: ES
    real (dp), dimension(:,:,:), private, allocatable :: dist
    real (dp), dimension(8    ), private              :: ESmax, delta
    character (len = 8)        , private              :: spin, current
    integer                    , private              :: dimX, dimP

  contains

    final                                             :: delete_object
    procedure, private                                :: callVegas
    procedure                                         :: List

  end type MCtop

!ccccccccccccccc

  interface MCtop
    module procedure InMCtop
  end interface MCtop

  contains

!ccccccccccccccc

  subroutine delete_object(this)
    type (MCtop) :: this

     if ( allocated(this%dist ) ) deallocate(this%dist)
     if ( allocated(this%ES   ) ) deallocate(this%ES  )
     if ( allocated(this%MatEl) ) deallocate(this%MatEl  )

  end subroutine delete_object

!ccccccccccccccc

   type (MCtop) function InMCtop(MatEl, Spin, current, ESmax, Nbins, Nevent, Niter)
     class (MatrixElements) , intent(in) :: MatEl
     character (len = *)    , intent(in) :: Spin, current
     integer                , intent(in) :: Nbins, Nevent, Niter
     real (dp), dimension(8), intent(in) :: ESmax
     real (dp), dimension(8)             :: delta
     integer                             :: i

     InMCtop%Nbins = Nbins ; InMCtop%Nevent = Nevent; InMCtop%Niter = Niter
     InMCtop%Spin  = Spin  ; InMCtop%current  = current

     allocate( InMCtop%dist(Nbins, 8, 2), InMCtop%ES(Nbins, 8) )

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

     delta = ESmax/Nbins; InMCtop%delta = delta;  InMCtop%ESmax = ESmax
     InMCtop%dimX = MatEl%dimX(); InMCtop%dimP = MatEl%dimP()

     do i = 1, Nbins
       InMCtop%ES(i,:) = Delta * (2 * i - 1)/2
     end do

     call InMCtop%callVegas( InMCtop%dist(:,:,1), InMCtop%dist(:,:,2)  )

   end function InMCtop

!ccccccccccccccc

  subroutine callVegas(self, dist, dist2)
    class (MCtop), intent(in)                        :: self
    real (dp), dimension(self%Nbins, 8), intent(out) :: dist , dist2
    real (dp), dimension(self%Nbins, 8, self%Niter)  :: distTot , distTot2
    real (dp)                                        :: AVGI, SD, CHI2A
    integer                                          :: i, j!, l

    NPRN = - 1; ITMX = 1

    do j = 1, self%Niter

      dist = 0;  dist2 = 0
      call VEGAS(self%dimX, FunMatEl, AVGI, SD, CHI2A)

      do i = 1, 8

        distTot(:,i,j) = dist(:,i)/self%Delta(i)

        distTot2(:,i,j) = sqrt(  ( dist2(:,i)/self%Delta(i)**2 - &
                                distTot(:,i,j)**2 )/self%Nevent  )
      end do
    end do

    dist = 0;  dist2 = 0;  distTot2 = 1/distTot2**2

    do j = 1, self%Niter
      dist  = dist  + distTot(:,:,j) * distTot2(:,:,j)
      dist2 = dist2 + distTot2 (:,:,j)
    end do

    dist2 = 1/dist2;  dist = dist * dist2;  dist2 = sqrt(dist2)

    do i = 1, 8
      do j = 1, self%Nbins
        if ( dist2(j,i) <= tiny(1._dp) ) dist(j,i) = 0
      enddo
    end do

    dist(2,:) = dist(2,:)/Abs(AVGI); dist2(2,:) = dist2(2,:)/Abs(AVGI)

    contains

!ccccccccccccccc

    real (dp) function FunMatEl(x, wgt)
      real (dp), dimension(self%dimX), intent(in) :: x
      real (dp)                      , intent(in) :: wgt
      real (dp), dimension(8)                     :: ES
      integer  , dimension(8)                     :: k
      real (dp), dimension(self%dimP,4)           :: p
      integer                                     :: i

      p = self%MatEl%GenerateVectors(x); ES = EScomputer(p)

      if ( self%spin(:6) == 'uncorr'  ) FunMatEl = 1
      if ( self%spin(:3) == 'top'     ) FunMatEl = self%MatEl%SpinWeight(p)
      if ( self%spin(:8) == 'complete') FunMatEl = self%MatEl%TotalSpinWeight(p, self%current)

      k = Ceiling( self%Nbins * ES/self%ESmax )

      do i = 1, 8

        if ( k(i)    <= 0          ) k(i) = 1
        if ( k(i)    >  self%Nbins ) k(i) = self%Nbins

        if ( k(i) > 0 ) then

          dist ( k(i), i) = dist ( k(i), i ) + wgt * FunMatEl
          dist2( k(i), i) = dist2( k(i), i ) + wgt * FunMatEl**2

        end if

      end do

    end function FunMatEl

!ccccccccccccccc

  end subroutine callVegas

!ccccccccccccccc

  function List(self) result(dist)
    class (MCtop) , intent(in)             :: self
    real (dp), dimension(self%Nbins, 8, 3) :: dist

    dist(:,:,1  ) = self%ES
    dist(:,:,2:3) = self%dist(:,:,:)

  end function List

!ccccccccccccccc

end module MCtopClass
