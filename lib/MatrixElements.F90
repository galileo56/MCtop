
! TODO: include matrix elements for 4-particle decay
! TODO: include non-zero width for top and W

module MatrixElementsClass
  use Constants, only: dp, Pi; implicit none;  private

  public :: FourProd, EScomputer, VecProd3, Abs3, MatrixElements, Cparam

!ccccccccccccccc

  type, abstract                         ::  MatrixElements
    private
    real (dp)                            :: mt, mb, mW, mt2, mW2, mb2
    integer                              :: sizeX, sizeP

  contains

    procedure, pass (self), public       :: ESMinMax, CparamMinMax, GenerateVectors, &
                                            SpinWeight, dimX, dimP, GenerateRestVectors
  end type MatrixElements

!ccccccccccccccc

  type, extends (MatrixElements), public ::  MatrixElements4
    private

  end type MatrixElements4

!ccccccccccccccc

  type, extends (MatrixElements), public ::  MatrixElements6
    private

  end type MatrixElements6

!ccccccccccccccc

  interface MatrixElements4
    module procedure InMatEl4
  end interface MatrixElements4

!ccccccccccccccc

  interface MatrixElements6
    module procedure InMatEl6
  end interface MatrixElements6

  contains

!ccccccccccccccc

  type (MatrixElements4) function InMatEl4(mt, mb, mW, Q)
    real (dp), intent(in) :: mt, mW, mb, Q

    InMatEl4%mt = mt/Q;  InMatEl4%mW = mW/Q;  InMatEl4%mb = mb/Q
    InMatEl4%mt2 = InMatEl4%mt**2;  InMatEl4%mW2 = InMatEl4%mW**2
    InMatEl4%mb2 = InMatEl4%mb**2;  InMatEl4%sizeX = 3;  InMatEl4%sizeP = 4

  end function InMatEl4

!ccccccccccccccc

  type (MatrixElements6) function InMatEl6(mt, mb, mW, Q)
    real (dp), intent(in) :: mt, mW, mb, Q

    InMatEl6%mt = mt/Q;  InMatEl6%mW = mW/Q;  InMatEl6%mb = mb/Q
    InMatEl6%mt2 = InMatEl6%mt**2;  InMatEl6%mW2 = InMatEl6%mW**2
    InMatEl6%mb2 = InMatEl6%mb**2;  InMatEl6%sizeX = 7;  InMatEl6%sizeP = 6

  end function InMatEl6

!ccccccccccccccc

  integer function dimX(self)
    class (MatrixElements), intent(in) :: self
    dimX = self%sizeX
  end function dimX

!ccccccccccccccc

  integer function dimP(self)
    class (MatrixElements), intent(in) :: self
    dimP = self%sizeP
  end function dimP

!ccccccccccccccc

! If one considers correlation among the W decay products, these do not exist
! In the case of stable W, there are no correlations within the top decay products

  real (dp) function SpinWeight(self, spin, current, p) ! TODO: normalize matrix elements for complete correlations
    class (MatrixElements), intent(in) :: self
    character (len = *)   , intent(in) :: spin, current
    real (dp)             , intent(in) :: p(self%sizeP,0:3)
    real (dp)                          :: p1p2, p1p4, p1p5, p1p6, p2p4, p2p6, &
                                          p2p5, p3p4, p3p5, p3p6, p4p6, p4p5, a
    select type (self)
    type is (MatrixElements4)
      SpinWeight = 1
    type is (MatrixElements6)

      if ( spin(:6) == 'uncorr' ) then
        SpinWeight = 1
      else if ( spin(:3) == 'top' ) then

        a = self%mt2 - self%mb2
        p1p2 = FourProd( p(1,:), p(2,:) );  p4p5 = FourProd( p(4,:), p(5,:) )
        SpinWeight = 144 * p1p2 * (a - 2 * p1p2) * p4p5 * (a - 2 * p4p5)/ &
        ( a**2 + self%mW2 * (self%mt2 + self%mb2 - 2 * self%mW2) )**2

      else if ( spin(:8) == 'complete' ) then

        p1p2 = FourProd( p(1,:), p(2,:) ); p1p4 = FourProd( p(1,:), p(4,:) )
        p1p5 = FourProd( p(1,:), p(5,:) ); p1p6 = FourProd( p(1,:), p(6,:) )
        p2p4 = FourProd( p(2,:), p(4,:) ); p2p5 = FourProd( p(2,:), p(5,:) )
        p2p6 = FourProd( p(2,:), p(6,:) ); p3p4 = FourProd( p(3,:), p(4,:) )
        p3p5 = FourProd( p(3,:), p(5,:) ); p3p6 = FourProd( p(3,:), p(6,:) )
        p4p6 = FourProd( p(4,:), p(6,:) ); SpinWeight = 0

        if ( current(:6) == 'vector' ) then

          SpinWeight = &
          - 8*p1p2**2*p4p6**2 - 4*self%mb2*p1p2*p4p6**2 - 4*self%mb2*p1p2**2* &
          p4p6 - 2*self%mb**4*p1p2*p4p6 + 4*self%mt2*p1p2*p4p6**2 - 8*self%mt2*p1p2* &
          p3p6*p4p6**2 - 8*self%mt2*p1p2*p3p5*p4p6**2 - 8*self%mt2*p1p2*p3p4* &
          p4p6**2 + 4*self%mt2*p1p2**2*p4p6 - 8*self%mt2*p1p2**2*p3p5*p4p6 - 8* &
          self%mt2*p1p2**2*p2p5*p4p6 - 8*self%mt2*p1p2**2*p1p5*p4p6 + 4*self%mt2* &
          self%mb2*p1p2*p4p6 - 4*self%mt2*self%mb2*p1p2*p3p6*p4p6 - 8*self%mt2*self%mb2* &
          p1p2*p3p5*p4p6 - 4*self%mt2*self%mb2*p1p2*p3p4*p4p6 - 4*self%mt2*self%mb2* &
          p1p2*p2p5*p4p6 - 4*self%mt2*self%mb2*p1p2*p1p5*p4p6 - 2*self%mt**4*p1p2* &
          p4p6 + 4*self%mt**4*p1p2*p3p6*p4p6 + 4*self%mt**4*p1p2*p3p4*p4p6 + 4*self%mt**4 &
          *p1p2*p2p5*p4p6 + 4*self%mt**4*p1p2*p1p5*p4p6

        else if ( current(:5) == 'axial' ) then

          SpinWeight = &
          - 16*p1p2**2*p4p6**2 - 8*self%mb2*p1p2*p4p6**2 - 8*self%mb2*p1p2**2* &
          p4p6 - 4*self%mb**4*p1p2*p4p6 + 8*self%mt2*p1p2*p4p6**2 - 16*self%mt2*p1p2* &
          p3p6*p4p6**2 - 16*self%mt2*p1p2*p3p5*p4p6**2 - 16*self%mt2*p1p2*p3p4* &
          p4p6**2 + 8*self%mt2*p1p2**2*p4p6 + 64*self%mt2*p1p2**2*p4p6**2 - 16* &
          self%mt2*p1p2**2*p3p5*p4p6 - 16*self%mt2*p1p2**2*p2p5*p4p6 - 16*self%mt2* &
          p1p2**2*p1p5*p4p6 + 8*self%mt2*self%mb2*p1p2*p4p6 + 32*self%mt2*self%mb2* &
          p1p2*p4p6**2 - 8*self%mt2*self%mb2*p1p2*p3p6*p4p6 - 16*self%mt2*self%mb2* &
          p1p2*p3p5*p4p6 - 8*self%mt2*self%mb2*p1p2*p3p4*p4p6 - 8*self%mt2*self%mb2* &
          p1p2*p2p5*p4p6 - 8*self%mt2*self%mb2*p1p2*p1p5*p4p6 + 32*self%mt2*self%mb2* &
          p1p2**2*p4p6 + 16*self%mt2*self%mb**4*p1p2*p4p6 - 4*self%mt**4*p1p2*p4p6 - 32 &
          *self%mt**4*p1p2*p4p6**2 + 8*self%mt**4*p1p2*p3p6*p4p6 + 32*self%mt**4*p1p2* &
          p3p6*p4p6**2 + 16*self%mt**4*p1p2*p3p5*p4p6 + 32*self%mt**4*p1p2*p3p5* &
          p4p6**2 - 32*self%mt**4*p1p2*p3p5*p3p6*p4p6 - 32*self%mt**4*p1p2*p3p5**2* &
          p4p6 + 8*self%mt**4*p1p2*p3p4*p4p6 + 32*self%mt**4*p1p2*p3p4*p4p6**2 - 32* &
          self%mt**4*p1p2*p3p4*p3p5*p4p6

          SpinWeight = SpinWeight + &
          8*self%mt**4*p1p2*p2p5*p4p6 - 32*self%mt**4*p1p2*p2p5*p3p6*p4p6 &
          - 32*self%mt**4*p1p2*p2p5*p3p5*p4p6 - 32*self%mt**4*p1p2*p2p5*p3p4*p4p6 &
          + 8*self%mt**4*p1p2*p1p5*p4p6 - 32*self%mt**4*p1p2*p1p5*p3p6*p4p6 - 32* &
          self%mt**4*p1p2*p1p5*p3p5*p4p6 - 32*self%mt**4*p1p2*p1p5*p3p4*p4p6 - 32* &
          self%mt**4*p1p2**2*p4p6 - 32*self%mt**4*p1p2**2*p4p6**2 + 32*self%mt**4*p1p2**2 &
          *p3p5*p4p6 + 32*self%mt**4*p1p2**2*p2p5*p4p6 + 32*self%mt**4*p1p2**2*p1p5* &
          p4p6 - 32*self%mt**4*self%mb2*p1p2*p4p6 - 16*self%mt**4*self%mb2*p1p2*p4p6**2 + &
          16*self%mt**4*self%mb2*p1p2*p3p6*p4p6 + 32*self%mt**4*self%mb2*p1p2*p3p5*p4p6 + &
          16*self%mt**4*self%mb2*p1p2*p3p4*p4p6 + 16*self%mt**4*self%mb2*p1p2*p2p5*p4p6 + &
          16*self%mt**4*self%mb2*p1p2*p1p5*p4p6 - 16*self%mt**4*self%mb2*p1p2**2*p4p6 - 8* &
          self%mt**4*self%mb**4*p1p2*p4p6 + 16*self%mt**6*p1p2*p4p6 + 16*self%mt**6*p1p2* &
          p4p6**2 - 16*self%mt**6*p1p2*p3p6*p4p6 - 32*self%mt**6*p1p2*p3p5*p4p6 - 16 &
          *self%mt**6*p1p2*p3p4*p4p6 - 16*self%mt**6*p1p2*p2p5*p4p6 - 16*self%mt**6*p1p2* &
          p1p5*p4p6 + 16*self%mt**6*p1p2**2*p4p6 + 16*self%mt**6*self%mb2*p1p2*p4p6 - 8 &
          *self%mt**8*p1p2*p4p6

          SpinWeight = 1.e5_dp * SpinWeight

        end if
      end if

    end select

  end function SpinWeight

!ccccccccccccccc

  function GenerateRestVectors(self, x) result(p)
    class (MatrixElements)          , intent(in) :: self
    real (dp), dimension(self%sizeX), intent(in) :: x
    real (dp), dimension(self%sizeP,0:3)         :: p
    real (dp), dimension(0:3)                    :: p1, q
    real (dp), dimension(self%sizeX/2 + 1)       :: Ctheta, Stheta
    real (dp), dimension(self%sizeX/2)           :: phi
    real (dp)                                    :: gammaW, Eb, eW, pb, modp1, vW, qnw

    Eb = (self%mt2 + self%mb2 - self%mW2)/2/self%mt
    EW = (self%mt2 + self%mW2 - self%mb2)/2/self%mt
    pb = sqrt(Eb**2 - self%mb2); phi = 2 * Pi * x(self%sizeX/2 + 2:)
    Ctheta = 2 * x(:self%sizeX/2 + 1) - 1;  Stheta = sqrt( 1 - Ctheta**2 )

    p(1,0) = Eb ;  p(1,1:3:2) = - pb * [ Stheta(1), Ctheta(1) ]; p(1,2) = 0

    select type (self)
    type is (MatrixElements6)

      p(4,0)   = Eb  ;  p(4,3) = - pb * Ctheta(3)
      p(4,1:2) = - pb * Stheta(3) * [ Cos( phi(2) ), Sin( phi(2) ) ]

      p1(0) = EW;  p1(1:) = - p(1,1:)

      modp1 = pb;  vW = pb/EW;  gammaW = 1/sqrt(1 - vW**2)

      q(0) = self%mW/2; q(3) = self%mW * Ctheta(2)/2
      q(1:2) = self%mW * Stheta(2)/2 * [ Cos( phi(1) ), Sin( phi(1) ) ]

      qnw = VecProd3(q, p1)/pb

      p(2:3,0) = gammaW * ( q(0) + [1,-1] * vW * qnw )
      p(2,1:)  = ( q(0) * vW * gammaW - (1 - gammaW) * qnw ) * p1(1:)/modp1 + q(1:)
      p(3,1:)  = ( q(0) * vW * gammaW + (1 - gammaW) * qnw ) * p1(1:)/modp1 - q(1:)

      p1(3)   = pb * Ctheta(3)
      p1(1:2) = pb * Stheta(3) * [ Cos( phi(2) ), Sin( phi(2) ) ]

      q(3)   = self%mW * Ctheta(4)/2
      q(1:2) = self%mW * Stheta(4)/2 * [ Cos( phi(3) ), Sin( phi(3) ) ]

      qnw = VecProd3(q, p1)/modp1

      p(5:6,0) = gammaW * ( q(0) + [1,-1] * vW * qnw )
      p(5,1:)  = ( q(0) * vW * gammaW - (1 - gammaW) * qnw ) * p1(1:)/modp1 + q(1:)
      p(6,1:)  = ( q(0) * vW * gammaW + (1 - gammaW) * qnw ) * p1(1:)/modp1 - q(1:)

    type is (MatrixElements4)

       p(3,0)   =   EW   ;  p(3,1:) = - p(1,1:)
       p(2,0)   =   Eb   ;  p(2,3) = - pb * Ctheta(2)
       p(2,1:2) = - pb * Stheta(2) * [ Cos( phi(1) ), Sin( phi(1) ) ]
       p(4,0)   =   EW   ;  p(4,1:) = - p(2,1:)

    end select

  end function GenerateRestVectors

!ccccccccccccccc

  function GenerateVectors(self, x) result(p)
    class (MatrixElements)          , intent(in) :: self
    real (dp), dimension(self%sizeX), intent(in) :: x
    real (dp), dimension(self%sizeP,0:3)         :: p
    real (dp), dimension(0:3)                    :: p1, q
    real (dp), dimension(self%sizeX/2 + 1)       :: Ctheta, Stheta
    real (dp), dimension(self%sizeX/2)           :: phi
    real (dp)                                    :: vT, gammaT, gammaW, Eb, eW, &
                                                    vW, qnw, pb, modp1

    Eb = (self%mt2 + self%mb2 - self%mW2)/2/self%mt
    EW = (self%mt2 + self%mW2 - self%mb2)/2/self%mt
    pb = sqrt(Eb**2 - self%mb2); phi = 2 * Pi * x(self%sizeX/2 + 2:)
    vT = sqrt(1 - 4 * self%mt2); gammaT = 1/sqrt(1 - vT**2)
    Ctheta = 2 * x(:self%sizeX/2 + 1) - 1;  Stheta = sqrt( 1 - Ctheta**2 )

    p(1,0) = gammaT * ( Eb - pb * vT * Ctheta(1) );  p(1,1) = - pb * Stheta(1) ! bottom from top
    p(1,3) = gammaT * ( Eb * vT - pb * Ctheta(1) );  p(1,2) = 0

    select type (self)
    type is (MatrixElements6)

      p(4,0)   = gammaT * ( Eb + pb * vT * Ctheta(3) )                         ! anti-bottom from anti-top
      p(4,1:2) = - pb * Stheta(3) * [ Cos( phi(2) ), Sin( phi(2) ) ]
      p(4,3)   = - gammaT * ( Eb * vT + pb * Ctheta(3) )

      p1(0) = gammaT * ( EW + pb * vT * Ctheta(1) );  p1(1:2) = - p(1,1:2)     ! W from top
      p1(3) = gammaT * ( pb * Ctheta(1) + EW * vT )

      modp1 = Abs3(p1);  vW = modp1/p1(0);  gammaW = 1/sqrt(1 - vW**2)

      q(0) = self%mW/2; q(3) = self%mW * Ctheta(2)/2
      q(1:2) = self%mW * Stheta(2)/2 * [ Cos( phi(1) ), Sin( phi(1) ) ]

      qnw = VecProd3(q, p1)/modp1

      p(2:3,0) = gammaW * ( q(0) + [1,-1] * vW * qnw )                         ! W decay procucts
      p(2,1:)  = ( q(0) * vW * gammaW - (1 - gammaW) * qnw ) * p1(1:)/modp1 + q(1:)
      p(3,1:)  = ( q(0) * vW * gammaW + (1 - gammaW) * qnw ) * p1(1:)/modp1 - q(1:)

      p1(0)   = gammaT * ( EW - pb * vT * Ctheta(3) )                          ! anti-W from anti-top
      p1(1:2) = pb * Stheta(3) * [ Cos( phi(2) ), Sin( phi(2) ) ]
      p1(3)   = gammaT * ( pb * Ctheta(3) - EW * vT )

      modp1 = Abs3(p1);  vW = modp1/p1(0);  gammaW = 1/sqrt(1 - vW**2)

      q(3) = self%mW * Ctheta(4)/2
      q(1:2) = self%mW * Stheta(4)/2 * [ Cos( phi(3) ), Sin( phi(3) ) ]

      qnw = VecProd3(q, p1)/modp1

      p(5:6,0) = gammaW * ( q(0) + [1,-1] * vW * qnw )                         ! anti-W decay procucts
      p(5,1:) = ( q(0) * vW * gammaW - (1 - gammaW) * qnw ) * p1(1:)/modp1 + q(1:)
      p(6,1:) = ( q(0) * vW * gammaW + (1 - gammaW) * qnw ) * p1(1:)/modp1 - q(1:)

    type is (MatrixElements4)

      p(3,0) = gammaT * ( EW + pb * vT * Ctheta(1) )   ;  p(3,1) = pb * Stheta(1) ! W from top
      p(3,3) = gammaT * ( EW * vT + pb * Ctheta(1) )   ;  p(3,2) = 0

      p(2,0)   =   gammaT * ( Eb + pb * vT * Ctheta(2) )                          ! anti-bottom from anti-top
      p(2,1:2) = - pb * Stheta(2) * [ Cos( phi(1) ), Sin( phi(1) ) ]
      p(2,3)   = - gammaT * ( Eb * vT + pb * Ctheta(2) )

      p(4,0) = gammaT * ( EW - pb * vT * Ctheta(2) )                              ! anti-W from anti-top
      p(4,1:2) = pb * Stheta(2) * [ Cos( phi(1) ), Sin( phi(1) ) ]
      p(4,3) = gammaT * ( pb * Ctheta(2) - EW * vT )

    end select

  end function GenerateVectors

 !ccccccccccccccc

  function CparamMinMax(self, n) result(res)
    class (MatrixElements), intent(in)   :: self
    integer               , intent(in)   :: n
    real (dp), dimension(2)              :: res
    real (dp), allocatable, dimension(:) :: x
    real (dp)                            :: ES
    integer                              :: i

    res(1) = 10; res(2) = 0

    select type (self)
    type is (MatrixElements4)
      allocate( x(3) )
    type is (MatrixElements6)
      allocate( x(7) )
    end select

    do i = 1, n
      call Random_number(x); ES = Cparam( self%GenerateVectors(x) )
      if ( ES > res(2) ) res(2) = ES;  if ( ES < res(1) ) res(1) = ES
    end do

  end function CparamMinMax

!ccccccccccccccc

  function ESMinMax(self, n) result(res)
    class (MatrixElements), intent(in)   :: self
    integer               , intent(in)   :: n
    real (dp), dimension(2,8)            :: res
    real (dp), dimension(8)              :: ES
    real (dp), allocatable, dimension(:) :: x
    integer                              :: i, j

    res(1,:) = 10; res(2,:) = 0

    select type (self)
    type is (MatrixElements4)
      allocate( x(3) )
    type is (MatrixElements6)
      allocate( x(7) )
    end select

    do i = 1, n

      call Random_number(x); ES = EScomputer( self%GenerateVectors(x) )

      do j = 1, 8
        if ( ES(j) > res(2,j) ) res(2,j) = ES(j)
        if ( ES(j) < res(1,j) ) res(1,j) = ES(j)
      end do

    end do

    deallocate(x)

  end function ESMinMax

!ccccccccccccccc

  function EScomputer(p) result(ES)
    real (dp), dimension(8)               :: ES
    real (dp), dimension(:,:), intent(in) :: p
    real (dp), dimension(3)               :: q1, eje
    real (dp), dimension(0:3)             :: q2
    real (dp), dimension(4,0:3)           :: axis
    integer                               :: i, j, k, l, npart
    real (dp)                             :: thrust, modulus, sa, sb, B, B1, HJM, &
                                             B2, LJM, vecprod, corr
    npart = size(p,1); thrust = 0

    do i = 1, npart
      do j = i + 1, npart

        q1 = CrossProd( p(i,2:), p(j,2:) );  q2 = 0

        do k = 1, npart
          if (k /= i .and. k /= j) then
            vecprod = sum( q1 * p(k,2:) )
            q2 = q2 + p(k,:) * int( vecprod/abs(vecprod) )
          end if
        end do

        axis(1,:) = q2 + p(i,:) + p(j,:);  axis(2,:) = q2 + p(i,:) - p(j,:)
        axis(3,:) = q2 - p(i,:) + p(j,:);  axis(4,:) = q2 - p(i,:) - p(j,:)

        do l = 1, 4

          modulus = sum( axis(l,1:)**2 )

          sa = (  (1 + axis(l,0) )**2 - modulus  )/4
          sb = (  (1 - axis(l,0) )**2 - modulus  )/4

          if (modulus > thrust) then
            thrust = modulus;  HJM = max(sa, sb);  LJM = min(sa, sb)
            eje    = axis(l,1:)/sqrt(modulus)
          end if

        end do
      end do
    end do

    thrust = 1 - sqrt(thrust);  B1 = 0;  B2 = 0

    do i = 1, npart

      vecprod = sum( eje * p(i,2:) )
      corr    = sum(  CrossProd( p(i,2:), eje )**2  )

      if (vecprod >= 0) then
        B1 = B1 + corr
      else
        B2 = B2 + corr
      end if
    end do

    B = (B1 + B2)/2

    ES(:6) = [ thrust, HJM, LJM, HJM + LJM, Cparam(p), B ]
    ES(7:8) = [ max(B1, B2), min(B1, B2) ]/2
    ! ES(1) = 1 - sqrt(1 - 2 * (HJM + LJM) + (HJM - LJM)**2 )

  end function EScomputer

!ccccccccccccccc

  real (dp) function Cparam(p)
    real (dp), dimension(:,:), intent(in) :: p
    integer                               :: i, j, npart

    npart = size(p,1); Cparam = 1

    do i = 1, npart
      do j = i + 1, npart
        Cparam  = Cparam - FourProd( p(i,:), p(j,:) )**2/p(i,1)/p(j,1)
      end do
    end do

    Cparam = 3 * Cparam

  end function Cparam

!ccccccccccccccc

  function CrossProd(p1, p2) result(q)
    real (dp), dimension(3), intent(in) :: p1, p2
    real (dp), dimension(3)             :: q

    q(1) = p1(2) * p2(3) - p2(2) * p1(3)
    q(2) = p1(3) * p2(1) - p2(3) * p1(1)
    q(3) = p1(1) * p2(2) - p2(1) * p1(2)

  end function CrossProd

!ccccccccccccccc

  real (dp) function FourProd(p1, p2)
    real (dp), dimension(0:3), intent(in) :: p1, p2

    FourProd = p1(0) * p2(0) - VecProd3(p1, p2)

  end function FourProd

!ccccccccccccccc

  real (dp) function VecProd3(p1, p2)
    real (dp), dimension(0:3) :: p1, p2

    VecProd3 = dot_product( p1(1:), p2(1:) )

  end function VecProd3

!ccccccccccccccc

  real (dp) function Abs3(p)
    real (dp), dimension(0:3), intent(in) :: p

    Abs3 = sqrt(  sum( p(1:)**2 )  )

  end function Abs3

!ccccccccccccccc

end module
