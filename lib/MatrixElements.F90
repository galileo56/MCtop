
! TODO: include non-zero width for top and W

module MatrixElementsClass
  use Constants, only: dp, Pi, sqrt3; implicit none;  private
  real (dp), parameter                   :: mth = 0.39307568887871164_dp

  public :: FourProd, EScomputer, VecProd3, Abs3, MatrixElements, Cparam

!ccccccccccccccc

  type, abstract                         ::  MatrixElements
    private
    integer                              :: sizeX, sizeP, sizeES
    real (dp)                            :: mt, mt2, mt4, vt
  contains

    procedure, pass (self), public       :: ESMinMax, dimX, dimP, ESMaxMin

  end type MatrixElements

  type, extends(MatrixElements), public, abstract   ::  MatrixUnstable
    private
    real (dp)                            :: mb, mb2, mb4, mW, mW2, mW4, mW6, mW8,&
                                            mt6, mt8, mb6, Eb, EW, pb
  contains

    procedure, pass (self), public       :: CparamMinMax, GenerateVectors, &
                                            SpinWeight, CparamBeta, CparamMaxMin,    &
                                            GenerateRestVectors, GenerateVectors2,   &
                                            SetMasses
  end type MatrixUnstable

!ccccccccccccccc

  type, extends (MatrixUnstable), public :: MatrixElements4
    private

  end type MatrixElements4

!ccccccccccccccc

  type, extends (MatrixUnstable), public :: MatrixElements6
    private

  end type MatrixElements6

!ccccccccccccccc

  type, extends (MatrixElements), public :: MatrixStable
    private

    character (len = 6), private :: oriented

  contains

    procedure                    :: MatElComputer, ESmin, ESmax, ZY
    procedure, private           :: modulus, zPlusMinus

  end type MatrixStable

!ccccccccccccccc

  interface MatrixElements4
    module procedure InMatEl4
  end interface MatrixElements4

!ccccccccccccccc

  interface MatrixElements6
    module procedure InMatEl6
  end interface MatrixElements6

!ccccccccccccccc

  interface MatrixStable
    module procedure InitStable
  end interface MatrixStable

  contains

!ccccccccccccccc

   type (MatrixStable) function InitStable(oriented, mt, Q)
     real (dp)          , intent(in) :: mt, Q
     character (len = *), intent(in) :: oriented

    InitStable%mt = mt/Q; InitStable%mt2 = InitStable%mt**2
    InitStable%mt4 = InitStable%mt2**2      ; InitStable%sizeX  = 2
    InitStable%oriented = oriented          ; InitStable%sizeP  = 0
    InitStable%vT = (1 - 4 * InitStable%mt2); InitStable%sizeES = 16

   end function InitStable

!ccccccccccccccc

  type (MatrixElements4) function InMatEl4(mt, mb, mW, Q)
    real (dp), intent(in) :: mt, mW, mb, Q

    call InMatEl4%SetMasses(mt, mb, mW, Q); InMatEl4%sizeX = 3;  InMatEl4%sizeP = 4

  end function InMatEl4

!ccccccccccccccc

  type (MatrixElements6) function InMatEl6(mt, mb, mW, Q)
    real (dp), intent(in) :: mt, mW, mb, Q

    call InMatEl6%SetMasses(mt, mb, mW, Q); InMatEl6%sizeX = 7;  InMatEl6%sizeP = 6

  end function InMatEl6

!ccccccccccccccc

  subroutine SetMasses(self, mt, mb, mW, Q)
    class (MatrixUnstable), intent(inout) :: self
    real (dp)             , intent(in)    :: mt, mW, mb, Q

    self%mt = mt/Q;  self%mW = mW/Q;  self%mb = mb/Q; self%mt2 = self%mt**2
    self%mW2 = self%mW**2 ;  self%mb2 = self%mb**2;   self%mt4 = self%mt2**2
    self%mt6 = self%mt2**3;  self%mb4 = self%mb2**2;  self%mt8 = self%mt4**2
    self%mW4 = self%mW2**2;  self%mW6 = self%mW2**3;  self%mW8 = self%mW4**2
    self%mb6 = self%mb2**3;  self%vT = sqrt(1 - 4 * self%mt2)
    self%Eb = (self%mt2 + self%mb2 - self%mW2)/2/self%mt
    self%EW = (self%mt2 + self%mW2 - self%mb2)/2/self%mt
    self%pb = sqrt(self%Eb**2 - self%mb2); self%sizeES = 8

  end subroutine SetMasses

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

  real (dp) function SpinWeight(self, spin, current, p)
    class (MatrixUnstable), intent(in) :: self
    character (len = *)   , intent(in) :: spin, current
    real (dp)             , intent(in) :: p(self%sizeP,0:3)
    real (dp)                          :: p1p2, p1p4, p1p5, p1p6, p2p4, p2p6, a, &
                                          p2p5, p3p4, p3p5, p3p6, p4p6, p4p5
    select type (self)
    type is (MatrixElements4)
      if ( spin(:8) == 'complete' ) then

        if ( current(:6) == 'vector' ) then

          SpinWeight = 4 * self%mt4 * self%mW2 *( (self%mb2 - self%mt2)**2 + self%mb2 *      &
          self%mW2 - 2 * self%mW4 ) + ( (self%mb2 - self%mt2)**4 + 2 * self%mb2 *            &
          (self%mb2 - self%mt2)**2 * self%mW2 + (- 3 * self%mb4 + 8 * self%mb2 * self%mt2 -  &
          3 * self%mt4) * self%mW4 - 4 * self%mb2 * self%mW6 + 4 * self%mW8 ) + 2 * self%mt2 &
          * (self%mb2 - self%mt2 + 2 * self%mW2)**2 * (  (self%mb2 - self%mt2 - self%mW2) *  &
          (  FourProd( p(1,:), p(4,:) ) + FourProd( p(2,:), p(3,:) )  ) +                    &
          2 * (self%mb2 - self%mW2) * FourProd( p(3,:), p(4,:) )  )

          SpinWeight = 2 * SpinWeight/(1 + 2 * self%mt2)/( (self%mb2 - self%mt2)**2 + &
          (self%mb2 + self%mt2) * self%mW2 - 2 * self%mW4 )**2

        else if ( current(:5) == 'axial' ) then

          SpinWeight = 4 * self%mt2 * ( - (self%mb2 - self%mt2)**4 - (2 * self%mb6 -        &
          3 * self%mb4 * self%mt2 + self%mt6) * self%mW2 + 3 * (self%mb4 - 3 * self%mb2     &
          * self%mt2 + self%mt4) * self%mW4 + 2 * (2 * self%mb2 + self%mt2) * self%mW6 -    &
          4 * self%mW8 ) + ( (self%mb2 - self%mt2)**4 + 2 * self%mb2 * (self%mb2 -          &
          self%mt2)**2 * self%mW2 + (8 * self%mb2 * self%mt2 - 3 * self%mb4 - 3 * self%mt4) &
          * self%mW4 - 4 * self%mb2 * self%mW6 + 4 * self%mW8) + 2 * self%mt2 * (self%mb2   &
          - self%mt2 + 2 * self%mW2)**2 * (  - (self%mt2 - self%mb2 + self%mW2) *           &
          (  FourProd( p(1,:), p(4,:) ) + FourProd( p(2,:), p(3,:) )  ) +                   &
          2 * (self%mb2 - self%mW2) * FourProd( p(3,:), p(4,:) )  )

          SpinWeight = 2 * SpinWeight/(1 - 6 * self%mt2)/( (self%mb2 - self%mt2)**2 + &
          (self%mb2 + self%mt2) * self%mW2 - 2 * self%mW4 )**2

        end if

      else
        SpinWeight = 1
      end if
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
          p4p6 - 2*self%mb4*p1p2*p4p6 + 4*self%mt2*p1p2*p4p6**2 - 8*self%mt2*p1p2* &
          p3p6*p4p6**2 - 8*self%mt2*p1p2*p3p5*p4p6**2 - 8*self%mt2*p1p2*p3p4* &
          p4p6**2 + 4*self%mt2*p1p2**2*p4p6 - 8*self%mt2*p1p2**2*p3p5*p4p6 - 8* &
          self%mt2*p1p2**2*p2p5*p4p6 - 8*self%mt2*p1p2**2*p1p5*p4p6 + 4*self%mt2* &
          self%mb2*p1p2*p4p6 - 4*self%mt2*self%mb2*p1p2*p3p6*p4p6 - 8*self%mt2*self%mb2* &
          p1p2*p3p5*p4p6 - 4*self%mt2*self%mb2*p1p2*p3p4*p4p6 - 4*self%mt2*self%mb2* &
          p1p2*p2p5*p4p6 - 4*self%mt2*self%mb2*p1p2*p1p5*p4p6 - 2*self%mt4*p1p2* &
          p4p6 + 4*self%mt4*p1p2*p3p6*p4p6 + 4*self%mt4*p1p2*p3p4*p4p6 + 4*self%mt4 &
          *p1p2*p2p5*p4p6 + 4*self%mt4*p1p2*p1p5*p4p6

          SpinWeight = - 64 * self%mt2 * SpinWeight/(1 + 2 * self%mt2)/&
                        ( (self%mb2 - self%mt2)**2 - self%mW4 )**2

        else if ( current(:5) == 'axial' ) then

          SpinWeight = &
          - 16*p1p2**2*p4p6**2 - 8*self%mb2*p1p2*p4p6**2 - 8*self%mb2*p1p2**2* &
          p4p6 - 4*self%mb4*p1p2*p4p6 + 8*self%mt2*p1p2*p4p6**2 - 16*self%mt2*p1p2* &
          p3p6*p4p6**2 - 16*self%mt2*p1p2*p3p5*p4p6**2 - 16*self%mt2*p1p2*p3p4* &
          p4p6**2 + 8*self%mt2*p1p2**2*p4p6 + 64*self%mt2*p1p2**2*p4p6**2 - 16* &
          self%mt2*p1p2**2*p3p5*p4p6 - 16*self%mt2*p1p2**2*p2p5*p4p6 - 16*self%mt2* &
          p1p2**2*p1p5*p4p6 + 8*self%mt2*self%mb2*p1p2*p4p6 + 32*self%mt2*self%mb2* &
          p1p2*p4p6**2 - 8*self%mt2*self%mb2*p1p2*p3p6*p4p6 - 16*self%mt2*self%mb2* &
          p1p2*p3p5*p4p6 - 8*self%mt2*self%mb2*p1p2*p3p4*p4p6 - 8*self%mt2*self%mb2* &
          p1p2*p2p5*p4p6 - 8*self%mt2*self%mb2*p1p2*p1p5*p4p6 + 32*self%mt2*self%mb2* &
          p1p2**2*p4p6 + 16*self%mt2*self%mb4*p1p2*p4p6 - 4*self%mt4*p1p2*p4p6 - 32 &
          *self%mt4*p1p2*p4p6**2 + 8*self%mt4*p1p2*p3p6*p4p6 + 32*self%mt4*p1p2* &
          p3p6*p4p6**2 + 16*self%mt4*p1p2*p3p5*p4p6 + 32*self%mt4*p1p2*p3p5* &
          p4p6**2 - 32*self%mt4*p1p2*p3p5*p3p6*p4p6 - 32*self%mt4*p1p2*p3p5**2* &
          p4p6 + 8*self%mt4*p1p2*p3p4*p4p6 + 32*self%mt4*p1p2*p3p4*p4p6**2 - 32* &
          self%mt4*p1p2*p3p4*p3p5*p4p6

          SpinWeight = SpinWeight + &
          8*self%mt4*p1p2*p2p5*p4p6 - 32*self%mt4*p1p2*p2p5*p3p6*p4p6 &
          - 32*self%mt4*p1p2*p2p5*p3p5*p4p6 - 32*self%mt4*p1p2*p2p5*p3p4*p4p6 &
          + 8*self%mt4*p1p2*p1p5*p4p6 - 32*self%mt4*p1p2*p1p5*p3p6*p4p6 - 32* &
          self%mt4*p1p2*p1p5*p3p5*p4p6 - 32*self%mt4*p1p2*p1p5*p3p4*p4p6 - 32* &
          self%mt4*p1p2**2*p4p6 - 32*self%mt4*p1p2**2*p4p6**2 + 32*self%mt4*p1p2**2 &
          *p3p5*p4p6 + 32*self%mt4*p1p2**2*p2p5*p4p6 + 32*self%mt4*p1p2**2*p1p5* &
          p4p6 - 32*self%mt4*self%mb2*p1p2*p4p6 - 16*self%mt4*self%mb2*p1p2*p4p6**2 + &
          16*self%mt4*self%mb2*p1p2*p3p6*p4p6 + 32*self%mt4*self%mb2*p1p2*p3p5*p4p6 + &
          16*self%mt4*self%mb2*p1p2*p3p4*p4p6 + 16*self%mt4*self%mb2*p1p2*p2p5*p4p6 + &
          16*self%mt4*self%mb2*p1p2*p1p5*p4p6 - 16*self%mt4*self%mb2*p1p2**2*p4p6 - 8* &
          self%mt4*self%mb4*p1p2*p4p6 + 16*self%mt6*p1p2*p4p6 + 16*self%mt6*p1p2* &
          p4p6**2 - 16*self%mt6*p1p2*p3p6*p4p6 - 32*self%mt6*p1p2*p3p5*p4p6 - 16 &
          *self%mt6*p1p2*p3p4*p4p6 - 16*self%mt6*p1p2*p2p5*p4p6 - 16*self%mt6*p1p2* &
          p1p5*p4p6 + 16*self%mt6*p1p2**2*p4p6 + 16*self%mt6*self%mb2*p1p2*p4p6 - 8 &
          *self%mt8*p1p2*p4p6

          SpinWeight = - 32 * SpinWeight/(1 - 4 * self%mt2)/&
                       ( (self%mb2 - self%mt2)**2 - self%mW4 )**2

        end if
      end if

    end select

  end function SpinWeight

!ccccccccccccccc

  function GenerateRestVectors(self, x) result(p)
    class (MatrixUnstable)          , intent(in) :: self
    real (dp), dimension(self%sizeX), intent(in) :: x
    real (dp), dimension(self%sizeP,0:3)         :: p
    real (dp), dimension(0:3)                    :: p1, q
    real (dp), dimension(self%sizeX/2 + 1)       :: Ctheta, Stheta
    real (dp), dimension(self%sizeX/2)           :: phi
    real (dp)                                    :: gammaW, vW, qnw

    phi = 2 * Pi * x(self%sizeX/2 + 2:)
    Ctheta = 2 * x(:self%sizeX/2 + 1) - 1;  Stheta = sqrt( 1 - Ctheta**2 )

    p(1,0) = self%Eb ;  p(1,1:3:2) = - self%pb * [ Stheta(1), Ctheta(1) ]; p(1,2) = 0 ! bottom from top

    select type (self)
    type is (MatrixElements6)

      p(4,0)   =   self%Eb  ;  p(4,3) = - self%pb * Ctheta(3)                   ! bottom from top-bar
      p(4,1:2) = - self%pb * Stheta(3) * [ Cos( phi(2) ), Sin( phi(2) ) ]

      p1(0) = self%EW;  p1(1:) = - p(1,1:); vW = self%pb/self%EW;               ! W from top in top rest frame
      gammaW = self%EW/self%mW

      q(0)   = self%mW/2; q(3) = q(0) * Ctheta(2)                               ! W from top decay products in W rest frame
      q(1:2) = q(0) * Stheta(2) * [ Cos( phi(1) ), Sin( phi(1) ) ]

      qnw = VecProd3(q, p1)/self%pb

      p(2:3,0) = gammaW * ( q(0) + [1,-1] * vW * qnw )                          ! W decay products from top
      p(2,1:)  = ( q(0) * vW * gammaW - (1 - gammaW) * qnw ) * p1(1:)/self%pb + q(1:)
      p(3,1:)  = ( q(0) * vW * gammaW + (1 - gammaW) * qnw ) * p1(1:)/self%pb - q(1:)

      p1(1:)   = - p(4,1:);  q(3) = q(0) * Ctheta(4)

      q(1:2) = q(0) * Stheta(4) * [ Cos( phi(3) ), Sin( phi(3) ) ]

      qnw = VecProd3(q, p1)/self%pb

      p(5:6,0) = gammaW * ( q(0) + [1,-1] * vW * qnw )
      p(5,1:)  = ( q(0) * vW * gammaW - (1 - gammaW) * qnw ) * p1(1:)/self%pb + q(1:)
      p(6,1:)  = ( q(0) * vW * gammaW + (1 - gammaW) * qnw ) * p1(1:)/self%pb - q(1:)

    type is (MatrixElements4)

       p(2,0)   =   self%EW   ;  p(2,1:) = - p(1,1:)
       p(3,0)   =   self%Eb   ;  p(3,3) = - self%pb * Ctheta(2)
       p(3,1:2) = - self%pb * Stheta(2) * [ Cos( phi(1) ), Sin( phi(1) ) ]
       p(4,0)   =   self%EW   ;  p(4,1:) = - p(3,1:)

    end select

  end function GenerateRestVectors

!ccccccccccccccc

  function GenerateVectors2(self, x) result(p)
    class (MatrixUnstable)          , intent(in) :: self
    real (dp), dimension(self%sizeX), intent(in) :: x
    real (dp), dimension(self%sizeP,0:3)         :: p
    real (dp), dimension(0:3)                    :: p1, q
    real (dp), dimension(self%sizeX/2 + 1)       :: Ctheta, Stheta
    real (dp), dimension(self%sizeX/2)           :: phi
    real (dp)                                    :: gammaW, vW, qnw, modp1

    phi = 2 * Pi * x(self%sizeX/2 + 2:)
    Ctheta = 2 * x(:self%sizeX/2 + 1) - 1;  Stheta = sqrt( 1 - Ctheta**2 )

    p(1,0) = ( self%Eb - self%pb * self%vT * Ctheta(1) )/2/self%mt;  p(1,1) = - self%pb * Stheta(1) ! bottom from top
    p(1,3) = ( self%Eb * self%vT - self%pb * Ctheta(1) )/2/self%mt;  p(1,2) = 0

    select type (self)
    type is (MatrixElements6)

      p(4,0)   = ( self%Eb + self%pb * self%vT * Ctheta(3) )/2/self%mt                         ! anti-bottom from anti-top
      p(4,1:2) = - self%pb * Stheta(3) * [ Cos( phi(2) ), Sin( phi(2) ) ]
      p(4,3)   = - ( self%Eb * self%vT + self%pb * Ctheta(3) )/2/self%mt

      p1(0) = ( self%EW + self%pb * self%vT * Ctheta(1) )/2/self%mt;  p1(1:2) = - p(1,1:2)     ! W from top
      p1(3) = ( self%pb * Ctheta(1) + self%EW * self%vT )/2/self%mt

      modp1 = Abs3(p1);  vW = modp1/p1(0);  gammaW = p1(0)/self%mW

      q(0) = self%mW/2; q(3) = q(0) * Ctheta(2)
      q(1:2) = q(0) * Stheta(2) * [ Cos( phi(1) ), Sin( phi(1) ) ]

      qnw = VecProd3(q, p1)/modp1

      p(2:3,0) = gammaW * ( q(0) + [1,-1] * vW * qnw )                         ! W decay procucts
      p(2,1:)  = ( q(0) * vW * gammaW - (1 - gammaW) * qnw ) * p1(1:)/modp1 + q(1:)
      p(3,1:)  = ( q(0) * vW * gammaW + (1 - gammaW) * qnw ) * p1(1:)/modp1 - q(1:)

      p1(0)   = ( self%EW - self%pb * self%vT * Ctheta(3) )/2/self%mt                          ! anti-W from anti-top
      p1(1:2) = self%pb * Stheta(3) * [ Cos( phi(2) ), Sin( phi(2) ) ]
      p1(3)   = ( self%pb * Ctheta(3) - self%EW * self%vT )/2/self%mt

      modp1 = Abs3(p1);  vW = modp1/p1(0);  gammaW = p1(0)/self%mW

      q(3)   = q(0) * Ctheta(4)
      q(1:2) = q(0) * Stheta(4) * [ Cos( phi(3) ), Sin( phi(3) ) ]

      qnw = VecProd3(q, p1)/modp1

      p(5:6,0) = gammaW * ( q(0) + [1,-1] * vW * qnw )                         ! anti-W decay procucts
      p(5,1:) = ( q(0) * vW * gammaW - (1 - gammaW) * qnw ) * p1(1:)/modp1 + q(1:)
      p(6,1:) = ( q(0) * vW * gammaW + (1 - gammaW) * qnw ) * p1(1:)/modp1 - q(1:)

    type is (MatrixElements4)

      p(2,0) = ( self%EW + self%pb * self%vT * Ctheta(1) )/2/self%mt   ;  p(2,1) = self%pb * Stheta(1) ! W from top
      p(2,3) = ( self%EW * self%vT + self%pb * Ctheta(1) )/2/self%mt   ;  p(2,2) = 0

      p(3,0)   =   ( self%Eb + self%pb * self%vT * Ctheta(2) )/2/self%mt        ! anti-bottom from anti-top
      p(3,1:2) = - self%pb * Stheta(2) * [ Cos( phi(1) ), Sin( phi(1) ) ]
      p(3,3)   = - ( self%Eb * self%vT + self%pb * Ctheta(2) )/2/self%mt

      p(4,0)   = ( self%EW - self%pb * self%vT * Ctheta(2) )/2/self%mt            ! anti-W from anti-top
      p(4,1:2) = self%pb * Stheta(2) * [ Cos( phi(1) ), Sin( phi(1) ) ]
      p(4,3)   = ( self%pb * Ctheta(2) - self%EW * self%vT )/2/self%mt

    end select

  end function GenerateVectors2

!ccccccccccccccc

  function GenerateVectors(self, x) result(p)
    class (MatrixUnstable)          , intent(in) :: self
    real (dp), dimension(self%sizeX), intent(in) :: x
    real (dp), dimension(self%sizeP,0:3)         :: p
    integer                                      :: i

    p = self%GenerateRestVectors(x)

    do i = 1, self%sizeP/2
      p(i,0:3:3) = [ p(i,0) + self%vt * p(i,3), p(i,3) + self%vt * p(i,0)]/2/self%mt
    end do

    do i = self%sizeP/2 + 1, self%sizeP
      p(i,0:3:3) = [ p(i,0) - self%vt * p(i,3), p(i,3) - self%vt * p(i,0)]/2/self%mt
    end do

  end function GenerateVectors

 !ccccccccccccccc

  function CparamMaxMin(self, eps) result(res)
    class (MatrixUnstable)  , intent(in) :: self
    real (dp)               , intent(in) :: eps
    real (dp), dimension(0:self%sizeX,2) :: res
    real (dp), dimension(self%sizeX)     :: x
    real (dp)                            :: delta_init
    integer                              :: ktot, kmax, signo

    DELTA_INIT = 1.d-2;  KMAX = 100000000; x = 0.1_dp; signo = 1

    call f90compass_search( fun, self%sizeX, x, eps, delta_init, kmax, res(1:,1), &
                            res(0,1), ktot )
    x = 0.1_dp; signo = - 1

    call f90compass_search( fun, self%sizeX, x, eps, delta_init, kmax, res(1:,2), &
                        res(0,2), ktot ); res(0,2) = - res(0,2)

  contains

    real (dp) function fun(n, x)
      integer                         , intent(in) :: n
      real (dp), dimension(self%sizeX), intent(in) :: x
      fun = signo * Cparam( self%GenerateVectors(x) )
    end function fun

  end function CparamMaxMin

 !ccccccccccccccc

  function ESMaxMin(self, eps) result(res)
    class (MatrixElements)    , intent(in) :: self
    real (dp)                 , intent(in) :: eps
    real (dp), dimension(0:self%sizeX, self%sizeES, 2) :: res
    real (dp), dimension(self%sizeX)       :: x
    real (dp)                              :: delta_init
    integer                                :: ktot, kmax, signo, i

    DELTA_INIT = 1.d-2;  KMAX = 100000000

    do i = 1, self%sizeES

      x = 0.2_dp; signo = 1

      call f90compass_search( fun, self%sizeX, x, eps, delta_init, kmax, &
                              res(1:,i,1), res(0,i,1), ktot )
      x = 0.2_dp; signo = - 1

      call f90compass_search( fun, self%sizeX, x, eps, delta_init, kmax, res(1:,i,2), &
                              res(0,i,2), ktot ); res(0,i,2) = - res(0,i,2)
    end do

  contains

    real (dp) function fun(n, x)
      integer                         , intent(in) :: n
      real (dp), dimension(self%sizeX), intent(in) :: x
      real (dp), dimension(self%sizeES)            :: ES
      real (dp), dimension(2)                      :: ME

      select type (self)
      class is (MatrixUnstable)
        ES = EScomputer( self%GenerateVectors(x) )
      type is (MatrixStable)
        call self%MatElComputer(x(1), x(2), ME, ES)
      end select

      fun = signo * ES(i)

    end function fun

  end function ESMaxMin

 !ccccccccccccccc

  function CparamMinMax(self, n) result(res)
    class (MatrixUnstable)  , intent(in) :: self
    integer                 , intent(in) :: n
    real (dp), dimension(0:self%sizeX,2) :: res
    real (dp), dimension(self%sizeX)     :: x
    real (dp)                            :: ES
    integer                              :: i

    res(0,1) = 10; res(0,2) = 0

    do i = 1, n
      call Random_number(x); ES = Cparam( self%GenerateVectors(x) )
      if ( ES > res(0,2) ) then
        res(0,2) = ES; res(1:,2) = x
      else if ( ES < res(0,1) ) then
        res(0,1) = ES; res(1:,1) = x
      end if
    end do

  end function CparamMinMax

!ccccccccccccccc

  function ESMinMax(self, n) result(res)
    class (MatrixElements), intent(in)   :: self
    integer               , intent(in)   :: n
    real (dp), dimension(2,self%sizeES)  :: res
    real (dp), dimension(self%sizeES)    :: ES
    real (dp), dimension(self%sizeX)     :: x
    real (dp), dimension(2)              :: ME
    integer                              :: i, j

    res(1,:) = 10; res(2,:) = 0

    do i = 1, n

      call Random_number(x)
      select type(self)
      class is (MatrixUnstable)
        ES = EScomputer( self%GenerateVectors(x) )
      type is (MatrixStable)
        call self%MatElComputer(x(1), x(2), ME, ES)
      end select

      do j = 1, 8
        if ( ES(j) > res(2,j) ) res(2,j) = ES(j)
        if ( ES(j) < res(1,j) ) res(1,j) = ES(j)
      end do

    end do

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
            if ( abs(vecprod) > 0 ) q2 = q2 + p(k,:) * int( vecprod/abs(vecprod) )
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

  real (dp) function CparamBeta(self, x)
    class (MatrixUnstable)              , intent(in) :: self
    real (dp), dimension(self%sizeX)    , intent(in) :: x ! cartesian coordinates, top and anti-top rest frame
    real (dp), dimension(self%sizeP,0:3)             :: p ! cartesian coordinates, top and anti-top rest frame
    real (dp), dimension(self%sizeP,2)               :: q ! + and - light-cone coordinates
    integer                                          :: i, j

    p = self%GenerateRestVectors(x)

    do i = 1, self%sizeP
      q(i,1) = p(i,0) + p(i,3);  q(i,2) = p(i,0) - p(i,3)
    end do

    select type (self)
    type is (MatrixElements6)

      CparamBeta = 4 - self%mW4 * &
      ( 1/q(2,1)/q(3,1) + 1/q(5,2)/q(6,2) )  - 4 * (  FourProd( p(1,:), p(2,:) )**2/q(1,1)/q(2,1) &
      + FourProd( p(1,:), p(3,:) )**2/q(1,1)/q(3,1) + FourProd( p(4,:), p(5,:) )**2/q(4,2)/q(5,2) &
      + FourProd( p(4,:), p(6,:) )**2/q(4,2)/q(6,2)  )

    type is (MatrixElements4)

      CparamBeta = 4 - (self%mt2 - self%mb2 - self%mW2)**2 &
       * ( 1/q(1,1)/q(2,1) + 1/q(3,2)/q(4,2) )

    end select

    do i = 1, self%sizeP/2
      do j = self%sizeP/2 + 1, self%sizeP
        CparamBeta = CparamBeta - FourProdPerp( p(i,:), p(j,:) )**2/q(i,1)/q(j,2)
      end do
    end do

    CparamBeta = 3 * self%mt2 * CparamBeta

  end function CparamBeta

!ccccccccccccccc

  subroutine MatElComputer(self, h1, h2, MatEl, ES)
    class (MatrixStable)    , intent(in)  :: self
    real (dp)               , intent(in)  :: h1, h2
    real (dp), dimension( 2), intent(out) :: MatEl
    real (dp), dimension(16), intent(out) :: ES
    real (dp), dimension(2)               :: zPlusMinus
    real (dp)                             :: y, z, z2, y3, y1, y2, mod1, DeltaZ, &
    tauQ, tauE, tauJ, CJ, CE, CQ, CP, rho, rhoE , rhoQ, rhoP, rhoSum, Broadening, &
    BroadeningE, PNorm, z3, tau, BroadeningQ, mod2

    y = self%vt * h1; zPlusMinus = self%zPlusMinus(y)
    DeltaZ = zPlusMinus(1) - zPlusMinus(2); z = zPlusMinus(2) + DeltaZ * h2

    z2 = 1 - z; z3 = z * z2;  y3 = 1 - y; MatEl = 0

    mod1 = self%modulus(z, y); mod2 = self%modulus(z2, y); PNorm = (y + mod1 + mod2)/2

    y1 = (  sqrt( 1 - 4 * self%mt2 * (1 - z2**2) ) - z2  )/(1 - z2**2)
    y2 = (  sqrt( 1 - 4 * self%mt2 * (1 - z**2 ) ) - z   )/(1 - z**2 )

    if ( self%oriented(:2) == 'no' ) then

      MatEl = ( z3 * ( 2 * y3 + y**2 * (1 - 2 * z3) ) + [-4,8] * self%mt4 &
      - 2 * self%mt2 * (1 + [ - 2 * y3, 4 * y3 - y**2 ] * z3) )/y/z3**2

    else if ( self%oriented(:3) == 'yes' ) then

      if (z >= 0.5_dp .and. y < y1) then

        MatEl = 3 * ( z - 2 * self%mt2 * [z2, 1+z] ) * (z * z2 * y - self%mt2)/z2/z**2/mod2**2/4

      else if (z <= 0.5_dp .and. y < y2) then

        MatEl = 3 * ( z2 - 2 * self%mt2 * [z,1+z2] ) * (z * z2 * y - self%mt2)/z/z2**2/[mod2,mod1]**2/4


      else

        MatEl = 3 * ( 1 - self%mt2 * (1 + y)/z/z2/y + self%mt4/z**2/z2**2/y )/y/2

      end if

        MatEl =  MatEl + 3 * self%mt2/z/z2 * [(y3 - self%mt2/z/z2)/2/y**2, 0.25_dp]

    end if

! (1 - 4 * m**2) jacobian factor included in matrix elements

    MatEl = DeltaZ * self%vt * MatEl

    if (z >= 0.5_dp .and. y < y1) then

      tauJ = 1 - mod2;  tauQ = (y + mod1 - mod2)/2

      tauE = (  1 + y * z2 + y * (1 - 2 * z - y * z2)/mod2 - (1 - y * z) * &
      (y3 - 4 * self%mt2 - y**2 * z3)/mod1/mod2  )/2

      BroadeningQ = y * sqrt(y3 * z3 - self%mt2)/mod2
      BroadeningE = BroadeningQ * (1 - y * z + mod1)/mod1/2

      rho  = self%mt2 + y * z2;  rhoSum = self%mt2 + rho
      rhoQ = y * ( 1 - z * (2 - y) + mod1 )/2
      rhoE = y * (1 - y * z) * (  ( 1 - z * (2 - y) )/mod1 + 1  )/2

    else if (z <= 0.5 .and. y < y2) then

      tauJ = 1 - mod1;  tauQ = (y + mod2 - mod1)/2

      tauE = (  1 + y * z + y * (1 - 2 * z2 - y * z)/mod1 - (1 - y * z2) * &
      (y3 - 4 * self%mt2 - y**2 * z3)/mod1/mod2  )/2

      BroadeningQ = y * sqrt(y3 * z3 - self%mt2)/mod1
      BroadeningE = BroadeningQ * (1 - y * z2 + mod2)/mod2/2

      rho = self%mt2 + y * z;  rhoSum = self%mt2 + rho
      rhoQ = y * ( 1 - z2 * (2 - y) + mod2 )/2
      rhoE = y * (1 - y * z2) * ( (1 - z2 * (2 - y) )/mod2 + 1 )/2

    else

      tauJ = y3; rho = y3;  rhoSum = rho
      rhoQ = (1 - 4 * self%mt2 - y * (1 + y * z3) + mod1 * mod2)/2

      rhoE = (1 - y * z) * (1 - y * z2) * ( 1 + (1 - 4 * self%mt2 - y - y**2 * z3)/mod1/mod2 )/2

      tauQ = (mod1 + mod2 - y)/2

      tauE = (  2 - y - ( 1 -  z * (2 - y) ) * mod1/(1 - y * z )    &
                      - ( 1 - z2 * (2 - y) ) * mod2/(1 - y * z2)  )/2

      BroadeningQ = sqrt(y3 * z3 - self%mt2)
      BroadeningE = BroadeningQ * ( (1 - y * z)/mod1 + (1 - y * z2)/mod2 )/2

    end if

    tau = tauQ/PNorm;  Broadening = BroadeningQ/PNorm

    CE = y * (y3 * z3 - self%mt2)/mod1**2/mod2**2 * &
         ( 1 - 2 * self%mt2 * (2 - y) - y + y**2 *z3 )

    CJ = (z3 * y * y3 + 2 * self%mt2 * y3 - 2 * self%mt4)/(y3 + z3 * y**2)
    CQ = y * (y3 * z3 - self%mt2) * (mod1 + mod2 + y)/mod1/mod2/2
    CP = CQ/PNorm**2; rhoP = rhoQ/PNorm**2

    ES = [ tau, tauQ, tauE, tauJ, CJ, CE, CQ, CP, rho, rhoE, rhoQ, &
           rhoP, rhoSum, Broadening, BroadeningQ, BroadeningE ]

  end subroutine MatElComputer

!ccccccccccccccc

  function zPlusMinus(self, y) result(z)
    class(MatrixStable), intent(in) :: self
    real (dp)          , intent(in) :: y
    real (dp), dimension(2)         :: z
    real (dp)                       :: root

    root = sqrt( 1 - 4 * self%mt2/(1 - y) );   z = [1 + root, 1 - root]/2

  end function zPlusMinus

!ccccccccccccccc

  subroutine ZY(self, h1, h2, res)
    class(MatrixStable)    , intent(in)  :: self
    real (dp)              , intent(in)  :: h1, h2
    real (dp), dimension(2), intent(out) :: res
    real (dp), dimension(2)              :: zPlusMinus

    res(1) = self%vt * h1; zPlusMinus = self%zPlusMinus( res(1) )
    res(2) = zPlusMinus(2) + ( zPlusMinus(1) - zPlusMinus(2) ) * h2

  end subroutine ZY

!ccccccccccccccc

  real (dp) function modulus(self, z, y)
    class(MatrixStable), intent(in) :: self
    real (dp)          , intent(in) :: y, z

      modulus = sqrt( (1 - y * z)**2 - 4 * self%mt2 )

  end function modulus

!ccccccccccccccc

  function ESmax(self) result(ES)
    class(MatrixStable), intent(in) :: self
    real (dp), dimension(16)        :: ES
    real (dp)                       :: root1, root2

    root1 = Sqrt(1 - 3 * self%mt2); root2 = Sqrt(5 - 12 * self%mt2 - 4 * root1)

    ES(:4) = [ 1._dp, 2 * root1 - 1, (10 - 6 * self%mt2 - 8 * root1 + root2 - &
               2 * root1 * root2)/(2 - root1), 5 - 4 * sqrt(1 - 3 * self%mt2) ]/3

    if (self%mt < mth) then

      ES(5) = (1 + 16 * self%mt2 + 32 * self%mt4)/(1 + 2 * self%mt2)**2/8

    else

      ES(5) = 4 * self%mt2 * (1 + 2 * self%mt2)/(1 + 4 * self%mt2)**2

    end if

    ES(6:14) = [ 0.125_dp, (5 - 12 * self%mt2 - 4 * root1 * (1 - root2) - 2 * root2)/24,  &
               0.125_dp, ES(4), (root1 - 2)**2/3, (1 - 2 * root1) * (1 - 2 * root1 - 2 * &
               root2)/9, 1._dp/3, ES(4), 1/sqrt3/2 ]

    ES(15:) = [ 2 * root1 - 1, 2 - root1 ] * ES(14)

  end function ESmax

!ccccccccccccccc

  function ESmin(self) result(ES)
    class(MatrixStable), intent(in) :: self

    real (dp), dimension(16)        :: ES

    ES      =   0; ES(9) = self%mt2; ES(13)  = 2 * self%mt2
    ES(4:5) = [ 1 - sqrt(self%vt), 12 * self%mt2 * (1 - self%mt2)/6 ]

  end function ESmin

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

  real (dp) function FourProdPerp(p1, p2)
    real (dp), dimension(0:3), intent(in) :: p1, p2

    FourProdPerp = 4 * p1(0) * p2(0) - 2 * FourProd(p1, p2)

  end function FourProdPerp

!ccccccccccccccc

  real (dp) function VecProd3(p1, p2)
    real (dp), dimension(0:3) :: p1, p2

    VecProd3 = dot_product( p1(1:), p2(1:) )

  end function VecProd3

!ccccccccccccccc

  real (dp) function Abs3(p)
    real (dp), dimension(0:3), intent(in) :: p

    Abs3 = sqrt(  VecProd3(p, p)  )

  end function Abs3

!ccccccccccccccc

end module
