
! TODO: include non-zero width for top and W

module MatrixElementsClass
  use Constants, only: dp, Pi; implicit none;  private

  public :: FourProd, EScomputer, VecProd3, Abs3, MatrixElements, Cparam

!ccccccccccccccc

  type, abstract                         ::  MatrixElements
    private
    integer                              :: sizeX, sizeP
    real (dp)                            :: mt, mb, mW, mb4, mt2, mW2, mW4, mW6, mW8,&
                                            mb2, mt4, mt6, mt8, mb6, Eb, EW, pb, vt
  contains

    procedure, pass (self), public       :: ESMinMax, CparamMinMax, GenerateVectors, &
                                            SpinWeight, dimX, dimP, CparamBeta,      &
                                            GenerateRestVectors, GenerateVectors2,   &
                                            SetMasses
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

    call InMatEl4%SetMasses(mt, mb, mW, Q); InMatEl4%sizeX = 3;  InMatEl4%sizeP = 4

  end function InMatEl4

!ccccccccccccccc

  type (MatrixElements6) function InMatEl6(mt, mb, mW, Q)
    real (dp), intent(in) :: mt, mW, mb, Q

    call InMatEl6%SetMasses(mt, mb, mW, Q); InMatEl6%sizeX = 7;  InMatEl6%sizeP = 6

  end function InMatEl6

!ccccccccccccccc

  subroutine SetMasses(self, mt, mb, mW, Q)
    class (MatrixElements), intent(inout) :: self
    real (dp)             , intent(in)    :: mt, mW, mb, Q

    self%mt = mt/Q;  self%mW = mW/Q;  self%mb = mb/Q; self%mt2 = self%mt**2
    self%mW2 = self%mW**2 ;  self%mb2 = self%mb**2;   self%mt4 = self%mt2**2
    self%mt6 = self%mt2**3;  self%mb4 = self%mb2**2;  self%mt8 = self%mt4**2
    self%mW4 = self%mW2**2;  self%mW6 = self%mW2**3;  self%mW8 = self%mW4**2
    self%mb6 = self%mb2**3;  self%vT = sqrt(1 - 4 * self%mt2)
    self%Eb = (self%mt2 + self%mb2 - self%mW2)/2/self%mt
    self%EW = (self%mt2 + self%mW2 - self%mb2)/2/self%mt
    self%pb = sqrt(self%Eb**2 - self%mb2)

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
    class (MatrixElements), intent(in) :: self
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
    class (MatrixElements)          , intent(in) :: self
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
    class (MatrixElements)          , intent(in) :: self
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
    class (MatrixElements)          , intent(in) :: self
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

  function CparamMinMax(self, n) result(res)
    class (MatrixElements)  , intent(in) :: self
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

  real (dp) function CparamBeta(self, x)
    class (MatrixElements)              , intent(in) :: self
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
