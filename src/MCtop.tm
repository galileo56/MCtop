:Evaluate:   BeginPackage["MCtop`"]

:Evaluate:   Print["     Package for tree-level production and decay of tops MC "]
:Evaluate:   Print["     Author:            Vicent Mateu          "]
:Evaluate:   Print["     Last modification: 08 - 01 - 2017        "]
:Evaluate:   Print["     Version:           test 2                "]

:Evaluate:  B1::usage = "B1[m, Q] computes the residue for vector and axial current"
:Evaluate:  MatrixElements::usage = "MatrixElements[m, Q, h1, h2, oriented] computes the matrix elements for vector and axial current"
:Evaluate:  EShape::usage = "EShape[m, Q, h1, h2] computes the value of the event shape"
:Evaluate:  CparamLegendreDistro::usage = "CparamLegendreDistro[mt, mb, mW, Q, expand, method, Spin, decay, current, Cmin, Cmax, n, Nbins, Nevent, Niter] computes the Legendre expansion and distribution for the C-parameter event shape"
:Evaluate:  CparamLegendre::usage = "CparamLegendre[n, mt, mb, mW, Q, expand, method, Spin, decay, current, Cmin, Cmax, Nevent, Niter] computes the integration against Legendre Polynomial"
:Evaluate:  LegendreList::usage = "LegendreList[n, x] computes the of the first n + 1 Legendre Polynomial"
:Evaluate:  Cparam4::usage = "Cparam4[x, mt, mb, mW, Q] computes the value of C-parameter for top decay into 2 particles"
:Evaluate:  Cparam6::usage = "Cparam6[x, mt, mb, mW, Q] computes the value of C-parameter for top decay into 3 particles"
:Evaluate:  CparamBeta4::usage = "CparamBeta4[x, mt, mb, mW, Q] computes the expanded value of C-parameter for top decay into 2 particles"
:Evaluate:  CparamBeta6::usage = "CparamBeta6[x, mt, mb, mW, Q] computes the expanded value of C-parameter for top decay into 3 particles"
:Evaluate:  ESDistributions::usage = "ESDistributions[mt, mb, mW, Q, method, Spin, decay, current, ESmin, ESmax, Nbins, Nevent, Niter] computes the distribution of the event-shape variables"
:Evaluate:  ESLegendre::usage = "ESLegendre[mt, mb, mW, Q, method, Spin, decay, current, ESmin, ESmax, Nbins, Nevent, Niter] computes the Legendre coefficients of the event-shape variables"
:Evaluate:  CparamDistribution::usage = "CparamDistribution[mt, mb, mW, Q, expand, method, Spin, decay, current, Cmin, Cmax, Nbins, Nevent, Niter] computes the distribution of the C-parameter event shape"
:Evaluate:  CparamComputer::usage = "CparamComputer[p] computes the value of the C-parameter event shape"
:Evaluate:  CparamList::usage = "CparamList[mt, mb, mW, Q, Cmin, Cmax, Nbins] computes the values of the C-parameter event shape"
:Evaluate:  ESList::usage = "ESList[mt, mb, mW, Q, ESmin, ESmax, Nbins] computes the values of the event-shape variables"
:Evaluate:  ESListStable::usage = "ESListStable[mt, Q, Nbins] computes the values of the event-shape variables for a stable top"
:Evaluate:  EScomputer::usage = "EScomputer[p] computes the value of the various event shapes"
:Evaluate:  Vectors4::usage = "Vectors4[x, mt, mb, mW, Q] computes the value 4 four-vectors for top decay"
:Evaluate:  Vectors6::usage = "Vectors6[x, mt, mb, mW, Q] computes the value 6 four-vectors for top decay"
:Evaluate:  RestVectors4::usage = "Vectors4[x, mt, mb, mW, Q] computes the value 4 four-vectors for top decay, in the top rest frame"
:Evaluate:  RestVectors6::usage = "Vectors6[x, mt, mb, mW, Q] computes the value 6 four-vectors for top decay, in the top rest frame"
:Evaluate:  ESMinMax4::usage = "ESMinMax4[n, mt, mb, mW, Q] computes the maximum and minimum value for Event shapes with 4-particle final state"
:Evaluate:  ESMinMax6::usage = "ESMinMax6[n, mt, mb, mW, Q] computes the maximum and minimum value for Event shapes with 6-particle final state"
:Evaluate:  CparamMinMax4::usage = "CparamMinMax4[n, mt, mb, mW, Q] computes the maximum and minimum value for the C-parameter Event shape with 4-particle final state"
:Evaluate:  CparamMinMax6::usage = "CparamMinMax6[n, mt, mb, mW, Q] computes the maximum and minimum value for the C-parameter Event shape with 6-particle final state"
:Evaluate:  CparamMaxMin4::usage = "CparamMaxMin4[eps, mt, mb, mW, Q] computes the maximum and minimim values and locations for the C-parameter Event shape with 4-particle final state"
:Evaluate:  CparamMaxMin6::usage = "CparamMaxMin6[eps, mt, mb, mW, Q] computes the maximum and minimim values and locations for the C-parameter Event shape with 6-particle final state"
:Evaluate:  ESMaxMin6::usage = "ESMaxMin6[eps, mt, mb, mW, Q] computes the maximum and minimim values and locations for Event shapes with 4-particle final state"
:Evaluate:  ESMaxMin4::usage = "ESMaxMin4[eps, mt, mb, mW, Q] computes the maximum and minimim values and locations for Event shapes with 6-particle final state"
:Evaluate:  ESMax::usage = "ESMax[m,Q] computes the maximal value of the event shape"
:Evaluate:  ESMin::usage = "ESMin[m,Q] computes the minimal value of the event shape"
:Evaluate:  StableDistributions::usage = "StableDistributions[mt, Q, oriented, method, power, Nlin, Nlog, Nevent, Niter] computes the distribution of the event-shape variables"
:Evaluate:  LegendreStable::usage = "LegendreStable[mt, Q, oriented, method, n, Nevent, Niter] computes the legendre coefficients of the event-shape distributions"

:Evaluate:  Begin["`Private`"]

:Evaluate:  Print["You can access the complete function list typing '?MCtop`*' "]

:Begin:
:Function:      b1
:Pattern:       B1[m_, Q_]
:Arguments:     {m, Q}
:ArgumentTypes: {Real, Real}
:ReturnType:    Manual
:End:

:Begin:
:Function:      eshape
:Pattern:       EShape[m_, Q_, h1_, h2_]
:Arguments:     {m, Q, h1, h2}
:ArgumentTypes: {Real, Real, Real, Real}
:ReturnType:    Manual
:End:

:Begin:
:Function:      esmin
:Pattern:       ESMin[m_, Q_]
:Arguments:     {m, Q}
:ArgumentTypes: {Real, Real}
:ReturnType:    Manual
:End:

:Begin:
:Function:      esmax
:Pattern:       ESMax[m_, Q_]
:Arguments:     {m, Q}
:ArgumentTypes: {Real, Real}
:ReturnType:    Manual
:End:

:Begin:
:Function:      cparamcomputer
:Pattern:       CparamComputer[p_]
:Arguments:     {Flatten[Transpose[p]]}
:ArgumentTypes: {RealList}
:ReturnType:    Real
:End:

:Begin:
:Function:      cparamlist
:Pattern:       CparamList[mt_, mb_, mW_, Q_, Cmin_, Cmax_, Nbins_]
:Arguments:     {mt, mb, mW, Q, Cmin, Cmax, Nbins}
:ArgumentTypes: {Real, Real, Real, Real, Real, Real, Integer}
:ReturnType:    Manual
:End:

:Begin:
:Function:      legendrelist
:Pattern:       LegendreList[n_, x_]
:Arguments:     {n, x}
:ArgumentTypes: {Integer, Real}
:ReturnType:    Manual
:End:

:Begin:
:Function:      cparamdistribution
:Pattern:       CparamDistribution[mt_, mb_, mW_, Q_, expand_, method_, spin_, decay_,
                 current_, Cmin_, Cmax_, Nbins_, Nevent_, Niter_]
:Arguments:     {mt, mb, mW, Q, expand, method, spin, decay, current, Cmin, Cmax, Nbins,
                 Nevent, Niter}
:ArgumentTypes: {Real, Real, Real, Real, String, String, String, String, String, Real,
                 Real, Integer, Integer, Integer}
:ReturnType:    Manual
:End:

:Begin:
:Function:      cparamlegendredistro
:Pattern:       CparamLegendreDistro[mt_, mb_, mW_, Q_, expand_, method_, spin_,
                 decay_, current_, Cmin_, Cmax_, n_, Nbins_, Nevent_, Niter_]
:Arguments:     {mt, mb, mW, Q, expand, method, spin, decay, current, Cmin, Cmax,
                 n, Nbins, Nevent, Niter}
:ArgumentTypes: {Real, Real, Real, Real, String, String, String, String, String,
                 Real, Real, Integer, Integer, Integer, Integer}
:ReturnType:    Manual
:End:

:Begin:
:Function:      cparamlegendre
:Pattern:       CparamLegendre[n_, mt_, mb_, mW_, Q_, expand_, method_, spin_,
                 decay_, current_, Cmin_, Cmax_, Nevent_, Niter_]
:Arguments:     {n, mt, mb, mW, Q, expand, method, spin, decay, current, Cmin,
                 Cmax, Nevent, Niter}
:ArgumentTypes: {Integer, Real, Real, Real, Real, String, String, String, String,
                 String, Real, Real, Integer, Integer}
:ReturnType:    Manual
:End:

:Begin:
:Function:      esdistributions
:Pattern:       ESDistributions[mt_, mb_, mW_, Q_, method_, spin_, decay_, current_,
                 Cmin_, Cmax_, Nbins_, Nevent_, Niter_]
:Arguments:     {mt, mb, mW, Q, method, spin, decay, current, Cmin, Cmax, Nbins, Nevent, Niter}
:ArgumentTypes: {Real, Real, Real, Real, String, String, String, String, RealList, RealList,
                 Integer, Integer, Integer}
:ReturnType:    Manual
:End:

:Begin:
:Function:      stabledistributions
:Pattern:       StableDistributions[mt_, Q_, oriented_, method_, power_, Nlin_,
                 Nlog_, Nevent_, Niter_]
:Arguments:     {mt, Q, oriented, method, power, Nlin, Nlog, Nevent, Niter}
:ArgumentTypes: {Real, Real, String, String, Integer, Integer, Integer, Integer, Integer}
:ReturnType:    Manual
:End:

:Begin:
:Function:      legendrestable
:Pattern:       LegendreStable[mt_, Q_, oriented_, operation_, n_, Nevent_, Niter_]
:Arguments:     {mt, Q, oriented, method, n, Nevent, Niter}
:ArgumentTypes: {Real, Real, String, String, Integer, Integer, Integer}
:ReturnType:    Manual
:End:

:Begin:
:Function:      eslegendre
:Pattern:       ESLegendre[mt_, mb_, mW_, Q_, method_, spin_, decay_, current_,
                 Cmin_, Cmax_, n_, Nevent_, Niter_]
:Arguments:     {mt, mb, mW, Q, method, spin, decay, current, Cmin, Cmax, n, Nevent, Niter}
:ArgumentTypes: {Real, Real, Real, Real, String, String, String, String, RealList, RealList,
                 Integer, Integer, Integer}
:ReturnType:    Manual
:End:

:Begin:
:Function:      eslist
:Pattern:       ESList[mt_, mb_, mW_, Q_, ESmin_, ESmax_, Nbins_]
:Arguments:     {mt, mb, mW, Q, ESmin, ESmax, Nbins}
:ArgumentTypes: {Real, Real, Real, Real, RealList, RealList, Integer}
:ReturnType:    Manual
:End:

:Begin:
:Function:      esliststable
:Pattern:       ESListStable[mt_, Q_, Nbins_]
:Arguments:     {mt, Q, Nbins}
:ArgumentTypes: {Real, Real, Integer}
:ReturnType:    Manual
:End:

:Begin:
:Function:      escomputer
:Pattern:       EScomputer[p_]
:Arguments:     {Flatten[Transpose[p]]}
:ArgumentTypes: {RealList}
:ReturnType:    Manual
:End:

:Begin:
:Function:      esminmax4
:Pattern:       ESMinMax4[n_, mt_, mb_, mW_, Q_]
:Arguments:     {n, mt, mb, mW, Q}
:ArgumentTypes: {Integer, Real, Real, Real, Real}
:ReturnType:    Manual
:End:

:Begin:
:Function:      cparammaxmin4
:Pattern:       CparamMaxMin4[eps_, mt_, mb_, mW_, Q_]
:Arguments:     {eps, mt, mb, mW, Q}
:ArgumentTypes: {Real, Real, Real, Real, Real}
:ReturnType:    Manual
:End:

:Begin:
:Function:      esmaxmin4
:Pattern:       ESMaxMin4[eps_, mt_, mb_, mW_, Q_]
:Arguments:     {eps, mt, mb, mW, Q}
:ArgumentTypes: {Real, Real, Real, Real, Real}
:ReturnType:    Manual
:End:

:Begin:
:Function:      esmaxmin6
:Pattern:       ESMaxMin6[eps_, mt_, mb_, mW_, Q_]
:Arguments:     {eps, mt, mb, mW, Q}
:ArgumentTypes: {Real, Real, Real, Real, Real}
:ReturnType:    Manual
:End:

:Begin:
:Function:      cparammaxmin6
:Pattern:       CparamMaxMin6[eps_, mt_, mb_, mW_, Q_]
:Arguments:     {eps, mt, mb, mW, Q}
:ArgumentTypes: {Real, Real, Real, Real, Real}
:ReturnType:    Manual
:End:

:Begin:
:Function:      cparamminmax4
:Pattern:       CparamMinMax4[n_, mt_, mb_, mW_, Q_]
:Arguments:     {n, mt, mb, mW, Q}
:ArgumentTypes: {Integer, Real, Real, Real, Real}
:ReturnType:    Manual
:End:

:Begin:
:Function:      cparamminmax6
:Pattern:       CparamMinMax6[n_, mt_, mb_, mW_, Q_]
:Arguments:     {n, mt, mb, mW, Q}
:ArgumentTypes: {Integer, Real, Real, Real, Real}
:ReturnType:    Manual
:End:

:Begin:
:Function:      esminmax6
:Pattern:       ESMinMax6[n_, mt_, mb_, mW_, Q_]
:Arguments:     {n, mt, mb, mW, Q}
:ArgumentTypes: {Integer, Real, Real, Real, Real}
:ReturnType:    Manual
:End:

:Begin:
:Function:      vectors4
:Pattern:       Vectors4[x_, mt_, mb_, mW_, Q_]
:Arguments:     {x, mt, mb, mW, Q}
:ArgumentTypes: {RealList, Real, Real, Real, Real}
:ReturnType:    Manual
:End:

:Begin:
:Function:      cparam4
:Pattern:       Cparam4[x_, mt_, mb_, mW_, Q_]
:Arguments:     {x, mt, mb, mW, Q}
:ArgumentTypes: {RealList, Real, Real, Real, Real}
:ReturnType:    Real
:End:

:Begin:
:Function:      cparam6
:Pattern:       Cparam6[x_, mt_, mb_, mW_, Q_]
:Arguments:     {x, mt, mb, mW, Q}
:ArgumentTypes: {RealList, Real, Real, Real, Real}
:ReturnType:    Real
:End:

:Begin:
:Function:      cparambeta4
:Pattern:       CparamBeta4[x_, mt_, mb_, mW_, Q_]
:Arguments:     {x, mt, mb, mW, Q}
:ArgumentTypes: {RealList, Real, Real, Real, Real}
:ReturnType:    Real
:End:

:Begin:
:Function:      cparambeta6
:Pattern:       CparamBeta6[x_, mt_, mb_, mW_, Q_]
:Arguments:     {x, mt, mb, mW, Q}
:ArgumentTypes: {RealList, Real, Real, Real, Real}
:ReturnType:    Real
:End:

:Begin:
:Function:      vectors6
:Pattern:       Vectors6[x_, mt_, mb_, mW_, Q_]
:Arguments:     {x, mt, mb, mW, Q}
:ArgumentTypes: {RealList, Real, Real, Real, Real}
:ReturnType:    Manual
:End:

:Begin:
:Function:      restvectors4
:Pattern:       RestVectors4[x_, mt_, mb_, mW_, Q_]
:Arguments:     {x, mt, mb, mW, Q}
:ArgumentTypes: {RealList, Real, Real, Real, Real}
:ReturnType:    Manual
:End:

:Begin:
:Function:      restvectors6
:Pattern:       RestVectors6[x_, mt_, mb_, mW_, Q_]
:Arguments:     {x, mt, mb, mW, Q}
:ArgumentTypes: {RealList, Real, Real, Real, Real}
:ReturnType:    Manual
:End:

:Evaluate:   realQ = Head[# + 1.] === Real &

:Evaluate:   End[]

:Evaluate:   EndPackage[]

#include "mathlink.h"
#include "ftypes.h"
#include <stdio.h>
#include <unistd.h>

extern double f90eshape_(double* m, double* Q, double* h1, double* h2, double* res);

static void eshape(double m, double Q, double h1, double h2){
  double res[16];

   f90eshape_(&m, &Q, &h1, &h2, res);

   MLPutRealList(stdlink, res, 16);
   MLEndPacket(stdlink);
}

extern double f90matrixelements_(double* m, double* Q, double* h1, double* h2, char const* oriented,
                                 double* res);

static void matrixelements(double m, double Q, double h1, double h2, char const* oriented){
  double res[2];

   f90matrixelements_(&m, &Q, &h1, &h2, oriented, res);

   MLPutRealList(stdlink, res, 2);
   MLEndPacket(stdlink);
}

extern double f90residue_(double* m, double* Q, double* res);

static void b1(double m, double Q){
  double res[2];

   f90residue_(&m, &Q, res);

   MLPutRealList(stdlink, res, 2);
   MLEndPacket(stdlink);
}

extern double f90esmin_(double* m, double* Q, double* res);

static void esmin(double m, double Q){
  double res[16];

   f90esmin_(&m, &Q, res);

   MLPutRealList(stdlink, res, 16);
   MLEndPacket(stdlink);
}

extern double f90esmax_(double* m, double* Q, double* res);

static void esmax(double m, double Q){
  double res[16];

   f90esmax_(&m, &Q, res);

   MLPutRealList(stdlink, res, 16);
   MLEndPacket(stdlink);
}

extern double f90escomputer_(double* p, int* len, double* res);

static void escomputer(double p[], long clen){
  double res[8]; int len = clen/4;

   f90escomputer_(p, &len, res);

   MLPutRealList(stdlink, res, 8);
   MLEndPacket(stdlink);
}

extern double f90cparamcomputer_(double* p, int* len, double* res);

static double cparamcomputer(double p[], long clen){
  double res; int len = clen/4;

   f90cparamcomputer_(p, &len, &res);

   return res;

}

extern double f90cparam4_(double* x, double* mt, double* mb, double* mW, double* Q, double* res);

static double cparam4(double x[], long clen, double mt, double mb, double mW, double Q){
  double res;

   f90cparam4_(x, &mt, &mb, &mW, &Q, &res);

   return res;
}

extern double f90cparam6_(double* x, double* mt, double* mb, double* mW, double* Q, double* res);

static double cparam6(double x[], long clen, double mt, double mb, double mW, double Q){
  double res;

   f90cparam6_(x, &mt, &mb, &mW, &Q, &res);

   return res;
}

extern double f90cparambeta4_(double* x, double* mt, double* mb, double* mW, double* Q, double* res);

static double cparambeta4(double x[], long clen, double mt, double mb, double mW, double Q){
  double res;

   f90cparambeta4_(x, &mt, &mb, &mW, &Q, &res);

   return res;
}

extern double f90cparambeta6_(double* x, double* mt, double* mb, double* mW, double* Q, double* res);

static double cparambeta6(double x[], long clen, double mt, double mb, double mW, double Q){
  double res;

   f90cparambeta6_(x, &mt, &mb, &mW, &Q, &res);

   return res;
}

extern double f90vectors4_(double* x, double* mt, double* mb, double* mW, double* Q, double* res);

static void vectors4(double x[], long clen, double mt, double mb, double mW, double Q){
  double res[16];

   f90vectors4_(x, &mt, &mb, &mW, &Q, res);

   MLPutFunction(stdlink, "Transpose", 1);
   MLPutFunction(stdlink, "Partition", 2);
   MLPutRealList(stdlink, res, 16);
   MLPutInteger(stdlink, 4);
   MLEndPacket(stdlink);
}

extern double f90restvectors4_(double* x, double* mt, double* mb, double* mW, double* Q, double* res);

static void restvectors4(double x[], long clen, double mt, double mb, double mW, double Q){
  double res[16];

   f90restvectors4_(x, &mt, &mb, &mW, &Q, res);

   MLPutFunction(stdlink, "Transpose", 1);
   MLPutFunction(stdlink, "Partition", 2);
   MLPutRealList(stdlink, res, 16);
   MLPutInteger(stdlink, 4);
   MLEndPacket(stdlink);
}

extern double f90restvectors6_(double* x, double* mt, double* mb, double* mW, double* Q, double* res);

static void restvectors6(double x[], long clen, double mt, double mb, double mW, double Q){
  double res[24];

   f90restvectors6_(x, &mt, &mb, &mW, &Q, res);

   MLPutFunction(stdlink, "Transpose", 1);
   MLPutFunction(stdlink, "Partition", 2);
   MLPutRealList(stdlink, res, 24);
   MLPutInteger(stdlink, 6);
   MLEndPacket(stdlink);
}

extern double f90esminmax4_(int* n, double* mt, double* mb, double* mW, double* Q, double* res);

static void esminmax4(int n, double mt, double mb, double mW, double Q){
  double res[16];

   f90esminmax4_(&n, &mt, &mb, &mW, &Q, res);

   MLPutFunction(stdlink, "Partition", 2);
   MLPutRealList(stdlink, res, 16);
   MLPutInteger(stdlink, 2);
   MLEndPacket(stdlink);
}

extern double f90cparamminmax4_(int* n, double* mt, double* mb, double* mW, double* Q, double* res);

static void cparamminmax4(int n, double mt, double mb, double mW, double Q){
  double res[8];

   f90cparamminmax4_(&n, &mt, &mb, &mW, &Q, res);

   MLPutFunction(stdlink, "Partition", 2);
   MLPutRealList(stdlink, res, 8);
   MLPutInteger(stdlink, 4);
   MLEndPacket(stdlink);
}

extern double f90cparamminmax6_(int* n, double* mt, double* mb, double* mW, double* Q, double* res);

static void cparamminmax6(int n, double mt, double mb, double mW, double Q){
  double res[16];

   f90cparamminmax6_(&n, &mt, &mb, &mW, &Q, res);

   MLPutFunction(stdlink, "Partition", 2);
   MLPutRealList(stdlink, res, 16);
   MLPutInteger(stdlink, 8);
   MLEndPacket(stdlink);
}

extern double f90cparammaxmin6_(double* eps, double* mt, double* mb, double* mW, double* Q, double* res);

static void cparammaxmin6(double eps, double mt, double mb, double mW, double Q){
  double res[16];

   f90cparammaxmin6_(&eps, &mt, &mb, &mW, &Q, res);

   MLPutFunction(stdlink, "Partition", 2);
   MLPutRealList(stdlink, res, 16);
   MLPutInteger(stdlink, 8);
   MLEndPacket(stdlink);
}

extern double f90esmaxmin6_(double* eps, double* mt, double* mb, double* mW, double* Q, double* res);

static void esmaxmin6(double eps, double mt, double mb, double mW, double Q){
  double res[128];

   f90esmaxmin6_(&eps, &mt, &mb, &mW, &Q, res);

   MLPutFunction(stdlink, "Transpose", 1);
   MLPutFunction(stdlink, "Partition", 2);
   MLPutFunction(stdlink, "Partition", 2);
   MLPutRealList(stdlink, res, 128);
   MLPutInteger(stdlink, 8);
   MLPutInteger(stdlink, 8);
   MLEndPacket(stdlink);
}

extern double f90cparammaxmin4_(double* eps, double* mt, double* mb, double* mW, double* Q, double* res);

static void cparammaxmin4(double eps, double mt, double mb, double mW, double Q){
  double res[8];

   f90cparammaxmin4_(&eps, &mt, &mb, &mW, &Q, res);

   MLPutFunction(stdlink, "Partition", 2);
   MLPutRealList(stdlink, res, 8);
   MLPutInteger(stdlink, 4);
   MLEndPacket(stdlink);
}

extern double f90esmaxmin4_(double* eps, double* mt, double* mb, double* mW, double* Q, double* res);

static void esmaxmin4(double eps, double mt, double mb, double mW, double Q){
  double res[64];

   f90esmaxmin4_(&eps, &mt, &mb, &mW, &Q, res);

   MLPutFunction(stdlink, "Transpose", 1);
   MLPutFunction(stdlink, "Partition", 2);
   MLPutFunction(stdlink, "Partition", 2);
   MLPutRealList(stdlink, res, 64);
   MLPutInteger(stdlink, 4);
   MLPutInteger(stdlink, 8);
   MLEndPacket(stdlink);
}

extern double f90cparamlist_(double* mt, double* mb, double* mW, double* Q, double* Cmin,
                             double* Cmax, int* Nbins, double* res);

static void cparamlist(double mt, double mb, double mW, double Q, double Cmin,
                       double Cmax, int Nbins){
  double res[Nbins];

   f90cparamlist_(&mt, &mb, &mW, &Q, &Cmin, &Cmax, &Nbins, res);

   MLPutRealList(stdlink, res, Nbins);
   MLEndPacket(stdlink);

}

extern double f90legendrelist_(int* n, double* x, double* res);

static void legendrelist(int n, double x){
  double res[n + 1];

   f90legendrelist_(&n, &x, res);

   MLPutRealList(stdlink, res, n + 1);
   MLEndPacket(stdlink);

}

extern double f90cparamdistribution_(double* mt, double* mb, double* mW, double* Q,
  char const* expand, char const* method, char const* spin, char const* decay,
  char const* current, double* Cmin, double* Cmax, int* Nbins, int* Nevent,
  int* Niter, double* res);

static void cparamdistribution(double mt, double mb, double mW, double Q,
  char const* expand, char const* method, char const* spin, char const* decay,
  char const* current, double Cmin, double Cmax, int Nbins, int Nevent, int Niter){
  double res[3*Nbins];

   f90cparamdistribution_(&mt, &mb, &mW, &Q, expand, method, spin, decay, current,
   &Cmin, &Cmax, &Nbins, &Nevent, &Niter, res);

   MLPutFunction(stdlink, "Transpose", 1);
   MLPutFunction(stdlink, "Partition", 2);
   MLPutRealList(stdlink, res, 3*Nbins);
   MLPutInteger(stdlink, Nbins);
   MLEndPacket(stdlink);

}

extern double f90cparamlegendredistro_(double* mt, double* mb, double* mW, double* Q,
  char const* expand, char const* method, char const* spin, char const* decay,
  char const* current, double* Cmin, double* Cmax, int* n, int* Nbins, int* Nevent,
  int* Niter, double* res1,  double* res2);

static void cparamlegendredistro(double mt, double mb, double mW, double Q,
  char const* expand, char const* method, char const* spin, char const* decay,
  char const* current, double Cmin, double Cmax, int n, int Nbins, int Nevent,
  int Niter){
  double res1[3 * Nbins];
  double res2[2 * (n + 1)];

   f90cparamlegendredistro_(&mt, &mb, &mW, &Q, expand, method, spin, decay, current,
   &Cmin, &Cmax, &n, &Nbins, &Nevent, &Niter, res1, res2);

   MLPutFunction(stdlink, "List", 2);
   MLPutFunction(stdlink, "Transpose", 1);
   MLPutFunction(stdlink, "Partition", 2);
   MLPutRealList(stdlink, res1, 3*Nbins);
   MLPutInteger(stdlink, Nbins);
   MLPutFunction(stdlink, "Transpose", 1);
   MLPutFunction(stdlink, "Partition", 2);
   MLPutRealList(stdlink, res2, 2 * (n + 1) );
   MLPutInteger(stdlink, n + 1);
   MLEndPacket(stdlink);

}

extern double f90cparamlegendre_(int* n, double* mt, double* mb, double* mW, double* Q,
  char const* expand, char const* method, char const* spin, char const* decay,
  char const* current, double* Cmin, double* Cmax, int* Nevent, int* Niter, double* res);

static void cparamlegendre(int n, double mt, double mb, double mW, double Q,
  char const* expand, char const* method, char const* spin, char const* decay,
  char const* current, double Cmin, double Cmax, int Nevent, int Niter){
  double res[2 * (n + 1)];

   f90cparamlegendre_(&n, &mt, &mb, &mW, &Q, expand, method, spin, decay, current,
   &Cmin, &Cmax, &Nevent, &Niter, res);

   MLPutFunction(stdlink, "Transpose", 1);
   MLPutFunction(stdlink, "Partition", 2);
   MLPutRealList(stdlink, res, 2 * (n + 1) );
   MLPutInteger(stdlink, (n + 1));
   MLEndPacket(stdlink);

}

extern double f90esdistributions_(double* mt, double* mb, double* mW, double* Q,
char const* method, char const* spin, char const* decay, char const* current,
double* ESmin, double* ESmax, int* Nbins, int* Nevent, int* Niter, double* res);

static void esdistributions(double mt, double mb, double mW, double Q,
  char const* method, char const* spin, char const* decay, char const* current,
  double ESmin[], long lenmin, double ESmax[], long lenmax, int Nbins, int Nevent,
  int Niter){
  double res[24*Nbins];

   f90esdistributions_(&mt, &mb, &mW, &Q, method, spin, decay, current, ESmin,
     ESmax, &Nbins, &Nevent, &Niter, res);

   MLPutFunction(stdlink, "Partition", 2);
   MLPutFunction(stdlink, "Transpose", 1);
   MLPutFunction(stdlink, "Partition", 2);
   MLPutRealList(stdlink, res, 24*Nbins);
   MLPutInteger(stdlink, 8*Nbins);
   MLPutInteger(stdlink,   Nbins);
   MLEndPacket(stdlink);

}

extern double f90stabledistributions_(double* mt, double* Q, char const* oriented,
  char const* method, int* power, int* Nlin, int* Nlog, int* Nevent,
  int* Niter, double* res1, double* res2);

static void stabledistributions(double mt, double Q, char const* oriented,
  char const* method, int power, int Nlin, int Nlog, int Nevent,
  int Niter){
  double res1[80 * Nlin];
  double res2[80 * Nlog];

   f90stabledistributions_(&mt, &Q, oriented, method, &power, &Nlin, &Nlog,
                          &Nevent, &Niter, res1, res2);

   MLPutFunction(stdlink, "List", 2);
   MLPutFunction(stdlink, "Partition", 2);
   MLPutFunction(stdlink, "Transpose", 1);
   MLPutFunction(stdlink, "Partition", 2);
   MLPutRealList(stdlink, res1, 80*Nlin);
   MLPutInteger(stdlink,  16*Nlin);
   MLPutInteger(stdlink,   Nlin);
   MLPutFunction(stdlink, "Partition", 2);
   MLPutFunction(stdlink, "Transpose", 1);
   MLPutFunction(stdlink, "Partition", 2);
   MLPutRealList(stdlink, res2, 80*Nlog);
   MLPutInteger(stdlink,  16*Nlog);
   MLPutInteger(stdlink,   Nlog);
   MLEndPacket(stdlink);

}

extern double f90legendrestable_(double* mt, double* Q, char const* oriented,
  char const* method, int* n, int* Nevent, int* Niter, double* res);

static void legendrestable(double mt, double Q, char const* oriented,
  char const* method, int n, int Nevent, int Niter){
  double res[64 * (n + 1)];

   f90legendrestable_(&mt, &Q, oriented, method, &n, &Nevent, &Niter, res);

   MLPutFunction(stdlink, "Partition", 2);
   MLPutFunction(stdlink, "Transpose", 1);
   MLPutFunction(stdlink, "Partition", 2);
   MLPutRealList(stdlink, res, 64 * (n + 1) );
   MLPutInteger(stdlink,  16 * (n + 1));
   MLPutInteger(stdlink,  n + 1);

}

extern double f90eslegendre_(double* mt, double* mb, double* mW, double* Q,
char const* method, char const* spin, char const* decay, char const* current,
double* ESmin, double* ESmax, int* n, int* Nevent, int* Niter, double* res);

static void eslegendre(double mt, double mb, double mW, double Q,
  char const* method, char const* spin, char const* decay, char const* current,
  double ESmin[], long lenmin, double ESmax[], long lenmax, int n, int Nevent,
  int Niter){
  double res[16 * (n + 1) ];

   f90eslegendre_(&mt, &mb, &mW, &Q, method, spin, decay, current, ESmin,
     ESmax, &n, &Nevent, &Niter, res);

   MLPutFunction(stdlink, "Partition", 2);
   MLPutFunction(stdlink, "Transpose", 1);
   MLPutFunction(stdlink, "Partition", 2);
   MLPutRealList(stdlink, res, 16 * (n + 1) );
   MLPutInteger(stdlink ,       8 * (n + 1) );
   MLPutInteger(stdlink ,           (n + 1) );
   MLEndPacket(stdlink);

}

extern double f90eslist_(double* mt, double* mb, double* mW, double* Q, double* ESmin,
                         double* ESmax, int* Nbins, double* res);

static void eslist(double mt, double mb, double mW, double Q, double ESmin[], long clen1,
double ESmax[], long clen, int Nbins){
  double res[8*Nbins];

   f90eslist_(&mt, &mb, &mW, &Q, ESmin, ESmax, &Nbins, res);

   MLPutFunction(stdlink, "Partition", 2);
   MLPutRealList(stdlink, res, 8*Nbins);
   MLPutInteger(stdlink, Nbins);
   MLEndPacket(stdlink);

}

extern double f90esliststable_(double* mt, double* Q, int* Nbins, double* res);

static void esliststable(double mt, double Q, int Nbins){
  double res[16*Nbins];

   f90esliststable_(&mt, &Q, &Nbins, res);

   MLPutFunction(stdlink, "Partition", 2);
   MLPutRealList(stdlink, res, 16*Nbins);
   MLPutInteger(stdlink, Nbins);
   MLEndPacket(stdlink);

}

extern double f90esminmax6_(int* n, double* mt, double* mb, double* mW, double* Q, double* res);

static void esminmax6(int n, double mt, double mb, double mW, double Q){
  double res[16];

   f90esminmax6_(&n, &mt, &mb, &mW, &Q, res);

   MLPutFunction(stdlink, "Partition", 2);
   MLPutRealList(stdlink, res, 16);
   MLPutInteger(stdlink, 2);
   MLEndPacket(stdlink);
}

extern double f90vectors6_(double* x, double* mt, double* mb, double* mW, double* Q, double* res);

static void vectors6(double x[], long clen, double mt, double mb, double mW, double Q){
  double res[24];

   f90vectors6_(x, &mt, &mb, &mW, &Q, res);

   MLPutFunction(stdlink, "Transpose", 1);
   MLPutFunction(stdlink, "Partition", 2);
   MLPutRealList(stdlink, res, 24);
   MLPutInteger(stdlink, 6);
   MLEndPacket(stdlink);
}

int main(int argc, char *argv[]){
    return MLMain(argc, argv);
}
