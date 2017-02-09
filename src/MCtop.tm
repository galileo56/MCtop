:Evaluate:   BeginPackage["MCtop`"]

:Evaluate:   Print["     Package for tree-level production and decay of tops MC "]
:Evaluate:   Print["     Author:            Vicent Mateu          "]
:Evaluate:   Print["     Last modification: 08 - 01 - 2017        "]
:Evaluate:   Print["     Version:           test 2                "]

:Evaluate:  CparamDistribution::usage = "CparamDistribution[mt, mb, mW, Q, Spin, decay, current, Cmax, Nbins, Nevent, Niter] computes the distribution of the C-parameter event shape"
:Evaluate:  CparamComputer::usage = "CparamComputer[p] computes the value of the C-parameter event shape"
:Evaluate:  CparamList::usage = "CparamList[mt, mb, mW, Q, Cmax, Nbins] computes the values of the C-parameter event shape"
:Evaluate:  ESList::usage = "ESList[mt, mb, mW, Q, ESmax, Nbins] computes the values of the event-shape variables"
:Evaluate:  EScomputer::usage = "EScomputer[p] computes the value of the various event shapes"
:Evaluate:  Vectors4::usage = "Vectors4[x, mt, mb, mW, Q] computes the value 4 four-vectors for top decay"
:Evaluate:  Vectors6::usage = "Vectors6[x, mt, mb, mW, Q] computes the value 6 four-vectors for top decay"
:Evaluate:  ESMinMax4::usage = "ESMinMax4[n, mt, mb, mW, Q] computes the maximum and minimum value for Event shapes with 2-particle final state"
:Evaluate:  ESMinMax6::usage = "ESMinMax6[n, mt, mb, mW, Q] computes the maximum and minimum value for Event shapes with 3-particle final state"
:Evaluate:  CparamMinMax4::usage = "CparamMinMax4[n, mt, mb, mW, Q] computes the maximum and minimum value for the C-parameter Event shape with 2-particle final state"
:Evaluate:  CparamMinMax6::usage = "CparamMinMax6[n, mt, mb, mW, Q] computes the maximum and minimum value for the C-parameter Event shape with 3-particle final state"

:Evaluate:  Begin["`Private`"]

:Evaluate:  Print["You can access the complete function list typing '?MCtop`*' "]

:Begin:
:Function:      cparamcomputer
:Pattern:       CparamComputer[p_]
:Arguments:     {Flatten[Transpose[p]]}
:ArgumentTypes: {RealList}
:ReturnType:    Real
:End:

:Begin:
:Function:      cparamlist
:Pattern:       CparamList[mt_, mb_, mW_, Q_, Cmax_, Nbins_]
:Arguments:     {mt, mb, mW, Q, Cmax, Nbins}
:ArgumentTypes: {Real, Real, Real, Real, Real, Integer}
:ReturnType:    Manual
:End:

:Begin:
:Function:      cparamdistribution
:Pattern:       CparamDistribution[mt_, mb_, mW_, Q_, Spin_, decay_, current_,
                 Cmax_, Nbins_, Nevent_, Niter_]
:Arguments:     {mt, mb, mW, Q, Spin, decay, current, Cmax, Nbins, Nevent, Niter}
:ArgumentTypes: {Real, Real, Real, Real, String, String, String, Real, Integer, Integer, Integer}
:ReturnType:    Manual
:End:

:Begin:
:Function:      eslist
:Pattern:       ESList[mt_, mb_, mW_, Q_, ESmax_, Nbins_]
:Arguments:     {mt, mb, mW, Q, ESmax, Nbins}
:ArgumentTypes: {Real, Real, Real, Real, RealList, Integer}
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
:Function:      vectors6
:Pattern:       Vectors6[x_, mt_, mb_, mW_, Q_]
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
  double res[2];

   f90cparamminmax4_(&n, &mt, &mb, &mW, &Q, res);

   MLPutRealList(stdlink, res, 2);
   MLEndPacket(stdlink);
}

extern double f90cparamminmax6_(int* n, double* mt, double* mb, double* mW, double* Q, double* res);

static void cparamminmax6(int n, double mt, double mb, double mW, double Q){
  double res[2];

   f90cparamminmax6_(&n, &mt, &mb, &mW, &Q, res);

   MLPutRealList(stdlink, res, 2);
   MLEndPacket(stdlink);
}

extern double f90cparamlist_(double* mt, double* mb, double* mW, double* Q, double* Cmax,
                             int* Nbins, double* res);

static void cparamlist(double mt, double mb, double mW, double Q, double Cmax, int Nbins){
  double res[Nbins];

   f90cparamlist_(&mt, &mb, &mW, &Q, &Cmax, &Nbins, res);

   MLPutRealList(stdlink, res, Nbins);
   MLEndPacket(stdlink);

}

extern double f90cparamdistribution_(double* mt, double* mb, double* mW, double* Q,
char const* spin, char const* decay, char const* current, double* Cmax, int* Nbins,
int* Nevent, int* Niter, double* res);

static void cparamdistribution(double mt, double mb, double mW, double Q, char const* spin,
char const* decay, char const* current, double Cmax, int Nbins, int Nevent, int Niter){
  double res[3*Nbins];

   f90cparamdistribution_(&mt, &mb, &mW, &Q, spin, decay, current, &Cmax, &Nbins, &Nevent, &Niter, res);

   MLPutRealList(stdlink, res, 3*Nbins);
   MLEndPacket(stdlink);

}

extern double f90eslist_(double* mt, double* mb, double* mW, double* Q, double* ESmax,
                             int* Nbins, double* res);

static void eslist(double mt, double mb, double mW, double Q, double ESmax[], long clen, int Nbins){
  double res[8*Nbins];

   f90eslist_(&mt, &mb, &mW, &Q, ESmax, &Nbins, res);

   //MLPutFunction(stdlink, "Transpose", 1);
   MLPutFunction(stdlink, "Partition", 2);
   MLPutRealList(stdlink, res, 8*Nbins);
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
