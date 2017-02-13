:Evaluate:   BeginPackage["MCtop`"]

:Evaluate:   Print["     Package for tree-level production and decay of tops MC "]
:Evaluate:   Print["     Author:            Vicent Mateu          "]
:Evaluate:   Print["     Last modification: 08 - 01 - 2017        "]
:Evaluate:   Print["     Version:           test 2                "]

:Evaluate:  Cparam4::usage = "Cparam4[x, mt, mb, mW, Q] computes the value of C-parameter for top decay into 2 particles"
:Evaluate:  Cparam6::usage = "Cparam6[x, mt, mb, mW, Q] computes the value of C-parameter for top decay into 3 particles"
:Evaluate:  CparamBeta4::usage = "CparamBeta4[x, mt, mb, mW, Q] computes the expanded value of C-parameter for top decay into 2 particles"
:Evaluate:  CparamBeta6::usage = "CparamBeta6[x, mt, mb, mW, Q] computes the expanded value of C-parameter for top decay into 3 particles"
:Evaluate:  ESDistributions::usage = "ESDistributions[mt, mb, mW, Q, method, Spin, decay, current, ESmin, ESmax, Nbins, Nevent, Niter] computes the distribution of the event-shape variables"
:Evaluate:  CparamDistribution::usage = "CparamDistribution[mt, mb, mW, Q, expand, method, Spin, decay, current, Cmin, Cmax, Nbins, Nevent, Niter] computes the distribution of the C-parameter event shape"
:Evaluate:  CparamComputer::usage = "CparamComputer[p] computes the value of the C-parameter event shape"
:Evaluate:  CparamList::usage = "CparamList[mt, mb, mW, Q, Cmin, Cmax, Nbins] computes the values of the C-parameter event shape"
:Evaluate:  ESList::usage = "ESList[mt, mb, mW, Q, ESmin, ESmax, Nbins] computes the values of the event-shape variables"
:Evaluate:  EScomputer::usage = "EScomputer[p] computes the value of the various event shapes"
:Evaluate:  Vectors4::usage = "Vectors4[x, mt, mb, mW, Q] computes the value 4 four-vectors for top decay"
:Evaluate:  Vectors6::usage = "Vectors6[x, mt, mb, mW, Q] computes the value 6 four-vectors for top decay"
:Evaluate:  RestVectors4::usage = "Vectors4[x, mt, mb, mW, Q] computes the value 4 four-vectors for top decay, in the top rest frame"
:Evaluate:  RestVectors6::usage = "Vectors6[x, mt, mb, mW, Q] computes the value 6 four-vectors for top decay, in the top rest frame"
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
:Pattern:       CparamList[mt_, mb_, mW_, Q_, Cmin_, Cmax_, Nbins_]
:Arguments:     {mt, mb, mW, Q, Cmin, Cmax, Nbins}
:ArgumentTypes: {Real, Real, Real, Real, Real, Real, Integer}
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
:Function:      esdistributions
:Pattern:       ESDistributions[mt_, mb_, mW_, Q_, method_, spin_, decay_, current_,
                 Cmin_, Cmax_, Nbins_, Nevent_, Niter_]
:Arguments:     {mt, mb, mW, Q, method, spin, decay, current, Cmin, Cmax, Nbins, Nevent, Niter}
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

extern double f90cparamlist_(double* mt, double* mb, double* mW, double* Q, double* Cmin,
                             double* Cmax, int* Nbins, double* res);

static void cparamlist(double mt, double mb, double mW, double Q, double Cmin,
                       double Cmax, int Nbins){
  double res[Nbins];

   f90cparamlist_(&mt, &mb, &mW, &Q, &Cmin, &Cmax, &Nbins, res);

   MLPutRealList(stdlink, res, Nbins);
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
