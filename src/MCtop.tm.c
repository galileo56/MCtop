/*
 * This file automatically produced by /Applications/Mathematica.app/SystemFiles/Links/MathLink/DeveloperKit/MacOSX-x86-64/CompilerAdditions/mprep from:
 *	/Users/vmateu/GitHub/MCtop/src/MCtop.tm
 * mprep Revision 18 Copyright (c) Wolfram Research, Inc. 1990-2013
 */

#define MPREP_REVISION 18

#if CARBON_MPREP
#include <Carbon/Carbon.h>
#include <mathlink/mathlink.h>
#else
#include "mathlink.h"
#endif

int MLAbort = 0;
int MLDone  = 0;
long MLSpecialCharacter = '\0';

MLINK stdlink = 0;
MLEnvironment stdenv = 0;
#if MLINTERFACE >= 3
MLYieldFunctionObject stdyielder = (MLYieldFunctionObject)0;
MLMessageHandlerObject stdhandler = (MLMessageHandlerObject)0;
#else
MLYieldFunctionObject stdyielder = 0;
MLMessageHandlerObject stdhandler = 0;
#endif /* MLINTERFACE >= 3 */

#if DARWIN_MATHLINK && CARBON_MPREP
#define rMenuBar	1128
#define rAboutBox	1128
#define rBadSIZE	1127
#define mApple		1128
#define iAbout		1
#define mFile		1129
#define mEdit		1130

AEEventHandlerUPP handle_core_ae_upp;
ModalFilterUPP about_filter_upp;
UserItemUPP outline_hook_upp;

extern pascal OSErr handle_core_ae( const AppleEvent* event, AppleEvent* reply, long refcon);
extern int init_macintosh( void);
extern void do_about_box( void);
extern int _handle_user_event( unsigned long ticks);

pascal OSErr handle_core_ae( const AppleEvent* event, AppleEvent* reply, long refcon)
{
	DescType eventid, gottype;
	Size got;
	reply = (AppleEvent*)0; /* suppress unused warning */
	refcon = 0; /* suppress unused warning */
	if( noErr == AEGetAttributePtr(event, keyEventIDAttr, typeType, &gottype, (Ptr)&eventid, sizeof(eventid), &got)
	&& errAEDescNotFound == AEGetAttributePtr(event, keyMissedKeywordAttr, typeWildCard, &gottype, nil, 0, &got)
	){
		switch(eventid){
		case kAEQuitApplication:
			MLDone = MLAbort = 1;
		case kAEOpenApplication:
			return noErr;
		}
	}
	return errAEEventNotHandled;
}


static void set_about_item(void){
	Str255 aboutitem;
	StringHandle abouthandle;

	GetMenuItemText( GetMenuHandle(mApple), iAbout, aboutitem);
	abouthandle = NewString( aboutitem);
	if( abouthandle){
		StringPtr curApName = LMGetCurApName();
		long len = Munger( (Handle)abouthandle, 1, "MathLink\252", 9, curApName + 1, *curApName); 
		if( len > 0){
			**abouthandle = (unsigned char)len; 
			HLock( (Handle)abouthandle);
			SetMenuItemText( GetMenuHandle(mApple), iAbout, *abouthandle);
		}
		DisposeHandle( (Handle)abouthandle);
	}
}


static pascal Boolean about_filter(DialogPtr dptr, EventRecord *theEvent, short *theItem){
	if( theEvent->what == keyDown || theEvent->what == autoKey){
		unsigned char theKey = (unsigned char)(theEvent->message & charCodeMask);
		if( theKey == 0x0D || (theKey == 0x03 && !(theEvent->modifiers & controlKey))){
			short itemType;
			ControlHandle okHdl;
			Rect itemRect;
#if UNIVERSAL_INTERFACES_VERSION >= 0x0301
			unsigned long Ticks;
#else
			long Ticks;
#endif
			GetDialogItem( dptr, ok, &itemType, (Handle*) &okHdl, &itemRect);
			HiliteControl( okHdl, kControlButtonPart);
#ifdef __cplusplus
			Delay( 3, &Ticks);
#else
			Delay( 3, (void *)&Ticks);
#endif
			HiliteControl( okHdl, 0);
			*theItem = ok;
			return true;
		}
	}
	return false;
}

static pascal void outline_hook(DialogRef dptr, short theItem){
	short  itemType;
	Handle itemHdl;
	Rect itemRect;
	PenState oldpen;
	GetDialogItem( dptr, theItem, &itemType, &itemHdl, &itemRect);
	GetPenState( &oldpen);
	PenSize( 3, 3);
	FrameRoundRect( &itemRect, 16, 16);
	SetPenState( &oldpen);
}



/* edit here and in mathlink.r */
static unsigned short missing_DITL[] = { 5,
	0, 0, 76, 120, 96, 200, 0x5C, 0x30, 0x30, 0x34, 0x5C, 0x30, 0x30, 0x32, 0x4F, 0x4B, /* '\004\002', 'OK', */
	0, 0, 12, 13, 30, 28, 0x5C, 0x32, 0x31, 0x30, 0x5C, 0x30, 0x30, 0x31, 0x41, 0x5C, 0x30, /*'\210\001', 'A\0', */
	0, 0, 12, 27, 30, 96, 0x5C, 0x32, 0x31, 0x30, 0x5C, 0x30, 0x31, 0x30, 0x20, 0x4D, 0x61, 0x74, 0x68, 0x4C, 0x69, 0x6E, 0x6B, /*'\210\010', 'Ma','th','Li','nk', */
	0, 0, 12, 95, 30, 308, 0x5C, 0x32, 0x31, 0x30, 0x5C, 0x30, 0x33, 0x34, 0x5C, 0x32, 0x35, 0x32, 0x20, 0x70, 0x72, 0x6F, 0x67, 0x72, 0x61, 0x6D, 0x20, 0x67, 0x65, 0x6E, 0x65, 0x72, 0x61, 0x74, 0x65, 0x64, 0x20, 0x62, 0x79, 0x20, 0x6D, 0x70, 0x72, 0x65, 0x70, /*'\210\034', '\252','pr','og','ra','m ','ge','ne','ra','te','d ','by',' m','pr','ep', */
	0, 0, 42, 49, 56, 271, 0x5C, 0x32, 0x31, 0x30, 0x5C, 0x30, 0x35, 0x30, 0x6D, 0x70, 0x72, 0x65, 0x70, 0x5C, 0x32, 0x35, 0x31, 0x20, 0x57, 0x6F, 0x6C, 0x66, 0x72, 0x61, 0x6D, 0x20, 0x52, 0x65, 0x73, 0x65, 0x61, 0x72, 0x63, 0x68, 0x2C, 0x20, 0x49, 0x6E, 0x63, 0x2E, 0x20, 0x31, 0x39, 0x39, 0x30, 0x2D, 0x32, 0x30, 0x30, 0x32, /*'\210\050', 'mp','re','p ','\251 ','Wo','lf','ra','m','Re','se','ar','ch',', ','In','c.',' 1','99','0-','20','02', */ /* 1990-2002 */
	0, 0, 170, 10, 190, 30, 0x5C, 0x32, 0x30, 0x30, 0x5C, 0x30, 0x30, 0x30 /*'\200\000' */
};


int init_macintosh( void)
{
	static int initdone = 0;
	Handle menuBar;
	long attributes;

	/* semaphore required for preemptive threads */
	if( initdone) return initdone == 1;
	initdone = -1;

	/* should I check for MLNK resource too as launch-filtering is done based on it? */
	/* too late--since I'm running there likely wasn't a problem (this time anyway). */
	
	menuBar = GetNewMBar(rMenuBar);
	if( menuBar){
		SetMenuBar(menuBar);
		DisposeHandle(menuBar);
	}else{
		MenuHandle am, fm, em;
		am = NewMenu( mApple, (unsigned char*)"\001\024");
		fm = NewMenu( mFile, (unsigned char*)"\004File");
		em = NewMenu( mEdit, (unsigned char*)"\004Edit");
		if( !am || !fm || !em) return 0;
		AppendMenu( am, (unsigned char*)"\022About MathLink\252\311;-");
                DisableMenuItem(am, 0);
		InsertMenu( am, 0);
		AppendMenu( fm, (unsigned char*)"\006Quit/Q");
		InsertMenu( fm, 0);
		AppendMenu( em, (unsigned char*)"\043Undo/Z;-;Cut/X;Copy/C;Paste/V;Clear");
                DisableMenuItem(em, 0);
		InsertMenu( em, 0);
	}

	AppendResMenu( GetMenuHandle(mApple), 'DRVR');
	set_about_item();
	DrawMenuBar();
	about_filter_upp =  NewModalFilterUPP( about_filter);
	outline_hook_upp = NewUserItemUPP( outline_hook);
	if( Gestalt( gestaltAppleEventsAttr, &attributes) == noErr
	&& ((1 << gestaltAppleEventsPresent) & attributes)){
		handle_core_ae_upp = NewAEEventHandlerUPP( handle_core_ae);
		(void) AEInstallEventHandler( kCoreEventClass, typeWildCard, handle_core_ae_upp, 0, false);
	}else{
		return 0; /* this may be too strong since I am, after all, running. */
	}

	initdone = 1;
	return initdone == 1;
}

void do_about_box( void)
{
	GrafPtr oldPort;
	DialogPtr dptr;
	short item, itemType;
	Handle itemHdl;
	Rect itemRect;

	dptr = GetNewDialog( rAboutBox, nil, (WindowPtr)-1L);
	
	if( dptr == (DialogPtr)0){
		Handle items = NewHandle( sizeof(missing_DITL));
		static Rect bounds = {40, 20, 150, 340};

		if( ! items) return;
		BlockMove( missing_DITL, *items, sizeof(missing_DITL));

		dptr = NewColorDialog( nil, &bounds, (unsigned char*)"\005About",
					false, dBoxProc, (WindowPtr)-1L, false, 0, items);
                }
	
	if( dptr == (DialogPtr)0) return;
	GetPort (&oldPort);
	SetPort (GetDialogPort(dptr));
	GetDialogItem( dptr, ok, &itemType, &itemHdl, &itemRect);
	InsetRect( &itemRect, -4, -4);
	SetDialogItem( dptr, 6, userItem + itemDisable, (Handle)outline_hook_upp, &itemRect);

	FlushEvents( everyEvent, 0);
        ShowWindow( GetDialogWindow(dptr));

	do {
		ModalDialog( about_filter_upp, &item);
	} while( item != ok);

	DisposeDialog( dptr);
	SetPort( oldPort);
}

int _handle_user_event( unsigned long ticks)
{
	EventRecord event;

	if( WaitNextEvent(everyEvent, &event, ticks, nil)){
		long      menuResult = 0;
		short     menuID, menuItem;
		WindowPtr window;
		Str255    daName;

		switch ( event.what ) {
		case mouseDown:
			if( FindWindow(event.where, &window) == inMenuBar)
				menuResult = MenuSelect(event.where);
			break;
		case keyDown:
			if( event.modifiers & cmdKey )
				menuResult = MenuKey((short)event.message & charCodeMask);
			break;
		case kHighLevelEvent:
			AEProcessAppleEvent(&event);
			break;
		}

		menuID = HiWord(menuResult);
		menuItem = LoWord(menuResult);
		switch ( menuID ) {
		case mFile:
			MLDone = MLAbort = 1;
			break;
		case mApple:
			switch ( menuItem ) {
			case iAbout:
				do_about_box();
				break;
			default:
				GetMenuItemText(GetMenuHandle(mApple), menuItem, daName);
				break;
			}
			HiliteMenu(0);
		}
	}
	return MLDone;
}

#if MLINTERFACE >= 3
MLYDEFN( int, MLDefaultYielder, ( MLINK mlp, MLYieldParameters yp))
#else
MLYDEFN( devyield_result, MLDefaultYielder, ( MLINK mlp, MLYieldParameters yp))
#endif /* MLINTERFACE >= 3 */
{
	mlp = mlp; /* suppress unused warning */
#if MLINTERFACE >= 3
	return (int)_handle_user_event( MLSleepYP(yp));
#else
	return _handle_user_event( MLSleepYP(yp));
#endif /* MLINTERFACE >= 3 */
}

#endif /* (DARWIN_MATHLINK && CARBON_MPREP */

/********************************* end header *********************************/


# line 275 "/Users/vmateu/GitHub/MCtop/src/MCtop.tm"
#include "mathlink.h"
#include "ftypes.h"
#include <stdio.h>
#include <unistd.h>

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

extern double f90cparammaxmin4_(double* eps, double* mt, double* mb, double* mW, double* Q, double* res);

static void cparammaxmin4(double eps, double mt, double mb, double mW, double Q){
  double res[8];

   f90cparammaxmin4_(&eps, &mt, &mb, &mW, &Q, res);

   MLPutFunction(stdlink, "Partition", 2);
   MLPutRealList(stdlink, res, 8);
   MLPutInteger(stdlink, 4);
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
# line 691 "/Users/vmateu/GitHub/MCtop/src/MCtop.tm.c"


void esmin P(( double _tp1, double _tp2));

#if MLPROTOTYPES
static int _tr0( MLINK mlp)
#else
static int _tr0(mlp) MLINK mlp;
#endif
{
	int	res = 0;
	double _tp1;
	double _tp2;
	if ( ! MLGetReal( mlp, &_tp1) ) goto L0;
	if ( ! MLGetReal( mlp, &_tp2) ) goto L1;
	if ( ! MLNewPacket(mlp) ) goto L2;

	esmin(_tp1, _tp2);

	res = 1;
L2: L1: 
L0:	return res;
} /* _tr0 */


void esmax P(( double _tp1, double _tp2));

#if MLPROTOTYPES
static int _tr1( MLINK mlp)
#else
static int _tr1(mlp) MLINK mlp;
#endif
{
	int	res = 0;
	double _tp1;
	double _tp2;
	if ( ! MLGetReal( mlp, &_tp1) ) goto L0;
	if ( ! MLGetReal( mlp, &_tp2) ) goto L1;
	if ( ! MLNewPacket(mlp) ) goto L2;

	esmax(_tp1, _tp2);

	res = 1;
L2: L1: 
L0:	return res;
} /* _tr1 */


double cparamcomputer P(( double * _tp1, long _tpl1));

#if MLPROTOTYPES
static int _tr2( MLINK mlp)
#else
static int _tr2(mlp) MLINK mlp;
#endif
{
	int	res = 0;
	double * _tp1;
	long _tpl1;
	double _rp0;
	if ( ! MLGetRealList( mlp, &_tp1, &_tpl1) ) goto L0;
	if ( ! MLNewPacket(mlp) ) goto L1;

	_rp0 = cparamcomputer(_tp1, _tpl1);

	res = MLAbort ?
		MLPutFunction( mlp, "Abort", 0) : MLPutReal( mlp, _rp0);
L1:	MLReleaseReal64List(mlp, _tp1, _tpl1);

L0:	return res;
} /* _tr2 */


void cparamlist P(( double _tp1, double _tp2, double _tp3, double _tp4, double _tp5, double _tp6, int _tp7));

#if MLPROTOTYPES
static int _tr3( MLINK mlp)
#else
static int _tr3(mlp) MLINK mlp;
#endif
{
	int	res = 0;
	double _tp1;
	double _tp2;
	double _tp3;
	double _tp4;
	double _tp5;
	double _tp6;
	int _tp7;
	if ( ! MLGetReal( mlp, &_tp1) ) goto L0;
	if ( ! MLGetReal( mlp, &_tp2) ) goto L1;
	if ( ! MLGetReal( mlp, &_tp3) ) goto L2;
	if ( ! MLGetReal( mlp, &_tp4) ) goto L3;
	if ( ! MLGetReal( mlp, &_tp5) ) goto L4;
	if ( ! MLGetReal( mlp, &_tp6) ) goto L5;
	if ( ! MLGetInteger( mlp, &_tp7) ) goto L6;
	if ( ! MLNewPacket(mlp) ) goto L7;

	cparamlist(_tp1, _tp2, _tp3, _tp4, _tp5, _tp6, _tp7);

	res = 1;
L7: L6: L5: L4: L3: L2: L1: 
L0:	return res;
} /* _tr3 */


void legendrelist P(( int _tp1, double _tp2));

#if MLPROTOTYPES
static int _tr4( MLINK mlp)
#else
static int _tr4(mlp) MLINK mlp;
#endif
{
	int	res = 0;
	int _tp1;
	double _tp2;
	if ( ! MLGetInteger( mlp, &_tp1) ) goto L0;
	if ( ! MLGetReal( mlp, &_tp2) ) goto L1;
	if ( ! MLNewPacket(mlp) ) goto L2;

	legendrelist(_tp1, _tp2);

	res = 1;
L2: L1: 
L0:	return res;
} /* _tr4 */


void cparamdistribution P(( double _tp1, double _tp2, double _tp3, double _tp4, const char * _tp5, const char * _tp6, const char * _tp7, const char * _tp8, const char * _tp9, double _tp10, double _tp11, int _tp12, int _tp13, int _tp14));

#if MLPROTOTYPES
static int _tr5( MLINK mlp)
#else
static int _tr5(mlp) MLINK mlp;
#endif
{
	int	res = 0;
	double _tp1;
	double _tp2;
	double _tp3;
	double _tp4;
	const char * _tp5;
	const char * _tp6;
	const char * _tp7;
	const char * _tp8;
	const char * _tp9;
	double _tp10;
	double _tp11;
	int _tp12;
	int _tp13;
	int _tp14;
	if ( ! MLGetReal( mlp, &_tp1) ) goto L0;
	if ( ! MLGetReal( mlp, &_tp2) ) goto L1;
	if ( ! MLGetReal( mlp, &_tp3) ) goto L2;
	if ( ! MLGetReal( mlp, &_tp4) ) goto L3;
	if ( ! MLGetString( mlp, &_tp5) ) goto L4;
	if ( ! MLGetString( mlp, &_tp6) ) goto L5;
	if ( ! MLGetString( mlp, &_tp7) ) goto L6;
	if ( ! MLGetString( mlp, &_tp8) ) goto L7;
	if ( ! MLGetString( mlp, &_tp9) ) goto L8;
	if ( ! MLGetReal( mlp, &_tp10) ) goto L9;
	if ( ! MLGetReal( mlp, &_tp11) ) goto L10;
	if ( ! MLGetInteger( mlp, &_tp12) ) goto L11;
	if ( ! MLGetInteger( mlp, &_tp13) ) goto L12;
	if ( ! MLGetInteger( mlp, &_tp14) ) goto L13;
	if ( ! MLNewPacket(mlp) ) goto L14;

	cparamdistribution(_tp1, _tp2, _tp3, _tp4, _tp5, _tp6, _tp7, _tp8, _tp9, _tp10, _tp11, _tp12, _tp13, _tp14);

	res = 1;
L14: L13: L12: L11: L10: L9:	MLReleaseString(mlp, _tp9);
L8:	MLReleaseString(mlp, _tp8);
L7:	MLReleaseString(mlp, _tp7);
L6:	MLReleaseString(mlp, _tp6);
L5:	MLReleaseString(mlp, _tp5);
L4: L3: L2: L1: 
L0:	return res;
} /* _tr5 */


void cparamlegendredistro P(( double _tp1, double _tp2, double _tp3, double _tp4, const char * _tp5, const char * _tp6, const char * _tp7, const char * _tp8, const char * _tp9, double _tp10, double _tp11, int _tp12, int _tp13, int _tp14, int _tp15));

#if MLPROTOTYPES
static int _tr6( MLINK mlp)
#else
static int _tr6(mlp) MLINK mlp;
#endif
{
	int	res = 0;
	double _tp1;
	double _tp2;
	double _tp3;
	double _tp4;
	const char * _tp5;
	const char * _tp6;
	const char * _tp7;
	const char * _tp8;
	const char * _tp9;
	double _tp10;
	double _tp11;
	int _tp12;
	int _tp13;
	int _tp14;
	int _tp15;
	if ( ! MLGetReal( mlp, &_tp1) ) goto L0;
	if ( ! MLGetReal( mlp, &_tp2) ) goto L1;
	if ( ! MLGetReal( mlp, &_tp3) ) goto L2;
	if ( ! MLGetReal( mlp, &_tp4) ) goto L3;
	if ( ! MLGetString( mlp, &_tp5) ) goto L4;
	if ( ! MLGetString( mlp, &_tp6) ) goto L5;
	if ( ! MLGetString( mlp, &_tp7) ) goto L6;
	if ( ! MLGetString( mlp, &_tp8) ) goto L7;
	if ( ! MLGetString( mlp, &_tp9) ) goto L8;
	if ( ! MLGetReal( mlp, &_tp10) ) goto L9;
	if ( ! MLGetReal( mlp, &_tp11) ) goto L10;
	if ( ! MLGetInteger( mlp, &_tp12) ) goto L11;
	if ( ! MLGetInteger( mlp, &_tp13) ) goto L12;
	if ( ! MLGetInteger( mlp, &_tp14) ) goto L13;
	if ( ! MLGetInteger( mlp, &_tp15) ) goto L14;
	if ( ! MLNewPacket(mlp) ) goto L15;

	cparamlegendredistro(_tp1, _tp2, _tp3, _tp4, _tp5, _tp6, _tp7, _tp8, _tp9, _tp10, _tp11, _tp12, _tp13, _tp14, _tp15);

	res = 1;
L15: L14: L13: L12: L11: L10: L9:	MLReleaseString(mlp, _tp9);
L8:	MLReleaseString(mlp, _tp8);
L7:	MLReleaseString(mlp, _tp7);
L6:	MLReleaseString(mlp, _tp6);
L5:	MLReleaseString(mlp, _tp5);
L4: L3: L2: L1: 
L0:	return res;
} /* _tr6 */


void cparamlegendre P(( int _tp1, double _tp2, double _tp3, double _tp4, double _tp5, const char * _tp6, const char * _tp7, const char * _tp8, const char * _tp9, const char * _tp10, double _tp11, double _tp12, int _tp13, int _tp14));

#if MLPROTOTYPES
static int _tr7( MLINK mlp)
#else
static int _tr7(mlp) MLINK mlp;
#endif
{
	int	res = 0;
	int _tp1;
	double _tp2;
	double _tp3;
	double _tp4;
	double _tp5;
	const char * _tp6;
	const char * _tp7;
	const char * _tp8;
	const char * _tp9;
	const char * _tp10;
	double _tp11;
	double _tp12;
	int _tp13;
	int _tp14;
	if ( ! MLGetInteger( mlp, &_tp1) ) goto L0;
	if ( ! MLGetReal( mlp, &_tp2) ) goto L1;
	if ( ! MLGetReal( mlp, &_tp3) ) goto L2;
	if ( ! MLGetReal( mlp, &_tp4) ) goto L3;
	if ( ! MLGetReal( mlp, &_tp5) ) goto L4;
	if ( ! MLGetString( mlp, &_tp6) ) goto L5;
	if ( ! MLGetString( mlp, &_tp7) ) goto L6;
	if ( ! MLGetString( mlp, &_tp8) ) goto L7;
	if ( ! MLGetString( mlp, &_tp9) ) goto L8;
	if ( ! MLGetString( mlp, &_tp10) ) goto L9;
	if ( ! MLGetReal( mlp, &_tp11) ) goto L10;
	if ( ! MLGetReal( mlp, &_tp12) ) goto L11;
	if ( ! MLGetInteger( mlp, &_tp13) ) goto L12;
	if ( ! MLGetInteger( mlp, &_tp14) ) goto L13;
	if ( ! MLNewPacket(mlp) ) goto L14;

	cparamlegendre(_tp1, _tp2, _tp3, _tp4, _tp5, _tp6, _tp7, _tp8, _tp9, _tp10, _tp11, _tp12, _tp13, _tp14);

	res = 1;
L14: L13: L12: L11: L10:	MLReleaseString(mlp, _tp10);
L9:	MLReleaseString(mlp, _tp9);
L8:	MLReleaseString(mlp, _tp8);
L7:	MLReleaseString(mlp, _tp7);
L6:	MLReleaseString(mlp, _tp6);
L5: L4: L3: L2: L1: 
L0:	return res;
} /* _tr7 */


void esdistributions P(( double _tp1, double _tp2, double _tp3, double _tp4, const char * _tp5, const char * _tp6, const char * _tp7, const char * _tp8, double * _tp9, long _tpl9, double * _tp10, long _tpl10, int _tp11, int _tp12, int _tp13));

#if MLPROTOTYPES
static int _tr8( MLINK mlp)
#else
static int _tr8(mlp) MLINK mlp;
#endif
{
	int	res = 0;
	double _tp1;
	double _tp2;
	double _tp3;
	double _tp4;
	const char * _tp5;
	const char * _tp6;
	const char * _tp7;
	const char * _tp8;
	double * _tp9;
	long _tpl9;
	double * _tp10;
	long _tpl10;
	int _tp11;
	int _tp12;
	int _tp13;
	if ( ! MLGetReal( mlp, &_tp1) ) goto L0;
	if ( ! MLGetReal( mlp, &_tp2) ) goto L1;
	if ( ! MLGetReal( mlp, &_tp3) ) goto L2;
	if ( ! MLGetReal( mlp, &_tp4) ) goto L3;
	if ( ! MLGetString( mlp, &_tp5) ) goto L4;
	if ( ! MLGetString( mlp, &_tp6) ) goto L5;
	if ( ! MLGetString( mlp, &_tp7) ) goto L6;
	if ( ! MLGetString( mlp, &_tp8) ) goto L7;
	if ( ! MLGetRealList( mlp, &_tp9, &_tpl9) ) goto L8;
	if ( ! MLGetRealList( mlp, &_tp10, &_tpl10) ) goto L9;
	if ( ! MLGetInteger( mlp, &_tp11) ) goto L10;
	if ( ! MLGetInteger( mlp, &_tp12) ) goto L11;
	if ( ! MLGetInteger( mlp, &_tp13) ) goto L12;
	if ( ! MLNewPacket(mlp) ) goto L13;

	esdistributions(_tp1, _tp2, _tp3, _tp4, _tp5, _tp6, _tp7, _tp8, _tp9, _tpl9, _tp10, _tpl10, _tp11, _tp12, _tp13);

	res = 1;
L13: L12: L11: L10:	MLReleaseReal64List(mlp, _tp10, _tpl10);
L9:	MLReleaseReal64List(mlp, _tp9, _tpl9);
L8:	MLReleaseString(mlp, _tp8);
L7:	MLReleaseString(mlp, _tp7);
L6:	MLReleaseString(mlp, _tp6);
L5:	MLReleaseString(mlp, _tp5);
L4: L3: L2: L1: 
L0:	return res;
} /* _tr8 */


void eslegendre P(( double _tp1, double _tp2, double _tp3, double _tp4, const char * _tp5, const char * _tp6, const char * _tp7, const char * _tp8, double * _tp9, long _tpl9, double * _tp10, long _tpl10, int _tp11, int _tp12, int _tp13));

#if MLPROTOTYPES
static int _tr9( MLINK mlp)
#else
static int _tr9(mlp) MLINK mlp;
#endif
{
	int	res = 0;
	double _tp1;
	double _tp2;
	double _tp3;
	double _tp4;
	const char * _tp5;
	const char * _tp6;
	const char * _tp7;
	const char * _tp8;
	double * _tp9;
	long _tpl9;
	double * _tp10;
	long _tpl10;
	int _tp11;
	int _tp12;
	int _tp13;
	if ( ! MLGetReal( mlp, &_tp1) ) goto L0;
	if ( ! MLGetReal( mlp, &_tp2) ) goto L1;
	if ( ! MLGetReal( mlp, &_tp3) ) goto L2;
	if ( ! MLGetReal( mlp, &_tp4) ) goto L3;
	if ( ! MLGetString( mlp, &_tp5) ) goto L4;
	if ( ! MLGetString( mlp, &_tp6) ) goto L5;
	if ( ! MLGetString( mlp, &_tp7) ) goto L6;
	if ( ! MLGetString( mlp, &_tp8) ) goto L7;
	if ( ! MLGetRealList( mlp, &_tp9, &_tpl9) ) goto L8;
	if ( ! MLGetRealList( mlp, &_tp10, &_tpl10) ) goto L9;
	if ( ! MLGetInteger( mlp, &_tp11) ) goto L10;
	if ( ! MLGetInteger( mlp, &_tp12) ) goto L11;
	if ( ! MLGetInteger( mlp, &_tp13) ) goto L12;
	if ( ! MLNewPacket(mlp) ) goto L13;

	eslegendre(_tp1, _tp2, _tp3, _tp4, _tp5, _tp6, _tp7, _tp8, _tp9, _tpl9, _tp10, _tpl10, _tp11, _tp12, _tp13);

	res = 1;
L13: L12: L11: L10:	MLReleaseReal64List(mlp, _tp10, _tpl10);
L9:	MLReleaseReal64List(mlp, _tp9, _tpl9);
L8:	MLReleaseString(mlp, _tp8);
L7:	MLReleaseString(mlp, _tp7);
L6:	MLReleaseString(mlp, _tp6);
L5:	MLReleaseString(mlp, _tp5);
L4: L3: L2: L1: 
L0:	return res;
} /* _tr9 */


void eslist P(( double _tp1, double _tp2, double _tp3, double _tp4, double * _tp5, long _tpl5, double * _tp6, long _tpl6, int _tp7));

#if MLPROTOTYPES
static int _tr10( MLINK mlp)
#else
static int _tr10(mlp) MLINK mlp;
#endif
{
	int	res = 0;
	double _tp1;
	double _tp2;
	double _tp3;
	double _tp4;
	double * _tp5;
	long _tpl5;
	double * _tp6;
	long _tpl6;
	int _tp7;
	if ( ! MLGetReal( mlp, &_tp1) ) goto L0;
	if ( ! MLGetReal( mlp, &_tp2) ) goto L1;
	if ( ! MLGetReal( mlp, &_tp3) ) goto L2;
	if ( ! MLGetReal( mlp, &_tp4) ) goto L3;
	if ( ! MLGetRealList( mlp, &_tp5, &_tpl5) ) goto L4;
	if ( ! MLGetRealList( mlp, &_tp6, &_tpl6) ) goto L5;
	if ( ! MLGetInteger( mlp, &_tp7) ) goto L6;
	if ( ! MLNewPacket(mlp) ) goto L7;

	eslist(_tp1, _tp2, _tp3, _tp4, _tp5, _tpl5, _tp6, _tpl6, _tp7);

	res = 1;
L7: L6:	MLReleaseReal64List(mlp, _tp6, _tpl6);
L5:	MLReleaseReal64List(mlp, _tp5, _tpl5);
L4: L3: L2: L1: 
L0:	return res;
} /* _tr10 */


void esliststable P(( double _tp1, double _tp2, int _tp3));

#if MLPROTOTYPES
static int _tr11( MLINK mlp)
#else
static int _tr11(mlp) MLINK mlp;
#endif
{
	int	res = 0;
	double _tp1;
	double _tp2;
	int _tp3;
	if ( ! MLGetReal( mlp, &_tp1) ) goto L0;
	if ( ! MLGetReal( mlp, &_tp2) ) goto L1;
	if ( ! MLGetInteger( mlp, &_tp3) ) goto L2;
	if ( ! MLNewPacket(mlp) ) goto L3;

	esliststable(_tp1, _tp2, _tp3);

	res = 1;
L3: L2: L1: 
L0:	return res;
} /* _tr11 */


void escomputer P(( double * _tp1, long _tpl1));

#if MLPROTOTYPES
static int _tr12( MLINK mlp)
#else
static int _tr12(mlp) MLINK mlp;
#endif
{
	int	res = 0;
	double * _tp1;
	long _tpl1;
	if ( ! MLGetRealList( mlp, &_tp1, &_tpl1) ) goto L0;
	if ( ! MLNewPacket(mlp) ) goto L1;

	escomputer(_tp1, _tpl1);

	res = 1;
L1:	MLReleaseReal64List(mlp, _tp1, _tpl1);

L0:	return res;
} /* _tr12 */


void esminmax4 P(( int _tp1, double _tp2, double _tp3, double _tp4, double _tp5));

#if MLPROTOTYPES
static int _tr13( MLINK mlp)
#else
static int _tr13(mlp) MLINK mlp;
#endif
{
	int	res = 0;
	int _tp1;
	double _tp2;
	double _tp3;
	double _tp4;
	double _tp5;
	if ( ! MLGetInteger( mlp, &_tp1) ) goto L0;
	if ( ! MLGetReal( mlp, &_tp2) ) goto L1;
	if ( ! MLGetReal( mlp, &_tp3) ) goto L2;
	if ( ! MLGetReal( mlp, &_tp4) ) goto L3;
	if ( ! MLGetReal( mlp, &_tp5) ) goto L4;
	if ( ! MLNewPacket(mlp) ) goto L5;

	esminmax4(_tp1, _tp2, _tp3, _tp4, _tp5);

	res = 1;
L5: L4: L3: L2: L1: 
L0:	return res;
} /* _tr13 */


void cparammaxmin4 P(( double _tp1, double _tp2, double _tp3, double _tp4, double _tp5));

#if MLPROTOTYPES
static int _tr14( MLINK mlp)
#else
static int _tr14(mlp) MLINK mlp;
#endif
{
	int	res = 0;
	double _tp1;
	double _tp2;
	double _tp3;
	double _tp4;
	double _tp5;
	if ( ! MLGetReal( mlp, &_tp1) ) goto L0;
	if ( ! MLGetReal( mlp, &_tp2) ) goto L1;
	if ( ! MLGetReal( mlp, &_tp3) ) goto L2;
	if ( ! MLGetReal( mlp, &_tp4) ) goto L3;
	if ( ! MLGetReal( mlp, &_tp5) ) goto L4;
	if ( ! MLNewPacket(mlp) ) goto L5;

	cparammaxmin4(_tp1, _tp2, _tp3, _tp4, _tp5);

	res = 1;
L5: L4: L3: L2: L1: 
L0:	return res;
} /* _tr14 */


void cparammaxmin6 P(( double _tp1, double _tp2, double _tp3, double _tp4, double _tp5));

#if MLPROTOTYPES
static int _tr15( MLINK mlp)
#else
static int _tr15(mlp) MLINK mlp;
#endif
{
	int	res = 0;
	double _tp1;
	double _tp2;
	double _tp3;
	double _tp4;
	double _tp5;
	if ( ! MLGetReal( mlp, &_tp1) ) goto L0;
	if ( ! MLGetReal( mlp, &_tp2) ) goto L1;
	if ( ! MLGetReal( mlp, &_tp3) ) goto L2;
	if ( ! MLGetReal( mlp, &_tp4) ) goto L3;
	if ( ! MLGetReal( mlp, &_tp5) ) goto L4;
	if ( ! MLNewPacket(mlp) ) goto L5;

	cparammaxmin6(_tp1, _tp2, _tp3, _tp4, _tp5);

	res = 1;
L5: L4: L3: L2: L1: 
L0:	return res;
} /* _tr15 */


void cparamminmax4 P(( int _tp1, double _tp2, double _tp3, double _tp4, double _tp5));

#if MLPROTOTYPES
static int _tr16( MLINK mlp)
#else
static int _tr16(mlp) MLINK mlp;
#endif
{
	int	res = 0;
	int _tp1;
	double _tp2;
	double _tp3;
	double _tp4;
	double _tp5;
	if ( ! MLGetInteger( mlp, &_tp1) ) goto L0;
	if ( ! MLGetReal( mlp, &_tp2) ) goto L1;
	if ( ! MLGetReal( mlp, &_tp3) ) goto L2;
	if ( ! MLGetReal( mlp, &_tp4) ) goto L3;
	if ( ! MLGetReal( mlp, &_tp5) ) goto L4;
	if ( ! MLNewPacket(mlp) ) goto L5;

	cparamminmax4(_tp1, _tp2, _tp3, _tp4, _tp5);

	res = 1;
L5: L4: L3: L2: L1: 
L0:	return res;
} /* _tr16 */


void cparamminmax6 P(( int _tp1, double _tp2, double _tp3, double _tp4, double _tp5));

#if MLPROTOTYPES
static int _tr17( MLINK mlp)
#else
static int _tr17(mlp) MLINK mlp;
#endif
{
	int	res = 0;
	int _tp1;
	double _tp2;
	double _tp3;
	double _tp4;
	double _tp5;
	if ( ! MLGetInteger( mlp, &_tp1) ) goto L0;
	if ( ! MLGetReal( mlp, &_tp2) ) goto L1;
	if ( ! MLGetReal( mlp, &_tp3) ) goto L2;
	if ( ! MLGetReal( mlp, &_tp4) ) goto L3;
	if ( ! MLGetReal( mlp, &_tp5) ) goto L4;
	if ( ! MLNewPacket(mlp) ) goto L5;

	cparamminmax6(_tp1, _tp2, _tp3, _tp4, _tp5);

	res = 1;
L5: L4: L3: L2: L1: 
L0:	return res;
} /* _tr17 */


void esminmax6 P(( int _tp1, double _tp2, double _tp3, double _tp4, double _tp5));

#if MLPROTOTYPES
static int _tr18( MLINK mlp)
#else
static int _tr18(mlp) MLINK mlp;
#endif
{
	int	res = 0;
	int _tp1;
	double _tp2;
	double _tp3;
	double _tp4;
	double _tp5;
	if ( ! MLGetInteger( mlp, &_tp1) ) goto L0;
	if ( ! MLGetReal( mlp, &_tp2) ) goto L1;
	if ( ! MLGetReal( mlp, &_tp3) ) goto L2;
	if ( ! MLGetReal( mlp, &_tp4) ) goto L3;
	if ( ! MLGetReal( mlp, &_tp5) ) goto L4;
	if ( ! MLNewPacket(mlp) ) goto L5;

	esminmax6(_tp1, _tp2, _tp3, _tp4, _tp5);

	res = 1;
L5: L4: L3: L2: L1: 
L0:	return res;
} /* _tr18 */


void vectors4 P(( double * _tp1, long _tpl1, double _tp2, double _tp3, double _tp4, double _tp5));

#if MLPROTOTYPES
static int _tr19( MLINK mlp)
#else
static int _tr19(mlp) MLINK mlp;
#endif
{
	int	res = 0;
	double * _tp1;
	long _tpl1;
	double _tp2;
	double _tp3;
	double _tp4;
	double _tp5;
	if ( ! MLGetRealList( mlp, &_tp1, &_tpl1) ) goto L0;
	if ( ! MLGetReal( mlp, &_tp2) ) goto L1;
	if ( ! MLGetReal( mlp, &_tp3) ) goto L2;
	if ( ! MLGetReal( mlp, &_tp4) ) goto L3;
	if ( ! MLGetReal( mlp, &_tp5) ) goto L4;
	if ( ! MLNewPacket(mlp) ) goto L5;

	vectors4(_tp1, _tpl1, _tp2, _tp3, _tp4, _tp5);

	res = 1;
L5: L4: L3: L2: L1:	MLReleaseReal64List(mlp, _tp1, _tpl1);

L0:	return res;
} /* _tr19 */


double cparam4 P(( double * _tp1, long _tpl1, double _tp2, double _tp3, double _tp4, double _tp5));

#if MLPROTOTYPES
static int _tr20( MLINK mlp)
#else
static int _tr20(mlp) MLINK mlp;
#endif
{
	int	res = 0;
	double * _tp1;
	long _tpl1;
	double _tp2;
	double _tp3;
	double _tp4;
	double _tp5;
	double _rp0;
	if ( ! MLGetRealList( mlp, &_tp1, &_tpl1) ) goto L0;
	if ( ! MLGetReal( mlp, &_tp2) ) goto L1;
	if ( ! MLGetReal( mlp, &_tp3) ) goto L2;
	if ( ! MLGetReal( mlp, &_tp4) ) goto L3;
	if ( ! MLGetReal( mlp, &_tp5) ) goto L4;
	if ( ! MLNewPacket(mlp) ) goto L5;

	_rp0 = cparam4(_tp1, _tpl1, _tp2, _tp3, _tp4, _tp5);

	res = MLAbort ?
		MLPutFunction( mlp, "Abort", 0) : MLPutReal( mlp, _rp0);
L5: L4: L3: L2: L1:	MLReleaseReal64List(mlp, _tp1, _tpl1);

L0:	return res;
} /* _tr20 */


double cparam6 P(( double * _tp1, long _tpl1, double _tp2, double _tp3, double _tp4, double _tp5));

#if MLPROTOTYPES
static int _tr21( MLINK mlp)
#else
static int _tr21(mlp) MLINK mlp;
#endif
{
	int	res = 0;
	double * _tp1;
	long _tpl1;
	double _tp2;
	double _tp3;
	double _tp4;
	double _tp5;
	double _rp0;
	if ( ! MLGetRealList( mlp, &_tp1, &_tpl1) ) goto L0;
	if ( ! MLGetReal( mlp, &_tp2) ) goto L1;
	if ( ! MLGetReal( mlp, &_tp3) ) goto L2;
	if ( ! MLGetReal( mlp, &_tp4) ) goto L3;
	if ( ! MLGetReal( mlp, &_tp5) ) goto L4;
	if ( ! MLNewPacket(mlp) ) goto L5;

	_rp0 = cparam6(_tp1, _tpl1, _tp2, _tp3, _tp4, _tp5);

	res = MLAbort ?
		MLPutFunction( mlp, "Abort", 0) : MLPutReal( mlp, _rp0);
L5: L4: L3: L2: L1:	MLReleaseReal64List(mlp, _tp1, _tpl1);

L0:	return res;
} /* _tr21 */


double cparambeta4 P(( double * _tp1, long _tpl1, double _tp2, double _tp3, double _tp4, double _tp5));

#if MLPROTOTYPES
static int _tr22( MLINK mlp)
#else
static int _tr22(mlp) MLINK mlp;
#endif
{
	int	res = 0;
	double * _tp1;
	long _tpl1;
	double _tp2;
	double _tp3;
	double _tp4;
	double _tp5;
	double _rp0;
	if ( ! MLGetRealList( mlp, &_tp1, &_tpl1) ) goto L0;
	if ( ! MLGetReal( mlp, &_tp2) ) goto L1;
	if ( ! MLGetReal( mlp, &_tp3) ) goto L2;
	if ( ! MLGetReal( mlp, &_tp4) ) goto L3;
	if ( ! MLGetReal( mlp, &_tp5) ) goto L4;
	if ( ! MLNewPacket(mlp) ) goto L5;

	_rp0 = cparambeta4(_tp1, _tpl1, _tp2, _tp3, _tp4, _tp5);

	res = MLAbort ?
		MLPutFunction( mlp, "Abort", 0) : MLPutReal( mlp, _rp0);
L5: L4: L3: L2: L1:	MLReleaseReal64List(mlp, _tp1, _tpl1);

L0:	return res;
} /* _tr22 */


double cparambeta6 P(( double * _tp1, long _tpl1, double _tp2, double _tp3, double _tp4, double _tp5));

#if MLPROTOTYPES
static int _tr23( MLINK mlp)
#else
static int _tr23(mlp) MLINK mlp;
#endif
{
	int	res = 0;
	double * _tp1;
	long _tpl1;
	double _tp2;
	double _tp3;
	double _tp4;
	double _tp5;
	double _rp0;
	if ( ! MLGetRealList( mlp, &_tp1, &_tpl1) ) goto L0;
	if ( ! MLGetReal( mlp, &_tp2) ) goto L1;
	if ( ! MLGetReal( mlp, &_tp3) ) goto L2;
	if ( ! MLGetReal( mlp, &_tp4) ) goto L3;
	if ( ! MLGetReal( mlp, &_tp5) ) goto L4;
	if ( ! MLNewPacket(mlp) ) goto L5;

	_rp0 = cparambeta6(_tp1, _tpl1, _tp2, _tp3, _tp4, _tp5);

	res = MLAbort ?
		MLPutFunction( mlp, "Abort", 0) : MLPutReal( mlp, _rp0);
L5: L4: L3: L2: L1:	MLReleaseReal64List(mlp, _tp1, _tpl1);

L0:	return res;
} /* _tr23 */


void vectors6 P(( double * _tp1, long _tpl1, double _tp2, double _tp3, double _tp4, double _tp5));

#if MLPROTOTYPES
static int _tr24( MLINK mlp)
#else
static int _tr24(mlp) MLINK mlp;
#endif
{
	int	res = 0;
	double * _tp1;
	long _tpl1;
	double _tp2;
	double _tp3;
	double _tp4;
	double _tp5;
	if ( ! MLGetRealList( mlp, &_tp1, &_tpl1) ) goto L0;
	if ( ! MLGetReal( mlp, &_tp2) ) goto L1;
	if ( ! MLGetReal( mlp, &_tp3) ) goto L2;
	if ( ! MLGetReal( mlp, &_tp4) ) goto L3;
	if ( ! MLGetReal( mlp, &_tp5) ) goto L4;
	if ( ! MLNewPacket(mlp) ) goto L5;

	vectors6(_tp1, _tpl1, _tp2, _tp3, _tp4, _tp5);

	res = 1;
L5: L4: L3: L2: L1:	MLReleaseReal64List(mlp, _tp1, _tpl1);

L0:	return res;
} /* _tr24 */


void restvectors4 P(( double * _tp1, long _tpl1, double _tp2, double _tp3, double _tp4, double _tp5));

#if MLPROTOTYPES
static int _tr25( MLINK mlp)
#else
static int _tr25(mlp) MLINK mlp;
#endif
{
	int	res = 0;
	double * _tp1;
	long _tpl1;
	double _tp2;
	double _tp3;
	double _tp4;
	double _tp5;
	if ( ! MLGetRealList( mlp, &_tp1, &_tpl1) ) goto L0;
	if ( ! MLGetReal( mlp, &_tp2) ) goto L1;
	if ( ! MLGetReal( mlp, &_tp3) ) goto L2;
	if ( ! MLGetReal( mlp, &_tp4) ) goto L3;
	if ( ! MLGetReal( mlp, &_tp5) ) goto L4;
	if ( ! MLNewPacket(mlp) ) goto L5;

	restvectors4(_tp1, _tpl1, _tp2, _tp3, _tp4, _tp5);

	res = 1;
L5: L4: L3: L2: L1:	MLReleaseReal64List(mlp, _tp1, _tpl1);

L0:	return res;
} /* _tr25 */


void restvectors6 P(( double * _tp1, long _tpl1, double _tp2, double _tp3, double _tp4, double _tp5));

#if MLPROTOTYPES
static int _tr26( MLINK mlp)
#else
static int _tr26(mlp) MLINK mlp;
#endif
{
	int	res = 0;
	double * _tp1;
	long _tpl1;
	double _tp2;
	double _tp3;
	double _tp4;
	double _tp5;
	if ( ! MLGetRealList( mlp, &_tp1, &_tpl1) ) goto L0;
	if ( ! MLGetReal( mlp, &_tp2) ) goto L1;
	if ( ! MLGetReal( mlp, &_tp3) ) goto L2;
	if ( ! MLGetReal( mlp, &_tp4) ) goto L3;
	if ( ! MLGetReal( mlp, &_tp5) ) goto L4;
	if ( ! MLNewPacket(mlp) ) goto L5;

	restvectors6(_tp1, _tpl1, _tp2, _tp3, _tp4, _tp5);

	res = 1;
L5: L4: L3: L2: L1:	MLReleaseReal64List(mlp, _tp1, _tpl1);

L0:	return res;
} /* _tr26 */


static struct func {
	int   f_nargs;
	int   manual;
	int   (*f_func)P((MLINK));
	const char  *f_name;
	} _tramps[27] = {
		{ 2, 0, _tr0, "esmin" },
		{ 2, 0, _tr1, "esmax" },
		{ 1, 0, _tr2, "cparamcomputer" },
		{ 7, 0, _tr3, "cparamlist" },
		{ 2, 0, _tr4, "legendrelist" },
		{14, 0, _tr5, "cparamdistribution" },
		{15, 0, _tr6, "cparamlegendredistro" },
		{14, 0, _tr7, "cparamlegendre" },
		{13, 0, _tr8, "esdistributions" },
		{13, 0, _tr9, "eslegendre" },
		{ 7, 0, _tr10, "eslist" },
		{ 3, 0, _tr11, "esliststable" },
		{ 1, 0, _tr12, "escomputer" },
		{ 5, 0, _tr13, "esminmax4" },
		{ 5, 0, _tr14, "cparammaxmin4" },
		{ 5, 0, _tr15, "cparammaxmin6" },
		{ 5, 0, _tr16, "cparamminmax4" },
		{ 5, 0, _tr17, "cparamminmax6" },
		{ 5, 0, _tr18, "esminmax6" },
		{ 5, 0, _tr19, "vectors4" },
		{ 5, 0, _tr20, "cparam4" },
		{ 5, 0, _tr21, "cparam6" },
		{ 5, 0, _tr22, "cparambeta4" },
		{ 5, 0, _tr23, "cparambeta6" },
		{ 5, 0, _tr24, "vectors6" },
		{ 5, 0, _tr25, "restvectors4" },
		{ 5, 0, _tr26, "restvectors6" }
		};

static const char* evalstrs[] = {
	"BeginPackage[\"MCtop`\"]",
	(const char*)0,
	"Print[\"     Package for tree-level production and decay of tops ",
	"MC \"]",
	(const char*)0,
	"Print[\"     Author:            Vicent Mateu          \"]",
	(const char*)0,
	"Print[\"     Last modification: 08 - 01 - 2017        \"]",
	(const char*)0,
	"Print[\"     Version:           test 2                \"]",
	(const char*)0,
	"CparamLegendreDistro::usage = \"CparamLegendreDistro[mt, mb, mW, ",
	"Q, expand, method, Spin, decay, current, Cmin, Cmax, n, Nbins, N",
	"event, Niter] computes the Legendre expansion and distribution f",
	"or the C-parameter event shape\"",
	(const char*)0,
	"CparamLegendre::usage = \"CparamLegendre[n, mt, mb, mW, Q, expand",
	", method, Spin, decay, current, Cmin, Cmax, Nevent, Niter] compu",
	"tes the integration against Legendre Polynomial\"",
	(const char*)0,
	"LegendreList::usage = \"LegendreList[n, x] computes the of the fi",
	"rst n + 1 Legendre Polynomial\"",
	(const char*)0,
	"Cparam4::usage = \"Cparam4[x, mt, mb, mW, Q] computes the value o",
	"f C-parameter for top decay into 2 particles\"",
	(const char*)0,
	"Cparam6::usage = \"Cparam6[x, mt, mb, mW, Q] computes the value o",
	"f C-parameter for top decay into 3 particles\"",
	(const char*)0,
	"CparamBeta4::usage = \"CparamBeta4[x, mt, mb, mW, Q] computes the",
	" expanded value of C-parameter for top decay into 2 particles\"",
	(const char*)0,
	"CparamBeta6::usage = \"CparamBeta6[x, mt, mb, mW, Q] computes the",
	" expanded value of C-parameter for top decay into 3 particles\"",
	(const char*)0,
	"ESDistributions::usage = \"ESDistributions[mt, mb, mW, Q, method,",
	" Spin, decay, current, ESmin, ESmax, Nbins, Nevent, Niter] compu",
	"tes the distribution of the event-shape variables\"",
	(const char*)0,
	"ESLegendre::usage = \"ESLegendre[mt, mb, mW, Q, method, Spin, dec",
	"ay, current, ESmin, ESmax, Nbins, Nevent, Niter] computes the Le",
	"gendre coefficients of the event-shape variables\"",
	(const char*)0,
	"CparamDistribution::usage = \"CparamDistribution[mt, mb, mW, Q, e",
	"xpand, method, Spin, decay, current, Cmin, Cmax, Nbins, Nevent, ",
	"Niter] computes the distribution of the C-parameter event shape\"",
	(const char*)0,
	"CparamComputer::usage = \"CparamComputer[p] computes the value of",
	" the C-parameter event shape\"",
	(const char*)0,
	"CparamList::usage = \"CparamList[mt, mb, mW, Q, Cmin, Cmax, Nbins",
	"] computes the values of the C-parameter event shape\"",
	(const char*)0,
	"ESList::usage = \"ESList[mt, mb, mW, Q, ESmin, ESmax, Nbins] comp",
	"utes the values of the event-shape variables\"",
	(const char*)0,
	"ESListStable::usage = \"ESListStable[mt, Q, Nbins] computes the v",
	"alues of the event-shape variables for a stable top\"",
	(const char*)0,
	"EScomputer::usage = \"EScomputer[p] computes the value of the var",
	"ious event shapes\"",
	(const char*)0,
	"Vectors4::usage = \"Vectors4[x, mt, mb, mW, Q] computes the value",
	" 4 four-vectors for top decay\"",
	(const char*)0,
	"Vectors6::usage = \"Vectors6[x, mt, mb, mW, Q] computes the value",
	" 6 four-vectors for top decay\"",
	(const char*)0,
	"RestVectors4::usage = \"Vectors4[x, mt, mb, mW, Q] computes the v",
	"alue 4 four-vectors for top decay, in the top rest frame\"",
	(const char*)0,
	"RestVectors6::usage = \"Vectors6[x, mt, mb, mW, Q] computes the v",
	"alue 6 four-vectors for top decay, in the top rest frame\"",
	(const char*)0,
	"ESMinMax4::usage = \"ESMinMax4[n, mt, mb, mW, Q] computes the max",
	"imum and minimum value for Event shapes with 2-particle final st",
	"ate\"",
	(const char*)0,
	"ESMinMax6::usage = \"ESMinMax6[n, mt, mb, mW, Q] computes the max",
	"imum and minimum value for Event shapes with 3-particle final st",
	"ate\"",
	(const char*)0,
	"CparamMinMax4::usage = \"CparamMinMax4[n, mt, mb, mW, Q] computes",
	" the maximum and minimum value for the C-parameter Event shape w",
	"ith 2-particle final state\"",
	(const char*)0,
	"CparamMinMax6::usage = \"CparamMinMax6[n, mt, mb, mW, Q] computes",
	" the maximum and minimum value for the C-parameter Event shape w",
	"ith 3-particle final state\"",
	(const char*)0,
	"CparamMaxMin4::usage = \"CparamMaxMin4[eps, mt, mb, mW, Q] comput",
	"es the maximum and minimim values and locations for the C-parame",
	"ter Event shape with 2-particle final state\"",
	(const char*)0,
	"CparamMaxMin6::usage = \"CparamMaxMin6[eps, mt, mb, mW, Q] comput",
	"es the maximum and minimim values and locations for the C-parame",
	"ter Event shape with 3-particle final state\"",
	(const char*)0,
	"ESMax::usage = \"ESMax[m,Q] computes the maximal value of the eve",
	"nt shape\"",
	(const char*)0,
	"ESMin::usage = \"ESMin[m,Q] computes the minimal value of the eve",
	"nt shape\"",
	(const char*)0,
	"Begin[\"`Private`\"]",
	(const char*)0,
	"Print[\"You can access the complete function list typing '?MCtop`",
	"*' \"]",
	(const char*)0,
	"realQ = Head[# + 1.] === Real &",
	(const char*)0,
	"End[]",
	(const char*)0,
	"EndPackage[]",
	(const char*)0,
	(const char*)0
};
#define CARDOF_EVALSTRS 37

static int _definepattern P(( MLINK, char*, char*, int));

static int _doevalstr P(( MLINK, int));

int  _MLDoCallPacket P(( MLINK, struct func[], int));


#if MLPROTOTYPES
int MLInstall( MLINK mlp)
#else
int MLInstall(mlp) MLINK mlp;
#endif
{
	int _res;
	_res = MLConnect(mlp);
	if (_res) _res = _doevalstr( mlp, 0);
	if (_res) _res = _doevalstr( mlp, 1);
	if (_res) _res = _doevalstr( mlp, 2);
	if (_res) _res = _doevalstr( mlp, 3);
	if (_res) _res = _doevalstr( mlp, 4);
	if (_res) _res = _doevalstr( mlp, 5);
	if (_res) _res = _doevalstr( mlp, 6);
	if (_res) _res = _doevalstr( mlp, 7);
	if (_res) _res = _doevalstr( mlp, 8);
	if (_res) _res = _doevalstr( mlp, 9);
	if (_res) _res = _doevalstr( mlp, 10);
	if (_res) _res = _doevalstr( mlp, 11);
	if (_res) _res = _doevalstr( mlp, 12);
	if (_res) _res = _doevalstr( mlp, 13);
	if (_res) _res = _doevalstr( mlp, 14);
	if (_res) _res = _doevalstr( mlp, 15);
	if (_res) _res = _doevalstr( mlp, 16);
	if (_res) _res = _doevalstr( mlp, 17);
	if (_res) _res = _doevalstr( mlp, 18);
	if (_res) _res = _doevalstr( mlp, 19);
	if (_res) _res = _doevalstr( mlp, 20);
	if (_res) _res = _doevalstr( mlp, 21);
	if (_res) _res = _doevalstr( mlp, 22);
	if (_res) _res = _doevalstr( mlp, 23);
	if (_res) _res = _doevalstr( mlp, 24);
	if (_res) _res = _doevalstr( mlp, 25);
	if (_res) _res = _doevalstr( mlp, 26);
	if (_res) _res = _doevalstr( mlp, 27);
	if (_res) _res = _doevalstr( mlp, 28);
	if (_res) _res = _doevalstr( mlp, 29);
	if (_res) _res = _doevalstr( mlp, 30);
	if (_res) _res = _doevalstr( mlp, 31);
	if (_res) _res = _doevalstr( mlp, 32);
	if (_res) _res = _doevalstr( mlp, 33);
	if (_res) _res = _definepattern(mlp, (char *)"ESMin[m_, Q_]", (char *)"{m, Q}", 0);
	if (_res) _res = _definepattern(mlp, (char *)"ESMax[m_, Q_]", (char *)"{m, Q}", 1);
	if (_res) _res = _definepattern(mlp, (char *)"CparamComputer[p_]", (char *)"{Flatten[Transpose[p]]}", 2);
	if (_res) _res = _definepattern(mlp, (char *)"CparamList[mt_, mb_, mW_, Q_, Cmin_, Cmax_, Nbins_]", (char *)"{mt, mb, mW, Q, Cmin, Cmax, Nbins}", 3);
	if (_res) _res = _definepattern(mlp, (char *)"LegendreList[n_, x_]", (char *)"{n, x}", 4);
	if (_res) _res = _definepattern(mlp, (char *)"CparamDistribution[mt_, mb_, mW_, Q_, expand_, method_, spin_, decay_,                  current_, Cmin_, Cmax_, Nbins_, Nevent_, Niter_]", (char *)"{mt, mb, mW, Q, expand, method, spin, decay, current, Cmin, Cmax, Nbins,                  Nevent, Niter}", 5);
	if (_res) _res = _definepattern(mlp, (char *)"CparamLegendreDistro[mt_, mb_, mW_, Q_, expand_, method_, spin_,                  decay_, current_, Cmin_, Cmax_, n_, Nbins_, Nevent_, Niter_]", (char *)"{mt, mb, mW, Q, expand, method, spin, decay, current, Cmin, Cmax,                  n, Nbins, Nevent, Niter}", 6);
	if (_res) _res = _definepattern(mlp, (char *)"CparamLegendre[n_, mt_, mb_, mW_, Q_, expand_, method_, spin_, decay_,                  current_, Cmin_, Cmax_, Nevent_, Niter_]", (char *)"{n, mt, mb, mW, Q, expand, method, spin, decay, current, Cmin, Cmax,                  Nevent, Niter}", 7);
	if (_res) _res = _definepattern(mlp, (char *)"ESDistributions[mt_, mb_, mW_, Q_, method_, spin_, decay_, current_,                  Cmin_, Cmax_, Nbins_, Nevent_, Niter_]", (char *)"{mt, mb, mW, Q, method, spin, decay, current, Cmin, Cmax, Nbins, Nevent, Niter}", 8);
	if (_res) _res = _definepattern(mlp, (char *)"ESLegendre[mt_, mb_, mW_, Q_, method_, spin_, decay_, current_,                  Cmin_, Cmax_, n_, Nevent_, Niter_]", (char *)"{mt, mb, mW, Q, method, spin, decay, current, Cmin, Cmax, n, Nevent, Niter}", 9);
	if (_res) _res = _definepattern(mlp, (char *)"ESList[mt_, mb_, mW_, Q_, ESmin_, ESmax_, Nbins_]", (char *)"{mt, mb, mW, Q, ESmin, ESmax, Nbins}", 10);
	if (_res) _res = _definepattern(mlp, (char *)"ESListStable[mt_, Q_, Nbins_]", (char *)"{mt, Q, Nbins}", 11);
	if (_res) _res = _definepattern(mlp, (char *)"EScomputer[p_]", (char *)"{Flatten[Transpose[p]]}", 12);
	if (_res) _res = _definepattern(mlp, (char *)"ESMinMax4[n_, mt_, mb_, mW_, Q_]", (char *)"{n, mt, mb, mW, Q}", 13);
	if (_res) _res = _definepattern(mlp, (char *)"CparamMaxMin4[eps_, mt_, mb_, mW_, Q_]", (char *)"{eps, mt, mb, mW, Q}", 14);
	if (_res) _res = _definepattern(mlp, (char *)"CparamMaxMin6[eps_, mt_, mb_, mW_, Q_]", (char *)"{eps, mt, mb, mW, Q}", 15);
	if (_res) _res = _definepattern(mlp, (char *)"CparamMinMax4[n_, mt_, mb_, mW_, Q_]", (char *)"{n, mt, mb, mW, Q}", 16);
	if (_res) _res = _definepattern(mlp, (char *)"CparamMinMax6[n_, mt_, mb_, mW_, Q_]", (char *)"{n, mt, mb, mW, Q}", 17);
	if (_res) _res = _definepattern(mlp, (char *)"ESMinMax6[n_, mt_, mb_, mW_, Q_]", (char *)"{n, mt, mb, mW, Q}", 18);
	if (_res) _res = _definepattern(mlp, (char *)"Vectors4[x_, mt_, mb_, mW_, Q_]", (char *)"{x, mt, mb, mW, Q}", 19);
	if (_res) _res = _definepattern(mlp, (char *)"Cparam4[x_, mt_, mb_, mW_, Q_]", (char *)"{x, mt, mb, mW, Q}", 20);
	if (_res) _res = _definepattern(mlp, (char *)"Cparam6[x_, mt_, mb_, mW_, Q_]", (char *)"{x, mt, mb, mW, Q}", 21);
	if (_res) _res = _definepattern(mlp, (char *)"CparamBeta4[x_, mt_, mb_, mW_, Q_]", (char *)"{x, mt, mb, mW, Q}", 22);
	if (_res) _res = _definepattern(mlp, (char *)"CparamBeta6[x_, mt_, mb_, mW_, Q_]", (char *)"{x, mt, mb, mW, Q}", 23);
	if (_res) _res = _definepattern(mlp, (char *)"Vectors6[x_, mt_, mb_, mW_, Q_]", (char *)"{x, mt, mb, mW, Q}", 24);
	if (_res) _res = _definepattern(mlp, (char *)"RestVectors4[x_, mt_, mb_, mW_, Q_]", (char *)"{x, mt, mb, mW, Q}", 25);
	if (_res) _res = _definepattern(mlp, (char *)"RestVectors6[x_, mt_, mb_, mW_, Q_]", (char *)"{x, mt, mb, mW, Q}", 26);
	if (_res) _res = _doevalstr( mlp, 34);
	if (_res) _res = _doevalstr( mlp, 35);
	if (_res) _res = _doevalstr( mlp, 36);
	if (_res) _res = MLPutSymbol( mlp, "End");
	if (_res) _res = MLFlush( mlp);
	return _res;
} /* MLInstall */


#if MLPROTOTYPES
int MLDoCallPacket( MLINK mlp)
#else
int MLDoCallPacket( mlp) MLINK mlp;
#endif
{
	return _MLDoCallPacket( mlp, _tramps, 27);
} /* MLDoCallPacket */

/******************************* begin trailer ********************************/

#ifndef EVALSTRS_AS_BYTESTRINGS
#	define EVALSTRS_AS_BYTESTRINGS 1
#endif

#if CARDOF_EVALSTRS
static int  _doevalstr( MLINK mlp, int n)
{
	long bytesleft, charsleft, bytesnow;
#if !EVALSTRS_AS_BYTESTRINGS
	long charsnow;
#endif
	char **s, **p;
	char *t;

	s = (char **)evalstrs;
	while( n-- > 0){
		if( *s == 0) break;
		while( *s++ != 0){}
	}
	if( *s == 0) return 0;
	bytesleft = 0;
	charsleft = 0;
	p = s;
	while( *p){
		t = *p; while( *t) ++t;
		bytesnow = t - *p;
		bytesleft += bytesnow;
		charsleft += bytesnow;
#if !EVALSTRS_AS_BYTESTRINGS
		t = *p;
		charsleft -= MLCharacterOffset( &t, t + bytesnow, bytesnow);
		/* assert( t == *p + bytesnow); */
#endif
		++p;
	}


	MLPutNext( mlp, MLTKSTR);
#if EVALSTRS_AS_BYTESTRINGS
	p = s;
	while( *p){
		t = *p; while( *t) ++t;
		bytesnow = t - *p;
		bytesleft -= bytesnow;
		MLPut8BitCharacters( mlp, bytesleft, (unsigned char*)*p, bytesnow);
		++p;
	}
#else
	MLPut7BitCount( mlp, charsleft, bytesleft);
	p = s;
	while( *p){
		t = *p; while( *t) ++t;
		bytesnow = t - *p;
		bytesleft -= bytesnow;
		t = *p;
		charsnow = bytesnow - MLCharacterOffset( &t, t + bytesnow, bytesnow);
		/* assert( t == *p + bytesnow); */
		charsleft -= charsnow;
		MLPut7BitCharacters(  mlp, charsleft, *p, bytesnow, charsnow);
		++p;
	}
#endif
	return MLError( mlp) == MLEOK;
}
#endif /* CARDOF_EVALSTRS */


static int  _definepattern( MLINK mlp, char* patt, char* args, int func_n)
{
	MLPutFunction( mlp, "DefineExternal", (long)3);
	  MLPutString( mlp, patt);
	  MLPutString( mlp, args);
	  MLPutInteger( mlp, func_n);
	return !MLError(mlp);
} /* _definepattern */


int _MLDoCallPacket( MLINK mlp, struct func functable[], int nfuncs)
{
#if MLINTERFACE >= 4
	int len;
#else
	long len;
#endif
	int n, res = 0;
	struct func* funcp;

	if( ! MLGetInteger( mlp, &n) ||  n < 0 ||  n >= nfuncs) goto L0;
	funcp = &functable[n];

	if( funcp->f_nargs >= 0
#if MLINTERFACE >= 4
	&& ( ! MLTestHead(mlp, "List", &len)
#else
	&& ( ! MLCheckFunction(mlp, "List", &len)
#endif
	     || ( !funcp->manual && (len != funcp->f_nargs))
	     || (  funcp->manual && (len <  funcp->f_nargs))
	   )
	) goto L0;

	stdlink = mlp;
	res = (*funcp->f_func)( mlp);

L0:	if( res == 0)
		res = MLClearError( mlp) && MLPutSymbol( mlp, "$Failed");
	return res && MLEndPacket( mlp) && MLNewPacket( mlp);
} /* _MLDoCallPacket */


mlapi_packet MLAnswer( MLINK mlp)
{
	mlapi_packet pkt = 0;
#if MLINTERFACE >= 4
	int waitResult;

	while( ! MLDone && ! MLError(mlp)
		&& (waitResult = MLWaitForLinkActivity(mlp),waitResult) &&
		waitResult == MLWAITSUCCESS && (pkt = MLNextPacket(mlp), pkt) &&
		pkt == CALLPKT)
	{
		MLAbort = 0;
		if(! MLDoCallPacket(mlp))
			pkt = 0;
	}
#else
	while( !MLDone && !MLError(mlp) && (pkt = MLNextPacket(mlp), pkt) && pkt == CALLPKT){
		MLAbort = 0;
		if( !MLDoCallPacket(mlp)) pkt = 0;
	}
#endif
	MLAbort = 0;
	return pkt;
} /* MLAnswer */



/*
	Module[ { me = $ParentLink},
		$ParentLink = contents of RESUMEPKT;
		Message[ MessageName[$ParentLink, "notfe"], me];
		me]
*/

static int refuse_to_be_a_frontend( MLINK mlp)
{
	int pkt;

	MLPutFunction( mlp, "EvaluatePacket", 1);
	  MLPutFunction( mlp, "Module", 2);
	    MLPutFunction( mlp, "List", 1);
		  MLPutFunction( mlp, "Set", 2);
		    MLPutSymbol( mlp, "me");
	        MLPutSymbol( mlp, "$ParentLink");
	  MLPutFunction( mlp, "CompoundExpression", 3);
	    MLPutFunction( mlp, "Set", 2);
	      MLPutSymbol( mlp, "$ParentLink");
	      MLTransferExpression( mlp, mlp);
	    MLPutFunction( mlp, "Message", 2);
	      MLPutFunction( mlp, "MessageName", 2);
	        MLPutSymbol( mlp, "$ParentLink");
	        MLPutString( mlp, "notfe");
	      MLPutSymbol( mlp, "me");
	    MLPutSymbol( mlp, "me");
	MLEndPacket( mlp);

	while( (pkt = MLNextPacket( mlp), pkt) && pkt != SUSPENDPKT)
		MLNewPacket( mlp);
	MLNewPacket( mlp);
	return MLError( mlp) == MLEOK;
}


int MLEvaluate( MLINK mlp, char* s)
{
	if( MLAbort) return 0;
	return MLPutFunction( mlp, "EvaluatePacket", 1L)
		&& MLPutFunction( mlp, "ToExpression", 1L)
		&& MLPutString( mlp, s)
		&& MLEndPacket( mlp);
} /* MLEvaluate */


int MLEvaluateString( MLINK mlp, char* s)
{
	int pkt;
	if( MLAbort) return 0;
	if( MLEvaluate( mlp, s)){
		while( (pkt = MLAnswer( mlp), pkt) && pkt != RETURNPKT)
			MLNewPacket( mlp);
		MLNewPacket( mlp);
	}
	return MLError( mlp) == MLEOK;
} /* MLEvaluateString */


#if MLINTERFACE >= 3
MLMDEFN( void, MLDefaultHandler, ( MLINK mlp, int message, int n))
#else
MLMDEFN( void, MLDefaultHandler, ( MLINK mlp, unsigned long message, unsigned long n))
#endif /* MLINTERFACE >= 3 */
{
	mlp = (MLINK)0; /* suppress unused warning */
	n = 0; /* suppress unused warning */

	switch (message){
	case MLTerminateMessage:
		MLDone = 1;
	case MLInterruptMessage:
	case MLAbortMessage:
		MLAbort = 1;
	default:
		return;
	}
}

#if MLINTERFACE >= 3
static int _MLMain( char **argv, char **argv_end, char *commandline)
#else
static int _MLMain( charpp_ct argv, charpp_ct argv_end, charp_ct commandline)
#endif /* MLINTERFACE >= 3 */
{
	MLINK mlp;
#if MLINTERFACE >= 3
	int err;
#else
	long err;
#endif /* MLINTERFACE >= 3 */

#if (DARWIN_MATHLINK && CARBON_MPREP)
	if( !init_macintosh()) goto R0;
#endif /* (DARWIN_MATHLINK && CARBON_MPREP) */

#if MLINTERFACE >= 4
	if( !stdenv)
		stdenv = MLInitialize( (MLEnvironmentParameter)0);
#else
	if( !stdenv)
		stdenv = MLInitialize( (MLParametersPointer)0);
#endif

	if( stdenv == (MLEnvironment)0) goto R0;

#if (DARWIN_MATHLINK && CARBON_MPREP)
#if MLINTERFACE >= 3
	if( !stdyielder)
		stdyielder = (MLYieldFunctionObject)MLDefaultYielder;
#else
	if( !stdyielder)
		stdyielder = MLCreateYieldFunction( stdenv, NewMLYielderProc( MLDefaultYielder), 0);
#endif /* MLINTERFACE >= 3 */
#endif /* (DARWIN_MATHLINK && CARBON_MPREP)*/

#if MLINTERFACE >= 3
	if( !stdhandler)
		stdhandler = (MLMessageHandlerObject)MLDefaultHandler;
#else
	if( !stdhandler)
		stdhandler = MLCreateMessageHandler( stdenv, NewMLHandlerProc( MLDefaultHandler), 0);
#endif /* MLINTERFACE >= 3 */

#if (DARWIN_MATHLINK && CARBON_MPREP)
        MLSetDialogFunction(stdenv, MLRequestToInteractFunction, NewMLRequestToInteractProc(MLDontPermit_darwin));

	mlp = commandline
		? MLOpenString( stdenv, commandline, &err)
#if MLINTERFACE >= 3
			: MLOpenArgcArgv( stdenv, (int)(argv_end - argv), argv, &err);
#else
			: MLOpenArgv( stdenv, argv, argv_end, &err);
#endif

        MLSetDialogFunction(stdenv, MLRequestToInteractFunction, NewMLRequestToInteractProc(MLPermit_darwin));

	if( mlp == (MLINK)0){
                        mlp = commandline
                                ? MLOpenString( stdenv, commandline, &err)
#if MLINTERFACE < 3
                                : MLOpenArgv( stdenv, argv, argv_end, &err);
#else
                                : MLOpenArgcArgv( stdenv, (int)(argv_end - argv), argv, &err);
#endif
        }
#else /* !(DARWIN_MATHLINK && CARBON_MPREP)*/
	mlp = commandline
		? MLOpenString( stdenv, commandline, &err)
#if MLINTERFACE < 3
		: MLOpenArgv( stdenv, argv, argv_end, &err);
#else
		: MLOpenArgcArgv( stdenv, (int)(argv_end - argv), argv, &err);
#endif
#endif /* (DARWIN_MATHLINK && CARBON_MPREP)*/

	if( mlp == (MLINK)0){
		MLAlert( stdenv, MLErrorString( stdenv, err));
		goto R1;
	}

	if( stdyielder) MLSetYieldFunction( mlp, stdyielder);
	if( stdhandler) MLSetMessageHandler( mlp, stdhandler);

	if( MLInstall( mlp))
		while( MLAnswer( mlp) == RESUMEPKT){
			if( ! refuse_to_be_a_frontend( mlp)) break;
		}

	MLClose( mlp);
R1:	MLDeinitialize( stdenv);
	stdenv = (MLEnvironment)0;
R0:	return !MLDone;
} /* _MLMain */


#if MLINTERFACE >= 3
int MLMainString( char *commandline)
#else
int MLMainString( charp_ct commandline)
#endif /* MLINTERFACE >= 3 */
{
#if MLINTERFACE >= 3
	return _MLMain( (char **)0, (char **)0, commandline);
#else
	return _MLMain( (charpp_ct)0, (charpp_ct)0, commandline);
#endif /* MLINTERFACE >= 3 */
}

int MLMainArgv( char** argv, char** argv_end) /* note not FAR pointers */
{   
	static char FAR * far_argv[128];
	int count = 0;
	
	while(argv < argv_end)
		far_argv[count++] = *argv++;
		 
	return _MLMain( far_argv, far_argv + count, (charp_ct)0);

}

#if MLINTERFACE >= 3
int MLMain( int argc, char ** argv)
#else
int MLMain( int argc, charpp_ct argv)
#endif /* MLINTERFACE >= 3 */
{
#if MLINTERFACE >= 3
 	return _MLMain( argv, argv + argc, (char *)0);
#else
 	return _MLMain( argv, argv + argc, (charp_ct)0);
#endif /* MLINTERFACE >= 3 */
}
