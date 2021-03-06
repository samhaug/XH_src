/* ---   PKIKP    ---*/

#include <string.h>
#include <math.h>
#include <stdio.h>

#define npts_dat 8192  /* Maximum number of samples in time series */
#define npts_crc 4000  /* Maximum number of samples in traces used in cross_correlation  */
                                                                                      
extern char stnam[6], chnam[6];
extern char datcode[8];

extern int i30s, i50s;
extern int jrvw, jlog;
extern int PKIKPsample;
extern int Wlength, Mshift, decim;

extern float SignalToNoise, envAmpRatio_Min;
extern float data[npts_dat], synt[npts_dat], env_data[npts_dat];
extern float Z_noise, Z_noise_window, Z_noise_winMin;

extern float stlat,stlon,stelv,  evlat,evlon,evdep, gcarc;
extern float PKIKPtime, sPtime, pPtime;
extern float PhaseSepTime;


void measure_PKIKP(pck,log,rvw)
FILE    *pck, *log, *rvw;
{

int    iwindow1, iwindow2;
int    usefulrec = 0;
float  envAmpRatio;
float  Amp;


if (jlog) fprintf(log,"\n                                          PHASE= PKIKP\n");

/* VERIFY INTERFERENCE WITH OTHER PHASES -----------------*/
if ( fabs(PKIKPtime-pPtime) < PhaseSepTime) {
   if (jlog) {
     fprintf(log,"%s [%s] discarded because PKIKPtime-pPtime= %5.1f\n",
                stnam,chnam,PKIKPtime-pPtime);
   }
   return;
}
if ( fabs(PKIKPtime-sPtime) < PhaseSepTime) {
   if (jlog) {
     fprintf(log,"%s [%s] discarded because PKIKPtime-sPtime= %5.1f\n",
                stnam,chnam,PKIKPtime-sPtime);
   }
   return;
}



/* Check ENVELOPE noise level
   -- Defined as the minimum amplitude of ENV_DATA.   ----*/
iwindow1 = PKIKPsample-i50s;
iwindow2 = PKIKPsample+i50s;
signallevel(iwindow1,iwindow2,env_data,2,&envAmpRatio,log);

/*--  Check whether PKIKP signal is well above noise level --*/
if ( envAmpRatio < envAmpRatio_Min ) {
   if (jlog) {
     fprintf(log,"%s [%s] PKIKP_env amplitude is too small: %7.1f ...\n",
                stnam,chnam,envAmpRatio);
   }
   return;
}

/* Determine peak-to-trough PKIKP amplitude: */
iwindow1 = PKIKPsample - i30s; iwindow2 = PKIKPsample + i30s;
signallevel(iwindow1,iwindow2,data,1,&Amp,log);
if ( (Amp/Z_noise) < SignalToNoise) {
   if (jlog) {
     fprintf(log,"%s  PKIKP_amp/Z_noise smaller than allowed ...\n", stnam);
   }
   return;
}
/*  OUTPUT ....  */
if (jrvw) fprintf(rvw,"PKIKP   %s\n", datcode);
if (jrvw) write_review1(rvw,PKIKPsample,Amp,Z_noise,Z_noise_window,envAmpRatio);

  fprintf(pck,"%7s",datcode);
  fprintf(pck," %6.2f %7.2f %5.1f %6.2f",evlat,evlon,evdep,gcarc);
  fprintf(pck," %4s %3s %6.2f %7.2f %6.1f",stnam,chnam,stlat,stlon,stelv);
  fprintf(pck," %5.1f %5.1f", Amp/Z_noise, envAmpRatio);


/* PROCESSING ---------------------------*/
usefulrec = PROCESSING(data,synt,PKIKPsample,Wlength,Mshift,decim,log,pck,rvw);
fprintf(pck,"%8s","PKPdf");
fprintf(pck,"%2d\n",usefulrec);


return;
}
