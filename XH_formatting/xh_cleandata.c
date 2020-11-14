#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <sys/param.h>
#include "xhhead.h"		// XH   structures

// Removes traces below a SNR criteria

int usage();

main(int argc, char *argv[]){
  xhhed 	h;
  int ierr;
  int still_reading_data = 1;
  float seism[XH_NPTS];
  FILE *ifl, *ofl;

  // kwarg check
  if(argc != 4){
    usage();
    exit(1);
  }
  if ((ifl = fopen(argv[1],"r")) == NULL) {
       ierr = usage();
       exit(1);
  }
  if ((ofl = fopen(argv[2],"w")) == NULL) {
       ierr = usage();
       exit(1);
  }

  float SNR = atof(argv[3]);
  fprintf(stdout,"SNR cutoff: %f\n", SNR);

  // Start the process
  while (still_reading_data) {
    if (! xh_readhead(ifl,&h)) {
       still_reading_data = 0;
       exit(0);
    } 
    else {
      if (! xh_readdata(ifl,h,seism)) {
        still_reading_data = 0;
        exit(0);
      }
    }
    // check header
    if (! xh_checkheader(h) )
      exit(-1);

    // find maximum amplitude up to 10 seconds before signal (190 seconds at 5Hz)
    float noisemax=0;
    for (int i=0;i<(190*5);i++){
        if (fabs(seism[i]) >= noisemax){
           noisemax = seism[i];
        }
    }
    // find maximum amplitude of ten second window around signal (190-210 seconds)
    float sigmax=0;
    for (int i=(190*5);i<(210*5);i++){
        if (fabs(seism[i]) >= sigmax){
           sigmax = seism[i];
        }
    }

    // Only if the max signal amplitude is greater than SNR*noise, write the trace
    if (sigmax >= noisemax*SNR){
       if (! xh_writehead(ofl,h)) exit(-1);
       if (! xh_writedata(ofl,h,seism)) exit(-1);
    }

  } // while

  fclose (ifl);
  fclose (ofl);
}

int usage(){
        fprintf(stdout,"Write out traces that have a SNR above cutoff\n");
        fprintf(stdout,"Assumes trace has 5Hz sampling rate and begins\n");
        fprintf(stdout,"200 seconds before P wave\n");
        fprintf(stdout,"\n");
        fprintf(stderr,"Usage: xh_cleandata XH_in XH_out SNR_cutoff\n");
        fprintf(stdout,"\n");
        fprintf(stderr,"if SNR_cutoff = 3, XH_out will contain only \n");
        fprintf(stderr,"   traces where the signal amplitde is at least three\n");
        fprintf(stderr,"   times bigger than the highest noise amplitude\n");
        fprintf(stdout,"\n");
        return(-1);
}