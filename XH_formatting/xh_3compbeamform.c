#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <complex.h>
#include <sys/param.h>
#include "xhhead.h" /* XH format   structures */
#include "beamform.h" /* beamform  structure */

/* Beamforms an array of stations over backazimuth and incidence angle
 * Rotates each three component station and stacks component along inc/baz 
 */

/*
 * l = z * cos(inc) - n * sin(inc) * cos(ba) - e * sin(inc) * sin(ba)  # NOQA                       
 * q = z * sin(inc) + n * cos(inc) * cos(ba) + e * cos(inc) * sin(ba)  # NOQA
 * t = n * sin(ba) - e * cos(ba)  # NOQA                        
 */
//  1. make BH1 and BH2 compatable
//  2. account for sensor alignment with back-azimuth
//  3. using xh_readhead in large loop makes it slow
//
#define MAXTRACE 3000
#define MAXSAMP 65536
#define MAXP 500
#define TRACE 50
#define NPT 5000

int usage();
float max(float num1, float num2);
float max(float num1, float num2){
    return (num1 > num2 ) ? num1 : num2;
}

int main(int argc,char *argv[]){
  xhhed hz,hn,he;
  beamform beam;
  FILE *infz,*infe,*infn,*outf;
  int i_shift,b_idx,i_idx,ierr;
  int still_reading_data = 1;
  int jj=0,M=0;
  int bcount,icount, num_samp=0;
  int distaz();
  float v_o=5.80;
  float lat_mean=0,lon_mean=0;
  float l_max,l_min,l_norm;
  float seismz[XH_NPTS],seismn[XH_NPTS],seisme[XH_NPTS],seisml[XH_NPTS];
  float seism_roll[XH_NPTS];
  float seisms_top[XH_NPTS];
  float seisms_bot[XH_NPTS];
  float stack[XH_NPTS];
  float stack2[XH_NPTS];
  float linstack[XH_NPTS];
  float fenv[XH_NPTS];
  float x_vec[MAXTRACE],y_vec[MAXTRACE];
  float rad_i,rad_b,gcarc,az,baz,t_shift;

  // 2D arrays for data
  float dataz[TRACE][NPT],datan[TRACE][NPT],datae[TRACE][NPT];
  // arrays for station coords
  float zlat[TRACE],zlon[TRACE];
  float delta[TRACE];
  //float nlat[TRACE],nlon[TRACE];
  //float elat[TRACE],elon[TRACE];

  if(argc != 11) {
     ierr = usage();
     exit(ierr);
  }
  if ((infz = fopen(argv[1],"r")) == NULL) {
     fprintf(stdout,"Something wrong with xh_Z\n");
     exit(-1);
  }
  if ((infn = fopen(argv[2],"r")) == NULL) {
     fprintf(stdout,"Something wrong with xh_N\n");
     exit(-1);
  }
  if ((infe = fopen(argv[3],"r")) == NULL) {
     fprintf(stdout,"Something wrong with xh_E\n");
     exit(-1);
  }

  int baz_min=atoi(argv[4]); 
  int baz_max=atoi(argv[5]); 
  int baz_inc=atoi(argv[6]); 
  int i_min=atoi(argv[7]); 
  int i_max=atoi(argv[8]); 
  int i_inc=atoi(argv[9]); 
  beam.baz_min = baz_min;
  beam.baz_max = baz_max;
  beam.baz_inc = baz_inc;
  beam.i_min = i_min;
  beam.i_max = i_max;
  beam.i_inc = i_inc;

  //Find array centroid, read in Z data
  int ii=0;
  int numtrace=0;
  while (still_reading_data){
    if (! xh_readhead(infz,&hz)){
      still_reading_data = 0;
    } 
    else {	
      if (! xh_readdata(infz,hz,seismz)){
        still_reading_data = 0;
      }
      if (! xh_checkheader(hz)){
        still_reading_data = 0;
      }
    }
    // Fill some beam structure data
    if (ii == 0){
       num_samp=hz.ndata;
       beam.delta = hz.delta;
       beam.ndata = hz.ndata;
       beam.e_lat = hz.elat;
       beam.e_lon = hz.elon;
       beam.e_dep = hz.edep;
    }

    zlat[ii] = hz.slat;
    zlon[ii] = hz.slon;
    delta[ii] = hz.delta;

    for (int j=0;j<hz.ndata;j++){
        dataz[ii][j] = seismz[j];
    }
    printf("Z: %d\n",ii);
    lat_mean += hz.slat;
    lon_mean += hz.slon;
    ii++;
    numtrace++;
    M++;
  } //while
  fclose(infz);
  lat_mean = (float)(lat_mean/ii);
  lon_mean = (float)(lon_mean/ii);
  beam.a_lat = lat_mean;
  beam.a_lon = lon_mean;
  // End find array centroid
  printf("Number of traces: %d %d\n",numtrace,M);

  still_reading_data = 1;
  ii = 0;
  // Read in N data
  while (still_reading_data){
    if (! xh_readhead(infn,&hn)){
      still_reading_data = 0;
    } 
    else {	
      if (! xh_readdata(infn,hn,seismn)) {
        still_reading_data = 0;
      }
      if (! xh_checkheader(hn)){
        still_reading_data = 0;
      }
    }
    //nlat[ii] = hn.slat;
    //nlon[ii] = hn.slon;

    for (int j=0;j<hn.ndata;j++){
        datan[ii][j] = seismn[j];
    }
    printf("N: %d\n",ii);
    ii++;
  } //while
  still_reading_data = 1;
  fclose(infn);

  ii = 0;
  // Read in E data
  while (still_reading_data){
    if (! xh_readhead(infe,&he)){
      still_reading_data = 0;
    } 
    else {	
      if (! xh_readdata(infe,he,seisme)){
        still_reading_data = 0;
      }
      if (! xh_checkheader(he)){
        still_reading_data = 0;
      }
    }
    for (int j=0;j<he.ndata;j++){
        datae[ii][j] = seisme[j];
    }
    printf("E: %d\n",ii);
    ii++;
  } //while
  fclose(infe);

  // Find position vectors of each station from array centroid
  for (int j=0;j<numtrace;j++){
    //printf("%f %f %f %f\n",lat_mean,lon_mean,lat_mean,zlon[j]);
    distaz(lat_mean, lon_mean, lat_mean, zlon[j], &gcarc, &az, &baz);
    //if a_lat > tr.stats.sac['stla']:
    if (lon_mean > zlon[j]){
        //printf("%s %s slon \n",h.netw,h.stnm);
        x_vec[j] = gcarc*-111.195/v_o;
    }
    else {
        x_vec[j] = gcarc*111.195/v_o;
    }

    //printf("%f %f %f %f\n",lat_mean,lon_mean,zlat[j],lon_mean);
    distaz(lat_mean, lon_mean, zlat[j], lon_mean, &gcarc, &az, &baz);
    if (lat_mean > zlat[j]){
        //printf("%s %s slat \n",h.netw,h.stnm);
        y_vec[j] = gcarc*-111.195/v_o;
    }
    else {
        y_vec[j] = gcarc*111.195/v_o;
    }
    // printf("%s %s %8.3f %8.3f \n",h.netw,h.stnm,x_vec[ii],y_vec[ii]);
    // printf("%s %s %8.3f %8.3f \n",h.netw,h.stnm,h.slat,h.slon);
  } // for
  // END find position vectors of each station from array centroid
  
  
  // Begin beamforming
  b_idx = 0;
  i_idx = 0;
  for (bcount=baz_min;bcount<baz_max;bcount+=baz_inc){
    i_idx=0;
    fprintf(stdout,"%8.2f%%  complete\n",100*(float)bcount/(float)baz_max);
    for (icount=i_min;icount<i_max;icount+=i_inc){
      still_reading_data = 1;

      for (jj=0;jj<num_samp;jj++){
        stack[jj] = 0.;
        stack2[jj] = 0.;
        linstack[jj] = 0.;
        seisms_top[jj] = 0.;
        seisms_bot[jj] = 0.;
      }
      
      for (int k=0;k<numtrace;k++){
        rad_i = (M_PI/180.)*icount;
        rad_b = (M_PI/180.)*bcount;
        // Rotate to l component
        l_max = 0;
        l_min = 0;
        for (jj=0;jj<num_samp;jj++){
          seisml[jj] = dataz[k][jj]*cos(rad_i)-
                       datan[k][jj]*sin(rad_i)*cos(rad_b)-
                       datae[k][jj]*sin(rad_i)*sin(rad_b);
          // Find the min/max while you're at it
          if (seisml[jj]>=l_max) l_max = seisml[jj];
          if (seisml[jj]<=l_min) l_min = seisml[jj];
        }

        l_norm = max(fabs(l_min),l_max);

        for (jj=0;jj<num_samp;jj++){
          seisml[jj] = seisml[jj]/l_norm;
        }

        t_shift = sin(rad_i)*sin(rad_b)*x_vec[k]+sin(rad_i)*cos(rad_b)*y_vec[k];
        i_shift = (int)(t_shift/delta[k]);

        if (! roll(seisml,seism_roll,num_samp,i_shift)){
           fprintf(stderr,"Error timeshifting data ... \n");
           exit(1);
        }
        for (jj=0;jj<num_samp;jj++){
           // 4th root stack
           stack[jj] += pow(fabs(seism_roll[jj]),0.25)*
                        (seism_roll[jj]/fabs(seism_roll[jj]))/(float)M;
           //
           stack2[jj] += pow(fabs(seism_roll[jj]),0.5)*
                        (seism_roll[jj]/fabs(seism_roll[jj]))/(float)M;
           //linear_stack
           linstack[jj] += seism_roll[jj]/(float)M;
           //semblance denominator
           seisms_bot[jj] += pow(seism_roll[jj],2)*(float)M;
           seisms_top[jj] += seism_roll[jj];
        }

      } //for ktrace

      // 4th root stack cont'd
      for (jj=0;jj<num_samp;jj++){
         stack[jj] = pow(fabs(stack[jj]),4)*(stack[jj]/fabs(stack[jj]));
         stack2[jj] = pow(fabs(stack[jj]),2)*(stack[jj]/fabs(stack[jj]));
      }

      if (! envelope(num_samp,stack,fenv)){
         fprintf(stdout,"error with envelope\n");
         exit(-1);
      }
      for (jj=0;jj<num_samp;jj++){
         // dat1: Linear stack
         beam.dat1[b_idx][i_idx][jj] = linstack[jj];
         // dat2: 4th root envelope
         beam.dat2[b_idx][i_idx][jj] = fenv[jj];
         // dat3: semblance
         beam.dat3[b_idx][i_idx][jj] = pow(seisms_top[jj],2)/seisms_bot[jj];
         //#beam.dat4[b_idx][i_idx][jj] = 0.;
         ///beam.dat5[b_idx][i_idx][jj] = 0.;
      }
    i_idx++;
    } //icount loop
  b_idx++;
  } //bcount

  fflush(stdout);
  fprintf(stdout,"Writing beamform output...\n");
  outf = fopen(argv[10],"wb");
  fwrite(&beam,sizeof(beam),1,outf);
  fclose(outf);

  return(1);
}

int usage(){
   fprintf(stdout,"\n");
   fprintf(stdout,"xh_beamform stacks energy over backazimuth and incidence angle.\n");
   fprintf(stdout,"xh_Z,N,E must have same number of records and same samples per record\n");
   fprintf(stdout,"\n");
   fprintf(stdout,"Default usage :\n");
   fprintf(stdout,"xh_beamform xh_Z xh_N xh_E baz_min baz_max baz_inc i_min i_max i_inc OUTPUT\n");
   fprintf(stdout,"\n");
   fprintf(stdout,"xh_Z          : input Z-comp xh file\n");
   fprintf(stdout,"xh_N          : input N-comp xh file\n");
   fprintf(stdout,"xh_E          : input E-comp xh file\n");
   fprintf(stdout,"baz_min/max   : min/max back azimuth to scan (int)\n");
   fprintf(stdout,"baz_inc       : baz increment (int)\n");
   fprintf(stdout,"i_min/max     : min/max incidence angle to scan (int)\n");
   fprintf(stdout,"i_inc         : incidence angle increment)\n");
   fprintf(stdout,"OUTPUT        : Name of n-th root output file\n");
   fprintf(stdout,"\n");
   return(-1);
}
