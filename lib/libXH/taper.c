#include <math.h>
#include <sunmath.h>

/* 
       tapers a trace by
       applying a cosine taper to the first and last 50 s of the time
       series.
       y(i)  = original series
       dt    = sampling time interval 
       npts  = number of samples
*/

void taper(y, dt, npts)
     	float *y;
	float dt;
	int npts;
{

double pi;
double arg, sinarg;
int i, nn;

pi = 4.*atan(1.);
nn = 50./dt;

for (i=0; i<nn; ++i) { 
  arg = pi/2.*(i*1.)/(nn-1);
  sinarg = sin(arg);
  y[i] *= sinarg*sinarg;
}
for (i=npts-nn; i<npts; ++i) { 
  arg = pi/2. - pi/2.*(i-npts+nn)*1./(nn-1);
  sinarg = sin(arg);
  y[i] *= sinarg*sinarg;
}

return;

}
