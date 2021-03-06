#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <sys/param.h>
#include <math.h>

#include "xhhead97m.h"	/* XH format structures */

/* Reads the header of an XH file */

int xh_readhead97(ifl,xh)
xhhed97 *xh;
FILE *ifl;
{


int bRead = fread(xh,1,X_HDRSIZE,ifl);
if (bRead == X_HDRSIZE) {
  return 1;
}

// Check for end of file!
if (feof(ifl) != 0) {
  // make sure there was no trailing junk in file
  if (bRead != X_HDRSIZE && bRead != 0) {
    fprintf(stderr,"\nxh_readhead97() error ... ");
    fprintf(stderr,"read only %d bytes; expected %d bytes\n", bRead,X_HDRSIZE);
  } else {
    // normal end of file case
    // fprintf(stderr,"EOF reached\n");
  }
} else {
  // There was an error of some kind
  fprintf(stderr,"\nxh_readhead97() error ... %d", ferror(ifl));
  perror("xh_readhead97");
}
return 0;

} /* end */
