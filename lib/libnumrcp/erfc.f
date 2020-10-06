      FUNCTION erfc(x)
      REAL erfc,x
CU    USES gammp,gammq
      REAL gammp,gammq
      if(x.lt.0.)then
        erfc=1.+gammp(.5,x**2)
      else
        erfc=gammq(.5,x**2)
      endif
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software '%1&9p#!.
