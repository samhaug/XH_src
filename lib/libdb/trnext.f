

c-------------------------------------------------------------------

      logical function trnext(itre,idir,iok,ioi)
      include "dblib.h"
c ! extension chain address undefined
      ibig(itre+OEXCH)=-1
      if(ibig(itre+OTRFR).lt.0) then
        iok=-1
        ioi=-1
        trnext=.FALSE.
        return
      endif
      la=ibig(itre+OTRKO)
      istak=ibig(itre+OTRST)
      ilev=ibig(itre+OTRLV)
      if(ibig(ibig(itre+OTRPA)+ilev).lt.0) then
        ilev=ilev-1
        if(idir.ge.0) then
          ikeya=ibig(itre+OTRKY)+ilev
          ibig(ikeya)=ibig(ikeya)-1
        endif
      endif
      iptha=ibig(itre+OTRPA)+ilev
      ikeya=ibig(itre+OTRKY)+ilev
      call stakgt(istak,ibig(iptha),ibrec)
c ! step backwards
      if(idir.lt.0) then
   11   ipt=ibig(1+ibig(ikeya)*la+ibrec)
c ! down a level
        if(ipt.gt.0) then
          ilev=ilev+1
          iptha=iptha+1
          ikeya=ikeya+1
          ibig(iptha)=ipt
          call stakgt(istak,ibig(iptha),ibrec)
          ibig(ikeya)=ibig(ibrec)
          goto 11
        endif
   10   ibig(ikeya)=ibig(ikeya)-1
c ! up a level
        if(ibig(ikeya).lt.0) then
          ilev=ilev-1
          if(ilev.lt.0) then
            ioi=-1
            iok=-1
            trnext=.FALSE.
            return
          endif
          iptha=iptha-1
          ikeya=ikeya-1
          call stakgt(istak,ibig(iptha),ibrec)
          goto 10
        endif
c ! step forwards
      else
        ibig(ikeya)=1+ibig(ikeya)
   21   ipt=ibig(1+ibig(ikeya)*la+ibrec)
c ! down a level
        if(ipt.ge.0) then
          ilev=ilev+1
          iptha=iptha+1
          ikeya=ikeya+1
          ibig(iptha)=ipt
          call stakgt(istak,ibig(iptha),ibrec)
          ibig(ikeya)=0
          goto 21
        endif
c ! up a level
   20   if(ibig(ikeya).ge.ibig(ibrec)) then
          ilev=ilev-1
          if(ilev.lt.0) then
            iok=-1
            ioi=-1
            trnext=.FALSE.
            return
          endif
          iptha=iptha-1
          ikeya=ikeya-1
          call stakgt(istak,ibig(iptha),ibrec)
          goto 20
        endif
      endif
      ibig(itre+OTRLV)=ilev
      iok=ibrec+2+ibig(ikeya)*la
      ioi=iok+ibig(itre+OTRLK)+ibig(itre+OTRNR)
      trnext=.TRUE.
      return
      end
