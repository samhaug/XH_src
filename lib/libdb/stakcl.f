

c-------------------------------------------------------------------

      subroutine stakcl(istadr)
      include "dblib.h"
c     write(6,*) 'stakcl: istadr=',istadr,' ipstak=',ipstak
      if(ipstak.ne.istadr) then
        pause 'stakcl: not the last stack'
      else
        ipstak=ibig(istadr+OSTPP)
c     write(6,*) 'stakcl: ipstak set to val at',istadr+OSTPP,ipstak
      endif
      call stakfl(istadr,0)
      call dalloc(ibig(istadr+OSTSZ),istadr)
      ibig(istadr+OSTPP)=-1
      return
      end
