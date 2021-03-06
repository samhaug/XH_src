
c-------------------------------------------------------------------
      subroutine trwrit(itre,ibuf,nwords)
      include "dblib.h"
      integer*4 ibuf(*)
      la=ibig(itre+OTRKO)
      ilev=ibig(itre+OTRLV)
      lkey=ibig(itre+OTRLK)
      linfo=ibig(itre+OTRLI)
      lres=ibig(itre+OTRNR)
      iptha=ibig(itre+OTRPA)+ilev
      ikeya=ibig(itre+OTRKY)+ilev
      istak=ibig(itre+OTRST)
      if(ibig(iptha).lt.0) pause 'trwrit: no current key'
      lux=ibig(itre+OTRUX)
      obotol=ibig(istak+OSTBM)
      call stakgt(istak,ibig(iptha),ibrec)
      obotnw=ibig(istak+OSTBM)
      if(obotnw.ne.obotol) pause 'trwrit: not working as expected'
      k=ibrec+2+ibig(ikeya)*la+lres+lkey+linfo
      if(lux.gt.0) then
        if(nwords.gt.0) then
          iend=lenlu(lux)
c ! get info from entry
          if(ibig(itre+OEXCH).lt.0) then
c ! first extension
            if(ibig(k+OEXAD).lt.0) then
              ianex=iend
              nwnex=0
c ! save adress and length
              ibig(k+OEXAD)=ianex
c ! of this addition
              ibig(k+OEXLN)=nwords
c ! find first record
            else
              ianex=ibig(k+OEXAD)
              nwnex=ibig(k+OEXLN)
c ! new record won't fit
              if(nwnex.lt.nwords) ianex=iend
              ibig(k+OEXAD)=ianex
              ibig(k+OEXLN)=nwords
            endif
            call trtuch(itre)
c ! use info from last read
          else
c ! or write
            ianex=ibig(itre+OEXFP)
            nwnex=ibig(itre+OEXNW)
c ! new record won't fit
            if(nwnex.lt.nwords) ianex=iend
            call balloc(2,iex)
            ibig(iex)=ianex
c ! attach to chain
            ibig(iex+1)=nwords
            call byswap4(ibig(iex),2)
            call bffo(lux,1,ibig(iex),8,j,ibig(itre+OEXCH)+1)
            call byswap4(ibig(iex),2)
            call dalloc(2,iex)
          endif
          call balloc(nwords+2,iscr)
          do i=0,nwords-1
            ibig(iscr+i)=ibuf(i+1)
          enddo
c ! terminate chain
          ibig(iscr+nwords)=-1
          ibig(iscr+nwords+1)=0
          call byswap4(ibig(iscr+nwords),2)
          call bffo(lux,1,ibig(iscr),(nwords+2)*4,j,ianex+1)
          call byswap4(ibig(iscr+nwords),2)
          call dalloc(nwords+2,iscr)
c ! update append point
          ibig(k+OEXAP)=ianex+nwords*4
c ! and chaining address
          ibig(itre+OEXCH)=ianex+nwords*4
c ! and ptr to next
          ibig(itre+OEXFP)=-1
c ! and length of next
          ibig(itre+OEXNW)=0
          call trtuch(itre)
c ! do nothing
        else
        endif
      else
        pause 'trwrit: attempt to extend a non extendable entry'
      endif
      return
      end
