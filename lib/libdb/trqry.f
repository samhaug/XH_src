      subroutine trqry(itre)
      include "dblib.h"
      lkey=ibig(itre+OTRLK)
      call balloc(lkey,io)
      do i=0,lkey-1
        ibig(io+i)=z'7fffffff'
      enddo
      call trfind(itre,ibig(io),lkey,iok,ioi)
      call dalloc(lkey,io)
      write(6,"('Name: ',33a4)")
     1    (ibig(itre+OTRNM+i),i=0,ibig(itre+OTRLN)-1)
      write(6,"(4x,'mord :',i8)") ibig(itre+OTROR)
      write(6,"(4x,'lkey :',i8)") ibig(itre+OTRLK)
      write(6,"(4x,'linfo:',i8)") ibig(itre+OTRLI)
      write(6,"(4x,'type: ',z8)") ibig(itre+OTRTP)
      write(6,"(4x,'maxlv:',i8)") ibig(itre+OTRML)
      write(6,"(4x,'level:',i8)") ibig(itre+OTRLV)
      return
      end
