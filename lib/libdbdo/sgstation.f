c-------------------------------------------------------------------------------
      subroutine sgstation(iagram,string,nbyts)
      character*(*) string
      include 'gramblock.h'
      include '../libdb/dblib.h'
      lenstr=len(string)
      k=0
      ica=4*(iagram+OGSTNAM)
      do i=ica,ica+4
        k=k+1
        string(k:k)=cbig(i)
      enddo
      write(string(k+1:k+17),"(f8.3,f9.3)") rbig(iagram+OGSTLAT),rbig(iagram+OGSTLON)
      k=k+17
      string(k+1:k+2)=' ['
      k=k+2
      ica=4*(iagram+OGSTNET)
      do i=ica,ica+4*LGSTNET-1
        k=k+1
        string(k:k)=cbig(i)
      enddo
      k=istlen(string(1:k))
      string(k+1:k+2)='] '
      k=k+2
      ica=4*(iagram+OGSTSIT)
      do i=ica,ica+4*LGSTSIT-1
        k=k+1
        string(k:k)=cbig(i)
      enddo
      k=istlen(string(1:k))
      nbyts=k
      return
      end
