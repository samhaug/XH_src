

c-------------------------------------------------------------------

      subroutine balloc(nwords,ioff)
      include '../libdb/dblib.h'
      ioff=nexbig
      if(nwords.ge.0) then
        nexbig=nexbig+nwords
        if(nexbig.gt.bigsize) then
          write(0,'(i12,''  +'',i12,''  :'',i12)')
     1              nexbig-nwords,nwords,bigsize
          stop 'balloc: space exceeded'
        endif
      else
        nwords=bigsize-nexbig
        nexbig=bigsize
      endif
c     do i=ioff,nexbig-1
c       ibig(i)=0
c     enddo
c     write(6,*) 'balloc:',nexbig
      return
      end
