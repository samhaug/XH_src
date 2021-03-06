      parameter (MXBIG=10000)
      integer bigsize
      common/bigspa/nexbig,bigsize,ibig(0:MXBIG)
      data bigsize/MXBIG/

      character*80 exname
      logical trfind,trnext

      parameter (nops=30)
      dimension iops(nops)/1,5,2,3,4,6,9,7,8,10,23,12,14,15,16,17,18,19,20,21,22,11,13,-9,-7,9,7,-12,-14,-15/
c     parameter (nops=30)
c     dimension iops(nops)/1,2,3,4,5,6,7,8,9,10,12,14,15,16,17,18,19,20,23,21,22,11,13,-9,-7,9,7,-12,-14,-15/
c
c     parameter (nops=30)
c     dimension iops(nops)/1,2,3,4,5,6,7,8,9,10,12,14,15,16,17,18,19,20,23,21,22,11,13,-7,-9,7,9,-12,-14,-15/
c
c OK
c     parameter (nops=22)
c     dimension iops(nops)/1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,-13/
c BAD
c     parameter (nops=26)
c     dimension iops(nops)/1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,-1,1,-22,-21/   

c      parameter (nops=29)
c      dimension iops(nops)/1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,-1,0,1,0,-22,0,-21/      
   
c     parameter (nops=25)
c     dimension iops(nops)/1,2,3,4,5,6,7,8,9,10,12,13,14,15,16,17,18,19,20,21,22,11,0,-18,0/

      nstak=7
      maxlev=7
      mord=21
      lkey=1
      linfo=0
      ityp=0
      exname='*'
      call tropnn(-1,'root',3,4,ierr
     1   ,nstak,maxlev,itre
     1  ,mord,lkey,linfo,ityp,exname)

      do i=1,nops
        if(iops(i).ne.0) then
          key=iabs(iops(i))
    
          if(key.eq.iops(i)) then
            if(.not.trfind(itre,key,1,iok,ioi)) then
              write(6,"('adding:',i4,$)") key
              call staksn(1)
              call traddk(itre,key,1,info,0)
              call staksn(1)
              write(6,*) '  done'
            else
              pause 'key unexpectedly found'
            endif
          else
            if(trfind(itre,key,1,iok,ioi)) then
              write(6,"('deleting:',i4,$)") key
              call staksn(1)
              call trdelk(itre)
              call staksn(1)
              write(6,*) '  done'
            else
              pause 'key unexpectedly not found'
            endif
          endif
        else
          if(trfind(itre,z'80000000',1,iok,ioi)) pause 'small found'
          knt=0
          do while(trnext(itre,1,iok,ioi))
            knt=knt+1
            write(6,"(i3,'.',i6)") knt,ibig(iok)
          enddo
        endif
      enddo


 
      end

