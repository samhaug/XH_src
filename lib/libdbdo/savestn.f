c-----------------------------------------------------------------
      subroutine savestn()
      include 'seeddefs.h'
      include 'seedcommun.h'
      include 'seedtrees.h'
      include '../libdb/dblib.h'
      integer*4 itim1(2),itim2(2)
      integer*4 cinfo(2)
      character*24 tsta1,tsta2

      call openstn(stncall,netid,4)
      
      if(nstblocks.eq.0) return

      k1=iadstblock(1)+1
      k2=iadstblock(2)
      if(strings(k1:k1+2).ne.'050') pause 'savestn: unexpected blockette'
      call setabrn(itsblock,itsblockh,lssblock
     1         ,ikey,strings(k1:k2),tmpblk,ifound)

      tsta1=tgotstdstn(1,1)
      tsta2=tgotstdstn(2,1)
      write(6,*) stncall,':',tsta1,' to ',tsta2

      call inttime(tgotstdstn(1,1),itim1)
      call inttime(tgotstdstn(2,1),itim2)

      call intimvol(itim1,itim2,itim1vol,itim2vol,ierr)
      if(ierr.eq.0) then
        inrank=-3
        call setcldr(itsticl,mssticl,mtsticl,itim1,itim2,ikey,1,inrank)
      else
        write(6,*) 'savestn: Time error. Not entered in info calendar'
      endif


      do i=2,nstblocks
        k1=iadstblock(i)+1
        k2=iadstblock(i+1)
        if(strings(k1:k1+2).eq.'051') then
          call inttime(tgotstdstn(1,i),itim1)
          call inttime(tgotstdstn(2,i),itim2)
          call intimvol(itim1,itim2,itim1vol,itim2vol,ierr)
          if(ierr.eq.0) then
            cinfo(1)=kgotstdstn(1,i)
            if(kstr.ne.0) cinfo(1)=kabbrmap(cinfo(1),DICTCT)
            cinfo(2)=igotstdstn(1,i)
            inrank=-3
            call setcldr(itstccl,msstccl,mtstccl,itim1,itim2,cinfo,2,inrank)
          else
            write(6,*) 'savestn: Time error. Not entered in comment calendar'
          endif
        else
          pause 'savestn: unexpected blockette'
        endif
      enddo
      return
      end
