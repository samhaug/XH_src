      SUBROUTINE STATN(ID,NAME,INST,STLA,STLO,ILEV)
      save
      include 'sroida.h'
cc    parameter (mxsta=150)
cc    parameter (mxida=30)
cc    common/stdata/ nsta,index(mxsta),namest(mxsta),stlat(mxsta)
cc   1  ,stlong(mxsta),elev(mxsta)
cc    common/idasta/ nida,idaind(mxida),idanam(mxida),slaida(mxida)
cc   1  ,sloida(mxida),elvida(mxida)

      data ifirst/1/
      if(ifirst.eq.1) then
        ifirst=0
        nsta=0
        nida=0
        open(59,file='station_index',status='old')
  200   read(59,'(i5,1x,a4,i3,f8.3,f9.3,f8.1)',end=100) ind,ista,ins,xla,xlo,elvm
        if(ins.eq.3) then
          do i=1,nida
            if(ind.eq.idaind(i)) then
              if(ista.ne.idanam(i)) stop 'statn: inconsistent station name'
              slaida(i)=xla
              sloida(i)=xlo
              elvida(i)=elvm
              goto 40
            endif
          enddo
          nida=nida+1
          if(nida.gt.mxida) stop 'statn: too many ida stations'
          idaind(nida)=ind
          idanam(nida)=ista
          slaida(nida)=xla
          sloida(nida)=xlo
          elvida(nida)=elvm

   40     continue
        else

          do i=1,nsta
            if(ind.eq.index(i)) then
              if(ista.ne.namest(i)) stop 'statn: inconsistent station name'
              stlat(i)=xla
              stlong(i)=xlo
              elev(i)=elvm/1000.
              goto 50
            endif
          enddo
          nsta=nsta+1
          if(nsta.gt.mxsta) stop 'statn: too many stations'
          index(nsta)=ind
          namest(nsta)=ista
          stlat(nsta)=xla
          stlong(nsta)=xlo
          elev(nsta)=elvm/1000.

   50     continue


        endif
        goto 200
  100   continue
        close(59)
      endif


      if(inst.eq.3) go to 5
      do 1 i=1,nsta
      if(id.eq.index(i)) go to 2
    1 continue
      write(0,'(''statn: did not find:'',i6)') id
      stop 'station not in sro library'
    2 name=namest(i)
      stla=stlat(i)
      stlo=stlong(i)
      ilev=1000.*elev(i)+.499
c     write(0,'(''statn: found:'',i6,2x,a4)') id, name
      return
    5 do 3 i=1,nida
      if(id.eq.idaind(i)) go to 4
    3 continue
      stop 'station not in ida library'
    4 name=idanam(i)
      stla=slaida(i)
      stlo=sloida(i)
      ilev=elvida(i)+.499
      return
      end
