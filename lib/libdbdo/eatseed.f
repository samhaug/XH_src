c--------------------------------------------------------------------
      subroutine eatseed(fname,it,iforce,ntps,tps,ltps,tim1,tim2,ierr)
      character*(*) fname,tim1,tim2
      character*80 tps(*)
      dimension ltps(*)
      character*3 cod
      logical first
      character*5 chival
      character*200 fnamei

      include 'seedparam.h'
      include 'seedcommun.h'
      include 'seedbuf.h'

      include 'dbdocm.h'

      character*2 str2,str2l,locout
      character*3 str3,str3l,chnout
      character*5 str5,str5l,statout
      character*10 str10
      double precision smpin,smpinl,secs,tinc,terr
      logical started,got,flush,thru
      dimension itim1(2),itim2(2),keyts(2)
      character*5 statl,chanl
      include '../libdb/dblib.h'
      include 'seedtrees.h'
      logical calop
      character*12 ckey
      integer system


      character*60 tpnm
      common/tapenm/ltpnm,tpnm


      include 'cbufcm.h'
      integer SIADR,SCADR,CIADR,CCADR,CRADR,CTADR,NMADR
      parameter (SIADR=1,SCADR=2,CIADR=3,CCADR=4,CRADR=5,CTADR=6,NMADR=6)

      fnamei=fname
      lfname=istlen(fname)
      
      call setabrn(itblabr,itblabrh,lsblabr,idblf,fname(1:lfname),tmpblk,ifound)
      if(ifound.eq.1.and.iforce.lt.0) then
        write(6,'(a)') 'eatseed: '//fnamei(1:lfname)//' already read'
        volblock='NOT READ'
        lvolblock=8
        call savevol(fname(1:lfname),ntps,tps,ltps)
        return
      endif

      icompr=0
      write(6,'(2a)') 'Opening:',fname(1:lfname)
      call opnflc(1,fname(1:lfname),1,0,0,ierr,-1,0)
      if(ierr.ne.0) then
        call opnflc(1,fnamei(1:lfname)//'.z',1,0,0,ierr,-1,0)
        if(ierr.ne.0) then
          write(6,*) 'eatseed: Cannot open input file'
          return
        else
          call closfl(1,icstat)
          write(6,'(a)') 'Uncompressing ...'
          isys=system('uncompress -c < '''//fnamei(1:lfname)
     1             //'''.z > '''//fnamei(1:lfname)//''' ; exit')
          icompr=1
          call opnflc(1,fname(1:lfname),1,0,0,ierr,-1,0)
          if(ierr.ne.0) then
            write(6,*) 'eatseed: Cannot open input file'
            return
          endif
        endif
      endif
      call setup1(1,6,ierr)
      if(ierr.ne.0) then
         write(6,*) 'eatseed: Problem with seting up input file'
         goto 99
      endif

c     if(ltpnm.le.0) then
c       tpnm=tp(1:ltp)
c       ltpnm=ltp
c     endif

c initialize abbreviations
      do i=1,MXDICTS
        do j=1,MXABRS
          kabbr(j,i)=0
          kabbrmap(j,i)=0
        enddo
      enddo
      kstr=0

      nstblocks=0
      nchblocks=0
      lvolblock=0
      nstnc=0
      nchnc=0
      mode=1
      if(it.eq.SIADR) then
        mode=6
      else if(it.eq.SCADR) then
        mode=5
      else if(it.eq.CIADR) then
        if(kntstno.ne.1) then
          write(6,*) 'eatseed: more than one station selected'
          ierr=9
          return
        endif
        call openstn(stnopnd(1),inetopnd(1),4)
        mode=5
      else if(it.eq.CCADR) then
        mode=6
      else if(it.eq.CRADR) then
        if(chnseld.eq.' '.or.stnseld.eq.' '.or.inetseld.eq.0) then
          write(6,*) 'eatseed: station/channel not selected'
          ierr=9
          goto 99
        endif
        mode=6
      else if(it.eq.CTADR) then
        mode=8
      endif
      if(mode.ne.1) then
c set volume times
        stncall=stnseld
        netid=inetseld
        chnlocid=chnseld(1:2)
        chnname=chnseld(3:5)
        chnsub=chnseld(6:9)
        tgotstdchn(1,1)=tim1
        tgotstdchn(2,1)=tim2
        tgotstdstn(1,1)=tim1
        tgotstdstn(2,1)=tim2
        call inttime(timelo,itim1vol)
        call inttime(timehi,itim2vol)
      endif
      mode0=mode
      first=.TRUE.



  100 kstrin=kstr
      call getblock('   ',cod,inum,cbuf,10000,itp)
      if(itp.ne.STPA.and.kstr.ne.kstrin) call saveabbrvs(ierr)
      if(cod.eq.'010') mode=1
      if(cod.eq.'010') mode0=1

      goto (1,2,3,4,5,6,7),mode

c expecting volume id
    1   if(itp.ne.STPV.or.cod.ne.'010') then
          write(6,*) 'eatseed: volid expected'
          ierr=9
          goto 99
        endif
        mode=2
        goto 200

c expecting volume station index
         
   2    if(itp.ne.STPV.or.cod.ne.'011') then
          write(6,*) 'eatseed: volume station index expected'
          ierr=9
          goto 99
        endif
c        call savevol(fname,ntps,tps,ltps)
        mode=3
        goto 200

c expecting volume timespan index
   3    if(itp.ne.STPV.or.cod.ne.'012') then
          write(6,*) 'eatseed: volume timespan index expected'
c         ierr=9
c         goto 99
        endif
        mode=4
        goto 200

c accumulating abbreviations
    4   if(itp.ne.STPA) then
          call saveabbrvs(ierr)
          if(ierr.ne.0) then
            write(6,*) 'Unknown items in unit or format dictionaries'
            goto 99
          endif
          call savevol(fname(1:lfname),ntps,tps,ltps)
          mode=5
        endif
        goto 200

c accumulating station id + comments
    5   if(itp.ne.STPS.or.cod.eq.'052'.or.cod.eq.'050') then
          if(mode0.ne.1) then
            tgotstdstn(1,1)=tim1
            tgotstdstn(2,1)=tim2
          endif
          if(.NOT.first) call savestn()
          if(cod.eq.'052') then
            mode=6
          else if(itp.eq.STPT) then
            str10='xxxxxxxxxx'
            mode=7
          endif
        endif
        goto 200

c accumulating channel id + comments + responses
    6   if(itp.ne.STPS.or.cod.eq.'050'.or.cod.eq.'052') then
          if(mode0.ne.1) then
            tgotstdchn(1,1)=tim1
            tgotstdchn(2,1)=tim2
          endif
          call savechn()
          if(cod.eq.'050') then
            mode=5
          else if(itp.eq.STPT) then
            str10='xxxxxxxxxx'
            mode=7
          endif
        endif
        goto 200

c accumulating channel time series blockettes
    7   continue
        if(itp.ne.STPT.or.cod.ne.'074'.or.cbuf(1:10).ne.str10) then
          call savesegs()
        endif
        if(cod.eq.'074') str10=cbuf(1:10)
        goto 200

  200 if(itp.eq.STPV.or.itp.eq.STPA.or.itp.eq.STPS.or.itp.eq.STPT) then
        call eatblock(inum,cbuf,cod,itp)
        first=.FALSE.
        goto 100
      endif

   99 continue
      call closfl(1,icstat)
      if(icompr.ne.0) then
        write(6,'(a)') 'Compressing ...'
c       isys=system('zcomp '''//fnamei(1:lfname)//''' < /dev/null')
        isys=system('rm '''//fnamei(1:lfname)//''' < /dev/null ; exit')
      endif
      return
      end
