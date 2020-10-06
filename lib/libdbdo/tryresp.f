      subroutine tryresp(icrref,iopt,ifl,istage,freq1,freq2,freqstep)
      include 'seedparam.h'
      include 'seedtrees.h'
      include '../libdb/dblib.h'
      character*500 str500
      logical getabr
      include 'cbufcm.h'
      complex stgres
      common/rspsgs/stgres(20),stgdly(20)
      character*4 str4
      equivalence (str4,istr4)
      character*3 cod
      integer srespsetup
      complex sresp,resp
      data tpi/6.2831853/
      lencbuf=len(cbuf)
      if(getabr(itresp,icrref,str500,lstr500)) then
        k=1
        iofc=0
        do while(k.lt.lstr500)
          str4=str500(k:k+3)
          call getblkt(itsblock,istr4,cod,itp,cbuf(iofc+1:lencbuf),lcbuf,ierr)
          iofc=iofc+lcbuf
          if(ierr.ne.0) then
            ierr=9
            write(6,*) 'tryresp: unable to get response blockette'
            return
          endif
          k=k+4
        enddo
      else
        pause 'tryresp: unable to get channel response list'
      endif
      nwords=0
      call balloc(1,iach)
      call dalloc(1,iach)
      if(mod(iach,2).ne.0) then
        call balloc(1,iach0)
        nwords=nwords+1
        call balloc(1,iach)
        call dalloc(1,iach)
      else
        iach0=iach
      endif
      call stdunfm(cbuf(1:iofc))
      lchinfo=makechinfo(cbuf(1:iofc),rbig(iach),ibig(iach))
      call balloc(lchinfo,iach)
      nwords=nwords+lchinfo

      call balloc(1,iaresp)
      call dalloc(1,iaresp)
      lresp=srespsetup(cbuf(1:iofc),iopt
     1     ,SUNCNT,SUNM,SUNMPS,SUNMPS2,SUNNMPS
     1     ,ibig(iaresp),rbig(iaresp),ltalloc())
      
      call balloc(lresp,iaresp)
      nwords=nwords+lresp
      if(ifl.gt.0) then
        if(freq1.lt.freq2.and.freqstep.lt.0.) freqstep=-freqstep
        if(freq1.gt.freq2.and.freqstep.gt.0.) freqstep=-freqstep
        f=freq1
        write(6,'(''Stage:'',i4)') istage
        write(6,'(''     f(Hz)    amplitude   phase     delay'')')
        do while(f.le.freq2)
          om=f*tpi
          resp=sresp(om,ibig(iaresp),rbig(iaresp))
          gdel=srespd(om,ibig(iaresp),rbig(iaresp))
          if(istage.ne.0) then
            resp=stgres(istage)
            gdel=stgdly(istage)
          endif
          write(6,'(f10.4,1pe13.5,0pf8.2,f10.6)') 
     1        f,cabs(resp),atan2(aimag(resp),real(resp))*360./tpi,gdel
          f=f+freqstep
        enddo
      endif
c     call balloc(1,iaresp)
c     call dalloc(1,iaresp)
c     lresp=dsrespsetup(cbuf(1:iofc),iopt
c    1     ,SUNCNT,SUNM,SUNMPS,SUNMPS2,ibig(iaresp),dbig(iaresp/2),ltalloc())
c     
c     call balloc(lresp,iaresp)
c     nwords=nwords+lresp

      call dalloc(nwords,iach0)
      return
      end
