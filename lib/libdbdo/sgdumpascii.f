      subroutine sgdumpascii(lu,isub,iagram)
      include 'gramblock.h'
      include '../libdb/dblib.h'
      include 'seedparam.h'
      parameter (LENFLT=1)
      include 'rspaddrs.h'
      character*200 str
      iach=iagram+ibig(iagram+OGSTCHO+isub-1)
      if(iach.lt.iagram) return
      call sgstation(iagram,str,lstr)
      write(lu,"(200a1)") (str(i:i),i=1,lstr)
      call sgchannel(isub,iagram,str,lstr)
      write(lu,"(200a1)") (str(i:i),i=1,lstr)
      write(str,"(i6)") ibig(iach+OGRSREF)
      ip=iststa(str(1:6))
      str(7:12)=str(ip:6)
      iaresp=iach+ibig(iach+OGRSBLO)
      ipi=iaresp
      ipf=iaresp/LENFLT
      ipi0=iaresp
      nstage=ibig(ipi+IRSNCS)
      ipow=ibig(ipi+IRSPOW)

      write(lu,"('  Response cr.',a6,' has',i2,' stages. Units are ',a5
     1     ,'.',' Rate is ',f8.4,'Hz.  DS=',1pe13.5,' at ',0pf8.4,'Hz.')") 
     1  str(7:12),nstage,utext(ipow),rbig(ipf+FRSSRT),rbig(ipf+FRSOSN),rbig(ipf+FRSFOV)
      ipi=ipi+ILENRS
      ipf=ipi/LENFLT
      do istage=1,nstage
        if(rbig(ipf+FSGINR).lt.1000.) then
          write(lu,"('  Stage:',i2,'. ',2i1,2i2,1pe13.5,0p2f8.4,2i4,2f10.4)")
     1        istage, ibig(ipi+ISGPZC),ibig(ipi+ISGABD)
     1       ,ibig(ipi+ISGINU),ibig(ipi+ISGOUU),rbig(ipf+FSGGAI),rbig(ipf+FSGFGA)
     1       ,rbig(ipf+FSGINR),ibig(ipi+ISGDCF),ibig(ipi+ISGDCO)
     1       ,rbig(ipf+FSGEDL),rbig(ipf+FSGCAP)
        else
          write(lu,"('  Stage:',i2,'. ',2i1,2i2,1pe13.5,0pf8.4,f8.3,2i4,2f10.4)")
     1        istage, ibig(ipi+ISGPZC),ibig(ipi+ISGABD)
     1       ,ibig(ipi+ISGINU),ibig(ipi+ISGOUU),rbig(ipf+FSGGAI),rbig(ipf+FSGFGA)
     1       ,rbig(ipf+FSGINR),ibig(ipi+ISGDCF),ibig(ipi+ISGDCO)
     1       ,rbig(ipf+FSGEDL),rbig(ipf+FSGCAP)
        endif
        ipzco=ibig(ipi+ISGPZC)
        if(ipzco.eq.STRPZS) then
          write(lu,"(3x,'a0=',1pe13.5,' at ',0pf8.4,'Hz')") rbig(ipf+FSGAZE),rbig(ipf+FSGFAZ)
          ntop=ibig(ipi+ISGNTP)
          nbot=ibig(ipi+ISGNBT)
          write(lu,"(i4,' zeros')") ntop
          ipf1=ipf+FLENSG
          if(ntop.ne.0) write(lu,"(4x,1p6e13.5)") (rbig(ipf1+i-1),i=1,2*ntop)
          write(lu,"(i4,' poles')") nbot
          ipf1=ipf1+ibig(ipi+ISGLNT)
          if(nbot.ne.0) write(lu,"(4x,1p6e13.5)") (rbig(ipf1+i-1),i=1,2*nbot)
        else if(ipzco.eq.STRCOF) then
          ntop=ibig(ipi+ISGNTP)
          nbot=ibig(ipi+ISGNBT)
          write(lu,"(i4,' numerator coefficients')") ntop
          ipf1=ipf+FLENSG
          if(ntop.ne.0) write(lu,"(4x,1p6e13.5)") (rbig(ipf1+i-1),i=1,ntop)
          ipf1=ipf1+ibig(ipi+ISGLNT)
          write(lu,"(i4,' denominator coefficients')") nbot
          if(nbot.ne.0) write(lu,"(4x,1p6e13.5)") (rbig(ipf1+i-1),i=1,nbot)
        else 
          pause 'sgdumpascii: unknown response format'
        endif
        ipi=ipi0+ibig(ipi+ISGADN)
        ipf=ipi/LENFLT
      enddo
      


      iads=iach+ibig(iach+OGCHSGA)
      nsamp=ibig(iach+OGCHSGL)
      write(lu,"('Data ...')")
      write(lu,"(1p8e13.5)") (rbig(i),i=iads,iads+nsamp-1)
      return
      end
