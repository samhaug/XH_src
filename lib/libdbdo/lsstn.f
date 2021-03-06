c----------------------------------------------------------
      subroutine lsstn(ifinfo,ifcmd)
      dimension itim1(2),itim2(2)
      character*20 ckey
      integer*2 jkey(10)
      integer key(5)
      equivalence (key,jkey,ckey)
      character*5 stcode
      character*80 str80
      character*2 locid
      character*3 chcode
      character*4 chsub
      character*24 timestt,timeend
      character*10 cblock
      character*200 line,line1
      character*3 str3
      character*8 str8
      character*4 str4
      double precision dsmpin
      dimension itimlo(2),itimhi(2)
      include 'cbufcm.h'
      include 'dbdocm.h'
      character*3 cod


      character*10 tabs/'\t\t\t\t\t\t\t\t\t\t'/
      include 'seeddefs.h'
      include 'seedtrees.h'
      include '../libdb/dblib.h'
      logical trfind,trnext,pmatch,match,getabr
      dimension jtim(2)

      if(ifinfo.ge.0) then
        call inttime(timelo,itimlo)
        call inttime(timehi,itimhi)
      endif


      do istno=1,kntstno

          call openstn(stnopnd(istno),inetopnd(istno),4)
          lline=0
          if(ifinfo.ge.0) then
            jtim(1)=z'80000000'
            jtim(2)=0
            if(trfind(itsticl,jtim,2,iok,ioi)) pause 'lsstn: small found'
            if(trnext(itsticl,1,iok,ioi)) then
              if(itcmp(ibig(iok),itimhi).gt.0) goto 88
              jtim(1)=ibig(iok)
              jtim(2)=0
              call timeint(jtim,timestt,ltimestt)
              ltimestt=ltimestt-1
            else
              ltimestt=0
            endif

            jtim(1)=z'7fffffff'
            jtim(2)=0
            if(trfind(itsticl,jtim,2,iok,ioi)) pause 'lsstn: large found'
            if(trnext(itsticl,-1,iok,ioi)) then
              if(itcmp(ibig(iok),itimlo).lt.0) goto 88
              irefsi=ibig(ioi)
              jtim(1)=ibig(iok)
              jtim(2)=0
              call timeint(jtim,timeend,ltimeend)
              ltimeend=ltimeend-1

              call getblkt(itsblock,irefsi,cod,itp,cbuf,lcbuf,ierr)
              line1='\t\t\t\t'//cbuf(13:22)//'  '//cbuf(23:33)//'\t'
              lline1=28
              io=47
              call getstr(cbuf,io,line1(lline1+1:200),lgot)
              lline1=lline1+lgot


            else
              ltimeend=0
            endif

            write(line(lline+1:200),'(a)') ' '//timestt(1:ltimestt)//'--'//timeend(1:ltimeend)
            lline=lline+3 + ltimestt+ltimeend
          endif
          if(ifcmd.ge.0) then
            line(lline+1:lline+12+lnetopen)='sta '''//stnopen//' '//netopen(1:lnetopen)//''''
            lline=lline+12+lnetopen
          else
            line(lline+1:lline+8+lnetopen)='  '//stnopen//' '//netopen(1:lnetopen)
            lline=lline+8+lnetopen
          endif
          write(6,'(a)') line(1:lline)
          if(ifinfo.ge.0) write(6,'(a)') line1(1:lline1)
          do ichno=1,kntchno(istno)
            iach=ichno+iofchno(istno)
            latest(iach)='80000000'x
          enddo
          do ichno=1,kntchno(istno)
            iach=ichno+iofchno(istno)
            line(1:1)='\t'
            lline=1
            if(ifinfo.ge.0) then
              call openchnk(chnopnd(iach),4)
              jtim(1)=z'80000000'
              jtim(2)=0
              if(trfind(itchicl,jtim,2,iok,ioi)) pause 'lsstn: small found'
              if(trnext(itchicl,1,iok,ioi)) then
                if(itcmp(ibig(iok),itimhi).gt.0) goto 77
                jtim(1)=ibig(iok)
                jtim(2)=0
                call timeint(jtim,timestt,ltimestt)

                ltimestt=ltimestt-1
              else
                ltimestt=0
              endif

              jtim(1)=z'7fffffff'
              jtim(2)=0
              if(trfind(itchicl,jtim,2,iok,ioi)) pause 'lsstn: large found'
              if(trnext(itchicl,-1,iok,ioi)) then
                if(itcmp(ibig(iok),itimlo).lt.0) goto 77
                irefci=ibig(ioi)
                jtim(1)=ibig(iok)
                jtim(2)=0
                latest(iach)=jtim(1)
                call timeint(jtim,timeend,ltimeend)
                ltimeend=ltimeend-1
              else
                ltimeend=0
              endif
              line(lline+1:lline+4+ltimestt+ltimeend)=timestt(1:ltimestt)//'--'//timeend(1:ltimeend)//'  '
              lline=lline+4+ltimestt+ltimeend
 
            endif
            ckey=chnopnd(iach)

            call chnldcd(ckey,hz,isub,ifmt)


            write(str8,"(f8.4)") hz
            write(str4,"(i4)") isub
            do i=1,4
              if(str4(i:i).eq.' ') str4(i:i)='0'
            enddo
            write(str3,"(i3)") ifmt
            do i=1,3
              if(str3(i:i).eq.' ') str3(i:i)='0'
            enddo

            if(ifcmd.ge.0) then
              line='    cha '''//ckey(1:5)//'('//str4//') ['//str8//'Hz '//str3//'F]'''
              lline=39
            else
              str80=ckey(1:5)//'('//str4//') ['//str8//'Hz '//str3//'F]'
              lstr80=29
              line(lline+1:lline+lstr80)=str80(1:lstr80)
              lline=lline+lstr80
            endif
            write(6,'(a)') line(1:lline)
   77       continue

          enddo
   88     continue
          call closestn()
      enddo
      return
      end
