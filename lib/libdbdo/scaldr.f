
c--------------------------------------------------------------------
      subroutine scaldr(lu,ifsi,ifsc,ifci,ifcc,ifcr,iopt,ifct,ifdump,ierr)
      include 'seeddefs.h'
      include 'seedtrees.h'

      include 'dbdocm.h'
      parameter (MXDUMP=1500)
      character*2 prefx
      character*32 dumpfls
      common/fmtcommun/itypcl,ndump,ldumpfls(MXDUMP),dumpfls(MXDUMP),ncrefs,icrefs(MXDUMP),prefx

      external fmtscal

      dimension itim1(2),itim2(2)

      ierr=0
      if(ifdump.ge.0) then
        ndump=0
      else
        ndump=-1
      endif
      if(iopt.ge.0) then
        ncrefs=0
      else
        ncrefs=-1
      endif
      if(ifsi.lt.0.and.ifsc.lt.0
     1   .and.ifci.lt.0.and.ifcc.lt.0.and.ifcr.lt.0.and.ifct.lt.0) return
      if(kntstno.le.0) then
        write(6,*) 'No station selected'
        ierr=9
        return
      endif
      call inttime(timelo,itim1)
      call inttime(timehi,itim2)
      do istno=1,kntstno
        call openstn(stnopnd(istno),inetopnd(istno),4)
        if(ifsi.ge.0) then
          write(6,*) stnopen//' '//netopen(1:lnetopen)
          prefx='si'
          itypcl=0
          call showcldr(6,itsticl,mssticl,mtsticl
     1          ,itim1,itim2,1,12,fmtscal,iad)
        endif
        if(ifsc.ge.0) then
          write(6,*) stnopen//' '//netopen(1:lnetopen)
          prefx='sc'
          itypcl=1
          call showcldr(6,itstccl,msstccl,mtstccl
     1            ,itim1,itim2,1,12,fmtscal,iad)
        endif
        if(ifci.ge.0.or.ifcc.ge.0.or.ifcr.ge.0.or.ifct.ge.0) then
          do ichno=1,kntchno(istno)
            iach=ichno+iofchno(istno)
            call openchnk(chnopnd(iach),4)
            if(ifci.ge.0) then
              prefx='ci'
c             write(6,*) stnopen//' '//chnopen(1:5)
c    1              //' '//chzopen//' '//' '//csbopen//' '//netopen(1:lnetopen)
              write(6,'(a)') stnopen//' '//chnopen(1:5)//'('//csbopen//') ['
     1              //chzopen//'Hz '//cftopen//'F]'
              itypcl=0
              call showcldr(6,itchicl,mschicl,mtchicl
     1              ,itim1,itim2,1,12,fmtscal,iad)
            endif
            if(ifcc.ge.0) then
              prefx='cc'
c             write(6,*) stnopen//' '//chnopen(1:5)
c    1              //' '//chzopen//' '//' '//csbopen//' '//netopen(1:lnetopen)
              write(6,'(a)') stnopen//' '//chnopen(1:5)//'('//csbopen//') ['
     1              //chzopen//'Hz '//cftopen//'F]'
              itypcl=1
              call showcldr(6,itchccl,mschccl,mtchccl
     1                ,itim1,itim2,1,12,fmtscal,iad)
            endif
            if(ifcr.ge.0) then
              prefx='cr'
c             write(6,*) stnopen//' '//chnopen(1:5)
c    1              //' '//chzopen//' '//' '//csbopen//' '//netopen(1:lnetopen)
              write(6,'(a)') stnopen//' '//chnopen(1:5)//'('//csbopen//') ['
     1              //chzopen//'Hz '//cftopen//'F]'

              itypcl=0
              call showcldr(6,itchrcl,mschrcl,mtchrcl
     1                ,itim1,itim2,1,12,fmtscal,iad)
            endif
            if(ifct.ge.0) then
              prefx='vl'
c             write(6,*) stnopen//' '//chnopen(1:5)
c    1              //' '//chzopen//' '//' '//csbopen//' '//netopen(1:lnetopen)
              write(6,'(a)') stnopen//' '//chnopen(1:5)//'('//csbopen//') ['
     1              //chzopen//'Hz '//cftopen//'F]'
              itypcl=2
              call showcldr(6,itchpcl,mschpcl,mtchpcl
     1              ,itim1,itim2,1,20,fmtscal,iad)
            endif
          enddo
        endif
        call closestn()
      enddo

      return
      end
