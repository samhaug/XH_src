      subroutine glink(link,value,istat)
      character*(*) link,value
      character*160 cat2s
      llink=istlen(link)
      value=link
      istat=3
   10 call creadlink(cat2s(link(1:llink),char(0),lcat2s)
     1   ,value,len(value),ires,ierrno)
c     write(6,'(2i5,'':'',80a1)') ires,ierrno,(value(i:i),i=1,ires)
      if(ierrno.eq.0) then
        link=value
        llink=ires
        goto 10
      else if(ierrno.eq.2) then
        istat=1
      else if(ierrno.eq.22) then
        istat=0
      else




        call check('glink')
      endif

      value(llink+1:len(value))=char(0)
      return
      end
