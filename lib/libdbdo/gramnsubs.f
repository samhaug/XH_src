c-------------------------------------------------------------------------------
      integer function gramnsubs(igtab,ig,ifexst)
      dimension ifexst(*)
      include 'gramblock.h'
      include '../libdb/dblib.h'
      iadent=igtab+ibig(igtab+OTGETIT)+(ig-1)*LTENTRY
      gramnsubs=ibig(iadent+OTNSUBS)
      do isub=1,LMXCHNL
        ifexst(isub)=0
      enddo
      do isub=1,gramnsubs
        if(ibig(iadent+LTHEADR+LTSUBEN*(isub-1)+OTSBNPC).gt.0) ifexst(isub)=1
      enddo
      return
      end
