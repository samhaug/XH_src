      parameter (MXSTRINGS=100000)
      character*(MXSTRINGS) strings
      parameter (MXASTRINGS=100000)
      character*(MXASTRINGS) astrings
      parameter(MXTMPBLK=64000)
      character*(MXTMPBLK) tmpblk,tmpblk1
      parameter (MXABRS=10000)
      character*52 volblock
      character*5 stncall
      character*3 chnname
      character*2 chnlocid
      character*4 chnsub
      integer*2 kabbr(MXABRS,MXDICTS),labbr(MXABRS,MXDICTS)
     1    ,kabbrmap(MXABRS,MXDICTS)


      parameter (MXSTDT=2,MXSTDI=2,MXSTDK=4)
      character*24 tgotstd(MXSTDT)
      common/stdze/ngstdi,ngstdk,ngstdt
     1     ,igotstd(MXSTDI),kgotstd(MXSTDK)
     1     ,tgotstd



      character*24 tgotstdstn,tgotstdchn
      parameter (MXSTNBLKS=500)
      parameter (MXCHNBLKS=10000)   ! was 40
      dimension iadstblock(MXSTNBLKS),iadchblock(MXCHNBLKS)
      dimension 
     1         igotstdstn(MXSTDI,MXSTNBLKS)
     1         ,kgotstdstn(MXSTDK,MXSTNBLKS)
     1         ,tgotstdstn(MXSTDT,MXSTNBLKS)
     1         ,igotstdchn(MXSTDI,MXCHNBLKS)
     1         ,kgotstdchn(MXSTDK,MXCHNBLKS)
     1         ,tgotstdchn(MXSTDT,MXCHNBLKS)

      parameter (MXSTNC=500)
      character*5 stncalls(MXSTNC)
      integer*4 netids(MXSTNC),iadchnc(MXSTNC+1)

      parameter(MXCHNC=20000)
      character*5 chnids(MXCHNC)
      integer*4 krates(MXCHNC)
      integer*2 lfmts(MXCHNC)
      integer*4 itim1052(2,MXCHNC),itim2052(2,MXCHNC)



      parameter (MXSEGS=10000)
      integer*4 itimseg(7,MXSEGS)
      integer*4 lfmtseg(MXSEGS),ichrseg(MXSEGS)


      common/seedcommun/ kstr,kabbr,kabbrmap,labbr
     1     ,lvolblock,idvolm,ifvnew
     1     ,nstblocks,nchblocks,iadstblock,iadchblock
     1     ,igotstdstn,kgotstdstn,igotstdchn,kgotstdchn
     1     ,itim1vol(2),itim2vol(2)
     1     ,netid,nstnc,netids,iadchnc,nchnc
     1     ,nmsegs,itimseg,ifmultf
     1     ,ichnrate,ichnlfmt,krates,ichrate
     1     ,itim1052,itim2052,lfmtseg,ichrseg

     1     ,lfmts,jchfmt

     1     ,astrings,strings
     1     ,volblock
     1     ,tmpblk,tmpblk1
     1     ,stncall
     1     ,chnname,chnlocid,chnsub
     1     ,tgotstdstn,tgotstdchn,stncalls
     1     ,chnids

