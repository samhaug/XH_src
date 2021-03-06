c  record types
      integer STPV,STPA,STPS,STPT,STPD
      parameter (STPV=1,STPA=2,STPS=3,STPT=4,STPD=5, STPR=6)
c  local known format codes
      integer SFMSRO,SFMCDS,SFMDWW,SFMSTM,SFMST2,SFM32B
      integer SFMGR3,SFM3BT,SFMGR4,SFMCLG
      integer SFMMSR,SFMMDW,SFMMCD
      integer SFMGR3D,SFM3BTD,SFMGR4D,SFMUSN

      parameter (SFMSRO=1,SFMCDS=2,SFMDWW=3,SFMSTM=4,SFM32B=5)
      parameter (SFMGR3=6,SFM3BT=7,SFMGR4=8,SFMCLG=9)
      parameter (SFMMSR=10,SFMMDW=11,SFMMCD=12,SFMST2=14)
      parameter (SFMGR3D=15,SFM3BTD=16,SFMGR4D=17,SFMUSN=18)
      parameter (MXSFMS=18)
      integer mnsm(MXSFMS),nmsm(MXSFMS)
      data mnsm(SFMSRO),mnsm(SFMCDS),mnsm(SFMDWW),mnsm(SFMSTM),mnsm(SFMST2),mnsm(SFM32B)/2,2,2,1,1,4/
      data mnsm(SFMGR3),mnsm(SFM3BT),mnsm(SFMGR4)/2,3,2/
      data mnsm(SFMMSR),mnsm(SFMMDW),mnsm(SFMMCD)/2,2,2/
      data mnsm(SFMGR3D),mnsm(SFM3BTD),mnsm(SFMGR4D)/2,3,2/
      data mnsm(SFMUSN)/1/
      data nmsm(SFMSRO),nmsm(SFMCDS),nmsm(SFMDWW),nmsm(SFMSTM),nmsm(SFMST2),nmsm(SFM32B)/1,1,1,1,1,1/
      data nmsm(SFMGR3),nmsm(SFM3BT),nmsm(SFMGR4)/3,3,3/
      data nmsm(SFMGR3D),nmsm(SFM3BTD),nmsm(SFMGR4D)/1,1,1/
      data nmsm(SFMMSR),nmsm(SFMMDW),nmsm(SFMMCD)/3,3,3/
      data nmsm(SFMUSN)/1/
c  local known unit codes
      integer SUNCNT,SUNM,SUNMPS,SUNMPS2,SUNMS,SUNNMPS
      parameter (SUNCNT=1,SUNM=2,SUNMPS=3,SUNMPS2=4,SUNMS=5,SUNNMPS=16)
      character*5 utext(0:2)/'m    ','m/s  ','m/s/s'/
c  local known transfer function types
      integer STRFTA,STRFTD,STRFTB
      parameter (STRFTA=1,STRFTD=2,STRFTB=3)
      character*1 andi(3)/'A','D','B'/
c  response format -- poles and zeros or coefficients
      integer STRPZS,STRCOF
      parameter (STRPZS=1,STRCOF=2)
      character*2 pzco(2)/'PZ','CO'/
c data flag masks
      integer SPSLPS
      parameter (SPSLPS=00100000)
c dictionaries
      parameter (MXDICTS=10)
      integer DICTUT,DICTFT,DICTCT,DICTGC,DICTRE,DICTCD,DICTBM
      parameter(DICTUT=1,DICTFT=2,DICTCT=3)
      parameter (DICTGC=4,DICTRE=5,DICTCD=6,DICTBM=7)
      character*2 dicfls(MXDICTS)
      data dicfls(DICTUT)/'ud'/
      data dicfls(DICTFT)/'fd'/
      data dicfls(DICTCT)/'cd'/
      data dicfls(DICTGC)/'gd'/
      data dicfls(DICTRE)/'rd'/
      data dicfls(DICTCD)/'sd'/
      data dicfls(DICTBM)/'bd'/
c tape formats
      integer TPFSEED,TPFNDTF,TPFSROF,TPFIDAF,TPFHGLF,TPFGSPF
      parameter (TPFSEED=1,TPFNDTF=2,TPFSROF=3,TPFIDAF=4,TPFHGLF=5,TPFGSPF=6,MXTPFS=6)
      integer headln(MXTPFS)
      data headln(TPFSEED),headln(TPFNDTF),headln(TPFSROF)/48,20,20/
      data headln(TPFIDAF),headln(TPFHGLF),headln(TPFGSPF)/0,20,20/
c keys for USNS format
      integer usndf(0:15),usnnl(0:15)
      data usndf/4, 8, 12, 4, 8, 4, 8,  4,  8,  4,  4,  4,  4,  4,  4,  4/
      data usnnl/4, 4,  4, 6, 6, 8, 8, 10, 10, 12, 14, 16, 20, 24, 28, 32/
