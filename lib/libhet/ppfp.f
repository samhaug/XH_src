
C$**********************************************************************
      SUBROUTINE PPFP(LU,KIN,ISHFT,U)
      save
C
C     SUBROUTINE FOR PRINTER PLOTTING NODAL PLANE
C     BEACH BALLS
C
C     LU      LOGICAL UNIT
C     KIN     WIDTH AND HEIGHT OF PLOT IN INCHES
C     ISHFT   NUMBER OF CHARACTERS FROM LEFT MARGIN
C             TO START PLOT
C     U(3,3)  MATRIX WHOSE COLUMNS ARE THE EIGENVECTORS
C             OF THE MOMENT TENSOR (T,N,P) AS RETURNED
C             BY SUBROUTINE EQPAR
C
C
      DIMENSION U(3,3)
      CHARACTER*1 WORK(133)
      CHARACTER*16 FORM
C
      WRITE(LU,11)
   11 FORMAT(//)
      RT2=SQRT(2.)
      IX=1+ISHFT
      WRITE(FORM,1)  IX
    1 FORMAT(1H(,I3,8HX,132A1))
C
C
      NCOL=10*KIN
      NROW=6*KIN
      RK5=FLOAT(5*KIN)
      RK3=FLOAT(3*KIN)
C
      DO 20 IP=1,3,2
      UU=U(1,IP)
      FAC=0.
      DEN=1.-UU*UU
      IF(DEN.NE.0.) FAC=SQRT(2.*(1.+UU)/DEN)
      X=U(3,IP)*FAC
      Y=U(2,IP)*FAC
      ICOL=RK5*(X/RT2+1.)+1.
      IROW=RK3*(Y/RT2+1.)+1.
      IROW=MAX0(1,MIN0(IROW,NROW))
      ICOL=MAX0(1,MIN0(ICOL,NCOL))
      IF(IP.EQ.3) ICOLP=ICOL
      IF(IP.EQ.3) IROWP=IROW
      IF(IP.EQ.1) ICOLT=ICOL
      IF(IP.EQ.1) IROWT=IROW
   20 CONTINUE
C
C
      DO 10 IROW=1,NROW
      Y=RT2*((FLOAT(IROW)-.5)/RK3-1.)
      DO 30 ICOL=1,NCOL
      X=RT2*((FLOAT(ICOL)-.5)/RK5-1.)
      R2=X*X+Y*Y
      R=SQRT(R2)
      VR=-1.+.5*R2
      IF(VR.LE.0.) GOTO 40
      WORK(ICOL)=' '
      GOTO 50
   40 ST=SQRT(1.-VR*VR)
      VT=ST*Y/R
      VP=ST*X/R
      TESTT=ABS(U(1,1)*VR+U(2,1)*VT+U(3,1)*VP)
      TESTP=ABS(U(1,3)*VR+U(2,3)*VT+U(3,3)*VP)
      IF(TESTP.GT.TESTT) WORK(ICOL)='-'
      IF(TESTT.GE.TESTP) WORK(ICOL)='#'
C
   50 IF((IABS(ICOL-ICOLP).LE.1.AND.IABS(IROW-IROWP).LE.1)
     1 .OR.(IABS(ICOL-ICOLT).LE.1.AND.IABS(IROW-IROWT).LE.1))
     2 WORK(ICOL)=' '
      IF(ICOL.EQ.ICOLP.AND.IROW.EQ.IROWP) WORK(ICOL)='P'
      IF(ICOL.EQ.ICOLT.AND.IROW.EQ.IROWT) WORK(ICOL)='T'
   30 CONTINUE
C
C
      WRITE(LU,FORM)(WORK(ICOL),ICOL=1,NCOL)
   10 CONTINUE
      WRITE(LU,11)
      RETURN
      END
