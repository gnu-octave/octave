      SUBROUTINE setgmn(meanv,covm,p,parm)
C**********************************************************************
C
C     SUBROUTINE SETGMN( MEANV, COVM, P, PARM)
C            SET Generate Multivariate Normal random deviate
C
C
C                              Function
C
C
C      Places P, MEANV, and the Cholesky factoriztion of COVM
C      in GENMN.
C
C
C                              Arguments
C
C
C     MEANV --> Mean vector of multivariate normal distribution.
C                                        REAL MEANV(P)
C
C     COVM   <--> (Input) Covariance   matrix    of  the  multivariate
C                 normal distribution
C                 (Output) Destroyed on output
C                                        REAL COVM(P,P)
C
C     P     --> Dimension of the normal, or length of MEANV.
C                                        INTEGER P
C
C     PARM <-- Array of parameters needed to generate multivariate norma
C                deviates (P, MEANV and Cholesky decomposition of
C                COVM).
C                1 : 1                - P
C                2 : P + 1            - MEANV
C                P+2 : P*(P+3)/2 + 1  - Cholesky decomposition of COVM
C                                             REAL PARM(P*(P+3)/2 + 1)
C
C**********************************************************************
C     .. Scalar Arguments ..
      INTEGER p
C     ..
C     .. Array Arguments ..
      REAL covm(p,p),meanv(p),parm(p* (p+3)/2+1)
C     ..
C     .. Local Scalars ..
      INTEGER i,icount,info,j
C     ..
C     .. External Subroutines ..
      EXTERNAL spofa
C     ..
C     .. Executable Statements ..
C
C
C     TEST THE INPUT
C
      IF (.NOT. (p.LE.0)) GO TO 10
      WRITE (*,*) 'P nonpositive in SETGMN'
      WRITE (*,*) 'Value of P: ',p
      CALL XSTOPX ('P nonpositive in SETGMN')

   10 parm(1) = p
C
C     PUT P AND MEANV INTO PARM
C
      DO 20,i = 2,p + 1
          parm(i) = meanv(i-1)
   20 CONTINUE
C
C      Cholesky decomposition to find A s.t. trans(A)*(A) = COVM
C
      CALL spofa(covm,p,p,info)
      IF (.NOT. (info.NE.0)) GO TO 30
      WRITE (*,*) ' COVM not positive definite in SETGMN'
      CALL XSTOPX (' COVM not positive definite in SETGMN')

   30 icount = p + 1
C
C     PUT UPPER HALF OF A, WHICH IS NOW THE CHOLESKY FACTOR, INTO PARM
C          COVM(1,1) = PARM(P+2)
C          COVM(1,2) = PARM(P+3)
C                    :
C          COVM(1,P) = PARM(2P+1)
C          COVM(2,2) = PARM(2P+2)  ...
C
      DO 50,i = 1,p
          DO 40,j = i,p
              icount = icount + 1
              parm(icount) = covm(i,j)
   40     CONTINUE
   50 CONTINUE
      RETURN
C
      END
