*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE LSMOVE( HITCON, HITLOW, LINOBJ, UNITGZ,
     $                   NCLIN, NRANK, NZ1,
     $                   N, NROWR, JADD, NUMINF,
     $                   ALFA, CTP, CTX, XNORM,
     $                   AP, AX, BL, BU, GQ, HZ, P, RES,
     $                   R, X, WORK )

      IMPLICIT           DOUBLE PRECISION (A-H,O-Z)
      LOGICAL            HITCON, HITLOW, LINOBJ, UNITGZ
      DOUBLE PRECISION   AP(*), AX(*), BL(*), BU(*), GQ(*), HZ(*),
     $                   P(N), RES(*), R(NROWR,*), X(N)
      DOUBLE PRECISION   WORK(*)

************************************************************************
*     LSMOVE  changes X to X + ALFA*P and updates CTX, AX, RES and GQ
*     accordingly.
*
*     If a bound was added to the working set,  move X exactly on to it,
*     except when a negative step was taken (CMALF may have had to move
*     to some other closer constraint.)
*
*     Systems Optimization Laboratory, Stanford University.
*     Original version written 27-December-1985.
*     Level 2 BLAS added 11-June-1986.
*     This version of LSMOVE dated 11-June-1986.
************************************************************************
      COMMON    /SOL1CM/ NOUT

      LOGICAL            LSDBG
      PARAMETER         (LDBG = 5)
      COMMON    /LSDEBG/ ILSDBG(LDBG), LSDBG

      EXTERNAL           DDOT  , DNRM2
      INTRINSIC          ABS   , MIN
      PARAMETER        ( ZERO  = 0.0D+0, ONE = 1.0D+0 )

      CALL DAXPY ( N, ALFA, P, 1, X, 1 )
      IF (LINOBJ) CTX = CTX + ALFA*CTP

      IF (HITCON  .AND.  JADD .LE. N) THEN
         BND = BU(JADD)
         IF (HITLOW) BND = BL(JADD)
         IF (ALFA .GE. ZERO) X(JADD) = BND
      END IF
      XNORM  = DNRM2 ( N, X, 1 )

      IF (NCLIN .GT. 0)
     $   CALL DAXPY ( NCLIN, ALFA, AP, 1, AX, 1 )

      IF (NZ1 .LE. NRANK) THEN
         IF (UNITGZ) THEN
            RES(NZ1) = RES(NZ1) - ALFA*HZ(NZ1)
         ELSE
            CALL DAXPY ( NZ1, (-ALFA), HZ, 1, RES, 1  )
         END IF

         IF (NUMINF .EQ. 0) THEN

*           Update the transformed gradient GQ so that
*           GQ = GQ + ALFA*R'( HZ ).
*                            ( 0  )

            IF (UNITGZ) THEN
               CALL DAXPY ( N-NZ1+1, ALFA*HZ(NZ1), R(NZ1,NZ1), NROWR,
     $                                             GQ(NZ1)   , 1      )
            ELSE
               CALL DCOPY ( NZ1, HZ, 1, WORK, 1 )
               CALL DTRMV ( 'U', 'T', 'N', NZ1, R, NROWR, WORK, 1 )
               IF (NZ1 .LT. N)
     $            CALL DGEMV ( 'T', NZ1, N-NZ1, ONE, R(1,NZ1+1), NROWR,
     $                         HZ, 1, ZERO, WORK(NZ1+1), 1 )
               CALL DAXPY ( N, ALFA, WORK, 1, GQ, 1 )
            END IF
         END IF
      END IF

      RETURN

*     End of  LSMOVE.

      END
