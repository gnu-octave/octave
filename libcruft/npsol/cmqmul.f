*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE CMQMUL( MODE, N, NZ, NFREE, NQ, UNITQ,
     $                   KX, V, ZY, WRK )

      IMPLICIT           DOUBLE PRECISION(A-H,O-Z)
      LOGICAL            UNITQ
      INTEGER            KX(N)
      DOUBLE PRECISION   V(N), ZY(NQ,*), WRK(N)

************************************************************************
*     CMQMUL  transforms the vector  v  in various ways using the
*     matrix  Q = ( Z  Y )  defined by the input parameters.
*
*        MODE               result
*        ----               ------
*
*          1                v = Z v
*          2                v = Y v
*          3                v = Q v
*
*     On input,  v  is assumed to be ordered as  ( v(free)  v(fixed) ).
*     on output, v  is a full n-vector.
*
*
*          4                v = Z'v
*          5                v = Y'v
*          6                v = Q'v
*
*     On input,  v  is a full n-vector.
*     On output, v  is ordered as  ( v(free)  v(fixed) ).
*
*          7                v = Y'v
*          8                v = Q'v
*
*     On input,  v  is a full n-vector.
*     On output, v  is as in modes 5 and 6 except that v(fixed) is not
*     set.
*
*     Modes  1, 4, 7 and 8  do not involve  v(fixed).
*     Original F66 version  April 1983.
*     Fortran 77 version written  9-February-1985.
*     Level 2 BLAS added 10-June-1986.
*     This version of CMQMUL dated 10-June-1986.
************************************************************************
      EXTERNAL           DDOT
      PARAMETER        ( ZERO = 0.0D+0, ONE = 1.0D+0 )

      NFIXED = N - NFREE
      J1     = 1
      J2     = NFREE
      IF (MODE .EQ. 1  .OR.  MODE .EQ. 4) J2 = NZ
      IF (MODE .EQ. 2  .OR.  MODE .EQ. 5  .OR.  MODE .EQ. 7) J1 = NZ + 1
      LENV   = J2 - J1 + 1
      IF (MODE .LE. 3) THEN
*        ===============================================================
*        Mode = 1, 2  or  3.
*        ===============================================================

         IF (NFREE .GT. 0) CALL DLOAD ( NFREE, ZERO, WRK, 1 )

*        Copy  v(fixed)  into the end of  wrk.

         IF (MODE .GE. 2  .AND.  NFIXED .GT. 0)
     $      CALL DCOPY ( NFIXED, V(NFREE+1), 1, WRK(NFREE+1), 1 )

*        Set  WRK  =  relevant part of  ZY * V.

         IF (LENV .GT. 0)  THEN
            IF (UNITQ) THEN
               CALL DCOPY ( LENV, V(J1), 1, WRK(J1), 1 )
            ELSE
               CALL DGEMV ( 'N', NFREE, J2-J1+1, ONE, ZY(1,J1), NQ,
     $                      V(J1), 1, ONE, WRK, 1 )
            END IF
         END IF

*        Expand  WRK  into  V  as a full n-vector.

         CALL DLOAD ( N, ZERO, V, 1 )
         DO 220 K = 1, NFREE
            J    = KX(K)
            V(J) = WRK(K)
  220    CONTINUE

*        Copy  WRK(fixed)  into the appropriate parts of  V.

         IF (MODE .GT. 1)  THEN
            DO 320 L = 1, NFIXED
               J       = KX(NFREE+L)
               V(J)    = WRK(NFREE+L)
  320       CONTINUE
         END IF

      ELSE
*        ===============================================================
*        Mode = 4, 5, 6, 7  or  8.
*        ===============================================================
*        Put the fixed components of  V  into the end of  WRK.

         IF (MODE .EQ. 5  .OR.  MODE .EQ. 6)  THEN
            DO 420 L = 1, NFIXED
               J            = KX(NFREE+L)
               WRK(NFREE+L) = V(J)
  420       CONTINUE
         END IF

*        Put the free  components of  V  into the beginning of  WRK.

         IF (NFREE .GT. 0)  THEN
            DO 520 K = 1, NFREE
               J      = KX(K)
               WRK(K) = V(J)
  520       CONTINUE

*           Set  V  =  relevant part of  ZY' * WRK.

            IF (LENV .GT. 0)  THEN
               IF (UNITQ) THEN
                  CALL DCOPY ( LENV, WRK(J1), 1, V(J1), 1 )
               ELSE
                  CALL DGEMV ( 'T', NFREE, J2-J1+1, ONE, ZY(1,J1), NQ,
     $                         WRK, 1, ZERO, V(J1), 1 )
               END IF
            END IF
         END IF

*        Copy the fixed components of  WRK  into the end of  V.

         IF (NFIXED .GT. 0  .AND.  (MODE .EQ. 5  .OR.  MODE .EQ. 6))
     $      CALL DCOPY ( NFIXED, WRK(NFREE+1), 1, V(NFREE+1), 1 )
      END IF

      RETURN

*     End of  CMQMUL.

      END
