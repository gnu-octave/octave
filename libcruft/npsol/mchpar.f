*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     File  MCSUBS FORTRAN
*
*     MCHPAR   MCEPS    MCENV1   MCENV2   MCSTOR   MCSMAL   MCMIN
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE MCHPAR()

************************************************************************
*  MCHPAR  must define certain machine parameters as follows:
*     wmach(1)  = NBASE  = base of floating-point arithmetic.
*     wmach(2)  = NDIGIT = no. of base wmach(1) digits of precision.
*     wmach(3)  = EPS    = floating-point precision.
*     wmach(4)  = RTEPS  = sqrt(EPS).
*     wmach(5)  = RMIN   = smallest positive normalized floating-point
*                          number.
*     wmach(6)  = RTRMIN = sqrt(RMIN).
*     wmach(7)  = RMAX   = largest positive floating-point number.
*     wmach(8)  = RTRMAX = sqrt(RMAX).
*     wmach(9)  = UNDFLW = 0 if underflow is not fatal, +ve otherwise.
*     wmach(10) = NIN    = standard file number of the input stream.
*     wmach(11) = NOUT   = standard file number of the output stream.
************************************************************************

      DOUBLE PRECISION   WMACH
      COMMON    /SOLMCH/ WMACH(15)
      SAVE      /SOLMCH/

      EXTERNAL           MCENV2, MCEPS , MCSMAL, D1MACH, I1MACH
      INTRINSIC          SQRT
      LOGICAL            FIRST , HDWIRE
      INTEGER            EMIN  , NBASE , NDIGIT, NIN   , NOUT  , I1MACH
      DOUBLE PRECISION   BASE  , EPS   , MCEPS , MCSMAL, RMAX  , RMIN
      DOUBLE PRECISION   RTEPS , RTMAX , RTMIN , SMALL , UNDFLW, D1MACH
      SAVE               FIRST
      DATA               FIRST / .TRUE. /

      IF (FIRST) THEN
         FIRST = .FALSE.

*        ---------------------------------------------------------------
*        Machine-dependent code.
*        1. Set UNDFLW, NIN, NOUT, HDWIRE as desired.
*        2. If  HDWIRE = .TRUE.  set the machine constants
*               NBASE, NDIGIT, EPS, RMIN, RMAX
*           in-line.  Otherwise, they will be computed by MCENV2.
*           A call to MCENV2 will cause eight underflows.
*        ---------------------------------------------------------------

         UNDFLW = 0
         NIN    = I1MACH(1)
         NOUT   = I1MACH(2)
         HDWIRE = .TRUE.

         IF (HDWIRE) THEN
            NBASE  = I1MACH(10)
            NDIGIT = I1MACH(14)
            BASE   = NBASE
            EPS    = D1MACH(4)
            RMIN   = D1MACH(1)
            RMAX   = D1MACH(2)
         ELSE
            CALL MCENV2( NBASE, NDIGIT, EPS, EMIN, RMIN )

            EPS    = MCEPS ()
            SMALL  = MCSMAL()
            RMAX   = 1/SMALL
         END IF

         WMACH(1)  = NBASE
         WMACH(2)  = NDIGIT
         WMACH(3)  = EPS
         WMACH(4)  = SQRT( EPS )
         WMACH(5)  = RMIN
         WMACH(6)  = SQRT( RMIN )
         WMACH(7)  = RMAX
         WMACH(8)  = SQRT( RMAX )
         WMACH(9)  = UNDFLW
         WMACH(10) = NIN
         WMACH(11) = NOUT
      END IF

      RETURN

*     End of  MCHPAR.

      END
