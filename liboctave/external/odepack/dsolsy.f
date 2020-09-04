      SUBROUTINE DSOLSY (WM, IWM, X, TEM)
C***BEGIN PROLOGUE  DSOLSY
C***SUBSIDIARY
C***PURPOSE  ODEPACK linear system solver.
C***TYPE      DOUBLE PRECISION (SSOLSY-S, DSOLSY-D)
C***AUTHOR  Hindmarsh, Alan C., (LLNL)
C***DESCRIPTION
C
C  This routine manages the solution of the linear system arising from
C  a chord iteration.  It is called if MITER .ne. 0.
C  If MITER is 1 or 2, it calls DGETRS to accomplish this.
C  If MITER = 3 it updates the coefficient h*EL0 in the diagonal
C  matrix, and then computes the solution.
C  If MITER is 4 or 5, it calls DGBTRS.
C  Communication with DSOLSY uses the following variables:
C  WM    = real work space containing the inverse diagonal matrix if
C          MITER = 3 and the LU decomposition of the matrix otherwise.
C          Storage of matrix elements starts at WM(3).
C          WM also contains the following matrix-related data:
C          WM(1) = SQRT(UROUND) (not used here),
C          WM(2) = HL0, the previous value of h*EL0, used if MITER = 3.
C  IWM   = integer work space containing pivot information, starting at
C          IWM(21), if MITER is 1, 2, 4, or 5.  IWM also contains band
C          parameters ML = IWM(1) and MU = IWM(2) if MITER is 4 or 5.
C  X     = the right-hand side vector on input, and the solution vector
C          on output, of length N.
C  TEM   = vector of work space of length N, not used in this version.
C  IERSL = output flag (in COMMON).  IERSL = 0 if no trouble occurred.
C          IERSL = 1 if a singular matrix arose with MITER = 3.
C  This routine also uses the COMMON variables EL0, H, MITER, and N.
C
C***SEE ALSO  DLSODE
C***ROUTINES CALLED  DGBTRS, DGETRS
C***COMMON BLOCKS    DLS001
C***REVISION HISTORY  (YYMMDD)
C   791129  DATE WRITTEN
C   890501  Modified prologue to SLATEC/LDOC format.  (FNF)
C   890503  Minor cosmetic changes.  (FNF)
C   930809  Renamed to allow single/double precision versions. (ACH)
C   010418  Reduced size of Common block /DLS001/. (ACH)
C   031105  Restored 'own' variables to Common block /DLS001/, to
C           enable interrupt/restart feature. (ACH)
C***END PROLOGUE  DSOLSY
C**End
      INTEGER IWM
      DOUBLE PRECISION WM, X, TEM
      DIMENSION WM(*), IWM(*), X(*), TEM(*)
      INTEGER ILLIN, INIT, LYH, LEWT, LACOR, LSAVF, LWM, LIWM,
     1   MXSTEP, MXHNIL, NHNIL, NTREP, NSLAST, NYH,
     2   IALTH, IPUP, LMAX, MEO, NQNYH, NSLP
      INTEGER ICF, IERPJ, IERSL, JCUR, JSTART, KFLAG, L, METH, MITER,
     2   MAXORD, MAXCOR, MSBP, MXNCF, N, NQ, NST, NFE, NJE, NQU
      DOUBLE PRECISION CONIT, CRATE, EL, ELCO, HOLD, RMAX, TESCO,
     2   CCMAX, EL0, H, HMIN, HMXI, HU, RC, TN, UROUND
      COMMON /DLS001/ CONIT, CRATE, EL(13), ELCO(13,12),
     1   HOLD, RMAX, TESCO(3,12),
     2   CCMAX, EL0, H, HMIN, HMXI, HU, RC, TN, UROUND,
     2   ILLIN, INIT, LYH, LEWT, LACOR, LSAVF, LWM, LIWM,
     3   MXSTEP, MXHNIL, NHNIL, NTREP, NSLAST, NYH,
     3   IALTH, IPUP, LMAX, MEO, NQNYH, NSLP,
     4   ICF, IERPJ, IERSL, JCUR, JSTART, KFLAG, L, METH, MITER,
     5   MAXORD, MAXCOR, MSBP, MXNCF, N, NQ, NST, NFE, NJE, NQU
      INTEGER I, MEBAND, ML, MU
      INTEGER INLPCK
      DOUBLE PRECISION DI, HL0, PHL0, R
C
C***FIRST EXECUTABLE STATEMENT  DSOLSY
      IERSL = 0
      GO TO (100, 100, 300, 400, 400), MITER
 100  CALL DGETRS ( 'N', N, 1, WM(3), N, IWM(21), X, N, INLPCK)
      RETURN
C
 300  PHL0 = WM(2)
      HL0 = H*EL0
      WM(2) = HL0
      IF (HL0 .EQ. PHL0) GO TO 330
      R = HL0/PHL0
      DO 320 I = 1,N
        DI = 1.0D0 - R*(1.0D0 - 1.0D0/WM(I+2))
        IF (ABS(DI) .EQ. 0.0D0) GO TO 390
 320    WM(I+2) = 1.0D0/DI
 330  DO 340 I = 1,N
 340    X(I) = WM(I+2)*X(I)
      RETURN
 390  IERSL = 1
      RETURN
C
 400  ML = IWM(1)
      MU = IWM(2)
      MEBAND = 2*ML + MU + 1
      CALL DGBTRS ( 'N', N, ML, MU, 1, WM(3), MEBAND, IWM(21), X, N,
     * INLPCK)
      RETURN
C----------------------- END OF SUBROUTINE DSOLSY ----------------------
      END
