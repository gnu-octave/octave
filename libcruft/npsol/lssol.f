*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE LSSOL ( MM, N,
     $                   NCLIN, NROWA, NROWR,
     $                   A, BL, BU, CVEC,
     $                   ISTATE, KX, X, R, B,
     $                   INFORM, ITER, OBJ, CLAMDA,
     $                   IW, LENIW, W, LENW )

      IMPLICIT           DOUBLE PRECISION(A-H,O-Z)
      INTEGER            LENIW, LENW
      INTEGER            ISTATE(N+NCLIN), KX(N)
      INTEGER            IW(LENIW)
      DOUBLE PRECISION   BL(N+NCLIN), BU(N+NCLIN), A(NROWA,*)
      DOUBLE PRECISION   CLAMDA(N+NCLIN), CVEC(*)
      DOUBLE PRECISION   R(NROWR,*), X(N), B(*)
      DOUBLE PRECISION   W(LENW)

************************************************************************
*  LSSOL  solves problems of the form
*
*           Minimize               F(x)
*              x
*                                 (  x )
*           subject to    bl  .le.(    ).ge.  bu,
*                                 ( Ax )
*
*  where  '  denotes the transpose of a column vector,  x  denotes the
*  n-vector of parameters and  F(x) is one of the following functions..
*
*  FP =  None                         (find a feasible point).
*  LP =  c'x
*  QP1=        1/2 x'Rx                R  n times n, symmetric pos. def.
*  QP2=  c'x + 1/2 x'Rx                .  .   ..        ..       ..  ..
*  QP3=        1/2 x'R'Rx              R  m times n, upper triangular.
*  QP4=  c'x + 1/2 x'R'Rx              .  .   ..  .   ..      ...
*  LS1=        1/2 (b - Rx)'(b - Rx)   R  m times n, rectangular.
*  LS2=  c'x + 1/2 (b - Rx)'(b - Rx)   .  .   ..  .     ...
*  LS3=        1/2 (b - Rx)'(b - Rx)   R  m times n, upper triangular.
*  LS4=  c'x + 1/2 (b - Rx)'(b - Rx)   .  .   ..  .   ..      ...
*
*  The matrix  R  is entered as the two-dimensional array  R  (of row
*  dimension  NROWR).  If  NROWR = 0,  R  is not accessed.
*
*  The vector  c  is entered in the one-dimensional array  CVEC.
*
*  NCLIN  is the number of general linear constraints (rows of  A).
*  (NCLIN may be zero.)
*
*  The first  N  components of  BL  and   BU  are lower and upper
*  bounds on the variables.  The next  NCLIN  components are
*  lower and upper bounds on the general linear constraints.
*
*  The matrix  A  of coefficients in the general linear constraints
*  is entered as the two-dimensional array  A  (of dimension
*  NROWA by N).  If NCLIN = 0, A is not accessed.
*
*  The vector  x  must contain an initial estimate of the solution,
*  and will contain the computed solution on output.
*
*
*  Complete documentation for  LSSOL  is contained in Report SOL 86-1,
*  Users Guide for LSSOL (Version 1.0), by P.E. Gill, S. J. Hammarling,
*  W. Murray, M.A. Saunders and M.H. Wright, Department of
*  Operations Research, Stanford University, Stanford, California 94305.
*
*  Systems Optimization Laboratory, Stanford University.
*  Version 1.01 Dated  30-June-1986.
*
*  Copyright  1984  Stanford University.
*
*  This material may be reproduced by or for the U.S. Government pursu-
*  ant to the copyright license under DAR clause 7-104.9(a) (1979 Mar).
*
*  This material is based upon work partially supported by the National
*  Science Foundation under Grants MCS-7926009 and ECS-8312142; the
*  Department of Energy Contract AM03-76SF00326, PA No. DE-AT03-
*  76ER72018; the Army Research Office Contract DAA29-84-K-0156;
*  and the Office of Naval Research Grant N00014-75-C-0267.
************************************************************************
      DOUBLE PRECISION   WMACH
      COMMON    /SOLMCH/ WMACH(15)
      SAVE      /SOLMCH/
      COMMON    /SOL1CM/ NOUT
      COMMON    /SOL3CM/ LENNAM, NROWT, NCOLT, NQ
      COMMON    /SOL4CM/ EPSPT3, EPSPT5, EPSPT8, EPSPT9
      COMMON    /SOL5CM/ ASIZE, DTMAX, DTMIN

      PARAMETER         (LENLS = 20)
      COMMON    /SOL1LS/ LOCLS(LENLS)

      LOGICAL            LSDBG
      PARAMETER         (LDBG = 5)
      COMMON    /LSDEBG/ ILSDBG(LDBG), LSDBG
*-----------------------------------------------------------------------
      PARAMETER         (MXPARM = 30)
      INTEGER            IPRMLS(MXPARM), IPSVLS
      DOUBLE PRECISION   RPRMLS(MXPARM), RPSVLS

      COMMON    /LSPAR1/ IPSVLS(MXPARM),
     $                   IDBGLS, ITMAX1, ITMAX2, LCRASH, LDBGLS, LPROB ,
     $                   MSGLS , NN    , NNCLIN, NPROB , IPADLS(20)

      COMMON    /LSPAR2/ RPSVLS(MXPARM),
     $                   BIGBND, BIGDX , BNDLOW, BNDUPP, TOLACT, TOLFEA,
     $                   TOLRNK, RPADLS(23)

      EQUIVALENCE       (IPRMLS(1), IDBGLS), (RPRMLS(1), BIGBND)

      SAVE      /LSPAR1/, /LSPAR2/
*-----------------------------------------------------------------------
      EQUIVALENCE   (MSGLS , MSGLVL), (IDBGLS, IDBG), (LDBGLS, MSGDBG)

      INTRINSIC          MAX, MIN

*     Local variables.

      LOGICAL            COLD  , FACTRZ, LINOBJ, NAMED , ROWERR,
     $                   UNITQ , VERTEX
      CHARACTER*2        PRBTYP
      CHARACTER*8        NAMES(1)
      PARAMETER        ( ZERO   =0.0D+0, POINT1 =0.1D+0, POINT3 =3.3D-1)
      PARAMETER        ( POINT8 =0.8D+0, POINT9 =0.9D+0, ONE    =1.0D+0)

      CHARACTER*40       TITLE
      DATA               TITLE
     $                 / 'SOL/LSSOL  ---  Version 1.01   June 1986' /

*     Set the machine-dependent constants.

      CALL MCHPAR()

      EPSMCH = WMACH( 3)
      RTEPS  = WMACH( 4)
      NOUT   = WMACH(11)

      EPSPT3 = EPSMCH**POINT3
      EPSPT5 = RTEPS
      EPSPT8 = EPSMCH**POINT8
      EPSPT9 = EPSMCH**POINT9

      NAMED  = .FALSE.

      INFORM = 0
      ITER   = 0

      CONDMX = ONE / EPSPT5

      NCTOTL = N + NCLIN

*     Set the default values of the parameters.

      CALL LSDFLT( MM, N, NCLIN, TITLE )

*     Set all parameters determined by the problem type.

      IF      (LPROB .EQ. 1 ) THEN
         PRBTYP    = 'FP'
         M      = 0
         LINOBJ = .FALSE.
         FACTRZ = .TRUE.
      ELSE IF (LPROB .EQ. 2 ) THEN
         PRBTYP    = 'LP'
         M      = 0
         LINOBJ = .TRUE.
         FACTRZ = .TRUE.
      ELSE IF (LPROB .EQ. 3 ) THEN
         PRBTYP    = 'QP'
         M      = MM
         LINOBJ = .FALSE.
         FACTRZ = .TRUE.
      ELSE IF (LPROB .EQ. 4 ) THEN
         PRBTYP    = 'QP'
         M      = MM
         LINOBJ = .TRUE.
         FACTRZ = .TRUE.
      ELSE IF (LPROB .EQ. 5 ) THEN
         PRBTYP    = 'QP'
         M      = MM
         LINOBJ = .FALSE.
         FACTRZ = .FALSE.
      ELSE IF (LPROB .EQ. 6 ) THEN
         PRBTYP    = 'QP'
         M      = MM
         LINOBJ = .TRUE.
         FACTRZ = .FALSE.
      ELSE IF (LPROB .EQ. 7 ) THEN
         PRBTYP    = 'LS'
         M      = MM
         LINOBJ = .FALSE.
         FACTRZ = .TRUE.
      ELSE IF (LPROB .EQ. 8 ) THEN
         PRBTYP    = 'LS'
         M      = MM
         LINOBJ = .TRUE.
         FACTRZ = .TRUE.
      ELSE IF (LPROB .EQ. 9 ) THEN
         PRBTYP    = 'LS'
         M      = MM
         LINOBJ = .FALSE.
         FACTRZ = .FALSE.
      ELSE IF (LPROB .EQ. 10) THEN
         PRBTYP    = 'LS'
         M      = MM
         LINOBJ = .TRUE.
         FACTRZ = .FALSE.
      END IF

*     Assign the dimensions of arrays in the parameter list of LSCORE.
*     Economies of storage are possible if the minimum number of active
*     constraints and the minimum number of fixed variables are known in
*     advance.  The expert user should alter MINACT and MINFXD
*     accordingly.
*     If a linear program is being solved and the matrix of general
*     constraints is fat,  i.e.,  NCLIN .LT. N,  a non-zero value is
*     known for MINFXD.  Note that in this case, VERTEX must be
*     set  .TRUE..

      MINACT = 0
      MINFXD = 0

      VERTEX = .FALSE.
      IF (      (PRBTYP .EQ. 'LP'  .OR.  PRBTYP .EQ. 'FP')
     $    .AND.  NCLIN  .LT. N   ) THEN
         MINFXD = N - NCLIN - 1
         VERTEX = .TRUE.
      END IF

      MXFREE = N - MINFXD
      MAXACT = MAX( 1, MIN( N, NCLIN ) )
      MAXNZ  = N - ( MINFXD + MINACT )

      IF (NCLIN .EQ. 0) THEN
         NQ     = 1
         NROWT  = 1
         NCOLT  = 1
         VERTEX = .FALSE.
      ELSE
         NQ     = MAX( 1, MXFREE )
         NROWT  = MAX( MAXNZ, MAXACT )
         NCOLT  = MXFREE
      END IF

      NCNLN  = 0
      LENNAM = 1

*     Allocate certain arrays that are not done in LSLOC.

      LITOTL = 0

      LAX    = 1
      LWTOTL = LAX + NCLIN  - 1

*     Allocate remaining work arrays.

      CALL LSLOC ( LPROB, N, NCLIN, LITOTL, LWTOTL )

      COLD  = LCRASH .EQ. 0

*     Check input parameters and storage limits.

      CALL CMCHK ( NERROR, MSGLVL, COLD, (.NOT.FACTRZ),
     $             LENIW, LENW, LITOTL, LWTOTL,
     $             N, NCLIN, NCNLN,
     $             ISTATE, KX, NAMED, NAMES, LENNAM,
     $             BL, BU, X )

      IF (NERROR .GT. 0) THEN
         INFORM = 6
         GO TO 800
      END IF

      LKACTV = LOCLS( 1)

      LANORM = LOCLS( 2)
      LPX    = LOCLS( 4)
      LRES   = LOCLS( 5)
      LRES0  = LOCLS( 6)
      LGQ    = LOCLS( 8)
      LCQ    = LOCLS( 9)
      LRLAM  = LOCLS(10)
      LT     = LOCLS(11)
      LZY    = LOCLS(12)
      LWTINF = LOCLS(13)
      LWRK   = LOCLS(14)
      LFEATL = LOCLS(15)

      IF (TOLFEA .GT. ZERO)
     $   CALL DLOAD ( N+NCLIN, (TOLFEA), W(LFEATL), 1 )

      IANRMJ = LANORM
      DO 200 J = 1, NCLIN
         W(IANRMJ) = DNRM2 ( N, A(J,1), NROWA )
         IANRMJ    = IANRMJ + 1
  200 CONTINUE
      IF (NCLIN .GT. 0)
     $   CALL DCOND ( NCLIN, W(LANORM), 1, ASIZE, AMIN )

      CALL DCOND ( NCTOTL, W(LFEATL), 1, FEAMAX, FEAMIN )
      CALL DCOPY ( NCTOTL, W(LFEATL), 1, W(LWTINF), 1 )
      CALL DSCAL ( NCTOTL, (ONE/FEAMIN), W(LWTINF), 1 )

      SSQ1   = ZERO

      IF (FACTRZ) THEN
*        ===============================================================
*        Factorize R using QR or Cholesky.  KX must be initialized.
*        ===============================================================
         DO 210 I = 1, N
            KX(I) = I
  210    CONTINUE

         IF      (PRBTYP .EQ. 'LP'  .OR.  PRBTYP .EQ. 'FP') THEN
            NRANK = 0
         ELSE IF (PRBTYP .EQ. 'QP') THEN
*           ------------------------------------------------------------
*           Compute the Cholesky factorization of R.  The Hessian is
*           M times M and resides in the upper left-hand corner of R.
*           ------------------------------------------------------------
            DO 220 J = M+1, N
               CALL DLOAD ( M, (ZERO), R(1,J), 1 )
  220       CONTINUE

            CALL LSCHOL( NROWR, M, NRANK, TOLRNK, KX, R, INFO )

            IF (NRANK .GT. 0)
     $         CALL DLOAD ( NRANK, (ZERO), W(LRES0), 1 )

         ELSE IF (PRBTYP .EQ. 'LS') THEN
*           ------------------------------------------------------------
*           Compute the orthogonal factorization PRQ = ( U ),  where P
*                                                      ( 0 )
*           is an orthogonal matrix and Q is a permutation matrix.
*           Overwrite R with the upper-triangle U.  The orthogonal
*           matrix P is applied to the residual and discarded.  The
*           permutation is stored in the array KX.  Once U has been
*           computed we need only work with vectors of length N within
*           LSCORE.  However, it is necessary to store the sum of
*           squares of the terms  B(NRANK+1),...,B(M),  where B = Pr.
*           ------------------------------------------------------------
            CALL DGEQRP( 'Column iterchanges', M, N, R, NROWR,
     $                   W(LWRK), IW(LKACTV), W(LGQ), INFO )

            LJ  = LKACTV
            DO 230 J = 1, N
               JMAX = IW(LJ)
               IF (JMAX .GT. J) THEN
                  JSAVE    = KX(JMAX)
                  KX(JMAX) = KX(J)
                  KX(J)    = JSAVE
               END IF
               LJ = LJ + 1
  230       CONTINUE

            CALL DGEAPQ( 'Transpose', 'Separate', M, N, R, NROWR,
     $                   W(LWRK), 1, B, M, W(LGQ), INFO )

            NRANK = IDRANK( MIN(N, M), R, NROWR+1, TOLRNK )

            IF (M .GT. NRANK) SSQ1 = DNRM2 ( M-NRANK, B(NRANK+1), 1 )

            IF (NRANK .GT. 0)
     $         CALL DCOPY ( NRANK, B, 1, W(LRES0), 1 )
         END IF
      ELSE
*        ===============================================================
*        R is input as an upper-triangular matrix with M rows.
*        ===============================================================
         NRANK = M
         IF (NRANK .GT. 0) THEN
            IF      (PRBTYP .EQ. 'QP') THEN
               CALL DLOAD ( NRANK, (ZERO), W(LRES0), 1 )
            ELSE IF (PRBTYP .EQ. 'LS') THEN
               CALL DCOPY ( NRANK, B, 1, W(LRES0), 1 )
            END IF
         END IF
      END IF

      IF (       MSGLVL .GT. 0     .AND.  NRANK  .LT. N
     $    .AND.  PRBTYP .NE. 'LP'  .AND.  PRBTYP .NE. 'FP')
     $   WRITE (NOUT, 9000) NRANK

*     ------------------------------------------------------------------
*     Find an initial working set.
*     ------------------------------------------------------------------
      CALL LSCRSH( COLD, VERTEX,
     $             NCLIN, NCTOTL, NACTIV, NARTIF,
     $             NFREE, N, NROWA,
     $             ISTATE, IW(LKACTV),
     $             BIGBND, TOLACT,
     $             A, W(LAX), BL, BU, X, W(LGQ), W(LWRK) )

*     ------------------------------------------------------------------
*     Compute the TQ factorization of the constraints while keeping R in
*     upper-triangular form.  Transformations associated with Q are
*     applied to CQ.  Transformations associated with P are applied to
*     RES0.  If some simple bounds are in the working set,  KX is
*     re-ordered so that the free variables come first.
*     ------------------------------------------------------------------
*     First, add the bounds. To save a bit of work, CQ is not loaded
*     until after KX has been re-ordered.

      NGQ   = 0
      NRES  = 0
      IF (NRANK .GT. 0) NRES = 1
      UNITQ = .TRUE.

      CALL LSBNDS( UNITQ,
     $             INFORM, NZ, NFREE, NRANK, NRES, NGQ,
     $             N, NQ, NROWA, NROWR, NROWT,
     $             ISTATE, KX,
     $             CONDMX,
     $             A, R, W(LT), W(LRES0), W(LCQ),
     $             W(LZY), W(LGQ), W(LWRK) )

      IF (LINOBJ) THEN

*        Install the transformed linear term in CQ.
*        CMQMUL applies the permutations in KX to CVEC.

         NGQ = 1
         CALL DCOPY ( N, CVEC, 1, W(LCQ), 1 )
         CALL CMQMUL( 6, N, NZ, NFREE, NQ, UNITQ,
     $                KX, W(LCQ), W(LZY), W(LWRK) )
      END IF

      IF (NACTIV .GT. 0) THEN
         NACT1  = NACTIV
         NACTIV = 0

         CALL LSADDS( UNITQ, VERTEX,
     $                INFORM, 1, NACT1, NACTIV, NARTIF, NZ, NFREE,
     $                NRANK, NREJTD, NRES, NGQ,
     $                N, NQ, NROWA, NROWR, NROWT,
     $                ISTATE, IW(LKACTV), KX,
     $                CONDMX,
     $                A, R, W(LT), W(LRES0), W(LCQ),
     $                W(LZY), W(LGQ), W(LWRK) )
      END IF

*     ------------------------------------------------------------------
*     Move the initial  x  onto the constraints in the working set.
*     Compute the transformed residual vector  Pr = Pb - RQ'x.
*     ------------------------------------------------------------------
      CALL LSSETX( LINOBJ, ROWERR, UNITQ,
     $             NCLIN, NACTIV, NFREE, NRANK, NZ,
     $             N, NCTOTL, NQ, NROWA, NROWR, NROWT,
     $             ISTATE, IW(LKACTV), KX,
     $             JMAX, ERRMAX, CTX, XNORM,
     $             A, W(LAX), BL, BU, W(LCQ), W(LRES), W(LRES0),
     $             W(LFEATL), R, W(LT), X, W(LZY), W(LPX), W(LWRK) )

      JINF = 0

      CALL LSCORE( PRBTYP, NAMED, NAMES, LINOBJ, UNITQ,
     $             INFORM, ITER, JINF, NCLIN, NCTOTL,
     $             NACTIV, NFREE, NRANK, NZ, NZ1,
     $             N, NROWA, NROWR,
     $             ISTATE, IW(LKACTV), KX,
     $             CTX, OBJ, SSQ1,
     $             SUMINF, NUMINF, XNORM,
     $             BL, BU, A, CLAMDA, W(LAX),
     $             W(LFEATL), R, X, IW, W )

      OBJ    = OBJ    + CTX
      IF (PRBTYP .EQ. 'LS'  .AND.  NRANK .GT. 0)
     $   CALL DCOPY ( NRANK, W(LRES), 1, B, 1 )

*     ==================================================================
*     Print messages if required.
*     ==================================================================
  800 IF (MSGLVL .GT.   0) THEN
         IF (INFORM .EQ.   0) THEN
            IF (PRBTYP .EQ. 'FP') THEN
               WRITE (NOUT, 2001)
            ELSE
               WRITE (NOUT, 2002) PRBTYP
            END IF
         END IF
         IF (INFORM .EQ.   1) WRITE (NOUT, 2010) PRBTYP
         IF (INFORM .EQ.   2) WRITE (NOUT, 2020) PRBTYP
         IF (INFORM .EQ.   3) WRITE (NOUT, 2030)
         IF (INFORM .EQ.   4) WRITE (NOUT, 2040)
         IF (INFORM .EQ.   5) WRITE (NOUT, 2050)
         IF (INFORM .EQ.   6) WRITE (NOUT, 2060) NERROR

         IF (INFORM .LT.   6) THEN
            IF      (NUMINF .EQ. 0) THEN
                IF (PRBTYP .NE. 'FP') WRITE (NOUT, 3000) PRBTYP, OBJ
            ELSE IF (INFORM .EQ. 3) THEN
               WRITE (NOUT, 3010) SUMINF
            ELSE
               WRITE (NOUT, 3020) SUMINF
            END IF
            IF (NUMINF .GT. 0) OBJ = SUMINF
         END IF
      END IF

*     Recover the optional parameters set by the user.

      CALL ICOPY ( MXPARM, IPSVLS, 1, IPRMLS, 1 )
      CALL DCOPY ( MXPARM, RPSVLS, 1, RPRMLS, 1 )

      RETURN

 2001 FORMAT(/ ' Exit LSSOL - Feasible point found.     ')
 2002 FORMAT(/ ' Exit LSSOL - Optimal ', A2, ' solution.')
 2010 FORMAT(/ ' Exit LSSOL - Weak ',    A2, ' solution.')
 2020 FORMAT(/ ' Exit LSSOL - ', A2,         ' solution is unbounded.' )
 2030 FORMAT(/ ' Exit LSSOL - Cannot satisfy the linear constraints. ' )
 2040 FORMAT(/ ' Exit LSSOL - Too many iterations.')
 2050 FORMAT(/ ' Exit LSSOL - Too many iterations without changing X.' )
 2060 FORMAT(/ ' Exit LSSOL - ', I10, ' errors found in the input',
     $         ' parameters.  Problem abandoned.'         )
 3000 FORMAT(/ ' Final ', A2, ' objective value =', G16.7 )
 3010 FORMAT(/ ' Minimum sum of infeasibilities =', G16.7 )
 3020 FORMAT(/ ' Final sum of infeasibilities =',   G16.7 )

 9000 FORMAT(/ ' Rank of the objective function data matrix = ', I5 )

*     End of  LSSOL .

      END
