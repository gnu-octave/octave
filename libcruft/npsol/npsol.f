*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE NPSOL ( N, NCLIN, NCNLN, NROWA, NROWUJ, NROWR,
     $                   A, BL, BU,
     $                   CONFUN, OBJFUN,
     $                   INFORM, ITER, ISTATE,
     $                   C, UJAC, CLAMDA, OBJF, UGRAD, R, X,
     $                   IW, LENIW, W, LENW )

      IMPLICIT           DOUBLE PRECISION(A-H,O-Z)
      EXTERNAL           CONFUN, OBJFUN
      INTEGER            ISTATE(N+NCLIN+NCNLN)
      INTEGER            IW(LENIW)
      DOUBLE PRECISION   A(NROWA,*), BL(N+NCLIN+NCNLN),
     $                   BU(N+NCLIN+NCNLN)
      DOUBLE PRECISION   C(*), UJAC(NROWUJ,*), CLAMDA(N+NCLIN+NCNLN)
      DOUBLE PRECISION   UGRAD(N), R(NROWR,*), X(N)
      DOUBLE PRECISION   W(LENW)

*-----------------------------------------------------------------------
*
*  NPSOL   solves the nonlinear programming problem
*
*            minimize                   F(x)
*
*                                    (    x  )
*            subject to    bl  .le.  (  A*x  )  .le.  bu
*                                    (  c(x) )
*
*  where  F(x)  is a smooth scalar function,  A  is a constant matrix
*  and  c(x)  is a vector of smooth nonlinear functions.  The feasible
*  region is defined by a mixture of linear and nonlinear equality or
*  inequality constraints on  x.
*
*  The dimensions of the problem are...
*
*  N        the number of variables (dimension of  x),
*
*  NCLIN    the number of linear constraints (rows of the matrix  A),
*
*  NCNLN    the number of nonlinear constraints (dimension of  c(x)),
*
*
*  NPSOL   uses a sequential quadratic programming algorithm, with a
*  positive-definite quasi-Newton approximation to the transformed
*  Hessian  Q'HQ  of the Lagrangian function (which will be stored in
*  the array  R).
*
*
*  Complete documentation for  NPSOL  is contained in Report
*  SOL 86-2, Users guide for NPSOL (Version 4.0), by P.E. Gill,
*  W. Murray, M.A. Saunders and M.H. Wright, Department of Operations
*  Research,  Stanford University, Stanford, California 94305.
*
*  Systems Optimization Laboratory, Stanford University.
*  Version 1.1,  April     12, 1983. (The less said about this one.....)
*  Version 2.0,  April     30, 1984.
*  Version 3.0,  March     20, 1985. (First Fortran 77 version).
*  Version 3.2,  August    20, 1985.
*  Version 4.0,  April     16, 1986. (First version with differences).
*  Version 4.01, June      30, 1986. (Level 2 Blas + F77 linesearch).
*  Version 4.02, August     5, 1986. (Reset SSBFGS. One call to CHFD).
*
*  Copyright  1983  Stanford University.
*
*  This material may be reproduced by or for the U.S. Government pursu-
*  ant to the copyright license under DAR Clause 7-104.9(a) (1979 Mar).
*
*  This material is based upon work partially supported by the National
*  Science Foundation under Grants MCS-7926009 and ECS-8312142; the
*  Department of Energy Contract AM03-76SF00326, PA No. DE-AT03-
*  76ER72018; the Army Research Office Contract DAA29-84-K-0156;
*  and the Office of Naval Research Grant N00014-75-C-0267.
*  ---------------------------------------------------------------------

*  Common blocks.

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
*-----------------------------------------------------------------------
      INTEGER            IPRMNP(MXPARM), IPSVNP
      DOUBLE PRECISION   RPRMNP(MXPARM), RPSVNP

      COMMON    /NPPAR1/ IPSVNP(MXPARM),
     $                   IDBGNP, ITMXNP, JVRFY1, JVRFY2, JVRFY3, JVRFY4,
     $                   LDBGNP, LFORMH, LVLDER, LVERFY, MSGNP , NLNF  ,
     $                   NLNJ  , NLNX  , NNCNLN, IPADNP(15)

      COMMON    /NPPAR2/ RPSVNP(MXPARM),
     $                   CDINT , CTOL  , EPSRF , ETA   , FDINT , FTOL  ,
     $                   RPADNP(24)

      EQUIVALENCE       (IPRMNP(1), IDBGNP), (RPRMNP(1), CDINT)

      SAVE      /NPPAR1/, /NPPAR2/
*-----------------------------------------------------------------------
      EQUIVALENCE  (IDBGNP, IDBG  ), (ITMXNP, NMAJOR), (ITMAX2, NMINOR)
      EQUIVALENCE  (LDBGLS, MNRDBG), (LDBGNP, MJRDBG), (MSGLS , MSGQP )
      DOUBLE PRECISION   WMACH
      COMMON    /SOLMCH/ WMACH(15)
      SAVE      /SOLMCH/

      COMMON    /SOL1CM/ NOUT
      COMMON    /SOL3CM/ LENNAM, NROWT , NCOLT , NQ
      COMMON    /SOL4CM/ EPSPT3, EPSPT5, EPSPT8, EPSPT9
      COMMON    /SOL5CM/ ASIZE , DTMAX , DTMIN
      COMMON    /SOL6CM/ RCNDBD, RFROBN, DRMAX , DRMIN

      LOGICAL            UNITQ
      COMMON    /SOL1SV/ NACTIV, NFREE , NZ   , UNITQ
      SAVE      /SOL1SV/

      PARAMETER         (LENLS = 20)
      COMMON    /SOL1LS/ LOCLS(LENLS)

      PARAMETER         (LENNP = 35)
      COMMON    /SOL1NP/ LOCNP(LENNP)
      COMMON    /SOL4NP/ LVLDIF, NCDIFF, NFDIFF, LFDSET
      COMMON    /SOL5NP/ LVRFYC, JVERFY(4)
      LOGICAL            INCRUN
      COMMON    /SOL6NP/ RHOMAX, RHONRM, RHODMP, SCALE, INCRUN

      LOGICAL            CMDBG, LSDBG, NPDBG
      PARAMETER         (LDBG = 5)
      COMMON    /NPDEBG/ INPDBG(LDBG), NPDBG
      COMMON    /LSDEBG/ ILSDBG(LDBG), LSDBG
      COMMON    /CMDEBG/ ICMDBG(LDBG), CMDBG

      INTRINSIC          ABS   , MAX   , MIN   , MOD   , SQRT  , REAL

*     Local variables.

      EXTERNAL           DDIV  , DDOT  , DNORM , DNRM2
      CHARACTER*8        NAMES(1)
      LOGICAL            COLD  , LINOBJ, NAMED , OVERFL, ROWERR, VERTEX
      PARAMETER         (ZERO   =0.0D+0, POINT1 =0.1D+0, POINT3 =3.3D-1)
      PARAMETER         (POINT8 =0.8D+0, POINT9 =0.9D+0, ONE    =1.0D+0)
      PARAMETER         (GROWTH =1.0D+2                                )

      CHARACTER*40       TITLE
      DATA               TITLE
     $                 / 'SOL/NPSOL  ---  Version 4.02   Aug  1986' /

*     Set the machine-dependent constants.

      CALL MCHPAR()

      EPSMCH = WMACH( 3)
      RTEPS  = WMACH( 4)
      NOUT   = WMACH(11)

      EPSPT3 = EPSMCH**POINT3
      EPSPT5 = RTEPS
      EPSPT8 = EPSMCH**POINT8
      EPSPT9 = EPSMCH**POINT9

      RHOMAX = ONE/EPSMCH
      ROOTN  = SQRT(REAL(N))

*     Default names will be provided for variables during printing.

      NAMED  = .FALSE.
      INFORM = 0
      ITER   = 0

*     Set the default values for the parameters.

      CALL NPDFLT( N, NCLIN, NCNLN, LENIW, LENW, TITLE )

      COLD   = LCRASH .EQ. 0

      NPLIN  = N     + NCLIN
      NCTOTL = NPLIN + NCNLN

*     Assign the dimensions of arrays in the parameter list of NPCORE.
*     Economies of storage are possible if the minimum number of active
*     constraints and the minimum number of fixed variables are known in
*     advance.  The expert user should alter MINACT and MINFXD
*     accordingly.

      MINACT = 0
      MINFXD = 0

      MXFREE = N - MINFXD
      MAXACT = MAX( 1, MIN( N, NCLIN ) )
      MAXNZ  = N - ( MINFXD + MINACT )

      IF (NCLIN + NCNLN .EQ. 0) THEN
         NQ    = 1
         NROWT = 1
         NCOLT = 1
      ELSE
         NQ    = MAX( 1, MXFREE )
         NROWT = MAX( MAXNZ, MAXACT )
         NCOLT = MXFREE
      END IF

      LENNAM = 1

      NROWQP = MAX( NCLIN+NCNLN, 1 )
      IF (NCNLN .EQ. 0  .AND.  NCLIN .GT. 0) NROWQP = NROWA

*     NPLOC  defines the arrays that contain the locations of various
*     work arrays within  W  and  IW.

      LITOTL = 0
      LWTOTL = 0
      CALL NPLOC( N, NCLIN, NCNLN, NCTOTL, LITOTL, LWTOTL)

*     Allocate certain addresses that are not allocated in NPLOC.

      LAX    = LWTOTL + 1
      LWTOTL = LAX    + NCLIN - 1
      LAX    = MIN( LAX, LWTOTL )

*     Check input parameters and storage limits.

      CALL CMCHK ( NERROR, MSGNP, COLD, .FALSE.,
     $             LENIW, LENW, LITOTL, LWTOTL,
     $             N, NCLIN, NCNLN,
     $             ISTATE, IW, NAMED, NAMES, LENNAM,
     $             BL, BU, X )

      IF (NERROR .GT. 0) THEN
         INFORM = 9
         GO TO 800
      END IF

      LKACTV = LOCLS( 1)
      LANORM = LOCLS( 2)
      LCJDX  = LOCLS( 3)
      LRES   = LOCLS( 5)
      LRES0  = LOCLS( 6)
      LGQ    = LOCLS( 9)
      LT     = LOCLS(11)
      LZY    = LOCLS(12)
      LWTINF = LOCLS(13)
      LWRK1  = LOCLS(14)

      LKX    = LOCNP( 1)
      LIPERM = LOCNP( 2)
      LAQP   = LOCNP( 3)
      LDX    = LOCNP( 7)
      LFEATL = LOCNP(10)
      LWRK2  = LOCNP(12)

      LCMUL  = LOCNP(16)
      LWRK3  = LOCNP(21)
      LNEEDC = LOCNP(24)
      LHFRWD = LOCNP(25)
      LHCTRL = LOCNP(26)
      LCJAC  = LOCNP(27)
      LGRAD  = LOCNP(28)

      NROWJ  = MAX ( NCNLN, 1 )

      TOLRNK = ZERO
      RCNDBD = ONE/SQRT(EPSPT5)

      IF (TOLFEA .GT. ZERO)
     $   CALL DLOAD ( NPLIN, TOLFEA, W(LFEATL), 1 )

      IF (NCNLN .GT. 0  .AND.  CTOL .GT. ZERO)
     $   CALL DLOAD ( NCNLN, CTOL, W(LFEATL+NPLIN), 1 )

      IF (LFDSET .EQ. 0) THEN
         FDCHK = SQRT( EPSRF )
      ELSE IF (LFDSET .EQ. 1) THEN
         FDCHK = FDINT
      ELSE
         FDCHK = W(LHFRWD)
      END IF

      NFUN   = 0
      NGRAD  = 0
      NSTATE = 1

*     ------------------------------------------------------------------
*     If required,  compute the problem functions.
*     If the constraints are nonlinear,  the first call of CONFUN
*     sets up any constant elements in the Jacobian matrix.  A copy of
*     the Jacobian (with constant elements set) is placed in  UJAC.
*     ------------------------------------------------------------------
      IF (LVERFY .GE. 10) THEN
         XNORM  = DNRM2 ( N, X, 1 )
         LVRFYC = LVERFY - 10

         CALL NPCHKD( INFO, MSGNP, NSTATE, LVLDER, NFUN, NGRAD,
     $                NROWJ, NROWUJ, N, NCNLN,
     $                CONFUN, OBJFUN, IW(LNEEDC),
     $                BIGBND, EPSRF, CDINT, FDINT,
     $                FDCHK, FDNORM, OBJF, XNORM,
     $                BL, BU, C, W(LWRK3), W(LCJAC), UJAC, W(LCJDX),
     $                W(LDX), W(LGRAD), UGRAD, W(LHFRWD), W(LHCTRL),
     $                X, W(LWRK1), W(LWRK2), W, LENW )

         IF (INFO .NE. 0) THEN
            IF (INFO .GT. 0) INFORM = 7
            IF (INFO .LT. 0) INFORM = INFO
            GO TO 800
         END IF
         NSTATE = 0
      END IF

      IF (LCRASH .LT. 2) THEN
*        ===============================================================
*        Cold or warm start.  Use  LSCORE  to obtain a point that
*        satisfies the linear constraints.
*        ===============================================================
         CALL ICOPY ( LDBG, ILSDBG, 1, ICMDBG, 1 )

         IF (NCLIN .GT. 0) THEN
            IANRMJ = LANORM
            DO 110 J = 1, NCLIN
               W(IANRMJ) = DNRM2 ( N, A(J,1), NROWA )
               IANRMJ    = IANRMJ + 1
  110       CONTINUE
            CALL DCOND ( NCLIN, W(LANORM), 1, ASIZE, AMIN )
         END IF

         CALL DCOND ( NPLIN, W(LFEATL), 1, FEAMAX, FEAMIN )
         CALL DCOPY ( NPLIN, W(LFEATL), 1, W(LWTINF), 1 )
         CALL DSCAL ( NPLIN, (ONE/FEAMIN), W(LWTINF), 1 )

*        ===============================================================
*        The input values of X and (optionally)  ISTATE are used by
*        LSCRSH  to define an initial working set.
*        ===============================================================
         VERTEX = .FALSE.
         CALL LSCRSH( COLD, VERTEX,
     $                NCLIN, NPLIN, NACTIV, NARTIF,
     $                NFREE, N, NROWA,
     $                ISTATE, IW(LKACTV),
     $                BIGBND, TOLACT,
     $                A, W(LAX), BL, BU, X, W(LWRK1), W(LWRK2) )

         UNITQ  = .TRUE.
         NRES   = 0
         NGQ    = 0
         CONDMX = ONE / EPSPT5

         IKX    = LKX
         DO 120 I = 1, N
            IW(IKX) = I
            IKX     = IKX + 1
  120    CONTINUE

         IF (COLD) THEN
            NRANK  = 0
         ELSE
            NRANK  = NLNX
            CALL DLOAD ( NLNX, (ZERO), W(LRES0), 1 )
         END IF

*        ---------------------------------------------------------------
*        Re-order KX so that the free variables come first.
*        If a warm start is required, NRANK will be nonzero and the
*        factor R will be updated.
*        ---------------------------------------------------------------
         CALL LSBNDS( UNITQ,
     $                INFORM, NZ, NFREE, NRANK, NRES, NGQ,
     $                N, NQ, NROWA, NROWR, NROWT,
     $                ISTATE, IW(LKX),
     $                CONDMX,
     $                A, R, W(LT), W(LRES0), W(LGQ),
     $                W(LZY), W(LWRK1), W(LWRK2) )

*        ---------------------------------------------------------------
*        Factorize the initial working set.
*        ---------------------------------------------------------------
         IF (NACTIV .GT. 0) THEN
            NACT1  = NACTIV
            NACTIV = 0

            CALL LSADDS( UNITQ, VERTEX,
     $                   INFORM, 1, NACT1, NACTIV, NARTIF, NZ, NFREE,
     $                   NRANK, NREJTD, NRES, NGQ,
     $                   N, NQ, NROWA, NROWR, NROWT,
     $                   ISTATE, IW(LKACTV), IW(LKX),
     $                   CONDMX,
     $                   A, R, W(LT), W(LRES0), W(LGQ),
     $                   W(LZY), W(LWRK1), W(LWRK2) )
         END IF

         SSQ1 = ZERO

         LINOBJ = .FALSE.
         CALL LSSETX( LINOBJ, ROWERR, UNITQ,
     $                NCLIN, NACTIV, NFREE, NRANK, NZ,
     $                N, NPLIN, NQ, NROWA, NROWR, NROWT,
     $                ISTATE, IW(LKACTV), IW(LKX),
     $                JMAX, ERRMAX, CTX, XNORM,
     $                A, W(LAX), BL, BU, W(LGQ), W(LRES), W(LRES0),
     $                W(LFEATL), R, W(LT), X, W(LZY),W(LWRK1),W(LWRK2) )

*        ---------------------------------------------------------------
*        Call  LSCORE  to find a feasible  x.
*        ---------------------------------------------------------------
*        Use  WORK2  as the multiplier vector.

         JINF   = 0
         LCLAM  = LWRK2

         IDBGSV = IDBG
         IF (IDBG .GT. 0) THEN
            IDBG = NMINOR + 1
         END IF

         CALL LSCORE( 'FP problem', NAMED, NAMES, LINOBJ, UNITQ,
     $                NLPERR, ITER, JINF, NCLIN, NPLIN,
     $                NACTIV, NFREE, NRANK, NZ, NZ1,
     $                N, NROWA, NROWR,
     $                ISTATE, IW(LKACTV), IW(LKX),
     $                CTX, OBJ, SSQ1, SUMINF, NUMINF, XNORM,
     $                BL, BU, A, W(LCLAM), W(LAX),
     $                W(LFEATL), R, X, IW, W )

         IF (NLPERR .GT. 0) THEN
            INFORM = 2
            GO TO 800
         END IF
      END IF

      IDBG  = IDBGSV
      CALL ICOPY ( LDBG, INPDBG, 1, ICMDBG, 1 )

      LVRFYC = LVERFY
      IF (LVERFY .GE. 10) LVRFYC = -1

      CALL NPCHKD( INFO, MSGNP, NSTATE, LVLDER, NFUN, NGRAD,
     $             NROWJ, NROWUJ, N, NCNLN,
     $             CONFUN, OBJFUN, IW(LNEEDC),
     $             BIGBND, EPSRF, CDINT, FDINT,
     $             FDCHK, FDNORM, OBJF, XNORM,
     $             BL, BU, C, W(LWRK3), W(LCJAC), UJAC, W(LCJDX),
     $             W(LDX), W(LGRAD), UGRAD, W(LHFRWD), W(LHCTRL),
     $             X, W(LWRK1), W(LWRK2), W, LENW )

      IF (INFO .NE. 0) THEN
         IF (INFO .GT. 0) INFORM = 7
         IF (INFO .LT. 0) INFORM = INFO
         GO TO 800
      END IF

      CALL DCOPY ( N, W(LGRAD), 1, W(LGQ), 1 )
      CALL CMQMUL( 6, N, NZ, NFREE, NQ, UNITQ,
     $             IW(LKX), W(LGQ), W(LZY), W(LWRK1) )

      IF (COLD) THEN
*        ---------------------------------------------------------------
*        Cold start.  Initialize  R  as the identity matrix.
*        ---------------------------------------------------------------
         DO 210 J = 1, N
            CALL DLOAD ( N, ZERO, R(1,J), 1 )
  210    CONTINUE
         CALL DLOAD ( N, ONE, R, NROWR+1 )
         RFROBN = ROOTN

         IF (NCNLN .GT. 0) CALL DLOAD ( NCNLN, (ZERO), W(LCMUL), 1 )
      ELSE
*        ---------------------------------------------------------------
*        Warm start.
*        Set the multipliers for the nonlinear constraints.
*        Check the condition of the initial factor R.
*        ---------------------------------------------------------------
         IF (NCNLN .GT. 0)
     $      CALL DCOPY ( NCNLN, CLAMDA(NPLIN+1), 1, W(LCMUL), 1 )

         SCLE  = ZERO
         SUMSQ = ONE
         DO 220 J = 1, N
            CALL DSSQ  ( J, R(1,J), 1, SCLE, SUMSQ )
  220    CONTINUE
         RFROBN = DNORM( SCLE, SUMSQ )

         CALL DCOND ( N, R, NROWR+1, DRMAX, DRMIN )
         COND   = DDIV  ( DRMAX, DRMIN, OVERFL )

         IF (      COND   .GT. RCNDBD
     $       .OR.  RFROBN .GT. ROOTN*GROWTH*DRMAX) THEN
*           ------------------------------------------------------------
*           Refactorize the Hessian and bound the condition estimator.
*           ------------------------------------------------------------
            CALL NPRSET( UNITQ,
     $                   N, NFREE, NZ, NQ, NROWR,
     $                   IW(LIPERM), IW(LKX),
     $                   W(LGQ), R, W(LZY), W(LWRK1), W(LRES0) )
         END IF
      END IF

*     ==================================================================
*     Solve the problem.
*     ==================================================================
      IF (NCNLN .EQ. 0) THEN
*        ---------------------------------------------------------------
*        The problem has only linear constraints and bounds.
*        ---------------------------------------------------------------
         CALL NPCORE( NAMED, NAMES, UNITQ, INFORM, ITER,
     $                N, NCLIN, NCNLN, NCTOTL, NACTIV, NFREE, NZ,
     $                NROWA, NROWJ, NROWUJ, NROWQP, NROWR,
     $                NFUN, NGRAD, ISTATE, IW(LKACTV), IW(LKX),
     $                OBJF, FDNORM, XNORM, OBJFUN, CONFUN,
     $                A, W(LAX), BL, BU, C, W(LCJAC), UJAC, CLAMDA,
     $                W(LFEATL), W(LGRAD), UGRAD, R, X, IW, W, LENW )
      ELSE
*        ---------------------------------------------------------------
*        The problem has some nonlinear constraints.
*        ---------------------------------------------------------------
         IF (NCLIN .GT. 0) THEN
            LA1J = LAQP
            DO 520 J = 1, N
               CALL DCOPY ( NCLIN, A(1,J), 1, W(LA1J), 1 )
               LA1J = LA1J + NROWQP
  520       CONTINUE
         END IF

*        Try and add some nonlinear constraint indices to KACTIV.
*
         CALL NPCRSH( COLD, N, NCLIN, NCNLN,
     $                NCTOTL, NACTIV, NFREE, NZ,
     $                ISTATE, IW(LKACTV), BIGBND, TOLACT,
     $                BL, BU, C )

         CALL NPCORE( NAMED, NAMES, UNITQ, INFORM, ITER,
     $                N, NCLIN, NCNLN, NCTOTL, NACTIV, NFREE, NZ,
     $                NROWA, NROWJ, NROWUJ, NROWQP, NROWR,
     $                NFUN, NGRAD, ISTATE, IW(LKACTV),IW(LKX),
     $                OBJF, FDNORM, XNORM, OBJFUN, CONFUN,
     $                W(LAQP), W(LAX), BL, BU, C, W(LCJAC),UJAC,CLAMDA,
     $                W(LFEATL), W(LGRAD), UGRAD, R, X, IW, W, LENW )

      END IF

*     ------------------------------------------------------------------
*     If required, form the triangular factor of the Hessian.
*     ------------------------------------------------------------------
*     First,  form the square matrix  R  such that  H = R'R.
*     Compute the  QR  factorization of  R.

      IF (LFORMH .GT. 0) THEN
         LV     = LWRK2
         DO 400 J = 1, N
            IF (J .GT. 1)
     $         CALL DLOAD ( J-1, ZERO, W(LV), 1 )

            LVJ = LV + J - 1
            CALL DCOPY ( N-J+1, R(J,J), NROWR, W(LVJ), 1     )
            CALL CMQMUL( 3, N, NZ, NFREE, NQ, UNITQ,
     $                   IW(LKX), W(LV), W(LZY), W(LWRK1) )
            CALL DCOPY ( N    , W(LV) , 1    , R(J,1), NROWR )
  400    CONTINUE

         CALL DGEQR ( N, N, R, NROWR, W(LWRK1), INFO )
      END IF

*     Print messages if required.

  800 IF (MSGNP .GT.   0) THEN
         IF (INFORM .LT.   0) WRITE (NOUT, 3000)
         IF (INFORM .EQ.   0) WRITE (NOUT, 4000)
         IF (INFORM .EQ.   1) WRITE (NOUT, 4100)
         IF (INFORM .EQ.   2) WRITE (NOUT, 4200)
         IF (INFORM .EQ.   3) WRITE (NOUT, 4300)
         IF (INFORM .EQ.   4) WRITE (NOUT, 4400)
         IF (INFORM .EQ.   5) WRITE (NOUT, 4500)
         IF (INFORM .EQ.   6) WRITE (NOUT, 4600)
         IF (INFORM .EQ.   7) WRITE (NOUT, 4700)
         IF (INFORM .EQ.   9) WRITE (NOUT, 4900) NERROR

         IF (INFORM .GE. 0  .AND.  INFORM .NE. 9) THEN
            IF (NLPERR .EQ. 0) THEN
               WRITE (NOUT, 5000) OBJF
            ELSE
               IF (NLPERR .EQ. 3) THEN
                  WRITE (NOUT, 5010) SUMINF
               ELSE
                  WRITE (NOUT, 5020) SUMINF
               END IF
            END IF
         END IF
      END IF

*     Recover the optional parameters set by the user.

      CALL ICOPY ( MXPARM, IPSVLS, 1, IPRMLS, 1 )
      CALL DCOPY ( MXPARM, RPSVLS, 1, RPRMLS, 1 )
      CALL ICOPY ( MXPARM, IPSVNP, 1, IPRMNP, 1 )
      CALL DCOPY ( MXPARM, RPSVNP, 1, RPRMNP, 1 )

      RETURN

 3000 FORMAT(/ ' Exit NPSOL - User requested termination.'          )
 4000 FORMAT(/ ' Exit NPSOL - Optimal solution found.'              )
 4100 FORMAT(/ ' Exit NPSOL - Optimal solution found, ',
     $         ' but the requested accuracy could not be achieved.' )
 4200 FORMAT(/ ' Exit NPSOL - No feasible point for the linear',
     $         ' constraints.')
 4300 FORMAT(/ ' Exit NPSOL - No feasible point for the nonlinear',
     $         ' constraints.')
 4400 FORMAT(/ ' Exit NPSOL - Too many major iterations.             ')
 4500 FORMAT(/ ' Exit NPSOL - Problem is unbounded (or badly scaled).')
 4600 FORMAT(/ ' Exit NPSOL - Current point cannot be improved upon. ')
 4700 FORMAT(/ ' Exit NPSOL - Large errors found in the derivatives. ')

 4900 FORMAT(/ ' Exit NPSOL - ', I10, ' errors found in the input',
     $         ' parameters.  Problem abandoned.')
 5000 FORMAT(/ ' Final nonlinear objective value =', G16.7 )
 5010 FORMAT(/ ' Minimum sum of infeasibilities =',  G16.7 )
 5020 FORMAT(/ ' Final sum of infeasibilities =',    G16.7 )

*     End of  NPSOL .

      END
