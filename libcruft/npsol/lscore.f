*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE LSCORE( PRBTYP, NAMED, NAMES, LINOBJ, UNITQ,
     $                   INFORM, ITER, JINF, NCLIN, NCTOTL,
     $                   NACTIV, NFREE, NRANK, NZ, NZ1,
     $                   N, NROWA, NROWR,
     $                   ISTATE, KACTIV, KX,
     $                   CTX, SSQ, SSQ1, SUMINF, NUMINF, XNORM,
     $                   BL, BU, A, CLAMDA, AX,
     $                   FEATOL, R, X, IW, W )

      IMPLICIT           DOUBLE PRECISION(A-H,O-Z)
      CHARACTER*2        PRBTYP
      CHARACTER*8        NAMES(*)
      INTEGER            ISTATE(NCTOTL), KACTIV(N), KX(N)
      INTEGER            IW(*)
      DOUBLE PRECISION   BL(NCTOTL), BU(NCTOTL), A(NROWA,*),
     $                   CLAMDA(NCTOTL), AX(*),
     $                   FEATOL(NCTOTL), R(NROWR,*), X(N)
      DOUBLE PRECISION   W(*)
      LOGICAL            NAMED, LINOBJ, UNITQ

************************************************************************
*     LSCORE  is a subroutine for linearly constrained linear-least
*     squares.  On entry, it is assumed that an initial working set of
*     linear constraints and bounds is available.
*     The arrays ISTATE, KACTIV and KX will have been set accordingly
*     and the arrays T and ZY will contain the TQ factorization of
*     the matrix whose rows are the gradients of the active linear
*     constraints with the columns corresponding to the active bounds
*     removed.  the TQ factorization of the resulting (NACTIV by NFREE)
*     matrix is  A(free)*Q = (0 T),  where Q is (NFREE by NFREE) and T
*     is reverse-triangular.
*
*     Values of ISTATE(J) for the linear constraints.......
*
*     ISTATE(J)
*     ---------
*          0    constraint J is not in the working set.
*          1    constraint J is in the working set at its lower bound.
*          2    constraint J is in the working set at its upper bound.
*          3    constraint J is in the working set as an equality.
*
*     Constraint J may be violated by as much as FEATOL(J).
*
*     Systems Optimization Laboratory, Stanford University.
*     This version of  LSCORE  dated  1-August-1986.
*
*     Copyright  1984  Stanford University.
*
*  This material may be reproduced by or for the U.S. Government pursu-
*  ant to the copyright license under DAR clause 7-104.9(a) (1979 Mar).
*
*  This material is based upon work partially supported by the National
*  Science Foundation under grants MCS-7926009 and ECS-8012974; the
*  Department of Energy Contract AM03-76SF00326, PA No. DE-AT03-
*  76ER72018; and the Army Research Office Contract DAA29-79-C-0110.
************************************************************************
      DOUBLE PRECISION   WMACH
      COMMON    /SOLMCH/ WMACH(15)
      SAVE      /SOLMCH/
      COMMON    /SOL1CM/ NOUT
      COMMON    /SOL3CM/ LENNAM, NROWT, NCOLT, NQ
      COMMON    /SOL4CM/ EPSPT3, EPSPT5, EPSPT8, EPSPT9
      COMMON    /SOL5CM/ ASIZE, DTMAX, DTMIN

      INTEGER            LOCLS
      PARAMETER         (LENLS = 20)
      COMMON    /SOL1LS/ LOCLS(LENLS)

      LOGICAL            CMDBG, LSDBG
      PARAMETER         (LDBG = 5)
      COMMON    /LSDEBG/ ILSDBG(LDBG), LSDBG
      COMMON    /CMDEBG/ ICMDBG(LDBG), CMDBG
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

      EXTERNAL           DDIV  , DDOT  , DNRM2
      INTRINSIC          ABS   , MAX   , SQRT
      LOGICAL            CONVRG, CYCLIN, ERROR , FIRSTV, HITCON,
     $                   HITLOW, NEEDFG, OVERFL, PRNT  , PRNT1 , ROWERR
      LOGICAL            SINGLR, STALL , STATPT, UNBNDD, UNCON , UNITGZ,
     $                   WEAK
      PARAMETER        ( ZERO   =0.0D+0, HALF   =0.5D+0, ONE   =1.0D+0 )
      PARAMETER        ( MREFN  =1     , MSTALL =50                    )

*     Specify the machine-dependent parameters.

      EPSMCH = WMACH(3)
      FLMAX  = WMACH(7)
      RTMAX  = WMACH(8)

      LANORM = LOCLS( 2)
      LAP    = LOCLS( 3)
      LPX    = LOCLS( 4)
      LRES   = LOCLS( 5)
      LRES0  = LOCLS( 6)
      LHZ    = LOCLS( 7)
      LGQ    = LOCLS( 8)
      LCQ    = LOCLS( 9)
      LRLAM  = LOCLS(10)
      LT     = LOCLS(11)
      LZY    = LOCLS(12)
      LWTINF = LOCLS(13)
      LWRK   = LOCLS(14)

*     Set up the adresses of the contiguous arrays  ( RES0, RES )
*     and  ( GQ, CQ ).

      NRES   = 0
      IF (NRANK .GT. 0) NRES = 2
      NGQ    = 1
      IF (LINOBJ) NGQ = 2

*     Initialize.

      IREFN  =   0
      ITER   =   0
      ITMAX  =   ITMAX1
      JADD   =   0
      JDEL   =   0
      NCNLN  =   0
      NPHASE =   1
      NSTALL =   0
      NUMINF = - 1
      NZ1    =   0

      ALFA   = ZERO
      CONDMX = FLMAX
      DRZMAX = ONE
      DRZMIN = ONE
      SSQ    = ZERO

      CYCLIN = .FALSE.
      ERROR  = .FALSE.
      FIRSTV = .FALSE.
      PRNT   = .TRUE.
      PRNT1  = .TRUE.
      NEEDFG = .TRUE.
      STALL  = .TRUE.
      UNCON  = .FALSE.
      UNBNDD = .FALSE.

*     If debug output is required,  print nothing until iteration IDBG.

      MSGSVD = MSGLVL
      IF (IDBG .GT. 0  .AND.  IDBG .LE. ITMAX) THEN
         MSGLVL = 0
      END IF

*======================== start of the main loop =======================
*
*      cyclin = false
*      unbndd = false
*      error  = false
*      k      = 0
*
*      repeat
*            repeat
*                  compute Z'g,  print details of this iteration
*                  stat pt = (Z'g .eq. 0)
*                  if (not stat pt) then
*                     error =  k .ge. itmax
*                     if (not error) then
*                        compute p, alfa
*                        error = unbndd  or  cyclin
*                        if (not error) then
*                           k = k + 1
*                           x = x + alfa p
*                           if (feasible) update Z'g
*                           if necessary, add a constraint
*                        end if
*                     end if
*                  end if
*            until  stat pt  or  error
*
*            compute lam1, lam2, smllst
*            optmul =  smllst .gt. 0
*            if ( not (optmul .or. error) ) then
*                  delete an artificial or regular constraint
*            end if
*      until optmul  or  error
*
*=======================================================================

*     REPEAT
*        REPEAT
  100       IF (NEEDFG) THEN
               IF (NRANK .GT. 0) THEN
                  RESNRM = DNRM2 ( NRANK, W(LRES), 1 )
                  SSQ    = HALF*(SSQ1**2 + RESNRM**2 )
               END IF

               IF (NUMINF .NE. 0) THEN

*                 Compute the transformed gradient of either the sum of
*                 of infeasibilities or the objective.  Initialize
*                 SINGLR and UNITGZ.

                  CALL LSGSET( PRBTYP, LINOBJ, SINGLR, UNITGZ, UNITQ,
     $                         N, NCLIN, NFREE,
     $                         NROWA, NQ, NROWR, NRANK, NZ, NZ1,
     $                         ISTATE, KX,
     $                         BIGBND, TOLRNK, NUMINF, SUMINF,
     $                         BL, BU, A, W(LRES), FEATOL,
     $                         W(LGQ), W(LCQ), R, X, W(LWTINF),
     $                         W(LZY), W(LWRK) )

                  IF (PRBTYP .NE. 'FP'  .AND.  NUMINF .EQ. 0
     $                                  .AND.  NPHASE .EQ. 1) THEN
                     ITMAX  = ITER + ITMAX2
                     NPHASE = 2
                  END IF
               END IF
            END IF

            GZNORM = ZERO
            IF (NZ  .GT. 0 ) GZNORM = DNRM2 ( NZ, W(LGQ), 1 )

            IF (NZ1 .EQ. NZ) THEN
               GZ1NRM = GZNORM
            ELSE
               GZ1NRM = ZERO
               IF (NZ1 .GT. 0) GZ1NRM = DNRM2 ( NZ1, W(LGQ), 1 )
            END IF

            GFNORM = GZNORM
            IF (NFREE .GT. 0  .AND.  NACTIV .GT. 0)
     $         GFNORM = DNRM2 ( NFREE, W(LGQ), 1 )

*           ------------------------------------------------------------
*           Print the details of this iteration.
*           ------------------------------------------------------------
*           Define small quantities that reflect the size of X, R and
*           the constraints in the working set.  If feasible,  estimate
*           the rank and condition number of Rz1.
*           Note that NZ1 .LE. NRANK + 1.

            IF (NZ1 .EQ. 0) THEN
               SINGLR = .FALSE.
            ELSE
               IF (NUMINF .GT. 0  .OR.  NZ1 .GT. NRANK) THEN
                  ABSRZZ = ZERO
               ELSE
                  CALL DCOND ( NZ1, R, NROWR+1, DRZMAX, DRZMIN )
                  ABSRZZ = ABS( R(NZ1,NZ1) )
               END IF
               SINGLR = ABSRZZ .LE. DRZMAX*TOLRNK

               IF (LSDBG  .AND.  ILSDBG(1) .GT. 0)
     $            WRITE (NOUT, 9100) SINGLR, ABSRZZ, DRZMAX, DRZMIN

            END IF

            CONDRZ = DDIV  ( DRZMAX, DRZMIN, OVERFL )
            CONDT  = ONE
            IF (NACTIV .GT. 0)
     $         CONDT  = DDIV  ( DTMAX , DTMIN , OVERFL )

            IF (PRNT) THEN
               CALL LSPRT ( PRBTYP, PRNT1, ISDEL, ITER, JADD, JDEL,
     $                      MSGLVL, NACTIV, NFREE, N, NCLIN,
     $                      NRANK, NROWR, NROWT, NZ, NZ1,
     $                      ISTATE,
     $                      ALFA, CONDRZ, CONDT, GFNORM, GZNORM, GZ1NRM,
     $                      NUMINF, SUMINF, CTX, SSQ,
     $                      AX, R, W(LT), X, W(LWRK) )

               JDEL  = 0
               JADD  = 0
               ALFA  = ZERO
            END IF

            IF (NUMINF .GT. 0) THEN
               DINKY  = ZERO
            ELSE
               OBJSIZ = ONE  + ABS( SSQ + CTX )
               WSSIZE = ZERO
               IF (NACTIV .GT. 0) WSSIZE = DTMAX
               DINKY  = EPSPT8 * MAX( WSSIZE, OBJSIZ, GFNORM )
               IF (UNCON) THEN
                  UNITGZ = GZ1NRM .LE. DINKY
               END IF
            END IF

            IF (LSDBG  .AND.  ILSDBG(1) .GT. 0)
     $         WRITE (NOUT, 9000) UNITGZ, IREFN, GZ1NRM, DINKY

*           If the projected gradient  Z'g  is small and Rz is of full
*           rank, X is a minimum on the working set.  An additional
*           refinement step is allowed to take care of an inaccurate
*           value of DINKY.

            STATPT = .NOT. SINGLR  .AND.  GZ1NRM .LE. DINKY
     $                             .OR.   IREFN  .GT. MREFN

            IF (.NOT. STATPT) THEN
*              ---------------------------------------------------------
*              Compute a search direction.
*              ---------------------------------------------------------
               PRNT  = .TRUE.

               ERROR = ITER .GE. ITMAX
               IF (.NOT. ERROR) THEN

                  IREFN = IREFN + 1
                  ITER  = ITER  + 1

                  IF (ITER .EQ. IDBG) THEN
                     LSDBG  = .TRUE.
                     CMDBG  =  LSDBG
                     MSGLVL =  MSGSVD
                  END IF

                  CALL LSGETP( LINOBJ, SINGLR, UNITGZ, UNITQ,
     $                         N, NCLIN, NFREE,
     $                         NROWA, NQ, NROWR, NRANK, NUMINF, NZ1,
     $                         ISTATE, KX, CTP, PNORM,
     $                         A, W(LAP), W(LRES), W(LHZ), W(LPX),
     $                         W(LGQ), W(LCQ), R, W(LZY), W(LWRK) )

*                 ------------------------------------------------------
*                 Find the constraint we bump into along P.
*                 Update X and AX if the step ALFA is nonzero.
*                 ------------------------------------------------------
*                 ALFHIT is initialized to BIGALF.  If it remains
*                 that way after the call to CMALF, it will be
*                 regarded as infinite.

                  BIGALF = DDIV  ( BIGDX, PNORM, OVERFL )

                  CALL CMALF ( FIRSTV, HITLOW,
     $                         ISTATE, INFORM, JADD, N, NROWA,
     $                         NCLIN, NCTOTL, NUMINF,
     $                         ALFHIT, PALFA, ATPHIT,
     $                         BIGALF, BIGBND, PNORM,
     $                         W(LANORM), W(LAP), AX,
     $                         BL, BU, FEATOL, W(LPX), X )

*                 If  Rz1  is nonsingular,  ALFA = 1.0  will be the
*                 step to the least-squares minimizer on the
*                 current subspace. If the unit step does not violate
*                 the nearest constraint by more than FEATOL,  the
*                 constraint is not added to the working set.

                  HITCON = SINGLR  .OR.  PALFA  .LE. ONE
                  UNCON  = .NOT. HITCON

                  IF (HITCON) THEN
                     ALFA = ALFHIT
                  ELSE
                     JADD   = 0
                     ALFA   = ONE
                  END IF

*                 Check for an unbounded solution or negligible step.

                  UNBNDD =  ALFA .GE. BIGALF
                  STALL  = ABS( ALFA*PNORM ) .LE. EPSPT9*XNORM
                  IF (STALL) THEN
                     NSTALL = NSTALL + 1
                     CYCLIN = NSTALL .GT. MSTALL
                  ELSE
                     NSTALL = 0
                  END IF

                  ERROR = UNBNDD  .OR.  CYCLIN
                  IF (.NOT.  ERROR) THEN
*                    ---------------------------------------------------
*                    Set X = X + ALFA*P.  Update AX, GQ, RES and CTX.
*                    ---------------------------------------------------
                     IF (ALFA .NE. ZERO)
     $                  CALL LSMOVE( HITCON, HITLOW, LINOBJ, UNITGZ,
     $                               NCLIN, NRANK, NZ1,
     $                               N, NROWR, JADD, NUMINF,
     $                               ALFA, CTP, CTX, XNORM,
     $                               W(LAP), AX, BL, BU, W(LGQ),
     $                               W(LHZ), W(LPX), W(LRES),
     $                               R, X, W(LWRK) )

                     IF (HITCON) THEN
*                       ------------------------------------------------
*                       Add a constraint to the working set.
*                       Update the TQ factors of the working set.
*                       Use P as temporary work space.
*                       ------------------------------------------------
*                       Update  ISTATE.

                        IF (BL(JADD) .EQ. BU(JADD)) THEN
                           ISTATE(JADD) = 3
                        ELSE IF (HITLOW) THEN
                           ISTATE(JADD) = 1
                        ELSE
                           ISTATE(JADD) = 2
                        END IF
                        IADD = JADD - N
                        IF (JADD .LE. N) THEN

                           DO 510 IFIX = 1, NFREE
                              IF (KX(IFIX) .EQ. JADD) GO TO 520
  510                      CONTINUE
  520                   END IF

                        CALL LSADD ( UNITQ,
     $                               INFORM, IFIX, IADD, JADD,
     $                               NACTIV, NZ, NFREE, NRANK, NRES,NGQ,
     $                               N, NROWA, NQ, NROWR, NROWT,
     $                               KX, CONDMX,
     $                               A, R, W(LT), W(LRES), W(LGQ),
     $                               W(LZY), W(LWRK), W(LRLAM) )

                        NZ1    = NZ1 - 1
                        NZ     = NZ  - 1

                        IF (JADD .LE. N) THEN

*                          A simple bound has been added.

                           NFREE  = NFREE  - 1
                        ELSE

*                          A general constraint has been added.

                           NACTIV = NACTIV + 1
                           KACTIV(NACTIV) = IADD
                        END IF

                        IREFN  = 0

                     END IF

*                    ---------------------------------------------------
*                    Check the feasibility of constraints with non-
*                    negative ISTATE values.  If some violations have
*                    occurred.  Refine the current X and set INFORM so
*                    that feasibility is checked in LSGSET.
*                    ---------------------------------------------------
                     CALL LSFEAS( N, NCLIN, ISTATE,
     $                            BIGBND, CNORM, ERR1, JMAX1, NVIOL,
     $                            AX, BL, BU, FEATOL, X, W(LWRK) )

                     IF (ERR1 .GT. FEATOL(JMAX1)) THEN
                        CALL LSSETX( LINOBJ, ROWERR, UNITQ,
     $                               NCLIN, NACTIV, NFREE, NRANK, NZ,
     $                               N, NCTOTL, NQ, NROWA, NROWR, NROWT,
     $                               ISTATE, KACTIV, KX,
     $                               JMAX1, ERR2, CTX, XNORM,
     $                               A, AX, BL, BU, W(LCQ),
     $                               W(LRES), W(LRES0), FEATOL, R,
     $                               W(LT), X, W(LZY), W(LPX), W(LWRK) )

                        IF (LSDBG  .AND.  ILSDBG(1) .GT. 0)
     $                     WRITE (NOUT, 2100) ERR1, ERR2
                        IF (ROWERR)       WRITE (NOUT, 2200)

                        UNCON  =   .FALSE.
                        IREFN  =   0
                        NUMINF = - 1
                     END IF
                     NEEDFG = ALFA .NE. ZERO
                  END IF
               END IF
            END IF

*        UNTIL      STATPT  .OR.  ERROR
         IF (.NOT. (STATPT  .OR.  ERROR) ) GO TO 100

*        ===============================================================
*        Try and find the index JDEL of a constraint to drop from
*        the working set.
*        ===============================================================
         JDEL   = 0

         IF (NUMINF .EQ. 0  .AND.  PRBTYP .EQ. 'FP') THEN
            IF (N .GT. NZ)
     $         CALL DLOAD ( N-NZ, (ZERO), W(LRLAM), 1 )
            JTINY  = 0
            JSMLST = 0
            JBIGST = 0
         ELSE

            CALL LSMULS( PRBTYP,
     $                   MSGLVL, N, NACTIV, NFREE,
     $                   NROWA, NROWT, NUMINF, NZ, NZ1,
     $                   ISTATE, KACTIV, KX, DINKY,
     $                   JSMLST, KSMLST, JINF, JTINY,
     $                   JBIGST, KBIGST, TRULAM,
     $                   A, W(LANORM), W(LGQ), W(LRLAM),
     $                   W(LT), W(LWTINF) )

         END IF

         IF (.NOT. ERROR) THEN
            IF (     JSMLST .GT. 0) THEN

*              LSMULS found a regular constraint with multiplier less
*              than (-DINKY).

               JDEL   = JSMLST
               KDEL   = KSMLST
               ISDEL  = ISTATE(JDEL)
               ISTATE(JDEL) = 0

            ELSE IF (JSMLST .LT. 0) THEN

               JDEL   = JSMLST

            ELSE IF (NUMINF .GT. 0  .AND.  JBIGST .GT. 0) THEN

*              No feasible point exists for the constraints but the
*              sum of the constraint violations may be reduced by
*              moving off constraints with multipliers greater than 1.

               JDEL   = JBIGST
               KDEL   = KBIGST
               ISDEL  = ISTATE(JDEL)
               IF (TRULAM .LE. ZERO) IS = - 1
               IF (TRULAM .GT. ZERO) IS = - 2
               ISTATE(JDEL) = IS
               FIRSTV = .TRUE.
               NUMINF = NUMINF + 1
            END IF

            IF      (JDEL .NE. 0  .AND.  SINGLR) THEN

*              Cannot delete a constraint when Rz is singular.
*              Probably a weak minimum.

               JDEL = 0
            ELSE IF (JDEL .NE. 0               ) THEN

*              Constraint JDEL has been deleted.
*              Update the matrix factorizations.

               CALL LSDEL ( UNITQ,
     $                      N, NACTIV, NFREE, NRES, NGQ, NZ, NZ1,
     $                      NROWA, NQ, NROWR, NROWT, NRANK,
     $                      JDEL, KDEL, KACTIV, KX,
     $                      A, W(LRES), R, W(LT), W(LGQ),W(LZY),W(LWRK))

            END IF
         END IF

         IREFN  =  0
         CONVRG =  JDEL .EQ. 0
         PRNT   = .FALSE.
         UNCON  = .FALSE.
         NEEDFG = .FALSE.

*     until       convrg  .or.  error
      IF (.NOT.  (CONVRG  .OR.  ERROR)) GO TO 100

*  .........................End of main loop............................

      WEAK = JTINY .GT. 0  .OR.  SINGLR

      IF (ERROR) THEN
         IF (UNBNDD) THEN
            INFORM = 2
            IF (NUMINF .GT. 0) INFORM = 3
         ELSE IF (ITER .GE. ITMAX) THEN
            INFORM = 4
         ELSE IF (CYCLIN) THEN
            INFORM = 5
         END IF
      ELSE IF (CONVRG) THEN
         INFORM = 0
         IF (NUMINF .GT. 0) THEN
            INFORM = 3
         ELSE IF (PRBTYP .NE. 'FP'  .AND.  WEAK) THEN
            INFORM = 1
         END IF
      END IF

*     ------------------------------------------------------------------
*     Set   CLAMDA.  Print the full solution.
*     ------------------------------------------------------------------
      MSGLVL = MSGSVD
      IF (MSGLVL .GT. 0) WRITE (NOUT, 2000) PRBTYP, ITER, INFORM

      CALL CMPRT ( MSGLVL, NFREE, NROWA,
     $             N, NCLIN, NCNLN, NCTOTL, BIGBND,
     $             NAMED, NAMES, LENNAM,
     $             NACTIV, ISTATE, KACTIV, KX,
     $             A, BL, BU, X, CLAMDA, W(LRLAM), X )

      RETURN

 2000 FORMAT(/ ' Exit from ', A2, ' problem after ', I4, ' iterations.',
     $         '  INFORM =', I3 )
 2100 FORMAT(  ' XXX  Iterative refinement.  Maximum errors before and',
     $         ' after refinement are ',  1P2E14.2 )
 2200 FORMAT(  ' XXX  Warning.  Cannot satisfy the constraints to the',
     $         ' accuracy requested.')
 9000 FORMAT(/ ' //LSCORE//  UNITGZ IREFN     GZ1NRM      DINKY'
     $       / ' //LSCORE//  ', L6, I6, 1P2E11.2 )
 9100 FORMAT(/ ' //LSCORE//  SINGLR   ABS(RZZ1)      DRZMAX      DRZMIN'
     $       / ' //LSCORE//  ', L6,     1P3E12.4 )

*     End of  LSCORE.

      END
