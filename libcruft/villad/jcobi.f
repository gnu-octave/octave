****************************************************************
*
*     The following routines (JCOBI, DIF, DFOPR, INTRP, AND RADAU)
*     are the same as found in Villadsen, J. and M.L. Michelsen,
*     Solution of Differential Equation Models by Polynomial
*     Approximation, Prentice-Hall (1978) pages 418-420.
*
*     Cosmetic changes (elimination of arithmetic IF statements, most
*     GO TO statements, and indentation of program blocks) made by:
*
*     John W. Eaton
*     Department of Chemical Engineering
*     The University of Texas at Austin
*     Austin, Texas 78712
*
*     June 6, 1987
*
*     Some error checking additions also made on June 7, 1987
*
*     Further cosmetic changes made August 20, 1987
*
************************************************************************
*
      SUBROUTINE JCOBI
     +  (
     +  ND, N, N0, N1, ALPHA, BETA, DIF1, DIF2, DIF3, ROOT
     +  )
C
      INTEGER
     +
     +  ND, N, N0, N1
C 
      DOUBLE PRECISION
     +
     +  ALPHA, BETA, DIF1(ND), DIF2(ND), DIF3(ND), ROOT(ND)
C
C***********************************************************************
C
C     VILLADSEN AND MICHELSEN, PAGES 131-132, 418
C
C     THIS SUBROUTINE COMPUTES THE ZEROS OF THE JACOBI POLYNOMIAL
C
C        (ALPHA,BETA)
C       P  (X)
C        N
C
C     USE DIF (GIVEN BELOW) TO COMPUTE THE DERIVATIVES OF THE NODE
C     POLYNOMIAL
C
C                     N0     (ALPHA,BETA)           N1
C       P  (X)  =  (X)   *  P (X)         *  (1 - X)
C        NT                   N
C
C     AT THE INTERPOLATION POINTS.
C
C     INPUT PARAMETERS:
C
C       ND     : THE DIMENSION OF THE VECTORS DIF1, DIF2, DIF3, AND ROOT
C
C       N      : THE DEGREE OF THE JACOBI POLYNOMIAL, (i.e. THE NUMBER
C                OF INTERIOR INTERPOLATION POINTS)
C
C       N0     : DETERMINES WHETHER X = 0 IS INCLUDED AS AN
C                INTERPOLATION POINT
C
C                  N0 = 0  ==>  X = 0 IS NOT INCLUDED
C                  N0 = 1  ==>  X = 0 IS INCLUDED
C
C       N1     : DETERMINES WHETHER X = 1 IS INCLUDED AS AN
C                INTERPOLATION POINT
C
C                  N1 = 0  ==>  X = 1 IS NOT INCLUDED
C                  N1 = 1  ==>  X = 1 IS INCLUDED
C
C       ALPHA  : THE VALUE OF ALPHA IN THE DESCRIPTION OF THE JACOBI
C                POLYNOMIAL
C
C       BETA   : THE VALUE OF BETA IN THE DESCRIPTION OF THE JACOBI
C                POLYNOMIAL
C
C       FOR A MORE COMPLETE EXPLANATION OF ALPHA AN BETA, SEE VILLADSEN
C       AND MICHELSEN, PAGES 57 TO 59
C
C     OUTPUT PARAMETERS:
C
C       ROOT   : ONE DIMENSIONAL VECTOR CONTAINING ON EXIT THE
C                N + N0 + N1 ZEROS OF THE NODE POLYNOMIAL USED IN THE
C                INTERPOLATION ROUTINE
C
C       DIF1   : ONE DIMENSIONAL VECTOR CONTAINING THE FIRST DERIVATIVE
C                OF THE NODE POLYNOMIAL AT THE ZEROS
C
C       DIF2   : ONE DIMENSIONAL VECTOR CONTAINING THE SECOND DERIVATIVE
C                OF THE NODE POLYNOMIAL AT THE ZEROS
C
C       DIF3   : ONE DIMENSIONAL VECTOR CONTAINING THE THIRD DERIVATIVE
C                OF THE NODE POLYNOMIAL AT THE ZEROS
C
C     COMMON BLOCKS:      NONE
C
C     REQUIRED ROUTINES:  VILERR, DIF
C
C***********************************************************************
C
      INTEGER           I,J,NT,IER
      DOUBLE PRECISION  AB,AD,AP,Z1,Z,Y,X,XD,XN,XD1,XN1,XP,XP1,ZC
      DOUBLE PRECISION  ZERO,ONE,TWO
      LOGICAL           LSTOP
C
      PARAMETER ( ZERO = 0.0D+00, ONE = 1.0D+00, TWO = 2.0D+00 )
C
C -- ERROR CHECKING
C
      IF ((N0 .NE. 0) .AND. (N0 .NE. 1)) THEN
        IER   = 1
        LSTOP = .TRUE.
        CALL VILERR(IER,LSTOP)
      ELSE
      END IF
C
      IF ((N1 .NE. 0) .AND. (N1 .NE. 1)) THEN
        IER   = 2
        LSTOP = .TRUE.
        CALL VILERR(IER,LSTOP)
      ELSE
      END IF
C
      IF (ND .LT. (N + N0 + N1)) THEN
        IER   = 3
        LSTOP = .TRUE.
        CALL VILERR(IER,LSTOP)
      ELSE
      END IF
C
      IF ((N + N0 + N1) .LT. 1) THEN
        IER   = 7
        LSTOP = .TRUE.
        CALL VILERR(IER,LSTOP)
      ELSE
      END IF
C
C -- FIRST EVALUATION OF COEFFICIENTS IN RECURSION FORMULAS.
C -- RECURSION COEFFICIENTS ARE STORED IN DIF1 AND DIF2.
C
      AB      = ALPHA + BETA
      AD      = BETA - ALPHA
      AP      = BETA*ALPHA
      DIF1(1) = (AD/(AB + TWO) + ONE)/TWO
      DIF2(1) = ZERO
C
      IF(N .GE. 2) THEN
        DO 10 I = 2,N
C
          Z1      = DBLE(I) - ONE
          Z       = AB + 2*Z1
          DIF1(I) = (AB*AD/Z/(Z + TWO) + ONE)/TWO
C
          IF (I .EQ. 2) THEN
            DIF2(I) = (AB + AP + Z1)/Z/Z/(Z + ONE)
          ELSE
            Z       = Z*Z
            Y       = Z1*(AB + Z1)
            Y       = Y*(AP + Y)
            DIF2(I) = Y/Z/(Z - ONE)
          END IF
C
   10   CONTINUE
      ELSE
      END IF
C
C -- ROOT DETERMINATION BY NEWTON METHOD WITH SUPPRESSION OF
C -- PREVIOUSLY DETERMINED ROOTS
C
      X = ZERO
C
      DO 20 I = 1,N
C
   25   CONTINUE
        XD  = ZERO
        XN  = ONE
        XD1 = ZERO
        XN1 = ZERO
C
        DO 30 J = 1,N
          XP  = (DIF1(J) - X)*XN  - DIF2(J)*XD
          XP1 = (DIF1(J) - X)*XN1 - DIF2(J)*XD1 - XN
          XD  = XN
          XD1 = XN1
          XN  = XP
          XN1 = XP1
   30   CONTINUE
C
        ZC  = ONE
        Z   = XN/XN1
C
        IF (I .NE. 1) THEN
          DO 22 J = 2,I
            ZC = ZC - Z/(X - ROOT(J-1))
   22     CONTINUE
        ELSE
        END IF
C
        Z  = Z/ZC
        X  = X - Z
C
        IF (DABS(Z) .GT. 1.D-09) THEN
C
C -- BACKWARD BRANCH
C
          GO TO 25
        ELSE
        END IF
C
        ROOT(I) = X
        X = X + 0.0001D0
C
   20 CONTINUE
C
C -- ADD INTERPOLATION POINTS AT X = 0 AND/OR X = 1
C
      NT = N + N0 + N1
C
      IF (N0 .NE. 0) THEN
        DO 31 I = 1,N
          J = N + 1 - I
          ROOT(J+1) = ROOT(J)
   31   CONTINUE
        ROOT(1) = ZERO
      ELSE
      END IF
C
      IF (N1 .EQ. 1) THEN
        ROOT(NT) = ONE
      ELSE
      END IF
C
      CALL DIF ( NT, ROOT, DIF1, DIF2, DIF3 )
C
      RETURN
      END
