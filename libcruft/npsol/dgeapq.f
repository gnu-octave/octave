*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE DGEAPQ( TRANS, WHEREZ, M, N, A, LDA, ZETA,
     $                   NCOLB, B, LDB, WORK, INFORM )
      CHARACTER*1        TRANS, WHEREZ
      INTEGER            M, N, LDA, NCOLB, LDB, INFORM
      DOUBLE PRECISION   A( LDA, * ), ZETA( * ), B( LDB, * ), WORK( * )
C
C  1. Purpose
C     =======
C
C  DGEAPQ performs one of the transformations
C
C     B := Q'*B   or   B := Q*B,
C
C  where B is an m by ncolb matrix and Q is an m by m orthogonal matrix,
C  given as the product of  Householder transformation matrices, details
C  of  which are stored in the  m by n ( m.ge.n )  array  A  and, if the
C  parameter  WHEREZ = 'S' or 's', in the array ZETA.
C
C  This  routine is  intended for use following auxiliary linear algebra
C  routines such as  DGEQR , DGEHES and DSLTRI. ( See those routines for
C  example calls. )
C
C  2. Description
C     ===========
C
C  Q is assumed to be given by
C
C     Q = ( Q( p )*Q( p - 1 )*...*Q( 1 ) )',
C
C  Q( k ) being given in the form
C
C     Q( k ) = ( I     0   ),
C              ( 0  T( k ) )
C
C  where
C
C     T( k ) = I - u( k )*u( k )',   u( k ) = ( zeta( k ) ),
C                                             (    z( k ) )
C
C  zeta( k )  is a scalar and  z( k )  is an  ( m - k )  element vector.
C
C  z( k )  must  be  supplied  in  the  kth  column  of  A  in  elements
C  a( k + 1, k ), ..., a( m, k )  and  zeta( k ) must be supplied either
C  in  a( k, k )  or in  zeta( k ), depending upon the parameter WHEREZ.
C
C  To obtain Q explicitly B may be set to I and premultiplied by Q. This
C  is more efficient than obtaining Q'.
C
C  3. Parameters
C     ==========
C
C  TRANS  - CHARACTER*1.
C
C           On entry, TRANS  specifies the operation to be performed  as
C           follows.
C
C           TRANS = ' ' or 'N' or 'n'
C
C              Perform the operation  B := Q*B.
C
C           TRANS = 'T' or 't' or 'C' or 'c'
C
C              Perform the operation  B := Q'*B.
C
C           Unchanged on exit.
C
C  WHEREZ - CHARACTER*1.
C
C           On entry, WHEREZ specifies where the elements of zeta are to
C           be found as follows.
C
C           WHEREZ = 'I' or 'i'
C
C              The elements of zeta are in A.
C
C           WHEREZ = 'S' or 's'
C
C              The elements of zeta are separate from A, in ZETA.
C
C           Unchanged on exit.
C
C  M      - INTEGER.
C
C           On entry, M  must specify the number of rows of A. M must be
C           at least n.
C
C           Unchanged on exit.
C
C  N      - INTEGER.
C
C           On entry, N  must specify the number of columns of A. N must
C           be  at least zero. When  N = 0  then an immediate return  is
C           effected.
C
C           Unchanged on exit.
C
C  A      - 'real' array of DIMENSION ( LDA, n ).
C
C           Before entry, the leading  M by N  stricly lower  triangular
C           part of the array  A  must contain details of the matrix  Q.
C           In  addition, when  WHEREZ = 'I' or 'i'  then  the  diagonal
C           elements of A must contain the elements of zeta.
C
C           Unchanged on exit.
C
C  LDA    - INTEGER.
C
C           On  entry, LDA  must specify  the leading dimension  of  the
C           array  A  as declared in the calling (sub) program. LDA must
C           be at least m.
C
C           Unchanged on exit.
C
C  ZETA   - 'real' array of DIMENSION at least min( m - 1, n ).
C
C           Before entry with  WHEREZ = 'S' or 's', the array  ZETA must
C           contain the elements of the vector  zeta.
C
C           When  WHEREZ = 'I' or 'i', the array ZETA is not referenced.
C
C           Unchanged on exit.
C
C  NCOLB  - INTEGER.
C
C           On  entry, NCOLB  must specify  the number of columns of  B.
C           NCOLB  must  be  at  least  zero.  When  NCOLB = 0  then  an
C           immediate return is effected.
C
C           Unchanged on exit.
C
C  B      - 'real' array of DIMENSION ( LDB, ncolb ).
C
C           Before entry, the leading  M by NCOLB  part of  the array  B
C           must  contain  the matrix to be  transformed.
C
C           On  exit,  B  is  overwritten  by  the  transformed  matrix.
C
C  LDB    - INTEGER.
C
C           On  entry, LDB  must specify  the  leading dimension of  the
C           array  B as declared in the calling (sub) program. LDB  must
C           be at least m.
C
C           Unchanged on exit.
C
C  WORK   - 'real' array of DIMENSION at least ( ncolb ).
C
C           Used as internal workspace.
C
C  INFORM - INTEGER.
C
C           On  successful exit  INFORM  will be zero, otherwise  INFORM
C           will  be set to unity indicating that an input parameter has
C           been  incorrectly  set. See  the  next  section  for further
C           details.
C
C  4. Diagnostic Information
C     ======================
C
C  INFORM = 1
C
C     One or more of the following conditions holds:
C
C        TRANS  .ne. ' ' or 'N' or 'n' or 'T' or 't' or 'C' or 'c'
C        WHEREZ .ne. 'I' or 'i' or 'S' or 's'
C        M      .lt. N
C        N      .lt. 0
C        LDA    .lt. M
C        NCOLB  .lt. 0
C        LDB    .lt. M
C
C
C  Nag Fortran 77 Auxiliary linear algebra routine.
C
C  -- Written on 15-November-1984.
C     Sven Hammarling, Nag Central Office.
C
      EXTERNAL           DGEMV , DGER
      INTRINSIC          MIN
      INTEGER            J     , K     , KK    , LB
      DOUBLE PRECISION   TEMP
      DOUBLE PRECISION   ONE   ,         ZERO
      PARAMETER        ( ONE   = 1.0D+0, ZERO  = 0.0D+0 )

*     Check the input parameters.

      IF( MIN( N, NCOLB ).EQ.0 )THEN
         INFORM = 0
         RETURN
      END IF
      IF( ( ( TRANS .NE.' ' ).AND.
     $      ( TRANS .NE.'N' ).AND.( TRANS .NE.'n' ).AND.
     $      ( TRANS .NE.'T' ).AND.( TRANS .NE.'t' ).AND.
     $      ( TRANS .NE.'C' ).AND.( TRANS .NE.'c' )      ).OR.
     $    ( ( WHEREZ.NE.'I' ).AND.( WHEREZ.NE.'i' ).AND.
     $      ( WHEREZ.NE.'S' ).AND.( WHEREZ.NE.'s' )      ).OR.
     $    ( M.LT.N ).OR.( N.LT.0 ).OR.( LDA.LT.M ).OR.
     $    ( NCOLB.LT.0 ).OR.( LDB.LT.M )                      )THEN
         INFORM = 1
         RETURN
      END IF

*     Perform the transformation.

      LB = LDB
      DO 20, KK = 1, MIN( M - 1, N )
         IF( ( TRANS.EQ.'T' ).OR.( TRANS.EQ.'t' ).OR.
     $       ( TRANS.EQ.'C' ).OR.( TRANS.EQ.'c' )     )THEN

*           Q'*B = Q( p )*...*Q( 2 )*Q( 1 )*B,     p = min( m - 1, n ).

            K = KK
         ELSE

*           Q*B  = Q( 1 )'*Q( 2 )'*...*Q( p )'*B,  p = min( m - 1, n ).
*           Note that  Q( k )' = Q( k ).

            K = MIN( N, M - 1 ) + 1 - KK
         END IF
         IF( ( WHEREZ.EQ.'S' ).OR.( WHEREZ.EQ.'s' ) )THEN
            TEMP      = A( K, K )
            A( K, K ) = ZETA( K )
         END IF

*        If ZETA( k ) is zero then Q( k ) = I and we can skip the kth
*        transformation.

         IF( A( K, K ).GT.ZERO )THEN
            IF( NCOLB.EQ.1 )
     $         LB = M - K + 1

*           Let C denote the bottom ( m - k + 1 ) by ncolb part of B.

*           First form  work = C'*u.

            DO 10, J = 1, NCOLB
               WORK( J ) = ZERO
   10       CONTINUE
            CALL DGEMV ( 'Transpose', M - K + 1, NCOLB,
     $                   ONE, B( K, 1 ), LB, A( K, K ), 1,
     $                   ZERO, WORK, 1 )

*           Now form  C := C - u*work'.

            CALL DGER  ( M - K + 1, NCOLB, -ONE, A( K, K ), 1,
     $                   WORK, 1, B( K, 1 ), LB )
         END IF

*        Restore the diagonal element of A.

         IF( ( WHEREZ.EQ.'S' ).OR.( WHEREZ.EQ.'s' ) )
     $      A( K, K ) = TEMP
   20 CONTINUE

      INFORM = 0
      RETURN

*     End of DGEAPQ.

      END
