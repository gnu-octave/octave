*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE DGEQR ( M, N, A, LDA, ZETA, INFORM )
      INTEGER            M, N, LDA, INFORM
      DOUBLE PRECISION   A( LDA, * ), ZETA( * )
C
C  1. Purpose
C     =======
C
C  DGEQR  reduces the  m by n, m.ge.n, matrix A to upper triangular form
C  by means of orthogonal transformations.
C
C  2. Description
C     ===========
C
C  The m by n matrix A is factorized as
C
C     A = Q*( R )   when   m.gt.n,
C           ( 0 )
C
C     A = Q*R       when   m = n,
C
C  where  Q  is an  m by m  orthogonal matrix and  R  is an n by n upper
C  triangular matrix.
C
C  The  factorization  is  obtained  by  Householder's  method. The  kth
C  transformation matrix, Q( k ), which is used to introduce zeros  into
C  the kth column of A is given in the form
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
C  zeta( k )  and  z( k ) are chosen to annhilate the elements below the
C  triangular part of  A.
C
C  The vector  u( k )  is returned in the kth element of ZETA and in the
C  kth column of A, such that zeta( k ) is in ZETA( k ) and the elements
C  of z( k ) are in a( k + 1, k ), ..., a( m, k ). The elements of R are
C  returned in the upper triangular part of  A.
C
C  Q is given by
C
C     Q = ( Q( p )*Q( p - 1 )*...*Q( 1 ) )',
C
C  where p = min( n, m - 1 ).
C
C  3. Parameters
C     ==========
C
C  M      - INTEGER.
C
C           On entry, M must specify the number of rows of  A. M must be
C           at least  n.
C
C           Unchanged on exit.
C
C  N      - INTEGER.
C
C           On entry, N must specify the number of columns of  A. N must
C           be  at  least zero. When  N = 0  then an immediate return is
C           effected.
C
C           Unchanged on exit.
C
C  A      - 'real' array of DIMENSION ( LDA, n ).
C
C           Before entry, the leading  M by N  part of the array  A must
C           contain the matrix to be factorized.
C
C           On exit, the  N by N upper triangular part of A will contain
C           the  upper  triangular  matrix  R  and the  M by N  strictly
C           lower triangular part of  A  will  contain  details  of  the
C           factorization as described above.
C
C  LDA    - INTEGER.
C
C           On entry, LDA  must  specify  the  leading dimension of  the
C           array  A  as declared in the calling (sub) program. LDA must
C           be at least  m.
C
C           Unchanged on exit.
C
C  ZETA   - 'real' array of DIMENSION at least ( n ).
C
C           On  exit, ZETA( k )  contains the scalar  zeta( k )  for the
C           kth  transformation.  If  T( k ) = I  then   ZETA( k ) = 0.0
C           otherwise  ZETA( k )  contains  zeta( k ) as described above
C           and is always in the range ( 1.0, sqrt( 2.0 ) ).
C
C  INFORM - INTEGER.
C
C           On successful  exit  INFORM  will be zero, otherwise  INFORM
C           will  be set to unity indicating that an input parameter has
C           been  incorrectly  set. See  the  next section  for  further
C           details.
C
C  4. Diagnostic Information
C     ======================
C
C  INFORM = 1
C
C     One or more of the following conditions holds:
C
C        M   .lt. N
C        N   .lt. 0
C        LDA .lt. M
C
C  5. Further information
C     ===================
C
C  Following the use of this routine the operations
C
C     B := Q'*B   and   B := Q*B,
C
C  where  B  is an  m by k  matrix, can  be  performed  by calls to  the
C  auxiliary  linear  algebra routine  DGEAPQ. The  operation  B := Q'*B
C  can be obtained by the call:
C
C     INFORM = 0
C     CALL DGEAPQ( 'Transpose', 'Separate', M, N, A, LDA, ZETA,
C    $             K, B, LDB, WORK, INFORM )
C
C  and  B := Q*B  can be obtained by the call:
C
C     INFORM = 0
C     CALL DGEAPQ( 'No transpose', 'Separate', M, N, A, LDA, ZETA,
C    $             K, B, LDB, WORK, INFORM )
C
C  In  both  cases  WORK  must be a  k  element array  that  is used  as
C  workspace. If  B  is a one-dimensional array (single column) then the
C  parameter  LDB  can be replaced by  M. See routine DGEAPQ for further
C  details.
C
C  Operations involving the matrix  R  are performed by  the
C  Level 2 BLAS  routines  DTRMV  and DTRSV . Note that no test for near
C  singularity of R is incorporated in this routine or in routine  DTRSV
C  and  so it is  strongly recommended that the auxiliary linear algebra
C  routine  DUTCO  be called, prior to solving equations involving R, in
C  order  to determine whether  or not  R  is nearly singular. If  R  is
C  nearly  singular  then  the  auxiliary linear algebra  routine  DUTSV
C  can  be used to  determine  the  singular value decomposition  of  R.
C
C
C  Nag Fortran 77 Auxiliary linear algebra routine.
C
C  -- Written on 13-December-1984.
C     Sven Hammarling, Nag Central Office.
C
      EXTERNAL           DGEMV , DGER  , DGRFG
      INTRINSIC          MIN
      INTEGER            J     , K     , LA
      DOUBLE PRECISION   TEMP
      DOUBLE PRECISION   ONE   ,         ZERO
      PARAMETER        ( ONE   = 1.0D+0, ZERO  = 0.0D+0 )

*     Check the input parameters.

      IF( N.EQ.0 )THEN
         INFORM = 0
         RETURN
      END IF
      IF( ( M.LT.N ).OR.( N.LT.0 ).OR.( LDA.LT.M ) )THEN
         INFORM = 1
         RETURN
      END IF

*     Perform the factorization.

      LA = LDA
      DO 20, K = 1, MIN( M - 1, N )

*        Use a Householder reflection to zero the kth column of A.
*        First set up the reflection.

         CALL DGRFG ( M - K, A( K, K ), A( K + 1, K ), 1, ZERO,
     $                ZETA( K ) )
         IF( ( ZETA( K ).GT.ZERO ).AND.( K.LT.N ) )THEN
            IF( ( K + 1 ).EQ.N )
     $         LA = M - K + 1
            TEMP      = A( K, K )
            A( K, K ) = ZETA( K )

*           We now perform the operation  A := Q( k )*A.

*           Let B denote the bottom ( m - k + 1 ) by ( n - k ) part
*           of A.

*           First form  work = B'*u. ( work is stored in the elements
*           ZETA( k + 1 ), ..., ZETA( n ). )

            CALL DGEMV ( 'Transpose', M - K + 1, N - K,
     $                   ONE, A( K, K + 1 ), LA, A( K, K ), 1,
     $                   ZERO, ZETA( K + 1 ), 1 )

*           Now form  B := B - u*work'.

            CALL DGER  ( M - K + 1, N - K, -ONE, A( K, K ), 1,
     $                   ZETA( K + 1 ), 1, A( K, K + 1 ), LA )

*           Restore beta.

            A( K, K ) = TEMP
         END IF
   20 CONTINUE

*     Store the final zeta when m.eq.n.

      IF( M.EQ.N )
     $   ZETA( N ) = ZERO

      INFORM = 0
      RETURN

*     End of DGEQR .

      END
