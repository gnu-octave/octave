*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE DGEAP ( SIDE, TRANS, M, N, PERM, K, B, LDB )
*     .. Scalar Arguments ..
      INTEGER            K, LDB, M, N
      CHARACTER*1        SIDE, TRANS
*     .. Array Arguments ..
      DOUBLE PRECISION   B( LDB, * )
      INTEGER            PERM( * )
*     ..
*
*  Purpose
*  =======
*
*  DGEAP  performs one of the transformations
*
*     B := P'*B   or   B := P*B,   where B is an m by k matrix,
*
*  or
*
*     B := B*P'   or   B := B*P,   where B is a k by m matrix,
*
*  P being an m by m permutation matrix of the form
*
*     P = P( 1, index( 1 ) )*P( 2, index( 2 ) )*...*P( n, index( n ) ),
*
*  where  P( i, index( i ) ) is the permutation matrix that interchanges
*  items i and index( i ). That is P( i, index( i ) ) is the unit matrix
*  with rows and columns  i and index( i )  interchanged.  Of course, if
*  index( i ) = i  then  P( i, index( i ) ) = I.
*
*  This routine  is intended for use in  conjunction with  Nag auxiliary
*  routines that  perform  interchange  operations,  such  as  pivoting.
*
*  Parameters
*  ==========
*
*  SIDE   - CHARACTER*1.
*  TRANS
*           On entry,  SIDE  ( Left-hand side, or Right-hand side )  and
*           TRANS  ( Transpose, or No transpose )  specify the operation
*           to be performed as follows.
*
*           SIDE = 'L' or 'l'   and   TRANS = 'T' or 't'
*
*              Perform the operation   B := P'*B.
*
*           SIDE = 'L' or 'l'   and   TRANS = 'N' or 'n'
*
*              Perform the operation   B := P*B.
*
*           SIDE = 'R' or 'r'   and   TRANS = 'T' or 't'
*
*              Perform the operation   B := B*P'.
*
*           SIDE = 'R' or 'r'   and   TRANS = 'N' or 'n'
*
*              Perform the operation   B := B*P.
*
*           Unchanged on exit.
*
*  M      - INTEGER.
*
*           On entry, M must specify the order of the permutation matrix
*           P.  M must be at least zero.  When  M = 0  then an immediate
*           return is effected.
*
*           Unchanged on exit.
*
*  N      - INTEGER.
*
*           On entry,  N must specify the value of n. N must be at least
*           zero.  When  N = 0  then an  immediate  return is  effected.
*
*           Unchanged on exit.
*
*  PERM   - INTEGER array of DIMENSION at least ( n ).
*
*           Before  entry,  PERM  must  contain the  n  indices  for the
*           permutation matrices. index( i ) must satisfy
*
*              1 .le. index( i ) .le. m.
*
*           It is usual for index( i ) to be at least i, but this is not
*           necessary for this routine.
*
*           Unchanged on exit.
*
*  K      - INTEGER.
*
*           On entry with  SIDE = 'L' or 'l',  K must specify the number
*           of columns of B and on entry with  SIDE = 'R' or 'r', K must
*           specify the  number of rows of B.  K must be at least  zero.
*           When  K = 0  then an immediate return is effected.
*
*           Unchanged on exit.
*
*  B      - DOUBLE PRECISION array of  DIMENSION  ( LDB, ncolb ),  where
*           ncolb = k   when   SIDE = 'L' or 'l'  and   ncolb = m   when
*           SIDE = 'R' or 'r'.
*
*           Before entry  with  SIDE = 'L' or 'l',  the  leading  M by K
*           part  of  the  array   B  must  contain  the  matrix  to  be
*           transformed  and  before entry with  SIDE = 'R' or 'r',  the
*           leading  K by M part of the array  B must contain the matrix
*           to  be  transformed.  On  exit,  B  is  overwritten  by  the
*           transformed matrix.
*
*  LDB    - INTEGER.
*
*           On entry,  LDB  must specify  the  leading dimension  of the
*           array  B  as declared  in the  calling  (sub) program.  When
*           SIDE = 'L' or 'l'   then  LDB  must  be  at  least  m,  when
*           SIDE = 'R' or 'r'   then  LDB  must  be  at  least  k.
*           Unchanged on exit.
*
*
*  Nag Fortran 77 O( n**2 ) basic linear algebra routine.
*
*  -- Written on 13-January-1986.
*     Sven Hammarling, Nag Central Office.
*
*
*     .. Local Scalars ..
      DOUBLE PRECISION   TEMP
      INTEGER            I, J, L
      LOGICAL            LEFT, NULL, RIGHT, TRNSP
*     .. Intrinsic Functions ..
      INTRINSIC          MIN
*     ..
*     .. Executable Statements ..
      IF( MIN( M, N, K ).EQ.0 )
     $   RETURN
      LEFT  = ( SIDE .EQ.'L' ).OR.( SIDE .EQ.'l' )
      RIGHT = ( SIDE .EQ.'R' ).OR.( SIDE .EQ.'r' )
      NULL  = ( TRANS.EQ.'N' ).OR.( TRANS.EQ.'n' )
      TRNSP = ( TRANS.EQ.'T' ).OR.( TRANS.EQ.'t' )
      IF( LEFT )THEN
         IF( TRNSP )THEN
            DO 20, I = 1, N
               IF( PERM( I ).NE.I )THEN
                  L = PERM( I )
                  DO 10, J = 1, K
                     TEMP      = B( I, J )
                     B( I, J ) = B( L, J )
                     B( L, J ) = TEMP
   10             CONTINUE
               END IF
   20       CONTINUE
         ELSE IF( NULL )THEN
            DO 40, I = N, 1, -1
               IF( PERM( I ).NE.I )THEN
                  L = PERM( I )
                  DO 30, J = 1, K
                     TEMP      = B( L, J )
                     B( L, J ) = B( I, J )
                     B( I, J ) = TEMP
   30             CONTINUE
               END IF
   40       CONTINUE
         END IF
      ELSE IF( RIGHT )THEN
         IF( TRNSP )THEN
            DO 60, J = 1, N
               IF( PERM( J ).NE.J )THEN
                  L = PERM( J )
                  DO 50, I = 1, K
                     TEMP      = B( I, J )
                     B( I, J ) = B( L, J )
                     B( L, J ) = TEMP
   50             CONTINUE
               END IF
   60       CONTINUE
         ELSE IF( NULL )THEN
            DO 80, J = N, 1, -1
               IF( PERM( J ).NE.J )THEN
                  L = PERM( J )
                  DO 70, I = 1, K
                     TEMP      = B( L, J )
                     B( L, J ) = B( I, J )
                     B( I, J ) = TEMP
   70             CONTINUE
               END IF
   80       CONTINUE
         END IF
      END IF
*
      RETURN
*
*     End of DGEAP . ( F06QJF )
*
      END
