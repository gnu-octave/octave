      subroutine balgen (n,ma,a,mb,b,low,igh,cscale,cperm,wk)
c
c     *****parameters:
      integer igh,low,ma,mb,n
      double precision a(ma,n),b(mb,n),cperm(n),cscale(n),wk(n,6)
c
c     *****local variables:
c     none
c
c     *****functions:
c     none
c
c     *****subroutines called:
c     reduce, scaleg, gradeq
c
c     ---------------------------------------------------------------
c
c     *****purpose:
c     this subroutine balances the matrices a and b to improve the
c     accuracy of computing the eigensystem of the generalized
c     eigenproblem a*x = (lambda)*b*x.  the algorithm is specifically
c     designed to precede qz type algorithms, but improved performance
c     is expected from most eigensystem solvers.
c     ref.:  ward, r. c., balancing the generalized eigenvalue
c     problem, siam j. sci. stat. comput., vol. 2, no. 2, june 1981,
c     141-152.
c
c     *****parameter description:
c
c     on input:
c
c       ma,mb   integer
c               row dimensions of the arrays containing matrices
c               a and b respectively, as declared in the main calling
c               program dimension statement;
c
c       n       integer
c               order of the matrices a and b;
c
c       a       real(ma,n)
c               contains the a matrix of the generalized eigenproblem
c               defined above;
c
c       b       real(mb,n)
c               contains the b matrix of the generalized eigenproblem
c               defined above;
c
c       wk      real(n,6)
c               work array that must contain at least 6*n storage
c               locations.  wk is altered by this subroutine.
c
c     on output:
c
c       a,b     contain the balanced a and b matrices;
c
c       low     integer
c               beginning -1 of the submatrices of a and b
c               containing the non-isolated eigenvalues;
c
c       igh     integer
c               ending -1 of the submatrices of a and b
c               containing the non-isolated eigenvalues.  if
c               igh = 1 (low = 1 also), the a and b matrices have
c               been permuted into upper triangular form and have
c               not been balanced;
c
c       cscale  real(n)
c               contains the exponents of the column scaling factors
c               in its low through igh locations and the reducing
c               column permutations in its first low-1 and its
c               igh+1 through n locations;
c
c       cperm   real(n)
c               contains the column permutations applied in grading
c               the a and b submatrices in its low through igh
c               locations;
c
c       wk      contains the exponents of the row scaling factors
c               in its low through igh locations, the reducing row
c               permutations in its first low-1 and its igh+1
c               through n locations, and the row permutations
c               applied in grading the a and b submatrices in its
c               n+low through n+igh locations.
c
c     *****algorithm notes:
c     none
c
c     *****history:
c     written by r. c. ward.......
c
c     ---------------------------------------------------------------
c
      call reduce (n,ma,a,mb,b,low,igh,cscale,wk)
      if (low .eq. igh) go to 10
      call scaleg (n,ma,a,mb,b,low,igh,cscale,cperm,wk)
      call gradeq (n,ma,a,mb,b,low,igh,cperm,wk(1,2))
   10 continue
      return
c
c     last line of balgen
c
      end
