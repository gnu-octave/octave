      subroutine reduce (n,ma,a,mb,b,low,igh,cscale,wk)
c
c     *****parameters:
      integer igh,low,ma,mb,n
      double precision a(ma,n),b(mb,n),cscale(n),wk(n)
c
c     *****local variables:
      integer i,iflow,ii,ip1,is,j,jp1,k,l,lm1,m
      double precision f
c
c     *****functions:
c     none
c
c     *****subroutines called:
c     none
c
c     ---------------------------------------------------------------
c
c     *****purpose:
c     this subroutine reduces, if possible, the order of the
c     generalized eigenvalue problem a*x = (lambda)*b*x by permuting
c     the rows and columns of a and b so that they each have the
c     form
c                       u  x  y
c                       0  c  z
c                       0  0  r
c
c     where u and r are upper triangular and c, x, y, and z are
c     arbitrary.  thus, the isolated eigenvalues corresponding to
c     the triangular matrices are obtained by a division, leaving
c     only eigenvalues corresponding to the center matrices to be
c     computed.
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
c               defined above.
c
c     on output:
c
c       a,b     contain the permuted a and b matrices;
c
c       low     integer
c               beginning -1 of the submatrices of a and b
c               containing the non-isolated eigenvalues;
c
c       igh     integer
c               ending -1 of the submatrices of a and b
c               containing the non-isolated eigenvalues.  if
c               igh = 1 (low = 1 also), the permuted a and b
c               matrices are upper triangular;
c
c       cscale  real(n)
c               contains the required column permutations in its
c               first low-1 and its igh+1 through n locations;
c
c       wk      real(n)
c               contains the required row permutations in its first
c               low-1 and its igh+1 through n locations.
c
c     *****algorithm notes:
c     none
c
c     *****history:
c     written by r. c. ward.......
c
c     ---------------------------------------------------------------
c
      k = 1
      l = n
      go to 20
c
c     find row with one nonzero in columns 1 through l
c
   10 continue
      l = lm1
      if (l .ne. 1) go to 20
      wk(1) = 1
      cscale(1) = 1
      go to 200
   20 continue
      lm1 = l-1
      do 70 ii = 1,l
         i = l+1-ii
         do 30 j = 1,lm1
            jp1 = j+1
            if (a(i,j) .ne. 0.0d0 .or. b(i,j) .ne. 0.0d0) go to 40
   30    continue
         j = l
         go to 60
   40    continue
         do 50 j = jp1,l
            if (a(i,j) .ne. 0.0d0 .or. b(i,j) .ne. 0.0d0) go to 70
   50    continue
         j = jp1-1
   60    continue
         m = l
         iflow = 1
         go to 150
   70 continue
      go to 90
c
c     find column with one nonzero in rows k through n
c
   80 continue
      k = k+1
   90 continue
      do 140 j = k,l
         do 100 i = k,lm1
            ip1 = i+1
            if (a(i,j) .ne. 0.0d0 .or. b(i,j) .ne. 0.0d0) go to 110
  100    continue
         i = l
         go to 130
  110    continue
         do 120 i = ip1,l
            if (a(i,j) .ne. 0.0d0 .or. b(i,j) .ne. 0.0d0) go to 140
  120    continue
         i = ip1-1
  130    continue
         m = k
         iflow = 2
         go to 150
  140 continue
      go to 200
c
c     permute rows m and i
c
  150 continue
      wk(m) = i
      if (i .eq. m) go to 170
      do 160 is = k,n
         f = a(i,is)
         a(i,is) = a(m,is)
         a(m,is) = f
         f = b(i,is)
         b(i,is) = b(m,is)
         b(m,is) = f
  160 continue
c
c     permute columns m and j
c
  170 continue
      cscale(m) = j
      if (j .eq. m) go to 190
      do 180 is = 1,l
         f = a(is,j)
         a(is,j) = a(is,m)
         a(is,m) = f
         f = b(is,j)
         b(is,j) = b(is,m)
         b(is,m) = f
  180 continue
  190 continue
      go to (10,80), iflow
  200 continue
      low = k
      igh = l
      return
c
c     last line of reduce
c
      end
