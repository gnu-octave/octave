      subroutine gradeq (n,ma,a,mb,b,low,igh,cperm,wk)
c
c     *****parameters:
      integer igh,low,ma,mb,n
      double precision a(ma,n),b(mb,n),cperm(n),wk(n,2)
c
c     *****local variables:
      integer i,ighm1,im,ip1,j,jm,jp1,k
      double precision cmax,rmax,suma,sumb,temp
c
c     *****fortran functions:
      double precision dabs
c
c     *****subroutines called:
c     none
c
c     ---------------------------------------------------------------
c
c     *****purpose:
c     this subroutine grades the submatrices of a and b given by
c     starting -1 low and ending -1 igh in the generalized
c     eigenvalue problem a*x = (lambda)*b*x by permuting rows and
c     columns such that the norm of the i-th row (column) of the
c     a submatrix divided by the norm of the i-th row (column) of
c     the b submatrix becomes smaller as i increases.
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
c       low     integer
c               specifies the beginning -1 for the rows and
c               columns of a and b to be graded;
c
c       igh     integer
c               specifies the ending -1 for the rows and columns
c               of a and b to be graded;
c
c       wk      real(n,2)
c               work array that must contain at least 2*n locations.
c               only locations low through igh and n+low through
c               n+igh are referenced by this subroutine.
c
c     on output:
c
c       a,b     contain the permuted and graded a and b matrices;
c
c       cperm   real(n)
c               contains in its low through igh locations the
c               column permutations applied in grading the
c               submatrices.  the other locations are not referenced
c               by this subroutine;
c
c       wk      contains in its low through igh locations the row
c               permutations applied in grading the submatrices.
c
c     *****algorithm notes:
c     none.
c
c     *****history:
c     written by r. c. ward.......
c
c     ---------------------------------------------------------------
c
      if (low .eq. igh) go to 510
      ighm1 = igh-1
c
c     compute column norms of a / those of b
c
      do 420 j = low,igh
         suma = 0.0d0
         sumb = 0.0d0
         do 410 i = low,igh
            suma = suma + dabs(a(i,j))
            sumb = sumb + dabs(b(i,j))
  410    continue
         if (sumb .eq. 0.0d0) go to 415
         wk(j,2) = suma / sumb
         go to 420
  415    continue
         wk(j,2) = 1.0d38
  420 continue
c
c     permute columns to order them by decreasing quotients
c
      do 450 j = low,ighm1
         cmax = wk(j,2)
         jm = j
         jp1 = j+1
         do 430 k = jp1,igh
            if (cmax .ge. wk(k,2)) go to 430
            jm = k
            cmax = wk(k,2)
  430    continue
         cperm(j) = jm
         if (jm .eq. j) go to 450
         temp = wk(j,2)
         wk(j,2) = wk(jm,2)
         wk(jm,2) = temp
         do 440 i = 1,igh
            temp = b(i,j)
            b(i,j) = b(i,jm)
            b(i,jm) = temp
            temp = a(i,j)
            a(i,j) = a(i,jm)
            a(i,jm) = temp
  440    continue
  450 continue
      cperm(igh) = igh
c
c     compute row norms of a / those of b
c
      do 470 i = low,igh
         suma = 0.0d0
         sumb = 0.0d0
         do 460 j = low,igh
            suma = suma + dabs(a(i,j))
            sumb = sumb + dabs(b(i,j))
  460    continue
         if (sumb .eq. 0.0d0) go to 465
         wk(i,2) = suma / sumb
         go to 470
  465    continue
         wk(i,2) = 1.0d38
c
c     permute rows to order them by decreasing quotients
c
  470 continue
      do 500 i = low,ighm1
         rmax = wk(i,2)
         im = i
         ip1 = i+1
         do 480 k = ip1,igh
            if (rmax .ge. wk(k,2)) go to 480
            im = k
            rmax = wk(k,2)
  480    continue
         wk(i,1) = im
         if (im .eq. i) go to 500
         temp = wk(i,2)
         wk(i,2) = wk(im,2)
         wk(im,2) = temp
         do 490 j = low,n
            temp = b(i,j)
            b(i,j) = b(im,j)
            b(im,j) = temp
            temp = a(i,j)
            a(i,j) = a(im,j)
            a(im,j) = temp
  490    continue
  500 continue
      wk(igh,1) = igh
  510 continue
      return
c
c     last line of gradeq
c
      end
