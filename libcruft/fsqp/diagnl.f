c=== subroutines used in FSQPD 3.3  ===============================c
c                                                                  c
c  diagnl  error   estlam  fool    indexs  lfuscp  matrcp  matrvc  c
c  nullvc  resign  sbout1  sbout2  scaprd  shift   slope   small   c 
c                                                                  c
c==================================================================c
c
      subroutine diagnl(nrowa,diag,a)
c     implicit real*8(a-h,o-z)
      integer nrowa,i,j
      double  precision a(nrowa,1),diag
c     double  precision a(nrowa,nrowa),diag
c
c     set a=diag*I, the diagonal matrix
c
      do 200 i=1,nrowa
        do 100 j=i,nrowa
          a(i,j)=0.d0
 100      a(j,i)=0.d0
 200    a(i,i)=diag
      return
      end
