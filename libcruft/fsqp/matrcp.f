c
      subroutine matrcp(ndima,a,ndimb,b)
c     implicit real*8(a-h,o-z)
      integer ndima,ndimb,i,j
      double  precision a(ndima,1),b(ndimb,1)
c     double  precision a(ndima,ndima),b(ndimb,ndimb)
c
      do 100 i=1,ndima
        do 100 j=1,ndima
 100      b(i,j)=a(i,j)
      if(ndimb.le.ndima) goto 9000
        do 200 i=1,ndimb
          b(ndimb,i)=0.d0
 200      b(i,ndimb)=0.d0
 9000 return
      end
