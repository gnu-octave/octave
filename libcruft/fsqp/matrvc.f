c 
      subroutine matrvc(l,n,la,na,a,x,y)
c     implicit real*8(a-h,o-z)
      integer l,n,la,na,i,j
      double  precision a(l,n),x(n),y(l),yi
c     double  precision a(l,1),x(1),y(1),yi
c
c     computation of y=ax
c
      do 200 i=1,la
        yi=0.d0
        do 100 j=1,na
 100      yi=yi+a(i,j)*x(j)
 200      y(i)=yi
      return
      end       
