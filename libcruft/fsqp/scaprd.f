c
      double  precision function scaprd(n,x,y)
c     implicit real*8(a-h,o-z)
      integer n,i
      double  precision x(1),y(1),z
c     double  precision x(n),y(n),z
c
c     compute z=x'y
c
      z=0.d0
      do 100 i=1,n
        z=x(i)*y(i)+z 
 100  continue
      scaprd=z
      return
      end
