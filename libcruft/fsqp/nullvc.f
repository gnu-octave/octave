c
      subroutine nullvc(nparam,x)
c     implicit real*8(a-h,o-z)
      integer nparam,i
      double  precision x(nparam)
c
c     set x=0
c
      do 100 i=1,nparam
 100    x(i)=0.d0
      return
      end
