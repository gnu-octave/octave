c
      subroutine sbout2(io,n,i,s1,s2,z)
c     implicit real*8(a-h,o-z)
      integer io,n,i,j
      double precision z(n)
      character*8 s1
      character*1 s2
c
      write(io,9900) s1,i,s2,z(1)
      do 100 j=2,n
 100    write(io,9901) z(j)
      return
 9900 format(1x,t17,a8,i5,a1,t45,e22.14)
 9901 format(1x,t45,e22.14)
      end
