c
      subroutine sbout1(io,n,s1,z,z1,job,level)
c     implicit real*8(a-h,o-z)
      integer io,n,job,level,j
      double precision z,z1(1)
      character*20 s1
c
      if (job.eq.2) goto 10
      if (level.eq.1)write(io,9900) s1,z
      if (level.eq.2)write(io,9901) s1,z
      return
 10   if (level.eq.1)write(io,9900) s1,z1(1)
      if (level.eq.2)write(io,9901) s1,z1(1)
      do 100 j=2,n
        if (level.eq.1) write(io,9902) z1(j)
        if (level.eq.2) write(io,9903) z1(j)
 100  continue
      return
 9900 format(1x,a20,e22.14)
 9901 format(1x,t17,a20,t45,e22.14)
 9902 format(1x,t22,e22.14)
 9903 format(1x,t45,e22.14)
      end
