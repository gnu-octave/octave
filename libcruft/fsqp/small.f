c
      double precision function small()
c     implicit real*8(a-h,o-z)
      double precision one, two, z
c
      one=1.d0
      two=2.d0
      small=one
10    small=small/two
      call fool(small,one,z)
      if(z.gt.one) goto 10
      small=small*two*two
c
c The simpler sequence commented out below fails on some machines that use
c extra-length registers for internal computation.  This was pointed out
c to us by Roque Donizete de Oliveira (Michigan) who suggested to sequence
c used now.
c
c     small=1.d0
c100  if ((small+1.d0).eq.1.d0) goto 110
c     small=small/2.d0
c     goto 100
c110  small=small*4.d0
      return
      end
