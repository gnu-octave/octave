c
      double precision function lfuscp(val,thrshd)
c     implicit real*8(a-h,o-z)
      double precision val,thrshd
c
      if(dabs(val).le.thrshd) lfuscp=0
      if(dabs(val).gt.thrshd) lfuscp=1
      return
      end
