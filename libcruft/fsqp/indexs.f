c
      integer function indexs(i,nfs)
c     implicit real*8(a-h,o-z)
      integer i,nfs,mm
c
c     find the residue of i with respect to nfs
c
      mm=i
      if(mm.le.nfs) goto 120
 110  mm=mm-nfs
      if(mm.gt.nfs) goto 110
 120  indexs=mm
      return
      end
