c
      subroutine shift(n,ii,iact)
      integer n,ii,iact(1),j,k
c
      if(ii.eq.iact(1)) return
      do 200 j=1,n
        if(ii.ne.iact(j)) goto 200
        do 100 k=j,2,-1
 100      iact(k)=iact(k-1)
        goto 210
 200  continue
 210  iact(1)=ii
      return
      end
