c
      subroutine estlam(nparam,neq,ifail,iout,bigbnd,hess,cvec,a,b,
     *                  gradh,psb,bl,bu,x,w,lenw,iw,leniw)
      integer nparam,neq,ifail,iout,lenw,leniw,iw(leniw)
      double precision bigbnd,hess(neq,1),cvec(1),a(1),b(1),
     *                 gradh(nparam,1),psb(1),bl(1),bu(1),
     *                 x(1),w(lenw)
c     double precision bigbnd,hess(neq,neq),cvec(neq),a(1),b(1),
c    *                 gradh(nparam,neq),psb(nparam),bl(1),bu(1),
c    *                 x(neq),w(lenw)
c
c     compute an estimate of multipliers for updating penalty parameter
c
      integer i,j
      double precision scaprd
c
      do 200 i=1,neq
        bl(i)=-bigbnd
        bu(i)=bigbnd
        cvec(i)=scaprd(nparam,gradh(1,i),psb)
        x(i)=0.d0
        do 100 j=i,neq 
          hess(i,j)=scaprd(nparam,gradh(1,i),gradh(1,j))
 100      hess(j,i)=hess(i,j)
 200  continue
      iw(1)=1
      call ql0001(0,0,1,neq,neq,2*neq,hess,cvec,a,b,bl,bu,x,w,
     c            iout,ifail,0,w(2),lenw-1,iw,leniw)
c
      return
      end
