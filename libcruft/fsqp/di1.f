c
      subroutine di1(nparam,nqpram,nob,nobL,nineqn,neq,neqn,ncnstr,
     *               nclin,nctotl,nrowa,infoqp,mode,iw,leniw,x0,d0,
     *               xl,xu,f,fM,gradf,grdpsf,g,gradg,cvec,a,bl,bu,
     *               clamda,bj,hess1,x,w,lenw)
c     implicit real*8(a-h,o-z)
      integer nparam,nqpram,nob,nobL,nineqn,neq,neqn,ncnstr,nclin,
     *        nctotl,nrowa,infoqp,mode,leniw,lenw,iw(leniw)
      double  precision fM
      double  precision x0(nparam),d0(nparam),xl(nparam),xu(nparam),
     *        f(1),gradf(nparam,1),grdpsf(nparam),g(1),
     *        gradg(nparam,1),cvec(1),a(nrowa,1),
     *        bl(1),bu(1),clamda(1),bj(1),
     *        hess1(nparam+1,nparam+1),x(1),w(lenw)
c     double  precision x0(nparam),d0(nparam),xl(nparam),xu(nparam),
c    *        f(nob),gradf(nparam,nob),grdpsf(nparam),g(ncnstr),
c    *        gradg(nparam,ncnstr),cvec(nqpram),a(nrowa,nqpram),
c    *        bl(nctotl),bu(nctotl),clamda(nctotl+nqpram),bj(nrowa),
c    *        hess1(nparam+1,nparam+1),x(nqpram),w(lenw)
c
      integer io,idum1,idum2,idum3,idum4,idum5,idum6,idum7
      double  precision epsmac,rteps,dumm1,dumm2,bigbnd,dummy
      common  /fsqpp2/io,idum1,idum2,idum3,idum4,idum5,idum6,idum7,
     *        /fsqpp3/epsmac,rteps,dumm1,dumm2,
     *        /fsqpq1/bigbnd,dummy
c
c     bj(1) is equivalent to bl(nparam+3)
c
      integer i,ii,iout,j,mnn
      double  precision x0i,eta
c
      iout=io
      if(mode.eq.0) eta=0.1d0
      if(mode.eq.1) eta=3.d0
      do 100 i=1,nparam
        x0i=x0(i)
        bl(i)=xl(i)-x0i
        bu(i)=xu(i)-x0i
        if(mode.eq.0) cvec(i)=-eta*d0(i)
        if(mode.eq.1) cvec(i)=0.d0
 100  continue
      bl(nqpram)=-bigbnd
      bu(nqpram)=bigbnd
      cvec(nqpram)=1.d0
      ii=ncnstr-nineqn
      do 400 i=1,ncnstr
        bj(i)=-g(ncnstr+1-i)
        do 300 j=1,nparam
 300      a(i,j)=-gradg(j,ncnstr+1-i)
        a(i,nqpram)=0.d0
        if((i.gt.(neq-neqn).and.i.le.neq).or.i.gt.ii) a(i,nqpram)=1.d0
 400  continue
      if(mode.eq.1) goto 610
      do 600 i=1,nob
        ii=ncnstr+i
        bj(ii)=fM-f(i)
        do 500 j=1,nparam
          a(ii,j)=-gradf(j,i)+grdpsf(j)
          if(nobL.gt.nob) a(ii+nob,j)=gradf(j,i)+grdpsf(j)
 500    continue
        a(ii,nqpram)=1.d0
        if(nobL.gt.nob) a(ii+nob,nqpram)=1.d0
 600  continue
 610  call diagnl(nqpram,eta,hess1)
      call nullvc(nqpram,x)
      hess1(nqpram,nqpram)=0.d0
c
Ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  The following modification is done inside QP0001
c  for the ease of interfacing with QPSOL
c
c     hess1(nqpram,nqpram)=qleps
C
      mnn=nclin+2*nqpram
      iw(1)=1
      call QL0001(nclin,neq-neqn,nrowa,nqpram,nparam+1,mnn,hess1,cvec,A,
     *             bj,bL,bU,X,clamda,iout,infoqp,0,w,lenw,iw,leniw)
C
Ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      return
      end
