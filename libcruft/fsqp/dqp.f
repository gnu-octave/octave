c
      subroutine dqp(nparam,nqpram,nob,nobL,nineqn,neq,neqn,nn,ncnstr,
     *               nclin,nctotl,nrowa,infoqp,iw,leniw,x0,di,xl,xu,
     *               feasb,f,fM,gradf,grdpsf,g,gradg,a,cvec,bl,bu,
     *               clamda,cllamd,bj,hess,hess1,x,w,lenw,vv,job)
c     implicit double precision(a-h,o-z)
      integer nparam,nqpram,nob,nobL,nineqn,neq,neqn,nn,ncnstr,nclin,
     *        nctotl,nrowa,infoqp,leniw,lenw,job
      integer iw(leniw)
      double  precision fM,vv
      double  precision x0(nparam),di(1),xl(nparam),xu(nparam),
     *        f(1),gradf(nparam,1),grdpsf(nparam),g(1),
     *        gradg(nparam,1),
     *        a(nrowa,1),cvec(1),bl(1),bu(1),clamda(1),
     *        cllamd(1),bj(1),hess(nparam,nparam),
     *        hess1(nparam+1,nparam+1),x(1),w(lenw)
c     double  precision x0(nparam),di(nqpram),xl(nparam),xu(nparam),
c    *        f(nob),gradf(nparam,nob),grdpsf(nparam),g(ncnstr),
c    *        gradg(nparam,ncnstr),
c    *        a(nrowa,nqpram),cvec(nqpram),bl(nctotl),bu(nctotl),
c    *        clamda(nctotl+nqpram),cllamd(nctotl),bj(nrowa),
c    *        hess(nparam,nparam),hess1(nparam+1,nparam+1),
c    *        x(nqpram),w(lenw)
      logical feasb
c
      integer io,idum1,idum2,idum3,idum4,idum5,idum6,idum7
      double  precision bigbnd,dummy,epsmac,rteps,dummy1,dummy2
      common  /fsqpp2/io,idum1,idum2,idum3,idum4,idum5,idum6,idum7,
     *        /fsqpp3/epsmac,rteps,dummy1,dummy2,
     *        /fsqpq1/bigbnd,dummy
c
c     bj(1) is equivalent to bl(nparam+3)
c
c     job=0 : compute d0; job=1 : compute  d~
c
      integer i,ii,j,iout,mnn,nqnp
      double  precision x0i,xdi
c
      iout=io
      do 100 i=1,nparam
        x0i=x0(i)
        if(job.eq.1) xdi=di(i)
        if(job.eq.0) xdi=0.d0
        bl(i)=xl(i)-x0i-xdi
        bu(i)=xu(i)-x0i-xdi
        cvec(i)=cvec(i)-grdpsf(i)
 100  continue
      if(nobL.eq.1) goto 110
        bl(nqpram)=-bigbnd
        bu(nqpram)=bigbnd
 110  ii=ncnstr-nn
c
c     constraints are assigned to a in reverse order
c
      do 300 i=1,ncnstr
        x0i=vv
        if(i.le.(neq-neqn).or.(i.gt.neq.and.i.le.(ncnstr-nineqn)))
     *    x0i=0.d0
        if(.not.feasb) x0i=0.d0
        bj(i)=x0i-g(iw(ncnstr+1-i))
        do 200 j=1,nparam
 200      a(i,j)=-gradg(j,iw(ncnstr+1-i))
        if(nobL.gt.1) a(i,nqpram)=0.d0
 300  continue
      if(nobL.eq.1) goto 510
      do 500 i=1,nob
        ii=ncnstr+i
        bj(ii)=fM-f(iw(ii))
        if(nobL.gt.nob) bj(ii+nob)=fM+f(iw(ii))
        do 400 j=1,nparam
          a(ii,j)=-gradf(j,iw(ii))
          if(nobL.gt.nob) a(ii+nob,j)=gradf(j,iw(ii))
 400    continue
        a(ii,nqpram)=1.d0
        if(nobL.gt.nob) a(ii+nob,nqpram)=1.d0
 500  continue
      cvec(nqpram)=1.d0
      goto 610
 510  do 600 i=1,nparam
 600    cvec(i)=cvec(i)+gradf(i,1)
 610  call matrcp(nparam,hess,nparam+1,hess1)
      call nullvc(nqpram,x)
c
Ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C
c  The following modification is done inside QP0001
c  for the ease of interfacing with QPSOL
c
c     if(hess1(nqpram,nqpram).lt.qleps) hess1(nqpram,nqpram)=qleps
C
      iw(1)=1
      mnn=nclin+2*nqpram
      call QL0001(nclin,neq-neqn,nrowa,nqpram,nparam+1,mnn,hess1,cvec,A,
     *            bj,bL,bU,X,clamda,iout,infoqp,0,w,lenw,iw,leniw)
C
Ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C
      if(infoqp.ne.0.or.job.eq.1) goto 9000
      do 700 i=1,nqpram
        ii=nclin+i
        if(clamda(ii).eq.0.d0.and.clamda(ii+nqpram).eq.0.d0) then
          goto 700
        else if(clamda(ii).ne.0.d0) then
          clamda(ii)=-clamda(ii)
        else 
          clamda(ii)=clamda(ii+nqpram)
        endif
 700  continue
      nqnp=nqpram+ncnstr
      do 800 i=1,nctotl
        if(i.le.nqpram) then
          ii=nclin+i
        else if(i.gt.nqpram.and.i.le.nqnp) then
          ii=nqnp+1-i
        else if(i.gt.nqnp) then
          ii=i-nqpram
        endif
        cllamd(i)=clamda(ii)
 800  continue
      if(nobL.eq.nob) goto 9000
      do 900 i=1,nob
        ii=i+nqpram+ncnstr
        cllamd(ii)=cllamd(ii)-cllamd(ii+nob)
 900  continue
 9000 return
      end
