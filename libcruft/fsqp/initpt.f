      subroutine initpt(nparam,nnl,neq,neqn,nclin,nctotl,nrowa,x0,
     *                  bndl,bndu,iw,leniw,x,bl,bu,g,gradg,a,cvec,hess,
     *                  clamda,bj,w,lenw,constr,gradcn)
c
c     FSQP Version 3.3  : generation of a feasible point satisfying
c                         simple bounds and linear constraints
c
c     implicit real*8(a-h,o-z)
      integer nparam,nnl,neq,neqn,nclin,nctotl,nrowa,leniw,lenw
      integer iw(leniw)
      double  precision x0(nparam),bndl(nparam),bndu(nparam),x(nparam),
     *        bl(1),bu(1),g(1),gradg(nparam,1),
     *        a(nrowa,1),cvec(nparam),hess(nparam,nparam),
     *        clamda(1),bj(1),w(lenw)
c     double  precision x0(nparam),bndl(nparam),bndu(nparam),x(nparam),
c    *        bl(nctotl),bu(nctotl),g(nclin),gradg(nparam,nclin),
c    *        a(nrowa,nparam),cvec(nparam),hess(nparam,nparam),
c    *        clamda(nctotl+nparam),bj(nclin),w(lenw)
      external constr,gradcn
c
c     bj(1) is equivalent to bl(nparam+3)
c
      integer io,iprint,ipspan,ipyes,info,ipd,idum,idum2,maxit,
     *        nnineq,id1,id2,id3,id4,id5,id6
      double  precision epsmac,rteps,udelta,valnom,big,tolfea
      common  /fsqpp1/nnineq,id1,id2,id3,id4,id5,id6
     *        /fsqpp2/io,iprint,ipspan,ipyes,info,ipd,idum,idum2,
     *        /fsqpp3/epsmac,rteps,udelta,valnom,
     *        /fsqpq1/big,tolfea,/fsqpq2/maxit
c
      integer i,j,infoql,mnn
      double  precision x0i
c
      info=1
      do 10 i=1,nclin
        valnom=g(i)
        j=i+nnl
        if(j.le.nnineq) call gradcn(nparam,j,x0,gradg(1,i),constr)
        if(j.gt.nnineq) 
     *    call gradcn(nparam,j+neqn,x0,gradg(1,i),constr)
 10   continue
      do 20 i=1,nparam
        x0i=x0(i)
        bl(i)=bndl(i)-x0i
        bu(i)=bndu(i)-x0i
        cvec(i)=0.d0
 20   continue
      do 30 i=nclin,1,-1
 30     bj(nclin-i+1)=-g(i)
      do 60 i=nclin,1,-1
        do 50 j=1,nparam
 50       a(nclin-i+1,j)=-gradg(j,i)
 60   continue
      call diagnl(nparam,1.d0,hess)
      call nullvc(nparam,x)
C
      mnn=nrowa+2*nparam
      iw(1)=1
      call QL0001(nclin,neq-neqn,nrowa,nparam,nparam,mnn,hess,cvec,A,
     *            bj,bL,bU,X,clamda,io,infoql,0,w,lenw,iw,leniw)
      if(infoql.ne.0) goto 90
      do 70 i=1,nparam
 70     x0(i)=x0(i)+x(i)
      do 80 i=1,nclin
        j=i+nnl
        if(j.le.nnineq) call constr(nparam,j,x0,g(i))
        if(j.gt.nnineq) call constr(nparam,j+neqn,x0,g(i))
 80   continue
      info=0
 90   if(info.eq.1.and.iprint.ne.0) write(io,1000)
 1000 format(1x,'Error: No feasible point is found for the',
     *                 ' linear constraints',/)
      return
      end
