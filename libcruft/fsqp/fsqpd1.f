      subroutine FSQPD1(miter,nparam,nob,nobL,nineqn,neq,neqn,ncnstr,
     *                  nctotl,nrowa,feasb,epskt,epseqn,xl,xu,indxob,
     *                  indxcn,iact,iskip,istore,iw,leniw,x,di,d,g,gm,
     *                  gradg,f,gradf,grdpsf,penp,a,bl,bu,clamda,
     *                  cllamd,cvec,bj,hess,hess1,span,backup,signeq,
     *                  w,lenw,obj,constr,gradob,gradcn)
c
c     FSQP Version 3.3  : main routine for the optimization
c
c     implicit real*8(a-h,o-z)
      integer miter,nparam,nob,nobL,nineqn,neq,neqn,ncnstr,nctotl,nrowa,
     *        leniw,lenw
      integer indxob(1),indxcn(1),iact(1),iskip(1),
     *        istore(1),iw(leniw)
c     integer indxob(nob),indxcn(ncnstr),iact(nob+nineqn+neqn),iskip(1),
c    *        istore(nineqn+nob+neqn),iw(leniw)
      double  precision epskt,epseqn
      double  precision xl(nparam),xu(nparam),x(nparam+1),di(nparam+1),
     *        d(nparam+1),g(1),gm(1),gradg(nparam,1),
     *        f(1),gradf(nparam,1),grdpsf(nparam),penp(1),
     *        a(nrowa,1),bl(1),bu(1),clamda(1),
     *        cllamd(1),cvec(nparam+1),bj(1),
     *        hess(nparam,nparam),hess1(1),span(1),
     *        backup(1),signeq(1),w(lenw)
c     double  precision xl(nparam),xu(nparam),x(nparam+1),di(nparam+1),
c    *        d(nparam+1),g(ncnstr),gm(4*neqn),gradg(nparam,ncnstr),
c    *        f(nob),gradf(nparam,nob),grdpsf(nparam),penp(neqn),
c    *        a(nrowa,1),bl(nctotl),bu(nctotl),clamda(nctotl+nparam+1),
c    *        cllamd(nctotl),cvec(nparam+1),bj(nrowa),
c    *        hess(nparam,nparam),hess1(nparam+1,nparam+1),span(1),
c    *        backup(nob+ncnstr),signeq(neqn),w(lenw)
      external obj,constr,gradob,gradcn
      logical feasb
c
      integer nnineq,M,ncallg,ncallf,mode,io,iprint,info,ipd,iter,nstop,
     *        initvl,ipspan,ipyes,lstype
      double  precision bigbnd,tolfea,epsmac,rteps,udelta,valnom
      logical dlfeas,local,update,first
      common  /fsqpp1/nnineq,M,ncallg,ncallf,mode,lstype,nstop,
     *        /fsqpp2/io,iprint,ipspan,ipyes,info,ipd,iter,initvl,
     *        /fsqpp3/epsmac,rteps,udelta,valnom,
     *        /fsqpq1/bigbnd,tolfea,
c    *        /fsqp1/rentry,
     *        /fsqplo/dlfeas,local,update,first
c
c     bj(1+) is equivalent to bl(nparam+3+)
c
      integer i,iskp,nfs,ncf,ncg,nn,non,nstart,nrst,ncnst1,nctot1
      double  precision Cbar,Ck,dbar,fM,fMp,steps,d0nm,dummy,
     *        sktnom,scvneq,grdftd,dmax1,psf
c
      initvl=1
      first=.true.
      nrst=0
      ipd=0
      if(iter.eq.0) call diagnl(nparam,1.d0,hess)
      if(.not.feasb) goto 5
        first=.true.
        if(iter.gt.0) iter=iter-1
        if(iter.ne.0) call diagnl(nparam,1.d0,hess)
 5    Cbar=1.d-02
      Ck=Cbar
      dbar=5.0d0
      nstart=1
      ncallf=0
      nstop=1
      nfs=0
      non=miter
      if(mode.eq.0) goto 10
        nfs=M
        non=0
 10   if(feasb) then
        nn=nineqn+neqn
        ncnst1=ncnstr
        nctot1=nctotl
      else 
        nn=0
        ncnst1=ncnstr-nineqn-neqn
        nctot1=nnineq-nineqn+neq-neqn+nparam
        if(nob.gt.1) nctot1=nctot1+1
      endif
      scvneq=0.d0
      do 100 i=1,ncnst1
        valnom=g(indxcn(i))
        backup(i)=valnom
        if(feasb.and.i.gt.nineqn.and.i.le.nn) then
          gm(i-nineqn)=valnom*signeq(i-nineqn)
          scvneq=scvneq+dabs(valnom)
        endif
        if(.not.feasb.or.i.gt.nn) goto 20
          iact(i)=indxcn(i)
          istore(i)=0
          if(i.gt.nineqn) penp(i-nineqn)=10.d0
 20     call gradcn(nparam,indxcn(i),x,gradg(1,indxcn(i)),constr)
 100  continue
      call nullvc(nparam,grdpsf)
      psf=0.d0
      if(.not.feasb.or.neqn.eq.0) goto 110
        call resign(nparam,neqn,psf,grdpsf,penp,g(nnineq+1),
     *              gradg(1,nnineq+1),signeq,12,12)
 110  fM=-bigbnd
      do 140 i=1,nob
        if(.not.feasb) goto 120
          iact(nn+i)=i
          istore(nn+i)=0
          call obj(nparam,i,x,f(i))
          valnom=f(i)
          backup(i+ncnst1)=valnom
          call gradob(nparam,i,x,gradf(1,i),obj)
          ncallf=ncallf+1
          if(nobL.ne.nob) fM=dmax1(fM,-f(i))
        goto 130
 120      valnom=f(i)
          iact(i)=i
          istore(i)=0
          call gradcn(nparam,indxob(i),x,gradf(1,i),constr)
 130    fM=dmax1(fM,f(i))
 140  continue
      fMp=fM-psf
      span(1)=fM
c
      if(iprint.lt.3.or..not.first.or.ipyes.gt.0) goto 600
        do 300 i=1,nob
          if(.not.feasb) goto 250
            if(nob.gt.1) 
     *        call sbout2(io,nparam,i,'gradf(j,',')',gradf(1,i))
            if(nob.eq.1) 
     *        call sbout1(io,nparam,'gradf(j)            ',
     *                    dummy,gradf(1,1),2,2)
          goto 300
 250        call sbout2(io,nparam,indxob(i),'gradg(j,',')',gradf(1,i))
 300    continue
 310    if(ncnstr.eq.0) goto 410
        do 400 i=1,ncnst1
 400      call sbout2(io,nparam,i,'gradg(j,',')',gradg(1,i))
        if(neqn.eq.0) goto 410
        call sbout1(io,nparam,'grdpsf(j)           ',dummy,grdpsf,2,2)
        call sbout1(io,neqn,'P                   ',dummy,penp,2,2)
 410    do 500 i=1,nparam
 500      call sbout2(io,nparam,i,'hess (j,',')',hess(1,i))
c
c     main loop of the algorithm
c
 600  nstop=1
 601  continue
        call out(miter,nparam,nob,nineqn,nn,neqn,ncnst1,x,g,
     *           f,fM,psf,steps,sktnom,d0nm,feasb)
        if(nstop.ne.0) goto 810
        if(.not.feasb) goto 801
          do 700 i=1,ncnst1
 700        g(i)=backup(i)
          do 800 i=1,nob
 800        f(i)=backup(i+ncnst1)
 801      return
 810    continue
        if(ipspan.ge.10.and.iprint.ge.2.and.ipyes.eq.0) 
     *    write(io,9900) iter
        call dir(nparam,nob,nobL,nineqn,neq,neqn,nn,ncnst1,nctot1,nrowa,
     *           feasb,steps,epskt,epseqn,sktnom,scvneq,Ck,d0nm,grdftd,
     *           xl,xu,indxob,indxcn,iact,iskp,iskip,istore,iw,leniw,
     *           x,di,d,g,gradg,f,fM,fMp,psf,gradf,grdpsf,penp,a,
     *           bl,bu,clamda,cllamd,cvec,bj,hess,hess1,w,lenw,
     *           backup,signeq,obj,constr)
        if(nstop.eq.0) goto 601
        first=.false.
        if(update) goto 820
        call step(nparam,nob,nobL,nineqn,neq,neqn,nn,ncnst1,ncg,ncf,
     *            indxob,indxcn,iact,iskp,iskip,istore,feasb,grdftd,
     *            f,fM,fMp,psf,penp,steps,scvneq,bu,x,di,d,g,w,
     *            backup,signeq,obj,constr)
        if(nstop.eq.0) goto 601
 820    call hesian(nparam,nob,nobL,nineqn,neq,neqn,nn,ncnst1,nctot1,
     *              nfs,nstart,feasb,bigbnd,bu,x,f,fM,fMp,psf,
     *              gradf,grdpsf,penp,g,gm,gradg,indxob,indxcn,cllamd,
     *              bl,clamda,di,hess,d,steps,nrst,signeq,span,
     *              obj,constr,gradob,gradcn,
     *              hess1,cvec,bj,w,lenw,iw,leniw) 
        if(nstop.eq.0) goto 601
        if(mode.eq.0) goto 601
        if(d0nm.gt.dbar) Ck=dmax1(dble(0.5*Ck),Cbar)
        if(d0nm.le.dbar.and.dlfeas) Ck=Ck
        if(d0nm.le.dbar.and..not.dlfeas) Ck=10.0*Ck
      goto 601
 9900 format(1x,9hiteration,t22,i22)
      end
