      subroutine hesian(nparam,nob,nobL,nineqn,neq,neqn,nn,ncnstr,
     *                  nctotl,nfs,nstart,feasb,bigbnd,xnew,x,f,
     *                  fM,fMp,psf,gradf,grdpsf,penp,g,gm,gradg,indxob,
     *                  indxcn,cllamd,delta,eta,gamma,hess,hd,steps,
     *                  nrst,signeq,span,obj,constr,gradob,gradcn,
     *                  phess,psb,psmu,w,lenw,iw,leniw)
c
c     FSQP Version 3.3  : updating the Hessian matrix using BFGS
c                         formula with Powell's modification
c
c     implicit real*8(a-h,o-z)
      integer nparam,nob,nobL,nineqn,neq,neqn,nn,ncnstr,nctotl,nfs,
     *        nstart,indxob(1),indxcn(1),nrst,lenw,leniw,iw(leniw)
c    *        nstart,indxob(nob),indxcn(1),nrst,lenw,leniw,iw(leniw)
      double  precision bigbnd,steps,psf,fM,fMp,
     *        xnew(nparam),x(nparam),f(1),gradf(nparam,1),
     *        grdpsf(nparam),penp(1),g(1),gm(1),
     *        gradg(nparam,1),cllamd(1),delta(nparam),
     *        eta(nparam),gamma(nparam),hess(nparam,nparam),hd(nparam),
     *        signeq(1),span(1),phess(1),psb(1),psmu(1),w(lenw)
c     double  precision bigbnd,steps,psf,fM,fMp,
c    *        xnew(nparam),x(nparam),f(nob),gradf(nparam,nob),
c    *        grdpsf(nparam),penp(neqn),g(ncnstr),gm(4*neqn),
c    *        gradg(nparam,ncnstr),cllamd(nctotl),delta(nparam),
c    *        eta(nparam),gamma(nparam),hess(nparam,nparam),hd(nparam),
c    *        signeq(neqn),span(1),phess(neq,neq),psb(neq),
c    *        psmu(neq),w(lenw)
      external obj,constr,gradob,gradcn
      logical feasb
c
      integer nnineq,M,ncallg,ncallf,mode,io,iprint,ipspan,ipyes,info,
     *        ipd,iter,nstop,initvl,lstype
      double  precision epsmac,rteps,udelta,valnom,objeps,objrep,gLgeps
      common  /fsqpp1/nnineq,M,ncallg,ncallf,mode,lstype,nstop,
     *        /fsqpp2/io,iprint,ipspan,ipyes,info,ipd,iter,initvl,
     *        /fsqpp3/epsmac,rteps,udelta,valnom,
     *        /fsqpus/objeps,objrep,gLgeps
c
      integer ng,i,j,ifail,indexs,np,mnm,iout
      double  precision dhd,gammd,etad,scaprd,dummy,theta,signgj,psfnew
      logical done
c
      if(feasb.and.nstop.ne.0.and.neqn.eq.0) then
c
c       check of gLgeps is just after computing d0!
c
        if(dabs(w(1)-fM).le.objeps) then
          nstop=0
        else if(dabs(1.d0-fM/w(1)).le.objrep) then
          nstop=0
        endif
      endif
      if(nstop.eq.0) goto 810
c
      ipd=0
      done=.false.
      psfnew=0.d0
      call nullvc(nparam,delta)
      call nullvc(nparam,eta)
      if(nobL.gt.1) ng=2
      if(nobL.eq.1) ng=1
c
 100  continue
        call nullvc(nparam,gamma)
        if(nobL.gt.1) call matrvc(nparam,nob,nparam,nob,gradf,
     *                     cllamd(nparam+ng+ncnstr),hd)
        if(.not.feasb) goto 120
        if(nineqn.eq.0) goto 110
        call matrvc(nparam,nineqn,nparam,nineqn,gradg,cllamd(nparam+ng),
     *              gamma)
 110    if(neqn.eq.0) goto 120
        call matrvc(nparam,neqn,nparam,neqn,gradg(1,nnineq+1),
     *              cllamd(nparam+nnineq+ng),eta)
 120    do 200 i=1,nparam
          if(nobL.gt.1) then
            if(done) psb(i)=hd(i)+cllamd(i)+gamma(i)
            gamma(i)=gamma(i)+hd(i)-grdpsf(i)+eta(i)
          else if(nobL.eq.1) then
            if(done) psb(i)=gradf(i,1)+cllamd(i)+gamma(i)
            gamma(i)=gamma(i)+gradf(i,1)-grdpsf(i)+eta(i)
          endif
          if(.not.done) delta(i)=gamma(i)
 200    continue
        if(done) goto 410
        if(nn.eq.0) goto 310
        do 300 i=1,nn
          if(feasb.and.i.gt.nineqn)     signgj=signeq(i-nineqn)
          if(.not.feasb.or.i.le.nineqn) signgj=1.d0
          valnom=g(indxcn(i))*signgj
          call gradcn(nparam,indxcn(i),xnew,gradg(1,indxcn(i)),constr)
 300    continue
        call resign(nparam,neqn,psf,grdpsf,penp,g(nnineq+1),
     *              gradg(1,nnineq+1),signeq,11,11)
 310    do 400 i=1,nob
          valnom=f(i)
          if(feasb) call gradob(nparam,i,xnew,gradf(1,i),obj)
          if(.not.feasb)
     *      call gradcn(nparam,indxob(i),xnew,gradf(1,i),constr)
 400    continue
        done=.true.
      goto 100
c
 410  if(nrst.lt.(5*nparam).or.steps.gt.0.1d0) goto 420
        nrst=0
        call diagnl(nparam,1.d0,hess)
        goto 810
 420  nrst=nrst+1
      do 500 i=1,nparam
        gamma(i)=gamma(i)-delta(i)
        delta(i)=xnew(i)-x(i)
 500  continue
      call matrvc(nparam,nparam,nparam,nparam,hess,delta,hd)
      dhd=scaprd(nparam,delta,hd)
      gammd=scaprd(nparam,delta,gamma)
      if(gammd.ge.0.2d0*dhd) theta=1.d0
      if(gammd.lt.0.2d0*dhd) theta=.8d0*dhd/(dhd-gammd)
      do 600 i=1,nparam
 600    eta(i)=hd(i)*(1.d0-theta)+theta*gamma(i)
      etad=theta*gammd+(1.d0-theta)*dhd
      do 800  i=1,nparam
        do 700 j=i,nparam
          hess(i,j)=hess(i,j)-hd(i)*hd(j)/dhd+eta(i)*eta(j)/etad
 700    hess(j,i)=hess(i,j)
 800  continue
 810  do 900 i=1,nparam
 900    x(i)=xnew(i)
      if(nstop.eq.0) goto 9000
      if(neqn.eq.0.or..not.feasb) goto 1400
        iout=io
        i=nnineq-nineqn
        if(i.eq.0) goto 990
        call matrvc(nparam,i,nparam,i,gradg(1,nineqn+1),
     *              cllamd(nparam+ng+nineqn),gamma)
        do 950 i=1,nparam
 950      psb(i)=psb(i)+gamma(i)
 990    call estlam(nparam,neq,ifail,iout,bigbnd,phess,delta,eta,gamma,
     *              gradg(1,nnineq+1),psb,hd,xnew,psmu,w,lenw,iw,leniw)
        do 1000 i=1,neqn
          if(ifail.ne.0) then
            penp(i)=2.d0*penp(i)
          else if(ifail.eq.0) then
            etad=psmu(i)+penp(i)
            if(etad.ge.1.d0) goto 1000
            penp(i)=dmax1(1.0d0-psmu(i),5.0d0*penp(i))
          endif
 1000   continue
        call resign(nparam,neqn,psf,grdpsf,penp,g(nnineq+1),
     *              gradg(1,nnineq+1),signeq,20,12)
        fMp=fM-psf
 1400   if(nfs.eq.0) goto 1430
        nstart=nstart+1
        np=indexs(nstart,nfs)
        span(np)=fM
        do 1410 i=1,neqn
 1410     gm((np-1)*neqn+i)=g(nnineq+i)
        if(neqn.ne.0) call resign(nparam,neqn,psfnew,grdpsf,penp,
     *                            gm(1),gradg,signeq,20,10)
        fM=span(1)
        fMp=span(1)-psfnew
        mnm=min0(nstart,nfs)
        do 1420 i=2,mnm
          if(neqn.ne.0) call resign(nparam,neqn,psfnew,grdpsf,penp,
     *                           gm((i-1)*neqn+1),gradg,signeq,20,10)
          fM=dmax1(fM,span(i))
          fMp=dmax1(fMp,span(i)-psfnew)
 1420   continue
 1430 if(iprint.lt.3.or.ipyes.gt.0) goto 9000
        do 1700 i=1,nob
          if(.not.feasb) goto 1600
            if(nob.gt.1) call sbout2(io,nparam,i,'gradf(j,',')',
     *                               gradf(1,i))
            if(nob.eq.1) call sbout1(io,nparam,'gradf(j)            ',
     *                               dummy,gradf(1,i),2,2)
          goto 1700
 1600       call sbout2(io,nparam,indxob(i),'gradg(j,',')',
     *                  gradf(1,i))
 1700   continue
        if(ncnstr.eq.0) goto 1900
        do 1800 i=1,ncnstr
 1800     call sbout2(io,nparam,i,'gradg(j,',')',
     *                gradg(1,i))
        if(neqn.eq.0) goto 1900
        call sbout1(io,nparam,'grdpsf(j)           ',dummy,grdpsf,2,2)
        call sbout1(io,neqn,'P                   ',dummy,penp,2,2)
c       call sbout1(io,neqn,'psmu                ',dummy,psmu,2,2)
 1900   call sbout1(io,nparam,'multipliers  for  x ',dummy,cllamd,2,2)
        if(ncnstr.ne.0) call sbout1(io,ncnstr,'             for  g ',
     *                              dummy,cllamd(nparam+ng),2,2)
        if(nobL.gt.1) call sbout1(io,nob,'             for  f ',
     *                            dummy,cllamd(nparam+ng+ncnstr),2,2)
        do 2000 i=1,nparam
 2000     call sbout2(io,nparam,i,'hess (j,',')',hess(1,i))
 9000 return 
      end
