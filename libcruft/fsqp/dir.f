      subroutine dir(nparam,nob,nobL,nineqn,neq,neqn,nn,ncnstr,nctotl,
     *               nrowa,feasb,steps,epskt,epseqn,sktnom,scvneq,Ck,
     *               d0nm,grdftd,xl,xu,indxob,indxcn,iact,iskp,iskip,
     *               istore,iw,leniw,x,di,d,g,gradg,f,fM,fMp,psf,
     *               gradf,grdpsf,penp,a,bl,bu,clamda,cllamd,cvec,bj,
     *               hess,hess1,w,lenw,backup,signeq,obj,constr)
c
c     FSQP Version 3.3  : computation of a search direction
c
c     implicit real*8(a-h,o-z)
      integer nparam,nob,nobL,nineqn,neq,neqn,nn,ncnstr,nctotl,nrowa,
     *        iskp,leniw,lenw
      integer indxob(1),indxcn(1),iact(1),iskip(1),
     *        istore(1),iw(leniw)
c     integer indxob(nob),indxcn(ncnstr),iact(nob+nineqn+neqn),iskip(1),
c    *        istore(nineqn+nob+neqn),iw(leniw)
      double  precision steps,epskt,epseqn,sktnom,Ck,d0nm,grdftd,
     *        fM,fMp,psf,scvneq
      double  precision xl(nparam),xu(nparam),x(nparam+1),di(nparam+1),
     *        d(nparam+1),g(1),gradg(nparam,1),f(1),
     *        gradf(nparam,1),grdpsf(nparam),penp(1),
     *        a(nrowa,nparam+1),bl(1),bu(1),clamda(1),cllamd(1),
     *        cvec(nparam+1),bj(nrowa),hess(nparam,nparam),
     *        hess1(nparam+1,nparam+1),w(lenw),
     *        backup(1),signeq(1)
c     double  precision xl(nparam),xu(nparam),x(nparam+1),di(nparam+1),
c    *        d(nparam+1),g(ncnstr),gradg(nparam,ncnstr),f(nob),
c    *        gradf(nparam,nob), 
c    *        grdpsf(nparam),penp(neqn),a(nrowa,nparam+1),bl(nctotl),
c    *        bu(nctotl),clamda(nctotl+nparam+1),cllamd(nctotl),
c    *        cvec(nparam+1),bj(nrowa),hess(nparam,nparam),
c    *        hess1(nparam+1,nparam+1),w(lenw),
c    *        backup(nob+ncnstr),signeq(neqn)
      external obj,constr
      logical feasb
c
      integer nnineq,M,ncallg,ncallf,mode,io,iprint,ipspan,ipyes,info,
     *        ipd,iter,nstop,initvl,lstype
      double  precision epsmac,rteps,udelta,valnom,bigbnd,tolfea,
     *        objeps,objrep,gLgeps
      logical dlfeas,local,update,first,lqpsl,ld0
      common  /fsqpp1/nnineq,M,ncallg,ncallf,mode,lstype,nstop,
     *        /fsqpp2/io,iprint,ipspan,ipyes,info,ipd,iter,initvl,
     *        /fsqpp3/epsmac,rteps,udelta,valnom
     *        /fsqpq1/bigbnd,tolfea,
     *        /fsqplo/dlfeas,local,update,first,  
     *        /fsqpqp/lqpsl,ld0
      common  /fsqpus/objeps,objrep,gLgeps
c
c     bj(1) is equivalent to bl(nparam+3)
c
      integer i,j,k,kk,ncg,ncf,nqprm0,nclin0,nctot0,infoqp,nqprm1,ncl,
     *        nclin1,nctot1,ncc,nff,nrowa0,nrowa1,ninq,nobb,nobbL,nncn
      double  precision fmxl,vv,dx,dmx,dnm1,dnm,v0,v1,vk,temp1,temp2,
     *        theta,rhol,rhog,rho,grdfd0,grdfd1,dummy,grdgd0,grdgd1,
     *        thrshd,sign,scaprd,slope,lfuscp,dsqrt,dmin1,dmax1,dabs,
     *        adummy(1),dnmtil
      logical ltem1,ltem2
c
      ncg=0
      ncf=0
      iskp=0
      ncl=nnineq-nineqn
      local=.false.
      update=.false.
      lqpsl=.false.
      thrshd=tolfea
c
      if(nobL.eq.1) goto 10
        nqprm0=nparam+1
        nclin0=ncnstr+nobL
      goto 20
 10     nqprm0=nparam
        nclin0=ncnstr
 20   nctot0=nqprm0+nclin0
      vv=0.d0
      nrowa0=max0(nclin0,1)
      do 25 i=1,ncnstr
        if(feasb) then
          if(i.gt.nineqn.and.i.le.nnineq) iskip(nnineq+2-i)=i
          iw(i)=i
        else if(.not.feasb) then
          if(i.le.ncl) iskip(ncl+2-i)=nineqn+i
          if(i.le.ncl) iw(i)=nineqn+i
          if(i.gt.ncl) iw(i)=nineqn+neqn+i
        endif
 25   continue
      do 27 i=1,nob
 27     iw(ncnstr+i)=i
      ld0=.true.
      call nullvc(nparam,cvec)
      call dqp(nparam,nqprm0,nob,nobL,nineqn,neq,neqn,nn,ncnstr,nclin0,
     *         nctot0,nrowa0,infoqp,iw,leniw,x,di,xl,xu,feasb,f,fM,
     *         gradf,grdpsf,g,gradg,a,cvec,bl,bu,clamda,cllamd,bj,
     *         hess,hess1,di,w,lenw,vv,0)
      ld0=.false.
      if(infoqp.eq.0) goto 30
        info=5
        if(.not.feasb) info=2
        nstop=0
        goto 9000
c
c    reorder indexes of constraints and objectives
c
 30   if(nn.le.1) goto 45  
      j=1
      k=nn
      do 40 i=nn,1,-1
        if(lfuscp(cllamd(nqprm0+indxcn(i)),thrshd).ne.0) then
          iact(j)=indxcn(i)
          j=j+1
        else
          iact(k)=indxcn(i)
          k=k-1
        endif
 40   continue
 45   if(nobL.le.1) goto 60
      j=nn+1
      k=nn+nob
      do 50 i=nob,1,-1
        kk=nqprm0+ncnstr
        ltem1=lfuscp(cllamd(kk+i),thrshd).ne.0
        ltem2=nobL.ne.nob.and.(lfuscp(cllamd(kk+i+nob),thrshd).ne.0)
        if(ltem1.or.ltem2) then
          iact(j)=i
          j=j+1
        else
          iact(k)=i
          k=k-1
        endif
 50   continue
c
 60   vv=f(iact(nn+1))
      d0nm=dsqrt(scaprd(nparam,di,di))
      if(.not.first.or.nclin0.ne.0) goto 110
        dx=dsqrt(scaprd(nparam,x,x))
        dmx=dmax1(dx,1.d0)
        if(d0nm.le.dmx) goto 110
        do 100 i=1,nparam
 100      di(i)=di(i)*dmx/d0nm
        d0nm=dmx
 110  call matrvc(nparam,nparam,nparam,nparam,hess,di,w)
      if(nn.eq.0) grdftd=-scaprd(nparam,w,di)
      sktnom=dsqrt(scaprd(nparam,w,w))
      if(gLgeps.gt.0.d0.and.sktnom.le.gLgeps) goto 115
      if(d0nm.gt.epskt) goto 120
 115  if(neqn.ne.0.and.scvneq.gt.epseqn) goto 120
        nstop=0
        if(.not.feasb) info=2
        if(iprint.lt.3.or.ipyes.gt.0) goto 9000
        if(nobL.eq.1) nff=1
        if(nobL.gt.1) nff=2
        call sbout1(io,nparam,'multipliers  for  x ',dummy,cllamd,2,2)
        if(ncnstr.ne.0) call sbout1(io,ncnstr,'             for  g ',
     *                              dummy,cllamd(nparam+nff),2,2)
        if(nobL.gt.1) call sbout1(io,nob,'             for  f ',
     *                            dummy,cllamd(nparam+nff+ncnstr),2,2)
        goto 9000
 120  if(iprint.lt.3.or.ipyes.gt.0) goto 125
        call sbout1(io,nparam,'d0                  ',dummy,di,2,2)
        call sbout1(io,0,'d0norm              ',d0nm,adummy,1,2)
        call sbout1(io,0,'ktnorm              ',sktnom,adummy,1,2)
c
c     single objective without nonlinear constraints requires
c     no d1 and dtilde; multi-objectives without nonlinear 
c     constraints requires no d1
c
 125  call nullvc(nparam,w)
      if(nn.ne.0) grdftd=slope(nob,nobL,neqn,nparam,feasb,f,gradf,
     *                         grdpsf,di,w,fM,dummy,0)
      if(nn.eq.0.and.nobL.eq.1) goto 1130 
      if(nn.ne.0) goto 130
        dnm=d0nm
        rho=0.d0
        rhog=0.d0
        goto 310
c
c     compute modified first order direction d1
c
 130  nqprm1=nparam+1
      if(mode.eq.0) nclin1=ncnstr+nobL
      if(mode.eq.1) nclin1=ncnstr
      nctot1=nqprm1+nclin1
      nrowa1=max0(nclin1,1)
      ninq=nnineq
      call di1(nparam,nqprm1,nob,nobL,nineqn,neq,neqn,ncnstr,nclin1,
     *         nctot1,nrowa1,infoqp,mode,iw,leniw,x,di,xl,xu,f,fM,
     *         gradf,grdpsf,g,gradg,cvec,a,bl,bu,clamda,bj,hess1,d,
     *         w,lenw)
      if(infoqp.eq.0) goto 140
        info=6
        if(.not.feasb) info=2
        nstop=0
        goto 9000
 140  dnm1=dsqrt(scaprd(nparam,d,d))
      if(iprint.lt.3.or.ipyes.gt.0) goto 145
        call sbout1(io,nparam,'d1                  ',dummy,d,2,2)
        call sbout1(io,0,'d1norm              ',dnm1,adummy,1,2)
 145  if(mode.eq.1) goto 150
        v0=d0nm**2.1
        v1=dmax1(dble(0.5),dble(dnm1**2.5))
        rho=v0/(v0+v1)
        rhog=rho
      goto 250
 150    vk=dmin1(Ck*d0nm**2,d0nm)
        rhol=0.d0
        do 200 i=1,nn
          grdgd0=scaprd(nparam,gradg(1,indxcn(i)),di)
          grdgd1=scaprd(nparam,gradg(1,indxcn(i)),d)
          temp1=vk+g(indxcn(i))+grdgd0
          temp2=grdgd1-grdgd0
          if(temp1.le.0.d0) goto 200
          if(temp2.ge.0.d0) goto 190
          rhol=dmax1(rhol,-temp1/temp2)
          if(rhol.lt.1.d0) goto 200
 190        rhol=1.0d0
            goto 210
 200    continue
 210    theta=0.2d0
        if(rhol.ne.0.d0) goto 220
c
c       to check if rhol is reset
c
          rhog=0.d0
          rho=0.d0
          dnm=d0nm
        goto 310
 220    if(nobL.gt.1) goto 230
          grdfd0=grdftd
          grdfd1=scaprd(nparam,gradf(1,1),d)
          grdfd1=grdfd1-scaprd(nparam,grdpsf,d)
          temp1=grdfd1-grdfd0
          if(temp1.le.0.d0) then
            rhog=rhol
          else
            rhog=dmin1(rhol,(theta-1.d0)*grdfd0/temp1)
          endif
        goto 240
 230      rhog=slope(nob,nobL,neqn,nparam,feasb,f,gradf(1,1),grdpsf,
     *               di,d,fM,theta,mode)
          rhog=dmin1(rhol,rhog)
 240    rho=rhog
        if (steps.eq.1.d0.and.rhol.lt.0.5d0) rho=rhol
 250  continue
      do 300 i=1,nparam
        if (rho.ne.rhog) cvec(i)=di(i)
        di(i)=(1.d0-rho)*di(i)+rho*d(i)
 300  continue
      dnm=dsqrt(scaprd(nparam,di,di))
      if(iprint.lt.3.or.mode.eq.1.or.nn.eq.0.or.ipyes.gt.0) goto 310
        call sbout1(io,0,'rho                 ',rho,adummy,1,2)
        call sbout1(io,nparam,'d                   ',dummy,di,2,2)
        call sbout1(io,0,'dnorm               ',dnm,adummy,1,2)
 310  continue
 320  do 400 i=1,nob
 400    bl(i)=f(i)
      if (rho.eq.1.d0) goto 510
      if(nn.eq.0.or.iprint.ne.3.or.mode.eq.0.or.ipyes.gt.0) goto 410
        call sbout1(io,0,'Ck                  ',Ck,adummy,1,2)
        call sbout1(io,0,'rhol                ',rho,adummy,1,2)
        call sbout1(io,nparam,'dl                  ',dummy,di,2,2)
        call sbout1(io,0,'dlnorm              ',dnm,adummy,1,2)
 410  if(mode.eq.0) goto 510
        local=.true.
        call step(nparam,nob,nobL,nineqn,neq,neqn,nn,ncnstr,ncg,ncf,
     *            indxob,indxcn,iact,iskp,iskip,istore,feasb,grdftd,
     *            f,fM,fMp,psf,penp,steps,scvneq,bu,x,di,d,g,w,
     *            backup,signeq,obj,constr)
        if(update) goto 9000
        local=.false.
        if(rho.eq.rhog.or.nn.eq.0) goto 510
        do 500 i=1,nparam
 500      di(i)=(1-rhog)*cvec(i)+rhog*d(i)
        dnm=dsqrt(scaprd(nparam,di,di))
 510  if (nn.eq.0.or.iprint.lt.3.or.mode.eq.0.or.ipyes.gt.0) goto 520
        call sbout1(io,0,'rhog                ',rhog,adummy,1,2)
        call sbout1(io,nparam,'dg                  ',dummy,di,2,2)
        call sbout1(io,0,'dgnorm              ',dnm,adummy,1,2)
 520  if(rho.ne.0.d0) grdftd=slope(nob,nobL,neqn,nparam,feasb,bl,
     *                             gradf,grdpsf,di,d,fM,theta,0)
      if(mode.eq.1.and.rho.eq.rhog) goto 610
      do 600 i=1,nparam
 600    bu(i)=x(i)+di(i)
 610  if(rho.ne.rhog) ncg=0
      ncc=ncg+1
      fmxl=-bigbnd
      ninq=ncg
      nncn=ncg
      j=0
c
c     iskip(1) --- iskip(iskp) store the indexes of linear inequality
c     constraints that are not to be used to compute d~
c     iskip(nnineq-nineqn+1) --- iskip(nnineq-ncn+1-iskp) store those 
c     that are to be used to compute d~
c
      do 700 i=ncc,ncnstr
        kk=iact(i)
        if(i.gt.nn) kk=indxcn(i)
        if(kk.le.nineqn.or.kk.gt.nnineq) goto 615
          iskip(ncl+1-j)=kk
          j=j+1
 615    if(kk.gt.nnineq) goto 617
        temp1=-0.2d0*(dnm*dsqrt(scaprd(nparam,gradg(1,kk),gradg(1,kk))))
        temp2=cllamd(nqprm0+kk)
        if(temp2.eq.0.d0.and.g(kk).lt.temp1) goto 620
 617      ninq=ninq+1
          iw(ninq)=kk
          if(feasb.and.kk.le.nineqn) istore(kk)=1
          call constr(nparam,kk,bu,g(kk))
          if(.not.feasb.or.feasb.and.kk.gt.nnineq) goto 700
          if(kk.le.nineqn) nncn=ninq
          fmxl=dmax1(fmxl,g(kk))
          if(.not.feasb) goto 618
          if(kk.le.nineqn.or.kk.gt.nnineq.and.kk.le.(nnineq+neqn))
     *       ncallg=ncallg+1
 618      if(dabs(fmxl).gt.bigbnd) goto 1130
        goto 700
 620      if(kk.le.nineqn) goto 700
          iskp=iskp+1
          iskip(iskp)=kk
          j=j-1
 700  continue
      if(neqn.ne.0) call resign(nparam,neqn,psf,grdpsf,penp,g(nnineq+1),
     *                          gradg(1,nnineq+1),signeq,10,20)
      ninq=ninq-neq
      if(ncg.eq.0) goto 810
      do 800 i=1,ncg
        iw(i)=iact(i)
        istore(iact(i))=1
        fmxl=dmax1(fmxl,g(iact(i)))
        if(dabs(fmxl).gt.bigbnd) goto 1130
 800  continue
 810  if(nobL.gt.1) goto 820
        iw(1+ninq+neq)=1
        nobb=nob
        goto 1110
 820  if(rho.ne.rhog) ncf=0
      nff=ncf+1
      nobb=ncf
      sign=1.d0
      fmxl=-bigbnd
      if(cllamd(nqprm0+ncnstr+iact(nn+1)).lt.0.d0) sign=-1.d0
      do 1000 i=nff,nob
        kk=iact(nn+i)
        if(.not.feasb) kk=iact(i)
        if(feasb) k=nn+1
        if(.not.feasb) k=1
        do 900 j=1,nparam
 900      w(j)=sign*gradf(j,iact(k))-gradf(j,kk)
        temp1=dabs(f(kk)-sign*vv)
        temp2=dnm*dsqrt(scaprd(nparam,w,w))
        if(temp1.eq.0.d0.or.temp2.eq.0.d0) goto 910
        temp1=temp1/temp2
        temp2=cllamd(nqprm0+ncnstr+kk)
        if(temp2.eq.0.d0.and.temp1.gt.0.2d0) goto 1000
 910    nobb=nobb+1
        if(feasb) then
          iw(nobb+ninq+neq)=kk
          istore(nineqn+kk)=1
        else
          iw(nobb+ninq)=kk
          istore(kk)=1
        endif
        if(.not.feasb) goto 920
          call obj(nparam,kk,bu,f(kk))
          ncallf=ncallf+1
          if(nobL.ne.nob) fmxl=dmax1(fmxl,-f(kk))
        goto 930
 920      call constr(nparam,indxob(kk),bu,f(kk))
          ncallg=ncallg+1
 930    fmxl=dmax1(fmxl,f(kk))
        if(dabs(fmxl).gt.bigbnd) goto 1130
 1000 continue
      if(ncf.eq.0) goto 1110
      do 1100 i=1,ncf
        iw(ninq+neq+i)=iact(i+nn)
        istore(nineqn+iact(i+nn))=1
        fmxl=dmax1(fmxl,f(iact(i+nn)))
        if(nobL.ne.nob) fmxl=dmax1(fmxl,-f(iact(i+nn)))
        if(dabs(fmxl).gt.bigbnd) goto 1130
 1100 continue
 1110 call matrvc(nparam,nparam,nparam,nparam,hess,di,cvec)
      vv=-dmin1(0.01d0*dnm,dnm**2.5)
c
c     compute a correction dtilde to d=(1-rho)d0+rho*d1
c
      if(nobL.ne.nob) nobbL=2*nobb
      if(nobL.eq.nob) nobbL=nobb
      if(nobbL.eq.1) goto 1115
        nqprm0=nparam+1
        nclin0=ninq+neq+nobbL
      goto 1117
 1115   nqprm0=nparam
        nclin0=ninq+neq
 1117 nctot0=nqprm0+nclin0
      nrowa0=max0(nclin0,1)
      i=ninq+neq
      call dqp(nparam,nqprm0,nobb,nobbL,nncn,neq,neqn,nn,i,nclin0,
     *         nctot0,nrowa0,infoqp,iw,leniw,x,di,xl,xu,feasb,f,fmxl,
     *         gradf,grdpsf,g,gradg,a,cvec,bl,bu,clamda,cllamd,bj,
     *         hess,hess1,d,w,lenw,vv,1)
      if(infoqp.ne.0) goto 1130
      dnmtil=dsqrt(scaprd(nparam,d,d))
      if(dnmtil.gt.dnm) goto 1130
      if(dnmtil.eq.0.d0) goto 1119
        do 1118 i=1,nineqn+nob
 1118     istore(i)=0
 1119 if(iprint.lt.3.or.ipyes.gt.0) goto 9000
        call sbout1(io,nparam,'dtilde              ',dummy,d,2,2)
        call sbout1(io,0,'dtnorm              ',dnmtil,adummy,1,2)
        goto 9000
c
 1130 do 1200 i=1,nparam
 1200   d(i)=0.d0
      dnmtil=0.d0
 9000 return
      end
