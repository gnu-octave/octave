      subroutine step(nparam,nob,nobL,nineqn,neq,neqn,nn,ncnstr,ncg,
     *                ncf,indxob,indxcn,iact,iskp,iskip,istore,feasb,
     *                grdftd,f,fM,fMp,psf,penp,steps,scvneq,xnew,
     *                x,di,d,g,w,backup,signeq,obj,constr)
c
c     FSQP Version 3.3  : Armijo or nonmonotone line search, with
c                         some ad hoc strategies to decrease the number
c                         of function evaluation as much as possible
c
c     implicit real*8(a-h,o-z)
      integer nparam,nob,nobL,nineqn,neq,neqn,nn,ncnstr,ncg,ncf,iskp
      integer indxob(1),indxcn(1),iact(1),iskip(1),
     *        istore(1)
c     integer indxob(nob),indxcn(ncnstr),iact(nn+nob),iskip(4),
c    *        istore(nineqn+nob)
      double  precision grdftd,fM,fMp,steps,scvneq,psf
      double  precision xnew(nparam),x(nparam),di(nparam),d(nparam),
     *        f(1),penp(1),g(1),w(1),backup(1),
     *        signeq(1)
c     double  precision xnew(nparam),x(nparam),di(nparam),d(nparam),
c    *        f(nob),penp(neqn),g(ncnstr),w(1),backup(nob+ncnstr),
c    *        signeq(neqn)
      external obj,constr
      logical feasb
c
      integer nnineq,M,ncallg,ncallf,mode,io,iprint,ipspan,ipyes,info,
     *        idum1,idum2,idum3,nstop,lstype
      double  precision epsmac,bigbnd,tolfea,dum1,dum2,dum3
      logical lqpsl,ldummy,dlfeas,local,update,ldumm2
      common  /fsqpp1/nnineq,M,ncallg,ncallf,mode,lstype,nstop,
     *        /fsqpp2/io,iprint,ipspan,ipyes,info,idum1,idum2,idum3,
     *        /fsqpp3/epsmac,dum1,dum2,dum3,
     *        /fsqpq1/bigbnd,tolfea,
     *        /fsqplo/dlfeas,local,update,ldumm2,
     *        /fsqpqp/lqpsl,ldummy
c
      integer i,ii,ij,itry,ikeep,j,job,nlin,mnm
      double  precision prod1,prod,dummy,fmaxl,tolfe,dmax1,ostep,
     *                  adummy(1),temp
      logical ltem1,ltem2,reform,fbind,cdone,fdone,eqdone
c
      nlin=nnineq-nineqn
      ii=1
      itry=1
      steps=1.d0
      ostep=steps
      fbind=.false.
      cdone=.false.
      fdone=.false.
      reform=.true.
      eqdone=.false.
      if(local) dlfeas=.false.
      ikeep=nlin-iskp
      prod1=0.1d0*grdftd
      tolfe=0.d0
      if(lqpsl) tolfe=tolfea
      if(iprint.ge.3.and.ipyes.eq.0)
     *  call sbout1(io,0,'directional deriv   ',grdftd,adummy,1,2)
c
      w(1)=fM
 100  continue
        if(iprint.ge.3.and.ipyes.eq.0) 
     *    write(io,9901) itry
        prod=prod1*steps
        if(.not.feasb.or.nobL.gt.1) prod=prod+tolfe
        do 200 i=1,nparam
          if(local)      xnew(i)=x(i)+steps*di(i)
          if(.not.local) xnew(i)=x(i)+steps*di(i)+d(i)*steps**2
 200    continue
        if(iprint.lt.3.or.ipyes.gt.0) goto 205
          call sbout1(io,0,'trial step          ',steps,adummy,1,2)
          call sbout1(io,nparam,'trial point         ',
     *                dummy,xnew,2,2)
 205    if(iskp.eq.0) goto 209
          ostep=steps
          do 207 i=ii,iskp
            ij=iskip(i)
            call constr(nparam,ij,xnew,g(ij))
            if(iprint.lt.3.or.ipyes.gt.0) goto 206
              if(i.eq.1) write(io,9900) ij,g(ij)
              if(i.ne.1) write(io,9902) ij,g(ij)
 206        if(g(ij).le.tolfe) goto 207
            ii=i
            goto 1120
 207      continue
          iskp=0
 209    if(nn.eq.0) goto 310
        if(.not.local.and.fbind) goto 315
 210    continue
        do 300 i=1,nn
          ncg=i
          ii=iact(i)
          ij=nnineq+neqn
          if(ii.le.nnineq.and.istore(ii).eq.1) goto 215
          if(ii.gt.nnineq.and.ii.le.ij.and.eqdone) goto 215
            temp=1.d0
            if(ii.gt.nnineq.and.ii.le.ij) temp=signeq(ii-nnineq)
            call constr(nparam,ii,xnew,g(ii))
            g(ii)=g(ii)*temp
            ncallg=ncallg+1
 215      if(iprint.lt.3.or.ipyes.gt.0) goto 220
            if(i.eq.1.and.ikeep.eq.nlin) 
     *        write(io,9900) ii,g(ii)
            if(i.ne.1.or.ikeep.ne.nlin) write(io,9902) ii,g(ii)
 220      if(local.or.g(ii).le.tolfe) goto 230
            call shift(nn,ii,iact)
            goto 1110
 230      if(local.and.g(ii).gt.tolfe) goto 1500
 300    continue
 310    cdone=.true.
        eqdone=.true.
        if(local) dlfeas=.true.
 315    if(fdone) goto 410
        fmaxl=-bigbnd
        do 400 i=1,nob
          ncf=i
          ii=iact(nn+i)
          if(feasb) then
            if(eqdone.or.neqn.eq.0) goto 317
              do 316 j=1,neqn
 316            call constr(nparam,nnineq+j,xnew,g(nnineq+j))
              ncallg=ncallg+neqn
 317        if(neqn.eq.0) goto 318
              if(eqdone)      job=20
              if(.not.eqdone) job=10
              call resign(nparam,neqn,psf,w(2),penp,
     *                    g(nnineq+1),w(2),signeq,job,10)
 318        if(istore(nineqn+ii).eq.1) goto 320
              call obj(nparam,ii,xnew,f(ii))
              ncallf=ncallf+1
 320        if(i.eq.1.and.iprint.ge.3.and.ipyes.eq.0) 
     *        write(io,9903) ii,f(ii)-psf
            if(i.ne.1.and.iprint.ge.3.and.ipyes.eq.0) 
     *        write(io,9902) ii,f(ii)-psf
          else
            if(istore(ii).eq.1) goto 325
              call constr(nparam,indxob(ii),xnew,f(ii))
              ncallg=ncallg+1
 325        if(f(ii).gt.tolfe) reform=.false.
            if(i.eq.1.and.iprint.ge.3.and.ipyes.eq.0) 
     *        write(io,9903) indxob(ii),f(ii)
            if(i.ne.1.and.iprint.ge.3.and.ipyes.eq.0) 
     *        write(io,9902) indxob(ii),f(ii)
          endif
          fmaxl=dmax1(fmaxl,f(ii))
          if(nobL.ne.nob) fmaxl=dmax1(fmaxl,-f(ii))
          if(.not.feasb.and.reform) goto 400
          if(local) goto 340
          if((f(ii)-psf).le.(fMp+prod)) goto 330
            fbind=.true.
            call shift(nob,ii,iact(nn+1))
          goto 1110
 330      if(nobL.eq.nob.or.(-f(ii)-psf).le.(fMp+prod)) goto 400
            fbind=.true.
            call shift(nob,ii,iact(nn+1))
          goto 1110
 340      ltem1=(f(ii)-psf).gt.(fMp+prod)
          ltem2=nobL.ne.nob.and.(-f(ii)-psf).gt.(fMp+prod)
          if(ltem1.or.ltem2) goto 1500
 400    continue
        fbind=.false.
        fdone=.true.
        eqdone=.true.
        if(.not.cdone) goto 210
 410    if(ostep.eq.steps) mnm=ikeep+neq-neqn
        if(ostep.ne.steps) mnm=ncnstr-nn
        do 500 i=1,mnm
          ii=indxcn(i+nn)
          if(ikeep.ne.nlin.and.ostep.eq.steps.and.i.le.nlin) 
     *       ii=iskip(nlin+2-i)
          call constr(nparam,ii,xnew,g(ii))
 500    continue
        scvneq=0.d0
        do 600 i=1,ncnstr
          if(i.gt.nnineq.and.i.le.(nnineq+neqn)) scvneq=scvneq-g(i)
 600      backup(i)=g(i)
        do 700 i=1,nob
 700      backup(i+ncnstr)=f(i)
        if(feasb.or..not.reform) goto 810
          do 800 i=1,nparam
 800        x(i)=xnew(i)
          nstop=0
          goto 1500
 810    if(local) ncg=ncnstr
        if(local) update=.true.
        fM=fmaxl
        fMp=fmaxl-psf
        do 1000 i=1,nn
 1000     iact(i)=indxcn(i)
        do 1100 i=1,nob
 1100     iact(nn+i)=i
        goto 1500
c
 1110   cdone=.false.
        fdone=.false.
        eqdone=.false.
        reform=.false.
        if(lstype.eq.2) fbind=.false.
 1120   itry=itry+1
        if(steps.lt.1.d0) goto 1140
        do 1130 i=1,nob+nineqn
 1130     istore(i)=0
 1140   steps=steps*.5d0
        if(steps.lt.epsmac) goto 1150
      goto 100
c
 1150 info=4
      nstop=0
 1500 if(steps.lt.1.d0) goto 9000
        do 1600 i=1,nob+nineqn
 1600     istore(i)=0
 9000 return
 9900 format(1x,t17,17htrial constraints,t37,i7,t45,e22.14)
 9901 format(1x,t17,12htrial number,t45,i22)
 9902 format(1x,t37,i7,t45,e22.14)
 9903 format(1x,t17,16htrial objectives,t37,i7,t45,e22.14)
      end
