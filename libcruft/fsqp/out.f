      subroutine out(miter,nparam,nob,ncn,nn,neqn,ncnstr,x,g,f,fM,
     *               psf,steps,sktnom,d0norm,feasb)
c
c     FSQP Version 3.3  : output for different value of iprint
c
c     implicit real*8(a-h,o-z)
      integer miter,nparam,nob,ncn,nn,neqn,ncnstr
      double  precision fM,steps,sktnom,d0norm,psf
      double  precision x(nparam),g(1),f(1)
c     double  precision x(nparam),g(ncnstr),f(nob)
      logical feasb
c
      integer nnineq,M,ncallg,ncallf,mode,io,iprint,ipspan,ipyes,
     *        info,idum1,iter,nstop,initvl,lstype
      common  /fsqpp1/nnineq,M,ncallg,ncallf,mode,lstype,nstop,
     *        /fsqpp2/io,iprint,ipspan,ipyes,info,idum1,iter,initvl
c
      integer i
      double precision SCV,dummy,adummy(1)
c
      if(nstop.eq.0) ipyes=0
      if (iter.le.miter) goto 10
        info=3
        nstop=0
        goto 120
 10   if(iprint.eq.0.or.ipyes.gt.0) then
        iter=iter+1
        goto 9000
      endif
      if(info.gt.0) goto 120
      if(iprint.ne.1.or.nstop.eq.0) goto 20
        iter=iter+1
        if(initvl.eq.0) goto 9000
        if(feasb)
     *    call sbout1(io,nob,'objectives          ',dummy,f,2,1)
        if (mode.eq.1.and.iter.gt.1.and.feasb)
     *  call sbout1(io,0,'objective max4      ',fM,adummy,1,1)
        if(nob.gt.1) call sbout1(io,0,'objmax              ',
     *                           fM,adummy,1,1)
        if(ncnstr.eq.0) write(io,9909)
        call sbout1(io,ncnstr,'constraints         ',dummy,g,2,1)
        if(ncnstr.ne.0) write(io,9909)
        goto 9000
 20   if(iprint.eq.1.and.nstop.eq.0) write(io,9900) iter
      if(iprint.ge.2.and.nstop.eq.0.and.ipspan.ge.10) 
     *  write(io,9900) iter
      iter=iter+1
      if(initvl.eq.0) 
     *  call sbout1(io,nparam,'x                   ',dummy,x,2,1)
      call sbout1(io,nob,'objectives          ',dummy,f,2,1)
      if (mode.eq.1.and.iter.gt.1)
     *  call sbout1(io,0,'objective max4      ',fM,adummy,1,1)
      if(nob.gt.1) call sbout1(io,0,'objmax              ',
     *                          fM,adummy,1,1)
      if(ncnstr.eq.0) go to 110
      call sbout1(io,ncnstr,'constraints         ',dummy,g,2,1)
      SCV=0.d0
      do 100 i=ncn+1,ncnstr
        if(i.le.nnineq) SCV=SCV+dmax1(0.d0,g(i))
        if(i.gt.nnineq) SCV=SCV+dabs(g(i))
 100  continue
      if(initvl.eq.0)
     *  call sbout1(io,0,'SCV                 ',SCV,adummy,1,1)
 110  continue
      if(iter.le.1) write(io,9909)
      if(iter.le.1.and.ipspan.lt.10) write(io,9900) iter
      if(iter.le.1) goto 9000
      if(iprint.ge.2.and.initvl.eq.0)
     *  call sbout1(io,0,'step                ',steps,adummy,1,1)
      if(initvl.eq.0.and.(nstop.eq.0.or.info.ne.0.or.iprint.eq.2)) then
        call sbout1(io,0,'d0norm              ',d0norm,adummy,1,1)
        call sbout1(io,0,'ktnorm              ',sktnom,adummy,1,1)
      endif
      if(initvl.eq.0.and.feasb) write(io,9902) ncallf
      if(initvl.eq.0.and.(nn.ne.0.or..not.feasb)) write(io,9903) ncallg
      if(nstop.ne.0) write(io,9909)
      if(nstop.ne.0.and.iter.le.miter.and.ipspan.lt.10) 
     *  write(io,9900) iter
 120  if(nstop.ne.0.or.iprint.eq.0) goto 9000
      write(io,9909)
      write(io,9901) info
      if(info.eq.0) write(io,9904)
      if(info.eq.0.and.sktnom.gt.0.1d0) write(io,9910)
      if(info.eq.3) write(io,9905)
      if(info.eq.4) write(io,9906)
      if(info.eq.5) write(io,9907)
      if(info.eq.6) write(io,9908)
      write(io,9909)
 9000 initvl=0
      if(ipspan.ge.10) ipyes=mod(iter,ipspan)
      if(iter.le.miter) return
        nstop=0
        info=3
        write(io,9905)
      return
 9900 format(1x,9hiteration,t22,i22)
 9901 format(1x,6hinform,t22,i22)
 9902 format(1x,6hncallf,t22,i22)
 9903 format(1x,6hncallg,t22,i22)
 9904 format(1x,'Normal termination: You have obtained a solution !!')
 9905 format(1x,'Error : Maximum iterations have been reached ',
     *          'before obtaining a solution !!'/)
 9906 format(1x,'Error : Step size has been smaller than ',
     *          'the computed machine precision !!'/)
 9907 format(1x,'Error : Failure of the QP solver ',
     *          'in constructing d0 !!',
     *      /1x,'        A more robust QP solver may succeed.'/)
 9908 format(1x,'Error : Failure of the QP solver ',
     *          'in constructing d1 !!',
     *      /1x,'        A more robust QP solver may succeed.'/)
 9909 format(1x,/)
 9910 format(1x,'Warning: Norm of Kuhn-Tucker vector is large !!'/)
      end
