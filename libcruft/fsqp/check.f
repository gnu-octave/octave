      subroutine check(nparam,nf,Linfty,nAD,nineq,nnl,neq,neqn,
     *                 mode,modem,lstype,eps,bigbnd,bl,bu)
c
c     FSQP Version 3.3  : check input data
c
c     implicit real*8(a-h,o-z)
      integer nparam,nf,nineq,nnl,neq,neqn,mode,modem,lstype
      double  precision bigbnd,eps
      double  precision bl(nparam),bu(nparam)
      logical Linfty,nAD
c
      integer io,iprint,ipspan,ipyes,info,idum1,idum2,idum3
      double  precision epsmac,dummy1,dummy2,dummy3
      common  /fsqpp2/io,iprint,ipspan,ipyes,info,idum1,idum2,idum3,
     *        /fsqpp3/epsmac,dummy1,dummy2,dummy3
c
      integer i
      double  precision bli,bui
c
      if (nparam.le.0)  
     *  call error('nparam should be positive!              ',info,io)
      if (nf.lt.0)     
     *  call error('nf     should not be negative!          ',info,io)
      if (nnl.lt.0)     
     *  call error('nineqn should not be negative!          ',info,io)
      if (nineq.lt.nnl) 
     *  call error('nineq  should be no smaller than nineqn!',info,io)
      if (neqn.lt.0)  
     *  call error('neqn   should not be negative!          ',info,io)
      if (neq.lt.neqn)  
     *  call error('neq    should not be smaller than neqn  ',info,io)
      if (nparam.le.neq)  
     *  call error('FSQPD deals with nparam larger than neq ',info,io)
      if (iprint.lt.0.or.iprint.gt.3)     
     *  call error('iprint is not a valid number            ',info,io)
      if (eps.gt.epsmac) goto 10
      call error('eps    should be bigger than epsmac!    ',info,io)
      write(io,9902) epsmac
 10   if(mode.ge.200) then
        lstype=2
        mode=mode-100
      else
        lstype=1
      endif
      if (.not.(mode.eq.100.or.mode.eq.101.or.
     *          mode.eq.110.or.mode.eq.111))
     *  call error('mode   is not properly specified!       ',info,io)
      if (info.eq.0) goto 20
      write(io,9903)
      goto 9000
c
 20   do 30 i=1,nparam
        bli=bl(i)
        bui=bu(i)
        if (bli.le.bui) goto 25
        write(io,9901)
        info=7
 25     if (info.ne.0) goto 9000
        if (bli.lt.(-bigbnd)) bl(i)=-bigbnd
        if (bui.gt.bigbnd)    bu(i)=bigbnd
 30   continue
c
      i=mode-100
      if(i.lt.10) then
        modem=0
      else 
        modem=1
        i=i-10
      endif
      if(i.eq.0) Linfty=.false.
      if(i.eq.1) Linfty=.true.
c
 9000 return
 9901 format(1x,'lower bounds should be smaller than upper bounds',/)
 9902 format(1x,'epsmac = ',e22.14,' which is machine dependent',/)
 9903 format(1x,'Error: Input parameters are not consistent',/)
      end
