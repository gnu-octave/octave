c  THIS SOFTWARE MAY NOT BE COPIED TO MACHINES OUTSIDE THE SITE FOR
c  WHICH IT HAD BEEN PROVIDED.  SEE "Conditions for External Use"
c  BELOW FOR MORE DETAILS.  INDIVIDUALS INTERESTED IN OBTAINING
c  THE SOFTWARE SHOULD CONTACT THE AUTHORS.
c
      subroutine FSQPD(nparam,nf,nineqn,nineq,neqn,neq,mode,iprint,
     *                 miter,inform,bigbnd,eps,epseqn,udelta,bl,bu,x,
     *                 f,g,iw,iwsize,w,nwsize,obj,constr,gradob,gradcn)
c                                                                      
c     implicit real*8(a-h,o-z)
      integer nparam,nf,neqn,nineqn,nineq,neq,mode,iprint,miter,inform,
     *        iwsize,nwsize
      integer iw(iwsize)
      double  precision bl(nparam),bu(nparam),x(nparam),
     *        f(1),g(1),w(nwsize)
c     double  precision bl(nparam),bu(nparam),x(nparam),
c    *        f(nf),g(nineq+neq),w(nwsize)
      double  precision bigbnd,eps,epseqn,udelta
      external obj,constr,gradob,gradcn
c
c**********************************************************************c
c                                                                      c
c brief specification of various arrays and parameters in the calling  c
c sequence. See manual for more detailed description.                  c
c                                                                      c
c nparam : number of variables                                         c
c nf     : number of objective functions                               c
c nineqn : number of nonlinear inequality constraints                  c
c nineq  : number of inequality constraints                            c
c neqn   : number of nonlinear equality constraints                    c
c neq    : number of equality constraints                              c
c mode   : mode=CBA specifies job options as described below:          c
c          A = 0 : ordinary minimax problems                           c
c            = 1 : ordinary minimax problems with each individual      c
c                  function replaced by its absolute value, ie,        c
c                  an L_infty problem                                  c
c          B = 0 : monotone decrease of objective function             c
c                  after each iteration                                c
c            = 1 : monotone decrease of objective function after       c
c                  at most four iterations                             c
c          C = 1 : during line search, the function that rejected      c
c                  the previous step size is checked first;            c
c                  all functions of the same type ("objective" or      c
c                  "constraints") as the latter will then be checked   c
c                  first                                               c
c          C = 2 : all contraints will be checked first at every trial c
c                  point during the line search                        c
c iprint : print level indicator with the following options            c
c          iprint=0: no normal output except error information         c
c                    (this option is imposed during phase 1)           c
c          iprint=1:  a final printout at a local solution             c
c          iprint=2:  a brief printout at the end of each iteration    c
c          iprint=3:  detailed infomation is printed out at the end    c
c                     of each iteration for debugging purpose          c
c          iprint=10*N+M: N any positive integer, M=2 or 3.            c
c                     Information corresponding to iprint=M will be    c
c                     displayed at every 10*Nth iterations at the last c
c                     iteration                                        c
c miter  : maximum number of iterations allowed by the user to solve   c
c          the problem                                                 c
c inform : status report at the end of execution                       c
c          inform= 0:normal termination                                c
c          inform= 1:no feasible point found for linear constraints    c
c          inform= 2:no feasible point found for nonlinear constraints c
c          inform= 3:no solution has been found within miter iterations
c          inform= 4:stepsize is smaller than machine precision before c
c                    a successful new iterate is found                 c
c          inform= 5:failure of the QP solver in attempting to         c
c                    construct d0. A more robust QP solver may succeed.c
c          inform= 6:failure of the QP solver in attempting to         c
c                    construct d1. A more robust QP solver may succeed.c
c          inform= 7:inconsistent input data                           c
c bigbnd : plus infinity                                               c
c eps    : stopping criterion that ensures at a solution, the norm of  c
c          the Newton direction vector is smaller than eps             c
c epseqn : tolerance of the violation of nonlinear equality constraintsc
c          allowed by the user at an optimal solution                  c
c udelta : perturbation size in computing gradients by finite          c
c          difference and the true perturbation is determined by       c
c          sign(x_i) X max{udelta, rteps X max{1, |x_i|}} for each     c
c          component of x, where rteps is the square root of machine   c
c          precision 
c bl     : array of dimension nparam,containing lower bound of x       c
c bu     : array of dimension nparam,containing upper bound of x       c
c x      : array of dimension nparam,containing initial guess in input c
c          and final iterate at the end of execution                   c
c f      : array of dimension max{1,nf}, containing objective values   c
c          at x in output                                              c
c g      : array of dimension max{1,nineq+neq}, containing constraint  c
c          values at x in output                                       c
c iw     : integer working space of dimension iwsize                   c
c iwsize : length of integer array iw                                  c
c w      : double precision working space of dimension nwsize.         c
c          at output, it contains lagrange multipliers                 c
c nwsize : length of double precision array w                          c
c obj    : subroutine that returns the value of objective functions    c
c          one upon each call                                          c
c constr : subroutine that returns the value of constraints            c
c          one upon each call                                          c
c gradob : subroutine that computes gradients of f, alternatively      c
c          it can be replaced by grobfd that computes finite           c
c          difference approximations                                   c
c gradcn : subroutine that computes gradients of g, alternatively      c
c          it can be replaced by grcnfd that computes finite           c
c          difference approximations                                   c
c                                                                      c
cCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCc
c                                                                      c
c                                                                      c
c                        FSQP  Version 3.3                             c
c                                                                      c
c                  Jian L. Zhou  and  Andre L. Tits                    c
c                   Institute for Systems Research                     c
c                               and                                    c
c                Electrical Engineering Department                     c
c                     University of Maryland                           c
c                     College Park, Md 20742                           c
c                                                                      c
c                           April, 1993                                c
c                                                                      c
c                                                                      c
c  The purpose of FSQP is to solve general nonlinear constrained       c
c  minimax optimization problems of the form                           c
c                                                                      c
c   (A=0 in mode)     minimize    max_i f_i(x)   for i=1,...,n_f       c
c                        or                                            c
c   (A=1 in mode)     minimize    max_j |f_i(x)|   for i=1,...,n_f     c
c                       s.t.      bl   <= x <=  bu                     c
c                                 g_j(x) <= 0,   for j=1,...,nineqn    c
c                                 A_1 x - B_1 <= 0                     c
c                                                                      c
c                                 h_i(x)  = 0,   for i=1,...,neqn      c
c                                 A_2 x - B_2  = 0                     c
c                                                                      c
c                                                                      c
c                                                                      c
c                  Conditions for External Use                         c
c                  ===========================                         c
c                                                                      c
c   1. The FSQP routines may not be distributed to third parties.      c
c      Interested parties should contact the authors directly.         c
c   2. If modifications are performaed on the routines, these          c
c      modifications will remain the sole property of the authors.     c
c   3. Due acknowledgment must be made of the use of the FSQP routines c
c      in research reports or publications. A copy of such reports or  c
c      publications should be forwarded to the authors.                c
c   4. The FSQP routines may not be used in industrial production,     c
c      unless this has been agreed upon with the authors in writing.   c
c                                                                      c
c Copyright (c) 1989 --- 1993 by Jian L. Zhou and Andre L. Tits.       c
c All Rights Reserved.                                                 c
c                                                                      c
c                                                                      c
c Enquiries should be directed to:                                     c
c                                                                      c
c      Prof. Andre L. Tits                                             c
c      Electrical Engineering Dept.                                    c
c      and Institute for Systems Research                              c
c      University of Maryland                                          c
c      College Park, Md 20742                                          c
c      U. S. A.                                                        c
c                                                                      c
c      Phone : 301-405-3669                                            c
c      Fax   : 301-405-6707                                            c
c      E-mail: andre@eng.umd.edu                                       c
c                                                                      c
c                                                                      c
c              Enhancements in successive versions of FSQP             c
c              ===========================================             c
c                                                                      c
c  Version 3.3 : April 1993                                            c
c     1. If the user so requests (via "mode"), during the line search, c
c        FSQP will now evaluate objectives only after having           c
c        determined that all constraints are satisfied.  This is of    c
c        value when some objective functions are not defined outside   c
c        the feasible set.                                             c
c     2. The reserved common block "fsqpst" is no longer used by FSQP. c
c        Instead, a new reserved common block "fsqpus" is provided to  c
c        give the users a choice of several possible stopping criteria.c
c        (As a side-effect, the user is not allowed any more to have   c
c        his/her own block data; see Section 4 of the manual for       c
c        details.)                                                     c
c     3. Some imperfections are fixed (e.g., comparision of double     c
c        precision number to hard zero, and incorrect checking of      c
c        value of "mode").                                             c
c                                                                      c
c  Version 3.2 : March 1993                                            c 
c     1. The user is given the option to print output at every Nth     c
c        iteration and at the end, where N is a multiple of 10.        c
c                                                                      c
c  Version 3.1a : January 1993                                         c 
c     1. Bugs are fixed. This has to do with finding a feasible point  c
c        (with the help of Yaguang Yang). There should be no effect    c
c        if the user's problem does not contain both nonlinear and     c
c        linear equality constraints.                                  c
c                                                                      c
c  Version 3.1 : November 1992                                         c 
c     1. Possible division by zero is avoided.                         c
c     2. Objective and constraint values values at initial feasible    c
c        point is printed out if iprint >=1.                           c
c     3. Estimates of Lagrange multipliers are made available on outputc
c        even when execution is terminated abnormally in phase 2.      c
c     4. Incorrect descriptions of nineq, neq, iwsize and nwsize in thec
c        user's manual and in the comments in fsqpd.f are corrected.   c
c                                                                      c
c  Version 3.0d : October 1992                                         c 
c     1. Some imperfections (identified by WATFOR) are cleaned up.     c
c     2. Erroneous declaration of dummy argument in sampl*.f's         c
c        are corrected.                                                c
c                                                                      c
c  Version 3.0c : September 1992                                       c 
c     1. A bug in identifying active set of objectives fixed.          c 
c        (Thanks go to Yaguang Yang.)                                  c
c     2. Some imperfections (identified by WATFOR) are cleaned up.     c
c        (Thanks go to Jaroslav Dolezal and Jiri Fidler                c
c        at CZ Academy of Sciences.)                                   c
c                                                                      c
c  Version 3.0b : August 1992                                          c 
c     1. A bug in assigning iskip(*) is fixed. This has to do with     c
c        finding a feasible point.                                     c
c     2. Other bugs associated with nonlinear equality constraints     c 
c        are fixed. The effect is on nonmonotone line search.          c
c        (Thanks go to Yaguang Yang at Institute for Systems Research  c
c         the University of Maryland at College Park.)                 c
c                                                                      c
c                                                                      c
c  Version 3.0a : June 1992                                            c 
c     1. A bug in check.f is fixed and a typo is corrected.            c
c     2. A bug in initpt.f is fixed.                                   c
c     3. Printout message is adjusted for various situations.          c
c     4. Computation of initial equality constraint violation is       c
c        corrected.  (Thanks go to Jaroslav Dolezal and Jiri Fidler    c
c         at CZ Academy of Sciences)                                   c
c     5. An output error for function values is corrected.             c
c                                                                      c
c  Version 3.0 : June 1992                                             c
c     1. FSQP now also handles nonlinear equality constraints.         c
c        "Semi-feasibility" for these constraints is maintained in     c
c        the following sense: given a scalar constraint h(x)=0,        c
c        if h(x0)<=0 (resp. >=0), then h(xk)<=0 (resp. >=0) for all k. c
c     2. An option is added to allow users to have their own stopping  c
c        criterion.                                                    c
c     3. The interface for QPSOL is no longer part of the standard     c
c        distribution (but it is still available on request).          c
c     4. Objective and constraints now must be provided in Fortran     c
c        "subroutines" rather than "functions".                        c
c     5. Concerning the default stopping criterion, the norm           c
c        requirement on the Kuhn-Tucker vector is replaced by a norm   c
c        requirement on Newton direction.                              c
c     6. The meaning of "mode" is redefined to encompass several       c
c        attributes.                                                   c
c     7. The argument list to call FSQPD is modified.                  c
c     8. The Hessian matrix is reset to the identity whenever          c
c        the line search fails to complete after a specified number    c
c        of step reductions, provided the last reset occurred at least c
c        5*nparam iterations earlier (it used to be 1*nparam).         c
c                                                                      c
c  Version 2.4b : November 1991                                        c
c     1. Bugs are fixed that affected the computation of a feasible    c
c        point and the initialization of iskp.  (Thanks go to          c
c        Klaus Schittkowski at U Bayreuth and John Hauser at USC.)     c
c                                                                      c
c  Version 2.4a : November 1991                                        c
c     1. A bug is fixed that affected the multipliers given on output. c
c     2. A few unused statements are commented out.                    c
c     3. small() is modified to avoid too small a number on machines   c
c        that use extra-length registers for internal computations     c
c        (with the help of Roque Donizete de Oliveira at Michigan).    c
c                                                                      c
c  Version 2.4 : October 1991                                          c
c     1. The Hessian matrix is reset to the identity whenever          c
c        the line search fails to complete after a specified number    c
c        of step reductions, provided the last reset occurred at least c
c        nparam iterations earlier.                                    c
c                                                                      c
c  Version 2.3b : September 1991                                       c
c     1. A bug is fixed in the reordering of active functions.         c
c                                                                      c
c  Version 2.3a : September 1991                                       c
c     1. A bug is fixed in the reordering of active functions.         c
c                                                                      c
c  Version 2.3  : July 1991                                            c
c     1. Lagrange multipliers at the solution point are provided on    c
c        output.                                                       c
c     2. Bugs are fixed and code is adapted to be accepted by          c
c        some "tough" compilers (with the help of K. Schittkowski).    c
c                                                                      c
c  Version 2.2  : June 1991                                            c
c     1. In computing d~, only the most "active" constraints and       c
c        objectives are taken into account, thus reducing the          c
c        number of function evaluations.                               c
c     2. Refinements of nonmonotone line search are implemented        c
c        for minimax problems without nonlinear constraints.           c
c     3. Line search is more efficient.                                c
c     4. A bug is fixed in the computation of d~ in mode=1*.           c
c     5. The calling sequences of gradcn and gradob are simplified.    c
c                                                                      c
c  Version  2.1  : April 1991                                          c
c     1. FSQP can use either of two quadratic programming codes:       c
c        QPSOL or QLD.                                                 c
c     2. Reorder constraints and objectives to enable more efficient   c
c        line search.                                                  c
c                                                                      c
c  Version 2.0B : March 1991: Bugs are fixed                           c
c  Version 2.0A : October 1990: Bugs are fixed                         c
c  Version 2.0  : August 1990                                          c
c     1. Extension to the solution of constrained minimax problems.    c
c                                                                      c
c  Version 1.0B : June 1990: Bugs are fixed                            c
c  Version 1.0A : December  1989: Bugs are fixed                       c
c  Version 1.0  : August 1989                                          c
c                                                                      c
c  References:                                                         c
c  [1] E. Panier and A. Tits, `On Combining Feasibility, Descent and   c
c      Superlinear Convergence In Inequality Constrained Optimization',c
c      Mathematical Programming 59(1993), 261-276.                     c
c  [2] J. F. Bonnans, E. Panier, A. Tits and J. Zhou, `Avoiding the    c
c      Maratos Effect by Means of a Nonmonotone Line search: II.       c
c      Inequality Problems - Feasible Iterates', SIAM J. Numer. Anal.  c
c      29(1992), 1187-1202.                                            c
c  [3] J.L. Zhou and A. Tits, `Nonmonotone Line Search for Minimax     c
c      Problems', J. Optim. Theory Appl.76(1993), 455-476.             c
c  [4] J.L. Zhou and A. Tits, `User's Guide for FSQP Version 3.3:      c
c      A Fortran Code for Solving Optimization Programs, Possibly      c
c      Minimax,with General Inequality Constraints and Linear Equality c
c      Constraints, Generating Feasible Iterates', Institute for       c
c      Systems Research, University of Maryland,Technical Report       c
c      SRC-TR-92-107r3, College Park, MD 20742, 1993.                  c
c  [5] D.Q. Mayne and E. Polak, `Feasible Directions Algorithms for    c
c      Optimization Problems with Equality and Inequality Constraints',c
c      Mathematical Programming 11(1976)                               c
c                                                                      c
cCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCc
c
      integer i,io,ipp,iter,j,ncallg,ncallf,ncnstr,nclin,nctotl,leniw,
     *        lenw,nwx,nwbl,nwbu,nwgrg,nwgpf,nwpenp,nwa,nwcvec,nwhess,
     *        nwcla,nww,nrowa,modd,nppram,iwnob,iwncn,iwia,iwisp,iwist,
     *        iwiw,nwdi,nwd,nwff,nwgrf,nwclla,nwhes1,nwsp,nwbak,nwsg,M,
     *       maxit,nob,nobL,nnineq,info,idummy,nshift,max0,modem,lstype,
     *       nstop,initvl,nn,nnn,nwgm,ipsp,ipspan,ipyes,iprnto,mod
      double  precision epsmac,QLeps,small,xi,gi,gmax,dummy,big,tolfea,
     *        rteps,epskt,upert,valnom,dsqrt,dmax1
      logical feasbl,feasb,prnt,nspace,Linfty,nAD
      common  /fsqpp1/nnineq,M,ncallg,ncallf,modd,lstype,nstop,
     *        /fsqpp2/io,ipp,ipsp,ipyes,info,idummy,iter,initvl,
     *        /fsqpp3/epsmac,rteps,upert,valnom
     *        /fsqpq1/big,tolfea,/fsqpq2/maxit
      common  /CMACHE/QLeps
c
c     compute the machine precision
c
      io=6
c     iwiw=6*nparam+8*max0(1,nineq+neq)+7*max0(nf,1)+30
c     i=nineq+neq+1
c     nww=4*nparam**2+5*i*nparam+3*(nf+1)*nparam+26*(nparam+nf+1)
c    *   +45*i+100
c     if(iwsize.ge.iwiw.and.nwsize.ge.nww) goto 10
c       if(iwsize.lt.iwiw) write(io,9906) iwiw
c       if(nwsize.lt.nww)  write(io,9907) nww
c       info=7
c       goto 9000
c
 10   iter=0
      nstop=1
      nn=nineqn+neqn
      epsmac=small()
      QLeps=epsmac
      tolfea=epsmac*1.d+02
      big=bigbnd
      rteps=dsqrt(epsmac)
      upert=udelta
c
      i=mod(iprint,10)
      ipspan=max0(iprint-i,1)
      iprnto=iprint
      if(iprint.ge.10) iprint=i
      if(iprint.lt.2) ipspan=1
      if(ipspan.lt.10) ipyes=0
      nob=0
      gmax=-bigbnd
      info=0
      ipsp=ipspan
      ipp=iprint
      ncnstr=nineq+neq
      nnineq=nineq
c
c     check input data 
c
      if(iprint.gt.0) write(io,9900)
      call check(nparam,nf,Linfty,nAD,nineq,nineqn,neq,neqn,
     *           mode,modem,nwa,eps,bigbnd,bl,bu)
      if(info.eq.7) goto 9000
      lstype=nwa
c
      maxit=max0(max0(miter,10*max0(nparam,ncnstr)),1000)
      feasbl=.true.
      feasb=.true.
      prnt=.false.
      nspace=.false.
      nppram=nparam+1
      nshift=nparam**2+nppram**2
c
c  check whether x is within bounds
c
      do 100 i=1,nparam
        xi=x(i)
        if(bl(i).le.xi.and.bu(i).ge.xi) goto 100
        feasbl=.false.
        goto 110
 100  continue
 110  nclin=ncnstr-nn
c
c  check whether linear constraints are feasible
c
      if(nclin.eq.0) goto 210
      do 200 i=1,nclin
        j=i+nineqn
        if(j.le.nineq) then
          call constr(nparam,j,x,gi)
          if(gi.le.epsmac) goto120
          feasbl=.false.
        else if(j.gt.nineq) then
          call constr(nparam,j+neqn,x,gi)
          if(dabs(gi).le.epsmac) goto 120
          feasbl=.false.
        endif
 120    g(j)=gi
 200  continue
 210  if(feasbl) goto 240
      if(iprint.le.0) goto 230
        write(io,9901)
        call sbout1(io,nparam,'x                   ',dummy,x,2,1)
        prnt=.true.
 230  nctotl=nparam+nclin
      leniw=max0(2*nparam+2*nctotl+3,2*nclin+2*nparam+6)
      if(leniw.le.iwsize)then
        leniw=iwsize
      else
        write(io,9906) leniw
        info=7
        nspace=.true.
      endif
      nwx=1
      nwbl=nwx+nparam
      nwbu=nwbl+nctotl+4
      nwgrg=nwbu+nctotl+2
      nwa=nwgrg+nclin*nparam+1
      nwcvec=nwa+nparam*nclin+1
      nwhess=nwcvec+nparam
      nwcla=nwhess+nparam*nparam
      nww=nwcla+nctotl+nparam
      lenw=2*nparam**2+10*nparam+2*nctotl+1
      if((nww+lenw).le.nwsize) then
        lenw=nwsize-nww
        if(.not.nspace) goto 235
        write(io,9909)
        goto 9000
      else
        write (io,9907) nww+lenw
        write(io,9909)
        info=7
        goto 9000
      endif
c
c     attempt to generate a point satisfying all linear constraints
c 
 235  nrowa=max0(nclin,1)
      call initpt(nparam,nineqn,neq,neqn,nclin,nctotl,nrowa,x,bl,bu,
     *            iw,leniw,w(nwx),w(nwbl),w(nwbu),g(nineqn+1),w(nwgrg),
     *            w(nwa),w(nwcvec),w(nwhess),w(nwcla),w(nwbl+nparam+3),
     *            w(nww),lenw,constr,gradcn)
      if(info.ne.0) goto 9000
 240  do 245 i=1, neq-neqn
 245    g(nineq+neqn+i)=g(nineq+i) 
      if(nn.ne.0) goto 510
      goto 605
c
 290    do 300 i=1,nob
 300      w(i+nineqn+nshift)=w(i+nshift)
        nob=0
c
 510  continue
      if(info.eq.-1) goto 540
        do 520 i=1,nineqn
          call constr(nparam,i,x,w(i+nineqn+nshift))
          if(w(i+nineqn+nshift).gt.0.d0) feasb=.false.
 520    continue
        ncallg=nineqn
        if(feasb) goto 540
c
c     set indxob(i) in phase 1
c
        do 530 i=1,nineqn
          nob=nob+1
          iw(nob)=i
          w(nob+nshift)=w(i+nineqn+nshift)
          gmax=dmax1(gmax,w(nob+nshift))
 530    continue
      goto 580
 540    do 550 i=1,nineqn
          g(i)=w(i+nineqn+nshift)
          iw(nineqn+i+1)=i
 550    continue
        do 560 i=1,neq-neqn
          g(i+nineq+neqn)=g(i+nineq)
 560    continue
        do 570 i=1,neqn
          j=i+nineq
          call constr(nparam,j,x,g(j))
          iw(nineqn+nineqn+i+1)=j
 570    continue
        ncallg=ncallg+neqn
 580  continue
c
 605  if(iprint.le.0.or..not.feasb.or.prnt) goto 610
        write(io,9902)
        call sbout1(io,nparam,'x                   ',dummy,x,2,1)
        prnt=.true.
 610  if(nob.ne.0) goto 620
      if(iprint.le.0) goto 615
        if(info.eq.0) goto 613
        write(io,9904) ncallg
        if(ipp.eq.0) write(io,9910) iter
        if(ipp.gt.0) write(io,9910) iter-1
        if(ipp.eq.0) iter=iter+1
 613    if(.not.feasb.or.feasbl) goto 614
          write(io,9903)
          call sbout1(io,nparam,'x                   ',dummy,x,2,1)
 614    if(info.eq.0.and.prnt.and.feasb) goto 615
          write(io,9903)
          call sbout1(io,nparam,'x                   ',dummy,x,2,1)
 615  feasb=.true.
      feasbl=.true.
 620  nspace=.false.
      if(ipp.le.0.or.feasb.or.prnt) goto 630
        write(io,9901)
        call sbout1(io,nparam,'x                   ',dummy,x,2,1)
        prnt=.true.
 630  if(nob.eq.0) nob=1
c
c     set indxcn(1)--indxcn(ncnstr)
c
      if(feasb) nnn=nn
      if(.not.feasb) nnn=0
      do 700 i=1,nnn
 700    iw(nob+i)=iw(nineqn+i+1)
 710  do 800 i=1,nineq-nineqn
 800    iw(nob+nnn+i)=nineqn+i
      do 805 i=1,neq-neqn
        if(feasb) iw(nob+nineq+neqn+i)=nineq+neqn+i
        if(.not.feasb) iw(nineq+i)=nineq+neqn+i
 805  continue
      if(.not.feasb) goto 810
        nob=nf
        info=0
        ipp=iprint
        ipsp=ipspan
        modd=modem
        epskt=eps
        if(Linfty) nobL=2*nob
        if(.not.Linfty) nobL=nob
        if(nob.ne.0) goto 910
        write(io,9908)
      goto 9000
 810    ipp=0
        ipsp=1
        modd=0
        nobL=nob
        info=-1
        epskt=1.d-10
 910  nctotl=nppram+ncnstr+nobL
      iwnob=1
      if(feasb) iwncn=iwnob+1
      if(.not.feasb) iwncn=iwnob+nob
      iwia=iwncn+ncnstr
      iwisp=iwia+nn+nob
      iwist=iwisp+nnineq-nineqn+1
      iwiw=iwist+nn+nob
      leniw=2*(ncnstr+nobL)+2*nppram+6
c
      if((iwiw+leniw).le.iwsize) then
        leniw=iwsize-iwiw
      else
        write (io,9906) iwiw+leniw
        info=7
        nspace=.true.
      endif
      M=4
      if(modem.eq.1.and.nn.eq.0) M=3
      nwhess=1
      nwhes1=nwhess+nparam**2
      nwff=nwhes1+nppram**2
      nwx=nwff+nob+1
      nwdi=nwx+nppram
      nwd=nwdi+nppram
      nwgm=nwd+nppram
      nwgrg=nwgm+max0(1,4*neqn)
      nwgrf=nwgrg+ncnstr*nparam+1
      nwgpf=nwgrf+nparam*nob+1
      nwpenp=nwgpf+nparam
      nwa=nwpenp+neqn+1
      nwbl=nwa+(ncnstr+nobL)*(nppram+1)
      nwbu=nwbl+nctotl+4
      nwcla=nwbu+nctotl+2
      nwclla=nwcla+nctotl+nppram
      nwcvec=nwclla+nctotl
      nwsp=nwcvec+nppram
      nwbak=nwsp+M+1
      nwsg=nwbak+nob+ncnstr+1
      nww=nwsg+neqn+1
      lenw=2*nppram*nppram+10*nppram+6*(ncnstr+nobL+1)
c
      if((nww+lenw).le.nwsize) then
        lenw=nwsize-nww
        if(.not.nspace) goto 920
        write(io,9909)
        goto 9000
      else
        write (io,9907) nww+lenw
        write(io,9909)
        info=7
        goto 9000
      endif
c
 920  do 1000 i=nwx,nwx+nparam-1
 1000   w(i)=x(i-nwx+1)
      w(nwx+nparam)=gmax
      if(.not.feasb) goto 1150
        do 1100 i=1,neqn
          if(g(i+nineq).gt.0d0) w(nwsg+i-1)=-1.d0
          if(g(i+nineq).le.0d0) w(nwsg+i-1)=1.d0
 1100   continue
c
c     either attempt to generate a point satisfying all constraints 
c     or try to solve the original problem
c
 1150 nrowa=max0(ncnstr+nobL,1)
      call FSQPD1(miter,nparam,nob,nobL,nineqn,neq,neqn,ncnstr,nctotl,
     *            nrowa,feasb,epskt,epseqn,bl,bu,iw(iwnob),iw(iwncn),
     *            iw(iwia),iw(iwisp),iw(iwist),iw(iwiw),leniw,w(nwx),
     *            w(nwdi),w(nwd),g,w(nwgm),w(nwgrg),w(nwff),w(nwgrf),
     *            w(nwgpf),w(nwpenp),w(nwa),w(nwbl),w(nwbu),w(nwcla),
     *            w(nwclla),w(nwcvec),w(nwbl+nparam+3),w(nwhess),
     *            w(nwhes1),w(nwsp),w(nwbak),w(nwsg),w(nww),lenw,
     *            obj,constr,gradob,gradcn)
      do 1200 i=1,nparam
 1200   x(i)=w(nwx+i-1)
      if(info.eq.-1) goto 290
      if(info.eq.0.or.feasb) goto 1220
        info=2
        write(io,9905)
      goto 9000
 1220 do 1300 i=1,nf
 1300   f(i)=w(nwff+i-1)
      if(nobL.eq.1) idummy=0
      if(nobL.gt.1) idummy=1
      if(nf.eq.1) nob=0
      do 1400 i=1,nparam+ncnstr+nob
        j=i
        if(i.gt.nparam.and.i.le.(nparam+ncnstr)) 
     *    j=nparam+iw(iwncn+i-nparam)
        if(i.le.nparam) then
          w(i)=w(nwclla+j-1)
        else if(i.gt.nparam) then
          if(i.le.(nparam+ncnstr)) j=nparam+iw(iwncn+i-1-nparam)
          w(i)=w(nwclla+j-1+idummy)
        endif
 1400 continue
c
 9000 inform=info
      iprint=iprnto
      return
 9900 format(1x,// 1x,'       FSQP Version 3.3 (Released April 1993)'
     *   /   1x,'            Copyright (c) 1989 --- 1993         '
     *   /   1x,'              J.L. Zhou and A.L. Tits           '
     *   /   1x,'                All Rights Reserved             ',//)
 9901 format(1x,'The given initial point is infeasible for inequality',
     *       /10x,'constraints and linear equality constraints:')

 9902 format(1x,'The given initial point is feasible for inequality',
     *   /8x,'constraints and linear equality constraints:')
 9903 format(1x,'Starting from the generated point feasible for',
     *   ' inequality',
     *   /10x,'constraints and linear equality constraints:')
 9904 format(1x,'To generate a point feasible for nonlinear inequality',
     *   /1x,'constraints and linear equality constraints,',
     *   ' ncallg = ',i10)
 9905 format(1x,'Error: No feasible point is found for nonlinear',
     *   ' inequality',
     *    /8x,'constraints and linear equality constraints'/)
 9906 format(1x,'iwsize should be bigger than', i20)
 9907 format(1x,'nwsize should be bigger than', i20)
 9908 format(1x,'current feasible iterate with no objective specified'/)
 9909 format(1x,/)
 9910 format(43x,'iteration = ',i10)
      end
c
      block data
      double  precision objeps,objrep,gLgeps
      common  /fsqpus/objeps,objrep,gLgeps
c
      data objeps,objrep,gLgeps/-1.d0,-1.d0,-1.d0/
      end
