# Copyright (C) 1996 A. Scottedward Hodel 
#
# This file is part of Octave. 
#
# Octave is free software; you can redistribute it and/or modify it 
# under the terms of the GNU General Public License as published by the 
# Free Software Foundation; either version 2, or (at your option) any 
# later version. 
# 
# Octave is distributed in the hope that it will be useful, but WITHOUT 
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or 
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License 
# for more details.
# 
# You should have received a copy of the GNU General Public License 
# along with Octave; see the file COPYING.  If not, write to the Free 
# Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA. 
 
function [K,g,GW,Xinf,Yinf] = hinfsyn(Asys,nu,ny,gmin,gmax,gtol,ptol,tol)
  # [K,g,GW,Xinf,Yinf] = hinfsyn(Asys,nu,ny,gmin,gmax,gtol[,ptol,tol])
  #
  # [1] Doyle, Glover, Khargonekar, Francis, "State Space Solutions
  #     to Standard H2 and Hinf Control Problems," IEEE TAC August 1989
  #
  # [2] Maciejowksi, J.M.: "Multivariable feedback design,"
  #     Addison-Wesley, 1989, ISBN 0-201-18243-2
  #
  # [3] Keith Glover and John C. Doyle: "State-space formulae for all
  #     stabilizing controllers that satisfy and h-infinity-norm bound
  #     and relations to risk sensitivity,"
  #     Systems & Control Letters 11, Oct. 1988, pp 167-172.
  #
  # inputs: input system is passed as either
  #        Asys: system data structure (see ss2sys, sys2ss)
  #              - controller is implemented for continuous time systems 
  #              - controller is NOT implemented for discrete time systems 
  #        nu: number of controlled inputs
  #        ny: number of measured outputs
  #        gmin: initial lower bound on H-infinity optimal gain
  #        gmax: initial upper bound on H-infinity optimal gain
  #        gtol: gain threshhold.  Routine quits when gmax/gmin < 1+tol
  #        ptol: poles with abs(real(pole)) < ptol*||H|| (H is appropriate
  #              Hamiltonian) are considered to be on the imaginary axis.  
  #              Default: 1e-9
  #        tol: threshhold for 0.  Default: 200*eps
  #
  #        gmax, gmin, gtol, and tol must all be postive scalars.
  # 
  # outputs: 
  #        K:   system controller
  #        g:   designed gain value
  #       GW:   closed loop system
  #     Xinf:   ARE solution matrix for regulator subproblem
  #     Yinf:   ARE solution matrix for filter subproblem


  # A. S. Hodel August 1995
  # Updated for Packed system structures December 1996 by John Ingram
  # $Revision: 2.0.0.2 $
  #
  # Revised by Kai P Mueller April 1998 to solve the general H_infinity
  # problem using unitary transformations Q (on w and z)
  # and non-singular transformations R (on u and y).
  # $Revision: 2.0.0.2 $

  old_page_val = page_screen_output;
  page_screen_output = 0;

  if( (nargin < 1) | (nargin > 8) )
    usage("[K,g,GW,Xinf,Yinf] = hinfsyn(Asys,nu,ny,gmin,gmax,gtol,ptol,tol)");
  endif
  # set default arguments
  if(nargin < 8)
    tol = 200*eps;
  elseif(!is_sample(tol))
    error("tol must be a positive scalar.")
  endif
  if(nargin < 7)
    ptol = 1e-9;
  elseif(!is_sample(ptol))
    error("hinfsyn: ptol must be a positive scalar");
  endif
    
  if(!is_sample(gmax) | !is_sample(gmin) | !is_sample(gtol) )
    error(["hinfsyn: gmax=",num2str(gmax),", gmin=",num2str(gmin), ...
      "gtol=",num2str(gtol), " must be positive scalars."])
  endif

  [chkdgkf,dgs] = is_dgkf(Asys,nu,ny,tol);

  if (! chkdgkf )
    disp("hinfsyn: system does not meet required assumptions")
    help is_dgkf
    error("hinfsyn: exit");
  endif

  # extract dgs information
  			nw = dgs.nw;	nu = dgs.nu;
  A = dgs.A;		B1 = dgs.Bw;	B2 = dgs.Bu;
  C1 = dgs.Cz;		D11 = dgs.Dzw;	D12 = dgs.Dzu;		nz = dgs.nz;
  C2 = dgs.Cy;		D21 = dgs.Dyw;	D22 = dgs.Dyu;		ny = dgs.ny;
  d22nz = dgs.Dyu_nz;
  dflg = dgs.dflg;

  # recover i/o transformations
  R12 = dgs.Ru;		R21 = dgs.Ry;
  [ncstates, ndstates, nin, nout] = sysdimensions(Asys);
  Atsam = sysgettsam(Asys);
  [Ast, Ain, Aout] = sysgetsignals(Asys);

  BB = [B1 B2];
  CC = [C1 ; C2];
  DD = [D11 D12 ; D21  D22];

  if (dflg == 0)
    n = ncstates;
    # perform binary search to find gamma min
    ghi = gmax;
    # derive a lower lower bound for gamma from D matrices
    xx1 = norm((eye(nz) - (D12/(D12'*D12))*D12')*D11);
    xx2 = norm(D11*(eye(nw)-(D21'/(D21*D21'))*D21));
    glo = max(xx1, xx2);
    if (glo > gmin)
      disp(" *** D matrices indicate a greater value of gamma min.");
      fprintf("     gamma min (%f) superseeded by %f.", gmin, glo);
      glo = xx1;
    else
      glo = gmin;
    endif
    if (glo > ghi)
      fprintf(" *** lower bound of gamma greater than upper bound(%f)", ...
	      glo, ghi);
      disp(" *** unable to continue, Goodbye.");
      return;
    endif

    de = ghi - glo;
    g = glo;
    search_state = 0;
    iteration_finished = 0;
    disp(" o structural tests passed, start of iteration...");
    disp("        o <-> test passed   # <-> test failed   - <-> cannot test");
    printf("----------------------------------------");
    printf("--------------------------------------\n");

    # ------123456789012345678901234567890123456789012345678901234567890
    printf("           .........X......... .........Y......... ");
    printf(".Z. PASS REMARKS\n");
    printf("        ga iax nev ene sym pos iax nev ene sym pos ");
    printf("rho  y/n ======>\n");
    printf("----------------------------------------");
    printf("--------------------------------------\n");

    # now do the search
    while (!iteration_finished)
      switch (search_state)
        case (0)
	  g = ghi;
        case (1)
	  g = glo;
        case (2)
	  g = 0.5 * (ghi + glo);
        otherwise
	  error(" *** This should never happen!");
      endswitch
      printf("%10.4f ", g);

      # computing R and R~
      d1dot = [D11 D12];
      R = zeros(nin, nin);
      R(1:nw,1:nw) = -g*g*eye(nw);
      R = R + d1dot' * d1dot;
      ddot1 = [D11; D21];
      Rtilde = zeros(nout, nout);
      Rtilde(1:nz,1:nz) = -g*g*eye(nz);
      Rtilde = Rtilde + ddot1 * ddot1';

      # build hamiltonian Ha for X_inf
      xx = ([BB; -C1'*d1dot]/R) * [d1dot'*C1 BB'];
      Ha = [A 0*A; -C1'*C1 -A'] - xx;
      x_ha_err = 0;
      # copied from from are(...)...
      [d, Ha] = balance(Ha);
      [u, s] = schur(Ha, "A");
      rev = real(eig(s));
      if (any(abs(rev) <= ptol))
        # eigenvalues near the imaginary axis
        x_ha_err = 1;
      elseif (sum(rev > 0) != sum(rev < 0))
        # unequal number of positive and negative eigenvalues
	x_ha_err = 2;
      else
	# compute positive Riccati equation solution
      	u = d * u;
      	Xinf = u(n+1:2*n,1:n) / u(1:n,1:n);
	if (!all(all(finite(Xinf))))
	  x_ha_err = 3;
	elseif (norm(Xinf-Xinf') >= 10*ptol)
	  # solution not symmetric
	  x_ha_err = 4;
	else
	  # positive semidefinite?
	  rev = eig(Xinf);
	  if (any(rev <= -ptol))
	    x_ha_err = 5;
	  endif
	endif
      endif
      
      # build hamiltonian Ha for Y_inf
      xx = ([CC'; -B1*ddot1']/Rtilde) * [ddot1*B1' CC];
      Ha = [A' 0*A; -B1*B1' -A] - xx;
      y_ha_err = 0;
      # copied from from are(...)...
      [d, Ha] = balance(Ha);
      [u, s] = schur(Ha, "A");
      rev = real(eig(s));
      if (any(abs(rev) <= ptol))
        # eigenvalues near the imaginary axis
        y_ha_err = 1;
      elseif (sum(rev > 0) != sum(rev < 0))
        # unequal number of positive and negative eigenvalues
	y_ha_err = 2;
      else
	# compute positive Riccati equation solution
      	u = d * u;
      	Yinf = u(n+1:2*n,1:n) / u(1:n,1:n);
	if (!all(all(finite(Yinf))))
	  y_ha_err = 3;
	elseif (norm(Yinf-Yinf') >= 10*ptol)
	  # solution not symmetric
	  x_ha_err = 4;
     	else
	  # positive semidefinite?
	  rev = eig(Yinf);
	  if (any(rev <= -ptol))
	    y_ha_err = 5;
	  endif
	endif
      endif

      # assume failure for this gamma
      passed = 0;
      if (!x_ha_err && !y_ha_err)
	# test spectral radius condition
	rho = max(abs(eig(Xinf * Yinf)));
	if (rho < g*g)
	  # spectral radius condition passed
	  passed = 1;
	endif
      endif

      switch (x_ha_err)
	case (0)
	  #       iax nev ene sym pos
	  printf(" o   o   o   o   o  ");
          xerr = " ";
	case (1)
	  printf(" #   -   -   -   -  ");
          xerr = "X im.eig.";
	case (2)
	  printf(" o   #   -   -   -  ");
          xerr = "Hx not Ham.";
	case (3)
	  printf(" o   o   #   -   -  ");
          xerr = "X inf.eig.";
	case (4)
	  printf(" o   o   o   #   -  ");
          xerr = "X not symm.";
	case (5)
	  printf(" o   o   o   o   #  ");
          xerr = "X not pos.";
	otherwise
	  error(" *** Xinf fail: this should never happen!");
      endswitch
      switch (y_ha_err)
	case (0)
	  #       iax nev ene sym pos rho
	  if (passed)
	    printf(" o   o   o   o   o   o    y  all tests passed.\n");
	  elseif (x_ha_err)
	    printf(" o   o   o   o   o   -    n  %s\n", xerr);
	  else
	    printf(" o   o   o   o   o   #    n  rho=%f.\n", rho);
	  endif
	case (1)
	  printf(" #   -   -   -   -   -    n  %s/Y im. eig.", xerr);
	case (2)
	  printf(" o   #   -   -   -   -    n  %s/Hy not Ham.", xerr);
	case (3)
	  printf(" o   o   #   -   -   -    n  %s/Y inf.eig.", xerr);
	case (4)
	  printf(" o   o   o   #   -   -    n  %s/Y not symm.", xerr);
	case (5)
	  printf(" o   o   o   o   #   -    n  %s/Y not pos.", xerr);
	otherwise
	  error(" *** Yinf fail: this should never happen!");
      endswitch


      if (passed && (de/g < gtol))
	search_state = 3;
      endif

      switch (search_state)
        case (0)
	  if (!passed)
	    # upper bound must pass but did not
	    fprintf(" *** the upper bound of gamma (%f) is too small.\n", g);
	    iteration_finished = 2;
	  else
            search_state = 1;
	  endif
        case (1)
	  if (!passed)
            search_state = 2;
	  else
	    # lower bound must not pass but passed
	    fprintf(" *** the lower bound of gamma (%f) passed.\n", g);
	    iteration_finished = 3;
	  endif
        case (2)
	  if (!passed)
	    glo = g;
	  else
	    ghi = g;
	  endif
	    de = ghi - glo;
        case (3)
	  # done
	  iteration_finished = 1;
        otherwise
	  error(" *** This should never happen!");
      endswitch
    endwhile

    printf("----------------------------------------");
    printf("--------------------------------------\n");
    if (iteration_finished != 1)
      K = [];
    else
      # success: compute controller
      fprintf("   hinfsyn final: glo=%f ghi=%f, test gain g=%f\n", \
              glo, ghi, g);
      printf("----------------------------------------");
      printf("--------------------------------------\n");
      Z = inv(eye(ncstates) - Yinf*Xinf/g/g);
      F = -R \ (d1dot'*C1 + BB'*Xinf);
      H = -(B1*ddot1' + Yinf*CC') / Rtilde;
      K = hinf_ctr(dgs,F,H,Z,g);

      Kst = strappend(Ast,"_K");
      Kin = strappend(Aout((nout-ny+1):(nout)),"_K");
      Kout = strappend(Ain((nin-nu+1):(nin)),"_K");
      [Ac, Bc, Cc, Dc] = sys2ss(K);
      K = ss2sys(Ac,Bc,Cc,Dc,Atsam,ncstates,ndstates,Kst,Kin,Kout);
      if (nargout >= 3)
	GW = starp(Asys, K);
      endif
    endif
    
  elseif(ndstates)

    # discrete time solution
    error("hinfsyn: discrete-time case not yet implemented")

  endif

  page_screen_output = old_page_val;
endfunction
