## Copyright (C) 1996, 1998, 2000, 2004, 2005, 2006, 2007
##               Auburn University.  All rights reserved.
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {Function File} {[@var{k}, @var{g}, @var{gw}, @var{xinf}, @var{yinf}] =} hinfsyn (@var{asys}, @var{nu}, @var{ny}, @var{gmin}, @var{gmax}, @var{gtol}, @var{ptol}, @var{tol})
##
## @strong{Inputs} input system is passed as either
## @table @var
## @item asys
## system data structure (see @command{ss}, @command{sys2ss})
## @itemize @bullet
## @item controller is implemented for continuous time systems
## @item controller is @strong{not} implemented for discrete time systems  (see
## bilinear transforms in @command{c2d}, @command{d2c})
## @end itemize
## @item nu
## number of controlled inputs
## @item ny
## number of measured outputs
## @item gmin
## initial lower bound on 
## @iftex
## @tex
## $ { \cal H }_\infty $
## @end tex
## @end iftex
## @ifinfo
## H-infinity
## @end ifinfo
## optimal gain
## @item gmax
## initial upper bound on 
## @iftex
## @tex
## $ { \cal H }_\infty $
## @end tex
## @end iftex
## @ifinfo
## H-infinity
## @end ifinfo
## Optimal gain.
## @item gtol
## Gain threshold.  Routine quits when @var{gmax}/@var{gmin} < 1+tol.
## @item ptol
## poles with @code{abs(real(pole))} 
## @iftex
## @tex
## $ < ptol \Vert H \Vert $
## @end tex
## @end iftex
## @ifinfo
## < ptol*||H|| 
## @end ifinfo
## (@var{H} is appropriate
## Hamiltonian) are considered to be on the imaginary axis.
## Default: 1e-9.
## @item tol
## threshold for 0.  Default: 200*@code{eps}.
##
## @var{gmax}, @var{min}, @var{tol}, and @var{tol} must all be positive scalars.
## @end table
## @strong{Outputs}
## @table @var
## @item k
## System controller.
## @item g
## Designed gain value.
## @item gw
## Closed loop system.
## @item xinf
## @acronym{ARE} solution matrix for regulator subproblem.
## @item yinf
## @acronym{ARE} solution matrix for filter subproblem.
## @end table
##
## References:
## @enumerate
## @item Doyle, Glover, Khargonekar, Francis, @cite{State-Space Solutions
## to Standard}
## @iftex
## @tex
## $ { \cal H }_2 $ @cite{and} $ { \cal H }_\infty $
## @end tex
## @end iftex
## @ifinfo
## @cite{H-2 and H-infinity}
## @end ifinfo
## @cite{Control Problems}, @acronym{IEEE} @acronym{TAC} August 1989.
##
## @item Maciejowksi, J.M., @cite{Multivariable feedback design},
## Addison-Wesley, 1989, @acronym{ISBN} 0-201-18243-2.
##
## @item Keith Glover and John C. Doyle, @cite{State-space formulae for all
## stabilizing controllers that satisfy an}
## @iftex
## @tex
## $ { \cal H }_\infty $@cite{norm}
## @end tex
## @end iftex
## @ifinfo
## @cite{H-infinity-norm}
## @end ifinfo
## @cite{bound and relations to risk sensitivity},
## Systems & Control Letters 11, Oct. 1988, pp 167--172.
## @end enumerate
## @end deftypefn

## Author: A. S. Hodel <a.s.hodel@eng.auburn.edu>
## Created: August 1995
## Updated for Packed system structures December 1996 by John Ingram
##
## Revised by Kai P. Mueller April 1998 to solve the general H_infinity
## problem using unitary transformations Q (on w and z)
## and non-singular transformations R (on u and y).

function [K, g, GW, Xinf, Yinf] = hinfsyn (Asys, nu, ny, gmin, gmax, gtol, ptol, tol)

  if (nargin < 1 || nargin > 8)
    print_usage ();
  endif
  ## set default arguments
  if (nargin < 8)
    if (isa (Asys.a, "single") || isa (Asys.b, "single") || isa (Asys.c, "single") ||
	isa (Asys.d, "single"))
      tol = 200*eps("single");
    else
      tol = 200*eps;
    endif
  elseif (! is_sample (tol))
    error ("tol must be a positive scalar.")
  endif
  if (nargin < 7)
    ptol = 1e-9;
  elseif (! is_sample (ptol))
    error ("hinfsyn: ptol must be a positive scalar");
  endif

  if (! is_sample (gmax) || ! is_sample (gmin) || ! is_sample (gtol))
    error ("hinfsyn: gmax=%g, gmin=%g, gtol=%g must be positive scalars",
	   gmax, gmin, gtol);
  endif

  [chkdgkf, dgs] = is_dgkf (Asys, nu, ny, tol);

  if (! chkdgkf )
    error ("hinfsyn: system does not meet required assumptions")
  endif

  ## extract dgs information
  nw = dgs.nw;
  nu = dgs.nu;
  nz = dgs.nz;
  ny = dgs.ny;

  A = dgs.A;

  B1 = dgs.Bw;
  B2 = dgs.Bu;

  C1 = dgs.Cz;
  C2 = dgs.Cy;

  D11 = dgs.Dzw;
  D12 = dgs.Dzu;
  D21 = dgs.Dyw;
  D22 = dgs.Dyu;

  d22nz = dgs.Dyu_nz;

  dflg = dgs.dflg;

  ## recover i/o transformations
  R12 = dgs.Ru;
  R21 = dgs.Ry;

  [ncstates, ndstates, nin, nout] = sysdimensions (Asys);

  Atsam = sysgettsam (Asys);

  [Ast, Ain, Aout] = sysgetsignals (Asys);

  BB = [B1, B2];
  CC = [C1 ; C2];
  DD = [D11, D12 ; D21,  D22];

  if (dflg == 0)
    n = ncstates;
    ## perform binary search to find gamma min
    ghi = gmax;
    ## derive a lower lower bound for gamma from D matrices
    xx1 = norm ((eye(nz) - (D12/(D12'*D12))*D12')*D11);
    xx2 = norm (D11*(eye(nw)-(D21'/(D21*D21'))*D21));
    glo = max (xx1, xx2);
    if (glo > gmin)
      warning (" *** D matrices indicate a greater value of gamma min.");
      warning ("     gamma min (%f) superseeded by %f.", gmin, glo);
      glo = xx1;
    else
      glo = gmin;
    endif
    if (glo > ghi)
      error (" *** lower bound of gamma greater than upper bound (%g, %g)",
             glo, ghi);
    endif

    de = ghi - glo;
    g = glo;
    search_state = 0;
    iteration_finished = 0;
    disp(" o structural tests passed, start of iteration...");
    disp("        o <-> test passed   # <-> test failed   - <-> cannot test");
    printf("----------------------------------------");
    printf("--------------------------------------\n");

    ## ------123456789012345678901234567890123456789012345678901234567890
    printf("           .........X......... .........Y......... ");
    printf(".Z. PASS REMARKS\n");
    printf("        ga iax nev ene sym pos iax nev ene sym pos ");
    printf("rho  y/n ======>\n");
    printf("----------------------------------------");
    printf("--------------------------------------\n");

    ## set up error messages
    errmesg = {" o   o   o   o   o  ", ...
        " #   -   -   -   -  ", ...
        " o   #   -   -   -  ", ...
        " o   o   #   -   -  ", ...
        " o   o   o   #   -  ", ...
        " o   o   o   o   #  ", ...
        " -   -   -   -   -  "};
    errdesx = {"", ...
        "X im eig.", ...
        "Hx not Ham.", ...
        "X inf.eig", ...
        "X not symm.", ...
        "X not pos", ...
        "R singular"};

    errdesy = {" ", ...
        "Y im eig.", ...
        "Hy not Ham.", ...
        "Y inf.eig", ...
        "Y not symm.", ...
        "Y not pos", ...
        "Rtilde singular"};


    ## now do the search
    while (! iteration_finished)
      switch (search_state)
        case 0
          g = ghi;
        case 1
          g = glo;
        case 2
          g = 0.5 * (ghi + glo);
        otherwise
	  error (" *** This should never happen!");
      endswitch
      printf ("%10.4f ", g);

      ## computing R and R~
      d1dot = [D11, D12];
      R = zeros (nin, nin);
      R(1:nw,1:nw) = -g*g*eye(nw);
      R = R + d1dot' * d1dot;
      ddot1 = [D11; D21];
      Rtilde = zeros (nout, nout);
      Rtilde(1:nz,1:nz) = -g*g*eye(nz);
      Rtilde = Rtilde + ddot1 * ddot1';

      [Xinf, x_ha_err] = hinfsyn_ric (A, BB, C1, d1dot, R, ptol);
      [Yinf, y_ha_err] = hinfsyn_ric (A', CC', B1', ddot1', Rtilde, ptol);

      ## assume failure for this gamma
      passed = 0;
      rerr = "";
      if (! x_ha_err && ! y_ha_err)
        ## test spectral radius condition
        rho = max (abs (eig (Xinf * Yinf)));
        if (rho < g*g)
          ## spectral radius condition passed
          passed = 1;
        else
          rerr = sprintf ("rho=%f",rho);
        endif
      endif

      if (x_ha_err >= 0 && x_ha_err <= 6)
        printf ("%s", errmesg{x_ha_err+1});
        xerr = errdesx{x_ha_err+1};
      else
        error (" *** Xinf fail: this should never happen!");
      endif

      if (y_ha_err >= 0 && y_ha_err <= 6)
        printf ("%s", errmesg{y_ha_err+1});
        yerr = errdesy{y_ha_err+1};
      else
        error (" *** Yinf fail: this should never happen!");
      endif

      if (passed)
	printf ("  y all tests passed.\n");
      else
        printf ("  n %s/%s%s\n", xerr, yerr, rerr);
      endif

      if (passed && (de/g < gtol))
        search_state = 3;
      endif

      switch (search_state)
        case 0
          if (! passed)
            ## upper bound must pass but did not
            fprintf(" *** the upper bound of gamma (%f) is too small.\n", g);
            iteration_finished = 2;
          else
            search_state = 1;
          endif
        case 1
          if (! passed)
	    search_state = 2;
          else
            ## lower bound must not pass but passed
            fprintf (" *** the lower bound of gamma (%f) passed.\n", g);
            iteration_finished = 3;
          endif
        case 2
          ## Normal case; must check that singular R, Rtilde wasn't the problem.
          if (! passed && x_ha_err != 6 && y_ha_err != 6)
	    glo = g;
          else
            ghi = g;
          endif
          de = ghi - glo;
        case 3
	  iteration_finished = 1;        # done
        otherwise
	  error (" *** This should never happen!");
      endswitch
    endwhile

    printf ("----------------------------------------");
    printf ("--------------------------------------\n");
    if (iteration_finished != 1)
      K = [];
    else
      ## success: compute controller
      fprintf ("   hinfsyn final: glo=%f ghi=%f, test gain g=%f\n",
               glo, ghi, g);
      printf ("----------------------------------------");
      printf ("--------------------------------------\n");
      Z = inv (eye(ncstates) - Yinf*Xinf/g/g);
      F = -R \ (d1dot'*C1 + BB'*Xinf);
      H = -(B1*ddot1' + Yinf*CC') / Rtilde;
      K = hinf_ctr (dgs, F, H, Z, g);

      Kst = strappend (Ast, "_K");
      Kin = strappend (Aout((nout-ny+1):(nout)),"_K");
      Kout = strappend (Ain((nin-nu+1):(nin)),"_K");
      [Ac, Bc, Cc, Dc] = sys2ss (K);
      K = ss (Ac, Bc, Cc, Dc, Atsam, ncstates, ndstates, Kst, Kin, Kout);
      if (nargout >= 3)
        GW = starp (Asys, K);
      endif
    endif

  elseif (ndstates)

    ## discrete time solution
    error ("hinfsyn: discrete-time case not yet implemented")

  endif

endfunction
