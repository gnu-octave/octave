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
## @deftypefn {Function File} {[@var{K}, @var{gain}, @var{kc}, @var{kf}, @var{pc}, @var{pf}] =} h2syn (@var{asys}, @var{nu}, @var{ny}, @var{tol})
## Design 
## @iftex
## @tex
## $ { \cal H }_2 $
## @end tex
## @end iftex
## @ifinfo
## H-2
## @end ifinfo
## optimal controller per procedure in 
## Doyle, Glover, Khargonekar, Francis, @cite{State-Space Solutions to Standard}
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
## Discrete-time control per Zhou, Doyle, and Glover, @cite{Robust and optimal control}, Prentice-Hall, 1996.
##
## @strong{Inputs}
## @table @var
## @item asys
## system data structure (see ss, sys2ss)
## @itemize @bullet
## @item controller is implemented for continuous time systems
## @item controller is @strong{not} implemented for discrete time systems
## @end itemize
## @item nu
## number of controlled inputs
## @item ny
## number of measured outputs
## @item tol
## threshold for 0.  Default: 200*@code{eps}
## @end table
##
## @strong{Outputs}
## @table @var
## @item    k
## system controller
## @item    gain
## optimal closed loop gain
## @item    kc
## full information control (packed)
## @item    kf
## state estimator (packed)
## @item    pc
## @acronym{ARE} solution matrix for regulator subproblem
## @item    pf
## @acronym{ARE} solution matrix for filter subproblem
## @end table
## @end deftypefn

## Updated for System structure December 1996 by John Ingram

function [K, gain, Kc, Kf, Pc, Pf] = h2syn (Asys, nu, ny, tol)

  if (nargin < 3 || nargin > 4)
    print_usage ();
  elseif (nargin == 3)
    [chkdgkf, dgs] = is_dgkf (Asys, nu, ny);
  elseif (nargin == 4)
    [chkdgkf, dgs] = is_dgkf (Asys, nu, ny, tol);
  endif

  if (! chkdgkf)
    error ("h2syn: system does not meet required assumptions")
  endif

  ## extract dgs information
  nw = dgs.nw;
  nu = dgs.nu;
  nz = dgs.nz;
  ny = dgs.ny;

  A = dgs.A;

  Bw = dgs.Bw;
  Bu = dgs.Bu;

  Cz = dgs.Cz;
  Cy = dgs.Cy;

  Dzw = dgs.Dzw;
  Dzu = dgs.Dzu;

  Dyw = dgs.Dyw;
  Dyu = dgs.Dyu;

  d22nz = dgs.Dyu_nz;

  dflg = dgs.dflg;

  if (norm (Dzw, Inf) > norm ([Dzw, Dzu; Dyw, Dyu], Inf)*1e-12)
    warning ("h2syn: Dzw nonzero; feedforward not implemented")
    Dzw
    D = [Dzw, Dzu ; Dyw, Dyu]
  endif

  ## recover i/o transformations
  Ru = dgs.Ru;
  Ry = dgs.Ry;

  [ncstates, ndstates, nout, nin] = sysdimensions (Asys);
  Atsam = sysgettsam (Asys);
  [Ast, Ain, Aout] = sysgetsignals (Asys);

  if (dgs.dflg == 0)
    Pc = are (A, Bu*Bu', Cz'*Cz);    # solve control, filtering ARE's
    Pf = are(A', Cy'*Cy, Bw*Bw');
    F2 = -Bu'*Pc;                 # calculate feedback gains
    L2 = -Pf*Cy';

    AF2 = A + Bu*F2;
    AL2 = A + L2*Cy;
    CzF2 = Cz + (Dzu/Ru)*F2;
    BwL2 = Bw+L2*(Ry\Dyw);

  else
    ## discrete time solution
    error ("h2syn: discrete-time case not yet implemented")
    Pc = dare (A, Bu*Bu', Cz'*Cz);
    Pf = dare (A', Cy'*Cy, Bw*Bw');
  endif

  nn = ncstates + ndstates;
  In = eye (nn);
  KA = A + Bu*F2 + L2*Cy;
  Kc1 = ss (AF2, Bw, CzF2, zeros (nz, nw));
  Kf1 = ss (AL2, BwL2, F2, zeros (nu, nw));

  g1 = h2norm (Kc1);
  g2 = h2norm (Kf1);

  ## compute optimal closed loop gain
  gain = sqrt (g1*g1 + g2*g2);

  if (nargout)
    Kst = strappend (Ast, "_K");
    Kin = strappend (Aout((nout-ny+1):(nout)), "_K");
    Kout = strappend (Ain((nin-nu+1):(nin)), "_K");

    ## compute systems for return
    K = ss (KA, -L2/Ru, Ry\F2, zeros(nu,ny), Atsam, ncstates,
	    ndstates, Kst, Kin, Kout);
  endif

  if (nargout > 2)
    ## system full information control state names
    stname2 = strappend (Ast, "_FI");

   ## system full information control input names
   inname2 = strappend (Ast, "_FI_in");

    ## system full information control output names
    outname2 = strappend (Aout(1:(nout-ny)), "_FI_out");

    nz = rows (Cz);
    nw = columns (Bw);

    Kc = ss (AF2, In, CzF2, zeros(nz,nn), Atsam, 
             ncstates, ndstates, stname2, inname2, outname2);
  endif

  if (nargout >3)
    ## fix system state estimator state names
    stname3 = strappend (Ast, "_Kf");

    ## fix system state estimator input names
    inname3 = strappend (Ast, "_Kf_noise");

    ## fix system state estimator output names
    outname3 = strappend (Ast, "_est");

    Kf = ss (AL2, BwL2, In, zeros(nn,nw),Atsam,
	    ncstates, ndstates, stname3, inname3, outname3);
  endif

endfunction
