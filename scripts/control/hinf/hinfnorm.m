## Copyright (C) 1996, 1998 Auburn University.  All rights reserved.
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
## @deftypefn {Function File} {[@var{g}, @var{gmin}, @var{gmax}] =} hinfnorm (@var{sys}, @var{tol}, @var{gmin}, @var{gmax}, @var{ptol})
## Computes the 
## @iftex
## @tex
## $ { \cal H }_\infty $
## @end tex
## @end iftex
## @ifinfo
## H-infinity
## @end ifinfo
## norm of a system data structure.
##
## @strong{Inputs}
## @table @var
## @item sys
## system data structure
## @item tol
## @iftex
## @tex
## $ { \cal H }_\infty $
## @end tex
## @end iftex
## @ifinfo
## H-infinity
## @end ifinfo
## norm search tolerance (default: 0.001)
## @item gmin
## minimum value for norm search (default: 1e-9)
## @item gmax
## maximum value for norm search (default: 1e+9)
## @item ptol
## pole tolerance:
## @itemize @bullet
## @item if sys is continuous, poles with
## @iftex
## @tex
## $ \vert {\rm real}(pole) \vert < ptol \Vert H \Vert $
## @end tex
## @end iftex
## @ifinfo
## @math{ |real(pole))| < ptol*||H|| }
## @end ifinfo
## (@var{H} is appropriate Hamiltonian)
## are considered to be on the imaginary axis.
##
## @item if sys is discrete, poles with
## @iftex
## @tex
## $ \vert { \rm pole } - 1 \vert < ptol \Vert [ s_1 s_2 ] \Vert $
## @end tex
## @end iftex
## @ifinfo
## @math{|abs(pole)-1| < ptol*||[s1,s2]||}
## @end ifinfo
## (appropriate symplectic pencil)
## are considered to be on the unit circle.
##
## @item Default value: 1e-9
## @end itemize
## @end table
##
## @strong{Outputs}
## @table @var
## @item g
## Computed gain, within @var{tol} of actual gain.  @var{g} is returned as Inf
## if the system is unstable.
## @item gmin
## @itemx gmax
## Actual system gain lies in the interval [@var{gmin}, @var{gmax}].
## @end table
##
## References:
## Doyle, Glover, Khargonekar, Francis, @cite{State-space solutions to standard}
## @iftex
## @tex
## $ { \cal H }_2 $ @cite{and} $ { \cal H }_\infty $
## @end tex
## @end iftex
## @ifinfo
## @cite{H-2 and H-infinity}
## @end ifinfo
## @cite{control problems}, @acronym{IEEE} @acronym{TAC} August 1989;
## Iglesias and Glover, @cite{State-Space approach to discrete-time}
## @iftex
## @tex
## $ { \cal H }_\infty $
## @end tex
## @end iftex
## @ifinfo
## @cite{H-infinity}
## @end ifinfo
## @cite{control}, Int. J. Control, vol 54, no. 5, 1991;
## Zhou, Doyle, Glover, @cite{Robust and Optimal Control}, Prentice-Hall, 1996.
## @end deftypefn

function [g, gmin, gmax] = hinfnorm (sys, tol, gmin, gmax, ptol)

  if((nargin == 0) || (nargin > 4))
    print_usage ();
  elseif(!isstruct(sys))
    error("Sys must be a system data structure");
  endif

  ## set defaults where applicable
  if(nargin < 5)
    ptol = 1e-9;        # pole tolerance
  endif
  if(nargin < 4)
    gmax = 1e9;         # max gain value
  endif

  dflg = is_digital(sys);
  sys = sysupdate(sys,"ss");
  [A,B,C,D] = sys2ss(sys);
  [n,nz,m,p] = sysdimensions(sys);

  ## eigenvalues of A must all be stable
  if(!is_stable(sys))
    warning(["hinfnorm: unstable system (is_stable, ptol=",num2str(ptol), ...
      "), returning Inf"]);
    g = Inf;
  endif

  Dnrm = norm(D);
  if(nargin < 3)
    gmin = max(1e-9,Dnrm);      # min gain value
  elseif(gmin < Dnrm)
    warning(["hinfnorm: setting Gmin=||D||=",num2str(Dnrm)]);
  endif

  if(nargin < 2)
    tol = 0.001;        # convergence measure for gmin, gmax
  endif

  ## check for scalar input arguments 2...5
  if( ! (isscalar(tol) && isscalar(gmin)
        && isscalar(gmax) && isscalar(ptol)) )
    error("hinfnorm: tol, gmin, gmax, ptol must be scalars");
  endif

  In = eye(n+nz);
  Im = eye(m);
  Ip = eye(p);
  ## find the Hinf norm via binary search
  while((gmax/gmin - 1) > tol)
    g = (gmax+gmin)/2;

    if(dflg)
      ## multiply g's through in formulas to avoid extreme magnitudes...
      Rg = g^2*Im - D'*D;
      Ak = A + (B/Rg)*D'*C;
      Ck = g^2*C'*((g^2*Ip-D*D')\C);

      ## set up symplectic generalized eigenvalue problem per Iglesias & Glover
      s1 = [Ak , zeros(nz) ; -Ck, In ];
      s2 = [In, -(B/Rg)*B' ; zeros(nz) , Ak' ];

      ## guard against roundoff again: zero out extremely small values
      ## prior to balancing
      s1 = s1 .* (abs(s1) > ptol*norm(s1,"inf"));
      s2 = s2 .* (abs(s2) > ptol*norm(s2,"inf"));
      [cc,dd,s1,s2] = balance(s1,s2);
      [qza,qzb,zz,pls] = qz(s1,s2,"S"); # ordered qz decomposition
      eigerr = abs(abs(pls)-1);
      normH = norm([s1,s2]);
      Hb = [s1, s2];

      ## check R - B' X B condition (Iglesias and Glover's paper)
      X = zz((nz+1):(2*nz),1:nz)/zz(1:nz,1:nz);
      dcondfailed = min(real( eig(Rg - B'*X*B)) < ptol);
    else
      Rinv = inv(g*g*Im - (D' * D));
      H = [A + B*Rinv*D'*C,        B*Rinv*B'; ...
           -C'*(Ip + D*Rinv*D')*C, -(A + B*Rinv*D'*C)'];
      ## guard against roundoff: zero out extremely small values prior
      ## to balancing
      H = H .* (abs(H) > ptol*norm(H,"inf"));
      [DD,Hb] = balance(H);
      pls = eig(Hb);
      eigerr = abs(real(pls));
      normH = norm(H);
      dcondfailed = 0;          # digital condition; doesn't apply here
    endif
    if( (min(eigerr) <= ptol * normH) | dcondfailed)
      gmin = g;
    else
      gmax = g;
    endif
  endwhile
endfunction
