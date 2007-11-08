## Copyright (C) 1996, 1998, 2000, 2002, 2004, 2005, 2006, 2007
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
## @deftypefn {Function File} {} d2c (@var{sys}, @var{tol})
## @deftypefnx {Function File} {} d2c (@var{sys}, @var{opt})
## Convert a discrete (sub)system into a purely continuous one. 
## The sampling time used is @code{sysgettsam(@var{sys})}.
##
## @strong{Inputs}
## @table @var
## @item   sys
## system data structure with discrete components
## @item   tol
## Scalar value.
## Tolerance for convergence of default @code{"log"} option (see below)
## @item   opt
## conversion option.  Choose from:
## @table @code
## @item         "log"
## (default) Conversion is performed via a matrix logarithm.
## Due to some problems with this computation, it is
## followed by a steepest descent algorithm to identify continuous time
## @var{a}, @var{b}, to get a better fit to the original data.
##
## If called as @code{d2c (@var{sys}, @var{tol})}, with @var{tol}
## positive scalar, the @code{"log"} option is used.  The default value
## for @var{tol} is @code{1e-8}.
## @item        "bi"
## Conversion is performed via bilinear transform
## @math{z = (1 + s T / 2)/(1 - s T / 2)} where @math{T} is the
## system sampling time (see @code{sysgettsam}).
##
## FIXME: bilinear option exits with an error if @var{sys} is not purely
## discrete
## @end table
## @end table
## @strong{Output}
## @table @var
## @item csys 
## continuous time system (same dimensions and signal names as in @var{sys}).
## @end table
## @end deftypefn

## Author: R. Bruce Tenison <btenison@eng.auburn.edu>
## Created: August 23, 1994
## Updated by John Ingram for system data structure  August 1996

function csys = d2c (sys, opt)

  ## SYS_INTERNAL accesses members of system data structure

  if (nargin != 1 && nargin != 2)
    print_usage ();
  elseif (! isstruct (sys))
    error ("sys must be in system data structure");
  elseif (nargin == 1)
    opt = "log";
    tol = 1e-12;
  elseif (ischar (opt))   # all remaining cases are for nargin == 2
    tol = 1e-12;
    if (! (strcmp (opt, "log") || strcmp (opt, "bi")))
      error ("d2c: invalid opt passed=%s", opt);
    endif
  elseif (! is_sample (opt))
    error ("tol must be a positive scalar")
  elseif (opt > 1e-2)
    warning ("d2c: ridiculous error tolerance passed=%g, intended c2d call?",
	     opt);
  else
    tol = opt;
    opt = "log";
  endif
  T = sysgettsam (sys);

  if (strcmp (opt, "bi"))
    ## bilinear transform
    ## convert with bilinear transform
    if (! is_digital (sys) )
       error ("d2c requires a discrete time system for input")
    endif
    [a, b, c, d, tsam, n, nz, stname, inname, outname, yd] = sys2ss (sys);

    poles = eig (a);
    if (find (abs (poles-1) < 200*(n+nz)*eps))
      warning ("d2c: some poles very close to one.  May get bad results.");
    endif

    I = eye (size (a));
    tk = 2 / sqrt (T);
    A = (2/T)*(a-I)/(a+I);
    iab = (I+a)\b;
    B = tk*iab;
    C = tk*(c/(I+a));
    D = d- (c*iab);
    stnamec = strappend (stname, "_c");
    csys = ss (A, B, C, D, 0, rows (A), 0, stnamec, inname, outname);
  elseif (strcmp (opt, "log"))
    sys = sysupdate (sys, "ss");
    [n, nz, m, p] = sysdimensions (sys);

    if (nz == 0)
      warning ("d2c: all states continuous; setting outputs to agree");
      csys = syssetsignals (sys, "yd", zeros (1, 1:p));
      return;
    elseif (n != 0)
      warning ("d2c: n=%d > 0; performing c2d first", n);
      sys = c2d (sys, T);
    endif
    [a, b] = sys2ss (sys);

    [ma, na] = size (a);
    [mb, nb] = size (b);

    if (isempty (b))
      warning ("d2c: empty b matrix");
      Amat = a;
    else
      Amat = [a, b; zeros(nb,na), eye(nb)];
    endif

    poles = eig (a);
    if (find (abs (poles) < 200*(n+nz)*eps))
      warning ("d2c: some poles very close to zero.  logm not performed");
      Mtop = zeros (ma, na+nb);
    elseif (find (abs (poles-1) < 200*(n+nz)*eps))
      warning ("d2c: some poles very close to one.  May get bad results.");
      logmat = real (logm (Amat) / T);
      Mtop = logmat(1:na,:);
    else
      logmat = real (logm (Amat) / T);
      Mtop = logmat(1:na,:);
    endif

    ## perform simplistic, stupid optimization approach.
    ## should re-write with a Davidson-Fletcher CG approach
    mxthresh = norm (Mtop);
    if (mxthresh == 0)
      mxthresh = 1;
    endif
    eps1 = mxthresh;    #gradient descent step size
    cnt = max (20, (n*nz)*4);     #max number of iterations
    newgrad=1;  #signal for new gradient
    while ((eps1/mxthresh > tol) && cnt)
      cnt--;
      ## calculate the gradient of error with respect to Amat...
      geps = norm (Mtop) * 1e-8;
      if (geps == 0)
        geps = 1e-8;
      endif
      DMtop = Mtop;
      if (isempty (b))
        Mall = Mtop;
        DMall = DMtop;
      else
        Mall = [Mtop; zeros(nb,na+nb)];
        DMall = [DMtop; zeros(nb,na+nb) ];
      endif

      if (newgrad)
        GrMall = zeros (size (Mall));
        for ii = 1:rows(Mtop)
          for jj = 1:columns(Mtop)
          DMall(ii,jj) = Mall(ii,jj) + geps;
            GrMall(ii,jj) = norm (Amat - expm (DMall*T), "fro") ...
                - norm (Amat - expm (Mall*T), "fro");
          DMall(ii,jj) = Mall(ii,jj);
          endfor
        endfor
        GrMall = GrMall/norm(GrMall,1);
        newgrad = 0;
      endif

      ## got a gradient, now try to use it
      DMall = Mall-eps1*GrMall;

      FMall = expm (Mall*T);
      FDMall = expm (DMall*T);
      FmallErr = norm (Amat - FMall);
      FdmallErr = norm (Amat - FDMall);
      if (FdmallErr < FmallErr)
        Mtop = DMall(1:na,:);
        eps1 = min (eps1*2, 1e12);
        newgrad = 1;
      else
        eps1 = eps1/2;
      endif

      if (FmallErr == 0)
        eps1 = 0;
      endif

    endwhile

    [aa, bb, cc, dd, tsam, nn, nz, stnam, innam, outnam, yd] = sys2ss (sys);
    aa = Mall(1:na,1:na);
    if (! isempty (b))
      bb = Mall(1:na,(na+1):(na+nb));
    endif
    csys = ss (aa, bb, cc, dd, 0, na, 0, stnam, innam, outnam);

    ## update names
    nn = sysdimensions (sys);
    for ii = (nn+1):na
      strval = sprintf ("%s_c", sysgetsignals (csys, "st", ii, 1));
      csys = syssetsignals (csys, "st", strval, ii);
    endfor
  endif

endfunction
