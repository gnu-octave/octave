## Copyright (C) 1993, 1994, 1995, 2000, 2002, 2004, 2005, 2006, 2007
##               John W. Eaton
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
## @deftypefn {Function File} {} dcgain (@var{sys}, @var{tol})
## Returns dc-gain matrix. If dc-gain is infinite
## an empty matrix is returned.
## The argument @var{tol} is an optional tolerance for the condition
## number of the @math{A} Matrix in @var{sys} (default @var{tol} = 1.0e-10)
## @end deftypefn

## Author: Kai P. Mueller <mueller@ifr.ing.tu-bs.de>
## Created: October 1, 1997

function gm = dcgain (sys, tol)

  if (nargin < 1 || nargin > 2 || nargout > 1)
    print_usage ();
  endif
  if (! isstruct (sys))
    error ("dcgain: first argument is not a system data structure.")
  endif
  sys = sysupdate (sys, "ss");
  [aa, bb, cc, dd] = sys2ss (sys);
  if (is_digital (sys))
    aa = aa - eye (size (aa));
  endif
  if (nargin == 1)
    tol = 1.0e-10;
  endif
  r = rank (aa, tol);
  if (r < rows (aa))
    gm = [];
  else
    gm = -cc / aa * bb + dd;
  endif
  if (! is_stable (sys))
    [nn, nz, mm, pp] = sysdimensions (sys);
    warning ("dcgain: unstable system; dimensions: nc=%d, nz=%d, mm=%d, pp=%d",
	     nn, nz, mm, pp);
  endif

endfunction
