## Copyright (C) 1998 Auburn University.  All rights reserved.
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
## @deftypefn {Function File} {[@var{a}, @var{b}, @var{c}, @var{d}] =} quaternion (w)
## @deftypefnx {Function File} {[@var{vv}, @var{theta}] =} quaternion (w)
## @deftypefnx {Function File} {@var{w} =} quaternion (@var{a}, @var{b}, @var{c}, @var{d})
## @deftypefnx {Function File} {@var{w} =} quaternion (@var{vv}, @var{theta})
## Construct or extract a quaternion
##
## @example
## w = a*i + b*j + c*k + d
## @end example
##
## @noindent
## from given data.
## @end deftypefn

## Author: A. S. Hodel <a.s.hodel@eng.auburn.edu>
## Adapted-By: jwe

function [a, b, c, d] = quaternion (w, x, y, z)

  switch (nargin)
    case(1)
      if (! (isvector (w) && length (w) == 4))
	error ("input vector must be of length 4)");
      endif
      ## extract data
      switch (nargout)
	case(4)
	  a = w(1);
	  b = w(2);
	  c = w(3);
	  d = w(4);

	case(2)
	  if (abs (norm (w) - 1) > 1e-12)
	    warning ("quaternion: ||w||=%e, setting=1 for vv, theta", norm(w));
	    w = w/norm(w);
	  endif
	  [a, b, c, d] = quaternion (w);
	  theta = acos (d) * 2;
	  if (abs (theta) > pi)
	    theta = theta - sign (theta) * pi;
	  endif
	  sin_th_2 = norm ([a, b, c]);

	  if (sin_th_2 != 0)
	    vv = [a, b, c] / sin_th_2;
	  else
	    vv = [a, b, c];
	  endif
	  a = vv;
	  b = theta;
	otherwise
	  print_usage ();
      endswitch

  case(2)
    if (nargout != 1)
      print_usage ();
    endif
    vv = w;
    theta = x;

    if (! isvector (vv) || length (vv) != 3)
      error ("vv must be a length three vector");
    elseif (! isscalar (theta))
      error ("theta must be a scalar");
    elseif (norm (vv) == 0)
      error ("quaternion: vv is zero");
    elseif (abs (norm (vv) - 1) > 1e-12)
      warning ("quaternion: ||vv|| != 1, normalizing")
      vv = vv / norm (vv);
    endif

    if (abs (theta) > 2*pi)
      warning ("quaternion: |theta| > 2 pi, normalizing")
      theta = rem (theta, 2*pi);
    endif
    vv = vv * sin (theta / 2);
    d = cos (theta / 2);
    a = quaternion (vv(1), vv(2), vv(3), d);

  case(4)
    if (nargout != 1)
      print_usage ();
    endif
    if (! (isscalar (w) && isscalar (x) && isscalar (y) && isscalar (z)))
      error ("input values must be scalars");
    endif
    a = [w, x, y, z];

  otherwise
    print_usage ();

  endswitch

endfunction
