## Copyright (C) 1996, 1997 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} int2str (@var{n})
## @deftypefnx {Function File} {} num2str (@var{x}, @var{precision})
## @deftypefnx {Function File} {} num2str (@var{x}, @var{format})
## Convert a number to a string.  These functions are not very flexible,
## but are provided for compatibility with @sc{Matlab}.  For better control
## over the results, use @code{sprintf} (@pxref{Formatted Output}).
## @end deftypefn
##
## @seealso{sprintf and num2str}

## Author: jwe

function retval = int2str (x)

  if (nargin == 1)
    x = round (real(x));
    sz = size(x);
    nd = ndims (x);
    nc = columns (x);
    if (nc > 1)
      idx = cell ();
      for i = 1:nd
	idx {i} = 1:sz(i);
      endfor
      idx(2) = 1;
      ifmt = get_fmt (x(idx{:}), 0);
      idx(2) = 2:sz(2);
      rfmt = get_fmt (x(idx{:}), 2);
      fmt = strcat (ifmt, repmat (rfmt, 1, nc-1), "\n")
    else
      fmt = strcat (get_fmt (x, 0), "\n");
    endif
    tmp = sprintf (fmt, permute (x, [2, 1, 3 : nd]));
    tmp(end) = "";
    retval = split (tmp, "\n");
  else
    usage ("int2str (x)");
  endif

endfunction

function fmt = get_fmt (x, sep)

  t = x(:);
  t = t(t != 0);
  if (isempty (t))
    ## All zeros.
    fmt = sprintf ("%%%dd", 1 + sep);
  else
    ## Maybe have some zeros.
    nan_inf = isinf (t) | isnan (t);
    if (any (nan_inf))
      if (any (t(nan_inf) < 0))
	min_fw = 4 + sep;
      else
	min_fw = 3 + sep;
      endif
    else
      min_fw = 1 + sep;
    endif
    t = t(! nan_inf);
    if (isempty (t))
      ## Only zeros, Inf, and NaN.
      fmt = sprintf ("%%%dd", min_fw);
    else
      ## Could have anything.
      tfw = floor (log10 (abs (t))) + 1 + sep;
      fw = max (tfw);
      if (any (t(tfw == fw) < 0))
	fw++;
      endif
      fmt = sprintf ("%%%dd", max (fw, min_fw));
    endif
  endif

endfunction