## Copyright (C) 1995, 1996  Kurt Hornik
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
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Mapping Function} {} log2 (@var{x})
## @deftypefnx {Mapping Function} {[@var{f}, @var{e}] =} log2 (@var{x})
## Compute the base-2 logarithm of @var{x}.  With two outputs, returns
## @var{f} and @var{e} such that
## @iftex
## @tex
##  $1/2 <= |f| < 1$ and $x = f \cdot 2^e$.
## @end tex
## @end iftex
## @ifinfo
##  1/2 <= abs(f) < 1 and x = f * 2^e.
## @end ifinfo
## @seealso{log, log10, logspace, exp}
## @end deftypefn

## Author: AW <Andreas.Weingessel@ci.tuwien.ac.at>
## Created: 17 October 1994
## Adapted-By: jwe

function [f, e] = log2 (x)

  if (nargin != 1)
    usage ("y = log2 (x) or [f, e] = log2 (x)");
  endif

  if (nargout < 2)
    f = log (x) / log (2);
  elseif (nargout == 2)
    ## Only deal with the real parts ...
    x = real (x);
    ## Since log (0) gives problems, 0 entries are replaced by 1.
    ## This is corrected later by multiplication with the sign.
    f = abs (x) + (x == 0);
    e = (floor (log (f) / log (2)) + 1) .* (x != 0);
    f = sign (x) .* f ./ (2 .^ e);
  else
    error ("log2 takes at most 2 output arguments");
  endif

endfunction

