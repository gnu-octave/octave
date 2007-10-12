## Copyright (C) 1993, 1994, 1995, 1996, 1997, 1999, 2000, 2004, 2005,
##               2006, 2007 John W. Eaton
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
## @deftypefn {Function File} {} logspace (@var{base}, @var{limit}, @var{n})
## Similar to @code{linspace} except that the values are logarithmically
## spaced from
## @iftex
## @tex
## $10^{base}$ to $10^{limit}$.
## @end tex
## @end iftex
## @ifinfo
## 10^base to 10^limit.
## @end ifinfo
##
## If @var{limit} is equal to
## @iftex
## @tex
## $\pi$,
## @end tex
## @end iftex
## @ifinfo
## pi,
## @end ifinfo
## the points are between
## @iftex
## @tex
## $10^{base}$ and $\pi$,
## @end tex
## @end iftex
## @ifinfo
## 10^base and pi,
## @end ifinfo
## @emph{not}
## @iftex
## @tex
## $10^{base}$ and $10^{\pi}$,
## @end tex
## @end iftex
## @ifinfo
## 10^base and 10^pi,
## @end ifinfo
## in order to  be compatible with the corresponding @sc{Matlab}
## function.
##
## Also for compatibility, return the second argument if fewer than two
## values are requested.
## @seealso{linspace}
## @end deftypefn

## Author: jwe

function retval = logspace (x1, x2, n)

  if (nargin == 2)
    npoints = 50;
  elseif (nargin == 3)
    if (length (n) == 1)
      npoints = fix (n);
    else
      error ("logspace: arguments must be scalars");
    endif
  else
    print_usage ();
  endif

  if (length (x1) == 1 && length (x2) == 1)
    x2_tmp = x2;
    if (x2 == pi)
      x2_tmp = log10 (pi);
    endif
    retval = 10 .^ (linspace (x1, x2_tmp, npoints));
  else
    error ("logspace: arguments must be scalars");
  endif

endfunction
