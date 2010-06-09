## Copyright (C) 1993, 1994, 1995, 1996, 1997, 1999, 2000, 2004, 2005,
##               2006, 2007, 2008, 2009 John W. Eaton
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
## @deftypefn  {Function File} {} logspace (@var{base}, @var{limit})
## @deftypefnx {Function File} {} logspace (@var{base}, @var{limit}, @var{n})
## Similar to @code{linspace} except that the values are logarithmically
## spaced from
## @tex
## $10^{base}$ to $10^{limit}$.
## @end tex
## @ifnottex
## 10^base to 10^limit.
## @end ifnottex
##
## If @var{limit} is equal to
## @tex
## $\pi$,
## @end tex
## @ifnottex
## pi,
## @end ifnottex
## the points are between
## @tex
## $10^{base}$ and $\pi$,
## @end tex
## @ifnottex
## 10^base and pi,
## @end ifnottex
## @emph{not}
## @tex
## $10^{base}$ and $10^{\pi}$,
## @end tex
## @ifnottex
## 10^base and 10^pi,
## @end ifnottex
## in order to be compatible with the corresponding @sc{matlab}
## function.
## If @var{n} is unspecified it defaults to 50.
##
## Also for compatibility with @sc{matlab}, return the second argument if 
## fewer than two values are requested.
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

%!test
%! x1 = logspace (1, 2);
%! x2 = logspace (1, 2, 10);
%! x3 = logspace (1, -2, 10);
%! x4 = logspace (1, pi, 10);
%! assert((size (x1) == [1, 50] && x1(1) == 10 && x1(50) == 100
%! && size (x2) == [1, 10] && x2(1) == 10 && x2(10) == 100
%! && size (x3) == [1, 10] && x3(1) == 10 && x3(10) == 0.01
%! && size (x4) == [1, 10] && x4(1) == 10 && abs (x4(10) - pi) < sqrt (eps)));

%!error logspace ([1, 2; 3, 4], 5, 6);

%!error logspace ();

%!error logspace (1, 2, 3, 4);

