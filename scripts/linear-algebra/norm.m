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
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} norm (@var{a}, @var{p})
## Compute the p-norm of the matrix @var{a}.  If the second argument is
## missing, @code{p = 2} is assumed.
##
## If @var{a} is a matrix:
##
## @table @asis
## @item @var{p} = @code{1}
## 1-norm, the largest column sum of the absolute values of @var{a}.
##
## @item @var{p} = @code{2}
## Largest singular value of @var{a}.
##
## @item @var{p} = @code{Inf}
## @cindex infinity norm
## Infinity norm, the largest row sum of the absolute values of @var{a}.
##
## @item @var{p} = @code{"fro"}
## @cindex Frobenius norm
## Frobenius norm of @var{a}, @code{sqrt (sum (diag (@var{a}' * @var{a})))}.
## @end table
##
## If @var{a} is a vector or a scalar:
##
## @table @asis
## @item @var{p} = @code{Inf}
## @code{max (abs (@var{a}))}.
##
## @item @var{p} = @code{-Inf}
## @code{min (abs (@var{a}))}.
##
## @item other
## p-norm of @var{a}, @code{(sum (abs (@var{a}) .^ @var{p})) ^ (1/@var{p})}.
## @end table
## @seealso{cond, svd}
## @end deftypefn

## Author: jwe

function retval = norm (x, p)

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  if (isempty (x))
    retval = [];
    return;
  endif

  if (ndims (x) > 2)
    error ("norm: only valid on 2-D objects")
  endif

  if (nargin == 1)
    p = 2;
  endif

  ## Do we have a vector or matrix as the first argument?

  if (ndims(x) == 2 && (rows (x) == 1 || columns (x) == 1))
    if (isinteger (x) || issparse (x))
      if (ischar (p))
        if (strcmp (p, "fro"))
	  retval = sqrt (sum (abs (x) .^ 2));
        elseif (strcmp (p, "inf"))
          retval = max (abs (x));
        else
          error ("norm: unrecognized norm");
        endif
      else
        if (p == Inf)
          retval = max (abs (x));
        elseif (p == -Inf)
          retval = min (abs (x));
	elseif (p == 1)
	  retval = sum (abs (x));
        else
          retval = sum (abs (x) .^ p) ^ (1/p);
        endif
      endif
    else
      retval = __vnorm__ (x, p);
    endif
  else
    if (ischar (p))
      if (strcmp (p, "fro"))
	retval = sqrt (sum (sum (abs (x) .^ 2)));
      elseif (strcmp (p, "inf"))
        retval = max (sum (abs (x')));
      else
        error ("norm: unrecognized vector norm");
      endif
    else
      if (p == 1)
        retval = max (sum (abs (x)));
      elseif (p == 2)
        s = svd (x);
        retval = s (1);
      elseif (p == Inf)
        retval = max (sum (abs (x')));
      else
	error ("norm: unrecognized matrix norm");
      endif
    endif
  endif

endfunction

%!shared x
%! x = [1, -3, 4, 5, -7];
%!assert(norm(x,1), 20);
%!assert(norm(x,2), 10);
%!assert(norm(x,3), 8.24257059961711, -4*eps);
%!assert(norm(x,Inf), 7);
%!assert(norm(x,-Inf), 1);
%!assert(norm(x,"inf"), 7);
%!assert(norm(x,"fro"), 10);
%!assert(norm(x), 10);
%!assert(norm([1e200, 1]), 1e200);
%!shared m
%! m = magic (4);
%!assert(norm(m,1), 34);
%!assert(norm(m,2), 34);
%!assert(norm(m,Inf), 34);
%!assert(norm(m,"inf"), 34);
