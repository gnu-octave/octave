## Copyright (C) 1995-2012 Kurt Hornik
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
## @deftypefn  {Function File} {} studentize (@var{x})
## @deftypefnx {Function File} {} studentize (@var{x}, @var{dim})
## If @var{x} is a vector, subtract its mean and divide by its standard
## deviation.
##
## If @var{x} is a matrix, do the above along the first non-singleton
## dimension.
## If the optional argument @var{dim} is given, operate along this dimension.
## @seealso{center}
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Subtract mean and divide by standard deviation

function t = studentize (x, dim)
  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "studentize is obsolete and will be removed from a future version of Octave; please use zscore instead");
  endif

  if (nargin != 1 && nargin != 2)
    print_usage ();
  endif

  if (! isnumeric(x))
    error ("studentize: X must be a numeric vector or matrix");
  endif

  if (isinteger (x))
    x = double (x);
  endif

  nd = ndims (x);
  sz = size (x);
  if (nargin != 2)
    ## Find the first non-singleton dimension.
    dim = find (sz > 1, 1);
    if (isempty (dim))
      dim = 1;
    endif
  else
    if (!(isscalar (dim) && dim == fix (dim))
        || !(1 <= dim && dim <= nd))
      error ("studentize: DIM must be an integer and a valid dimension");
    endif
  endif

  c = sz(dim);
  if (c == 0)
    t = x;
  else
    idx = ones (1, nd);
    idx(dim) = c;
    t = x - repmat (mean (x, dim), idx);
    t = t ./ repmat (max (cat (dim, std(t, [], dim), ! any (t, dim)), [], dim), idx);
  endif

endfunction

%!assert(studentize ([1,2,3]), [-1,0,1])
%!assert(studentize (int8 ([1,2,3])), [-1,0,1])
#%!assert(studentize (ones (3,2,0,2)), zeros (3,2,0,2))
%!assert(studentize ([2,0,-2;0,2,0;-2,-2,2]), [1,0,-1;0,1,0;-1,-1,1])

%% Test input validation
%!error studentize ()
%!error studentize (1, 2, 3)
%!error studentize ([true true])
%!error studentize (1, ones(2,2))
%!error studentize (1, 1.5)
%!error studentize (1, 0)
%!error studentize (1, 3)

