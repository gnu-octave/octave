## Copyright (C) 2000-2011 Bill Lash
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
## @deftypefn  {Function File} {@var{b} =} unwrap (@var{x})
## @deftypefnx {Function File} {@var{b} =} unwrap (@var{x}, @var{tol})
## @deftypefnx {Function File} {@var{b} =} unwrap (@var{x}, @var{tol}, @var{dim})
##
## Unwrap radian phases by adding multiples of 2*pi as appropriate to
## remove jumps greater than @var{tol}.  @var{tol} defaults to pi.
##
## Unwrap will work along the dimension @var{dim}.  If @var{dim}
## is unspecified it defaults to the first non-singleton dimension.
## @end deftypefn

## Author: Bill Lash <lash@tellabs.com>

function retval = unwrap (x, tol, dim)

  if (nargin < 1 || nargin > 3)
    print_usage ();
  endif

  if (!isnumeric(x))
    error ("unwrap: X must be a numeric matrix or vector");
  endif

  if (nargin < 2 || isempty (tol))
    tol = pi;
  endif

  ## Don't let anyone use a negative value for TOL.
  tol = abs (tol);

  nd = ndims (x);
  sz = size (x);
  if (nargin == 3)
    if (!(isscalar (dim) && dim == fix (dim))
        || !(1 <= dim && dim <= nd))
      error ("unwrap: DIM must be an integer and a valid dimension");
    endif
  else
    ## Find the first non-singleton dimension
    dim = find (sz > 1, 1);
    if (isempty (dim))
      dim = 1;
    endif
  endif

  rng = 2*pi;
  m = sz(dim);

  ## Handle case where we are trying to unwrap a scalar, or only have
  ## one sample in the specified dimension.
  if (m == 1)
    retval = x;
    return;
  endif

  ## Take first order difference to see so that wraps will show up
  ## as large values, and the sign will show direction.
  idx = cell ();
  for i = 1:nd
    idx{i} = 1:sz(i);
  endfor
  idx{dim} = [1,1:m-1];
  d = x(idx{:}) - x;

  ## Find only the peaks, and multiply them by the range so that there
  ## are kronecker deltas at each wrap point multiplied by the range
  ## value.
  p =  rng * (((d > tol) > 0) - ((d < -tol) > 0));

  ## Now need to "integrate" this so that the deltas become steps.
  r = cumsum (p, dim);

  ## Now add the "steps" to the original data and put output in the
  ## same shape as originally.
  retval = x + r;

endfunction

%!function t = xassert(a,b,tol)
%!  if (nargin == 1)
%!    t = all(a(:));
%!  else
%!    if (nargin == 2)
%!      tol = 0;
%!    endif
%!    if (any (size(a) != size(b)))
%!      t = 0;
%!    elseif (any (abs(a(:) - b(:)) > tol))
%!      t = 0;
%!    else
%!      t = 1;
%!    endif
%!  endif
%!
%!test
%!
%! i = 0;
%! t = [];
%!
%! r = [0:100];                        # original vector
%! w = r - 2*pi*floor((r+pi)/(2*pi));  # wrapped into [-pi,pi]
%! tol = 1e3*eps;                      # maximum expected deviation
%!
%! t(++i) = xassert(r, unwrap(w), tol);               #unwrap single row
%! t(++i) = xassert(r', unwrap(w'), tol);             #unwrap single column
%! t(++i) = xassert([r',r'], unwrap([w',w']), tol);   #unwrap 2 columns
%! t(++i) = xassert([r;r], unwrap([w;w],[],2), tol);  #verify that dim works
%! t(++i) = xassert(r+10, unwrap(10+w), tol);         #verify that r(1)>pi works
%!
%! t(++i) = xassert(w', unwrap(w',[],2));  #unwrap col by rows should not change it
%! t(++i) = xassert(w, unwrap(w,[],1));    #unwrap row by cols should not change it
%! t(++i) = xassert([w;w], unwrap([w;w])); #unwrap 2 rows by cols should not change them
%!
%! ## verify that setting tolerance too low will cause bad results.
%! t(++i) = xassert(any(abs(r - unwrap(w,0.8)) > 100));
%!
%! assert(all(t));

