## Copyright (C) 2000, 2006, 2007, 2008, 2009 Paul Kienzle
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
## @deftypefn  {Function File} {@var{pp} =} mkpp (@var{x}, @var{p})
## @deftypefnx {Function File} {@var{pp} =} mkpp (@var{x}, @var{p}, @var{d})
## 
## Construct a piece-wise polynomial structure from sample points
## @var{x} and coefficients @var{p}.  The i-th row of @var{p},
## @code{@var{p} (@var{i},:)}, contains the coefficients for the polynomial
## over the @var{i}-th interval, ordered from highest to 
## lowest.  There must be one row for each interval in @var{x}, so 
## @code{rows (@var{p}) == length (@var{x}) - 1}.  
##
## @var{p} may also be a multi-dimensional array, specifying a vector-valued
## or array-valued polynomial.  The shape is determined by @var{d}.  If @var{d}
## is
## not given, the default is @code{size (p)(1:end-2)}.  If @var{d} is given, the
## leading dimensions of @var{p} are reshaped to conform to @var{d}.
##
## @seealso{unmkpp, ppval, spline}
## @end deftypefn

function pp = mkpp (x, P, d)
  if (nargin < 2 || nargin > 3)
    print_usage ();
  endif
  pp.x = x(:);
  n = length (x) - 1;
  if (n < 1)
    error ("mkpp: at least one interval is needed");
  endif
  nd = ndims (P);
  k = size (P, nd);
  if (nargin < 3)
    if (nd == 2)
      d = 1;
    else
      d = prod (size (P)(1:nd-1));
    endif
  endif
  pp.d = d;
  pp.P = P = reshape (P, prod (d), [], k);
  pp.orient = 0;

  if (size (P, 2) != n)
    error ("mkpp: num intervals in X doesn't match num polynomials in P");
  endif
endfunction

%!demo # linear interpolation
%! x=linspace(0,pi,5)'; 
%! t=[sin(x),cos(x)];
%! m=diff(t)./(x(2)-x(1)); 
%! b=t(1:4,:);
%! pp = mkpp(x, [m(:),b(:)]);
%! xi=linspace(0,pi,50);
%! plot(x,t,"x",xi,ppval(pp,xi));
%! legend("control","interp");
