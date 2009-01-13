## Copyright (C) 2000, 2006, 2007 Paul Kienzle
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
## @deftypefn {Function File} {@var{pp} =} mkpp (@var{x}, @var{p})
## @deftypefnx {Function File} {@var{pp} =} mkpp (@var{x}, @var{p}, @var{d})
## 
## Construct a piece-wise polynomial structure from sample points
## @var{x} and coefficients @var{p}.  The i-th row of @var{p},
## @code{@var{p} (@var{i},:)}, contains the coefficients for the polynomial
## over the @var{i}-th interval, ordered from highest to 
## lowest. There must be one row for each interval in @var{x}, so 
## @code{rows (@var{p}) == length (@var{x}) - 1}.  
##
## You can concatenate multiple polynomials of the same order over the 
## same set of intervals using @code{@var{p} = [ @var{p1}; @var{p2}; 
## @dots{}; @var{pd} ]}.  In this case, @code{rows (@var{p}) == @var{d} 
## * (length (@var{x}) - 1)}.
##
## @var{d} specifies the shape of the matrix @var{p} for all except the
## last dimension. If @var{d} is not specified it will be computed as
## @code{round (rows (@var{p}) / (length (@var{x}) - 1))} instead.
##
## @seealso{unmkpp, ppval, spline}
## @end deftypefn

function pp = mkpp (x, P, d)
  if (nargin < 2 || nargin > 3)
    print_usage ();
  endif
  pp.x = x(:);
  pp.P = P;
  pp.n = length (x) - 1;
  pp.k = columns (P);
  if (nargin < 3)
    d = round (rows (P) / pp.n); 
  endif
  pp.d = d;
  if (pp.n * prod (d) != rows (P))
    error ("mkpp: num intervals in x doesn't match num polynomials in P");
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
