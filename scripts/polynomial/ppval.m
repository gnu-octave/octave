## Copyright (C) 2000 Paul Kienzle
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
## @deftypefn {Function File} {@var{yi} =} ppval (@var{pp}, @var{xi})
## Evaluate piece-wise polynomial @var{pp} at the points @var{xi}.  
## If @code{@var{pp}.d} is a scalar greater than 1, or an array,
## then the returned value @var{yi} will be an array that is 
## @code{d1, d1, @dots{}, dk, length (@var{xi})]}.
## @seealso{mkpp, unmkpp, spline}
## @end deftypefn 

function yi = ppval (pp, xi)

  if (nargin != 2)
    print_usage ();
  endif
  if (! isstruct (pp))
    error ("ppval: expects a pp structure");
  endif
  if (isempty (xi))
    yi = [];
  else
    transposed = (columns (xi) == 1);
    xi = xi(:);
    xn = length (xi);
    idx = lookup (pp.x(2:pp.n), xi) + 1;
    dx = (xi - pp.x(idx)).';
    dx = reshape (dx(ones(1,prod(pp.d)),:),[pp.d,xn]);
    c = reshape (pp.P(:,1), pp.n, prod (pp.d));
    yi = reshape (c(idx,:).', [pp.d, xn]);
    for i  = 2 : pp.k;
      c = reshape (pp.P(:,i), pp.n, prod (pp.d));
      yi = yi .* dx + reshape (c(idx,:).', [pp.d, xn]);
    endfor
    if (transposed && isscalar (pp.d) && pp.d == 1)
      yi = yi.';
    endif
  endif
endfunction
