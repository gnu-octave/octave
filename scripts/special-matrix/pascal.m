## Copyright (C) 1999-2011 Peter Ekberg
## Copyright (C) 2009 VZLU Prague
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
## @deftypefn  {Function File} {} pascal (@var{n})
## @deftypefnx {Function File} {} pascal (@var{n}, @var{t})
## Return the Pascal matrix of order @var{n} if @code{@var{t} = 0}.
## @var{t} defaults to 0.  Return lower triangular Cholesky factor of 
## the Pascal matrix if @code{@var{t} = 1}.  This matrix is its own
## inverse, that is @code{pascal (@var{n}, 1) ^ 2 == eye (@var{n})}.
## If @code{@var{t} = -1}, return its absolute value.  This is the
## standard Pascal triangle as a lower-triangular matrix.
## If @code{@var{t} = 2}, return a transposed and permuted version of
## @code{pascal (@var{n}, 1)}, which is the cube-root of the identity
## matrix.  That is @code{pascal (@var{n}, 2) ^ 3 == eye (@var{n})}.
##
## @seealso{hankel, vander, sylvester_matrix, hilb, invhilb, toeplitz
##          hadamard, wilkinson, compan, rosser}
## @end deftypefn

## Author: Peter Ekberg
##         (peda)

function retval = pascal (n, t)

  if (nargin > 2) || (nargin == 0)
    print_usage ();
  endif

  if (nargin == 1)
    t = 0;
  endif

  if (! isscalar (n) || ! isscalar (t))
    error ("pascal: expecting scalar arguments, found something else");
  endif

  if (t < -1 || t > 2)
    error ("pascal: expecting t to be -1, 0, 1, or 2, found %d", t);
  endif

  retval = zeros (n);
  retval(:,1) = 1;

  if (t == -1)
    for j = 2:n
      retval(j:n,j) = cumsum (retval (j-1:n-1,j-1));
    endfor
  else
    for j = 2:n
      retval(j:n,j) = -cumsum (retval (j-1:n-1,j-1));
    endfor
  endif

  if (t == 0)
    retval = retval*retval';
  elseif (t == 2)
    retval = retval';
    retval = retval(n:-1:1,:);
    retval(:,n) = -retval(:,n);
    retval(n,:) = -retval(n,:);
    if (rem(n,2) != 1)
      retval = -retval;
    endif
  endif

endfunction

%!assert (pascal(3,-1), [1,0,0;1,1,0;1,2,1])
%!assert (pascal(3,0), [1,1,1;1,2,3;1,3,6])
%!assert (pascal(3,0), pascal(3))
%!assert (pascal(3,1), [1,0,0;1,-1,0;1,-2,1])
%!assert (pascal(3,2), [0,0,-1;0,-1,2;-1,-1,1])
%!error (pascal(3,4))
%!error (pascal(3,-2))
%!error (pascal())
%!error (pascal(1,2,3))
