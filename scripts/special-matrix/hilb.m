########################################################################
##
## Copyright (C) 1993-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

## -*- texinfo -*-
## @deftypefn {} {@var{h} =} hilb (@var{n})
## Return the Hilbert matrix of order @var{n}.
##
## The @math{i,j} element of a Hilbert matrix is defined as
## @tex
## $$
## H(i, j) = {1 \over (i + j - 1)}
## $$
## @end tex
## @ifnottex
##
## @example
## H(i, j) = 1 / (i + j - 1)
## @end example
##
## @end ifnottex
##
## Hilbert matrices are close to being singular which make them difficult to
## invert with numerical routines.  Comparing the condition number of a random
## matrix 5x5 matrix with that of a Hilbert matrix of order 5 reveals just how
## difficult the problem is.
##
## @example
## @group
## cond (rand (5))
##    @result{} 14.392
## cond (hilb (5))
##    @result{} 4.7661e+05
## @end group
## @end example
##
## @seealso{invhilb}
## @end deftypefn

function h = hilb (n)

  if (nargin != 1)
    print_usage ();
  elseif (! isscalar (n))
    error ("hilb: N must be a scalar integer");
  endif

  ## Very elegant solution by N. Higham
  ## https://nhigham.com/2020/06/30/what-is-the-hilbert-matrix/
  j = 1:n;
  h = 1 ./ (j' + j - 1);

endfunction


%!assert (hilb (2), [1, 1/2; 1/2, 1/3])
%!assert (hilb (3), [1, 1/2, 1/3; 1/2, 1/3, 1/4; 1/3, 1/4, 1/5])

%!error <Invalid call> hilb ()
%!error <N must be a scalar integer> hilb (ones (2))
