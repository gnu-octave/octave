## Copyright (C) 1995, 1996, 1997, 1998, 2000, 2002, 2005, 2006, 2007, 2009
##               Kurt Hornik
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
## @deftypefn {Function File} {} kendall (@var{x}, @var{y})
## Compute Kendall's @var{tau} for each of the variables specified by
## the input arguments.
##
## For matrices, each row is an observation and each column a variable;
## vectors are always observations and may be row or column vectors.
##
## @code{kendall (@var{x})} is equivalent to @code{kendall (@var{x},
## @var{x})}.
##
## For two data vectors @var{x}, @var{y} of common length @var{n},
## Kendall's @var{tau} is the correlation of the signs of all rank
## differences of @var{x} and @var{y}; i.e., if both @var{x} and
## @var{y} have distinct entries, then
##
## @tex
## $$ \tau = {1 \over n(n-1)} \sum_{i,j} {\rm sign}(q_i-q_j) {\rm sign}(r_i-r_j) $$
## @end tex
## @ifnottex
##
## @example
## @group
##          1    
## tau = -------   SUM sign (q(i) - q(j)) * sign (r(i) - r(j))
##       n (n-1)   i,j
## @end group
## @end example
##
## @end ifnottex
## @noindent
## in which the
## @tex
## $q_i$ and $r_i$
## @end tex
## @ifnottex
## @var{q}(@var{i}) and @var{r}(@var{i})
## @end ifnottex
## are the ranks of @var{x} and @var{y}, respectively.
##
## If @var{x} and @var{y} are drawn from independent distributions,
## Kendall's @var{tau} is asymptotically normal with mean 0 and variance
## @tex
## ${2 (2n+5) \over 9n(n-1)}$.
## @end tex
## @ifnottex
## @code{(2 * (2@var{n}+5)) / (9 * @var{n} * (@var{n}-1))}.
## @end ifnottex
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Kendall's rank correlation tau

function tau = kendall (x, y)

  if ((nargin < 1) || (nargin > 2))
    print_usage ();
  endif

  if (rows (x) == 1)
    x = x';
  endif
  [n, c] = size (x);

  if (nargin == 2)
    if (rows (y) == 1)
      y = y';
    endif
    if (rows (y) != n)
      error ("kendall: x and y must have the same number of observations");
    else
      x = [x, y];
    endif
  endif

  r   = ranks (x);
  m   = sign (kron (r, ones (n, 1)) - kron (ones (n, 1), r));
  tau = cor (m);

  if (nargin == 2)
    tau = tau (1 : c, (c + 1) : columns (x));
  endif

endfunction
