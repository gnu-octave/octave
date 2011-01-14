## Copyright (C) 1995-2011 Kurt Hornik
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
## @deftypefn  {Function File} {} spearman (@var{x})
## @deftypefnx {Function File} {} spearman (@var{x}, @var{y})
## @cindex Spearman's Rho
## Compute Spearman's rank correlation coefficient @var{rho}.
##
## For two data vectors @var{x} and @var{y}, Spearman's @var{rho} is the
## correlation coefficient of the ranks of @var{x} and @var{y}.
##
## If @var{x} and @var{y} are drawn from independent distributions,
## @var{rho} has zero mean and variance @code{1 / (n - 1)}, and is
## asymptotically normally distributed.
##
## @code{spearman (@var{x})} is equivalent to @code{spearman (@var{x},
## @var{x})}.
## @seealso{ranks, kendall}
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Spearman's rank correlation rho

function rho = spearman (x, y = [])

  if ((nargin < 1) || (nargin > 2))
    print_usage ();
  endif

  if (! (isnumeric (x) && isnumeric (y)))
    error ("spearman: X and Y must be numeric matrices or vectors");
  endif

  if (ndims (x) != 2 || ndims (y) != 2)
    error ("spearman: X and Y must be 2-D matrices or vectors");
  endif

  if (rows (x) == 1)
    x = x';
  endif
  n = rows (x);

  if (nargin == 1)
    rho = corrcoef (ranks (x));
  else
    if (rows (y) == 1)
      y = y';
    endif
    if (rows (y) != n)
      error ("spearman: X and Y must have the same number of observations");
    endif
    rho = corrcoef (ranks (x), ranks (y));
  endif

endfunction

%% Test input validation
%!error spearman ();
%!error spearman (1, 2, 3);
%!error spearman ([true, true]);
%!error spearman (ones(1,2), [true, true]);
%!error spearman (ones (2,2,2));
%!error spearman (ones (2,2), ones (2,2,2));
%!error spearman (ones (2,2), ones (3,2));
