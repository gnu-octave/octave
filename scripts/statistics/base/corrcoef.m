## Copyright (C) 1996, 1997, 1998, 1999, 2004, 2005, 2006, 2007, 2008, 2009
##               John W. Eaton
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
## @deftypefn  {Function File} {} corrcoef (@var{x})
## @deftypefnx {Function File} {} corrcoef (@var{x}, @var{y})
## Compute matrix of correlation coefficients.
##
## If each row of @var{x} and @var{y} is an observation and each column is
## a variable, then the @w{(@var{i}, @var{j})-th} entry of
## @code{corrcoef (@var{x}, @var{y})} is the correlation between the
## @var{i}-th variable in @var{x} and the @var{j}-th variable in @var{y}.
## @tex
## $$
## {\rm corrcoef}(x,y) = {{\rm cov}(x,y) \over {\rm std}(x) {\rm std}(y)}
## $$
## @end tex
## @ifnottex
##
## @example
## corrcoef(x,y) = cov(x,y)/(std(x)*std(y))
## @end example
##
## @end ifnottex
## If called with one argument, compute @code{corrcoef (@var{x}, @var{x})},
## the correlation between the columns of @var{x}.
## @seealso{cov}
## @end deftypefn

## Author: Kurt Hornik <hornik@wu-wien.ac.at>
## Created: March 1993
## Adapted-By: jwe

function retval = corrcoef (x, y = [])

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  if (! (isnumeric (x) && isnumeric (y)))
    error ("corrcoef: X and Y must be numeric matrices or vectors");
  endif

  if (ndims (x) != 2 || ndims (y) != 2)
    error ("corrcoef: X and Y must be 2-D matrices or vectors");
  endif

  if (isscalar (x))
    retval = 1;
    return;
  endif

  ## No check for division by zero error, which happens only when
  ## there is a constant vector and should be rare.
  if (nargin == 2)
    c = cov (x, y);
    s = std (x)' * std (y);
    retval = c ./ s;
  else
    c = cov (x);
    s = sqrt (diag (c));
    retval = c ./ (s * s');
  endif

endfunction

%!test
%! x = rand (10);
%! cc1 = corrcoef (x);
%! cc2 = corrcoef (x, x);
%! assert((size (cc1) == [10, 10] && size (cc2) == [10, 10]
%! && abs (cc1 - cc2) < sqrt (eps)));

%!assert(corrcoef (5), 1);

%% Test input validation
%!error corrcoef ();
%!error corrcoef (1, 2, 3);
%!error corrcoef ([true, true]);
%!error corrcoef ([1, 2], [true, true]);
%!error corrcoef (ones (2,2,2));
%!error corrcoef (ones (2,2), ones (2,2,2));

