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
## @deftypefn  {Function File} {} cor (@var{x})
## @deftypefnx {Function File} {} cor (@var{x}, @var{y})
## Compute matrix of correlation coefficients.
##
## This is an alias for @code{corrcoef}.
## @seealso{corrcoef}
## @end deftypefn

function retval = cor (x, y = x)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "cor is obsolete and will be removed from a future version of Octave; please use corr instead");
  endif

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  retval = corrcoef (x, y);

endfunction


%!test
%! x = rand (10, 2);
%! assert (cor (x), corrcoef (x), 5*eps);
%! assert (cor (x(:,1), x(:,2)) == corrcoef (x(:,1), x(:,2)));

%% Test input validation
%!error corrcoef ();
%!error corrcoef (1, 2, 3);

