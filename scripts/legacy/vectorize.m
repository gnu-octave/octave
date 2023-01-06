########################################################################
##
## Copyright (C) 2020-2023 The Octave Project Developers
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
## @deftypefn {} {@var{vfcn} =} vectorize (@var{fcn})
## Create a vectorized version of the anonymous function or expression
## @var{fcn} by replacing all occurrences of @code{*}, @code{/}, etc.,
## with @code{.*}, @code{./}, etc.
##
## Note that the transformation is extremely simplistic.  Use of this
## function is strongly discouraged.  It may be removed from a future
## version of Octave.
## @end deftypefn

function vfcn = vectorize (fcn)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:legacy-function",
             "vectorize is unreliable; its use is strongly discouraged\n");
  endif

  if (nargin < 1)
    print_usage ();
  endif

  if (is_function_handle (fcn))
    finfo = functions (fcn);
    if (! strcmp (finfo.type, "anonymous"))
      error ("vectorize: FCN must be a string or anonymous function handle");
    endif
    expr = finfo.function;
    idx = index (expr, ")");
    args = expr(1:idx);
    expr = expr(idx+1:end);
    new_expr = __vectorize__ (expr);
    vfcn = str2func ([args, new_expr]);
  elseif (ischar (fcn))
    vfcn = __vectorize__ (fcn);
  else
    error ("vectorize: FCN must be a string or anonymous function handle");
  endif

endfunction


%!assert (vectorize ("x.^2 + 1"), "x.^2 + 1")
%!test
%! fh = @(x) x.^2 + 1;
%! finfo = functions (vectorize (fh));
%! assert (finfo.function, "@(x) x .^ 2 + 1");

%!assert (vectorize ("1e-3*y + 2e4*z"), "1e-3.*y + 2e4.*z")
%!test
%! fh = @(x, y, z) 1e-3*y + 2e4*z;
%! finfo = functions (vectorize (fh));
%! assert (finfo.function, "@(x, y, z) 1e-3 .* y + 2e4 .* z");

%!assert (vectorize ("2^x^5"), "2.^x.^5")
%!test
%! fh = @(x) 2^x^5;
%! finfo = functions (vectorize (fh));
%! assert (finfo.function, "@(x) 2 .^ x .^ 5");

## Test input validation
%!error <Invalid call> vectorize ()
%!error <FCN must be a string or anonymous function handle> vectorize (1)
