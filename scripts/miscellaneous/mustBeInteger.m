########################################################################
##
## Copyright (C) 2019-2023 The Octave Project Developers
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
## @deftypefn {} {} mustBeInteger (@var{x})
##
## Require that input @var{x} is integer-valued (but not necessarily
## integer-typed).
##
## Raise an error if any element of the input @var{x} is not a finite,
## real, integer-valued numeric value, as determined by various checks.
##
## @seealso{mustBeNumeric}
## @end deftypefn

function mustBeInteger (x)

  if (nargin < 1)
    print_usage ();
  endif

  if (isinteger (x) || islogical (x))
    return;
  endif

  but = [];
  if (! isnumeric (x))
    but = sprintf ("it was non-numeric (found a %s)", class (x));
  elseif (! isreal (x))
    but = "it was complex";
  elseif (issparse (x) && (any (isinf (x)) || any (isnan (x))))
    but = "there were non-finite values";
  elseif (! issparse (x) && any (! isfinite (x)))
    but = "there were non-finite values";
  elseif (any (x != fix (x)))
    but = "it had fractional values in some elements";
  endif

  if (! isempty (but))
    label = inputname (1);
    if (isempty (label))
      label = "input";
    endif
    error ("%s must be integer-valued; but %s", label, but);
  endif

endfunction


%!test
%! mustBeInteger ([]);
%! mustBeInteger (42);
%! mustBeInteger (1:1000);
%! mustBeInteger (int32 (42));
%! mustBeInteger (true);

%!error <Invalid call> mustBeInteger ()
%!error <it was non-numeric> mustBeInteger ("Hello World")
%!error <it was complex> mustBeInteger ([1, 2i])
%!error <there were non-finite values> mustBeInteger (Inf)
%!error <there were non-finite values> mustBeInteger (NaN)
%!error <it had fractional values> mustBeInteger ([1 2 3 4.4])
