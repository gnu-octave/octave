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
## @deftypefn {} {} mustBeNonpositive (@var{x})
##
## Require that input @var{x} is not positive.
##
## Raise an error if any element of the input @var{x} is positive, as
## determined by @code{@var{x} <= 0}.
##
## @seealso{mustBeNegative, mustBeNonzero}
## @end deftypefn

function mustBeNonpositive (x)

  if (nargin < 1)
    print_usage ();
  endif

  tf = (x(:) <= 0);
  if (! all (tf))
    label = inputname (1);
    if (isempty (label))
      label = "input";
    endif
    bad_idx = find (! tf);
    try
      bad_val = x(bad_idx);
      errmsg = sprintf ("%s must be non-positive; found %d elements that were not: values %s", ...
                        label, numel (bad_idx), mat2str (bad_val));
    catch
      errmsg = sprintf ("%s must be non-positive; found %d elements that were not: indexes %s", ...
                        label, numel (bad_idx), mat2str (bad_idx));
    end_try_catch
    error (errmsg);
  endif

endfunction


%!test
%! mustBeNonpositive (0);
%! mustBeNonpositive (-1);
%! mustBeNonpositive (-5:-1);
%! mustBeNonpositive (-Inf);

%!error <Invalid call> mustBeNonpositive ()
%!error <must be non-positive> mustBeNonpositive (1)
%!error <found 2 elements> mustBeNonpositive (-10:2)
%!error <must be non-positive> mustBeNonpositive (NaN)
