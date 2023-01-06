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
## @deftypefn {} {} mustBeGreaterThanOrEqual (@var{x}, @var{c})
##
## Require that input @var{x} is greater than or equal to @var{c}.
##
## Raise an error if any element of the input @var{x} is not greater than
## or equal to @var{c}, as determined by @code{@var{x} >= @var{c}}.
##
## @seealso{mustBeGreaterThan, mustBeLessThanOrEqual, ge}
## @end deftypefn

function mustBeGreaterThanOrEqual (x, c)

  if (nargin != 2)
    print_usage ();
  endif

  tf = (x >= c)(:);
  if (! all (tf))
    label = inputname (1);
    if (isempty (label))
      label = "input";
    endif
    bad_idx = find (! tf);
    try
      bad_val = x(bad_idx);
      errmsg = sprintf ("%s must be greater than or equal to %f; found %d elements that were not: values %s", ...
                        label, c, numel (bad_idx), mat2str (bad_val));
    catch
      errmsg = sprintf ("%s must be greater than or equal to %f; found %d elements that were not: indexes %s", ...
                        label, c, numel (bad_idx), mat2str (bad_idx));
    end_try_catch
    error (errmsg);
  endif

endfunction


%!test
%! mustBeGreaterThanOrEqual (42, 0);
%! mustBeGreaterThanOrEqual (Inf, 9999);
%! mustBeGreaterThanOrEqual (42, 42);
%! mustBeGreaterThanOrEqual (Inf, Inf);

%!error <Invalid call> mustBeGreaterThanOrEqual ()
%!error <Invalid call> mustBeGreaterThanOrEqual (1)
%!error <called with too many inputs> mustBeGreaterThanOrEqual (1, 2, 3)
%!error <must be greater than or equal to 2> mustBeGreaterThanOrEqual (1, 2)
%!error <must be greater than or equal to 0> mustBeGreaterThanOrEqual (NaN, 0)
