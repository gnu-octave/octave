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
## @deftypefn {} {} mustBeLessThanOrEqual (@var{x}, @var{c})
##
## Require that input is less than or equal to a given value.
##
## Raise an error if any element of the input @var{x} is not less than
## or equal to @var{c}, as determined by @code{@var{x} <= @var{c}}.
##
## @seealso{mustBeLessThan, mustBeGreaterThanOrEqual, le}
## @end deftypefn

function mustBeLessThanOrEqual (x, c)

  if (nargin != 2)
    print_usage ();
  endif

  tf = (x <= c)(:);
  if (! all (tf))
    label = inputname (1);
    if (isempty (label))
      label = "input";
    endif
    bad_idx = find (! tf);
    try
      bad_val = x(bad_idx);
      errmsg = sprintf ("%s must be less than or equal to %f; found %d elements that were not: values %s", ...
                        label, c, numel (bad_idx), mat2str (bad_val));
    catch
      errmsg = sprintf ("%s must be less than or equal to %f; found %d elements that were not: indexes %s", ...
                        label, c, numel (bad_idx), mat2str (bad_idx));
    end_try_catch
    error (errmsg);
  endif

endfunction


%!test
%! mustBeLessThanOrEqual (0, 1);
%! mustBeLessThanOrEqual (-Inf, 42);
%! mustBeLessThanOrEqual (42, Inf);
%! mustBeLessThanOrEqual (1:41, 42);
%! mustBeLessThanOrEqual (1:42, 42);
%! mustBeLessThanOrEqual (1, 1);
%! mustBeLessThanOrEqual (Inf, Inf);

%!error <Invalid call> mustBeLessThanOrEqual ()
%!error <Invalid call> mustBeLessThanOrEqual (1)
%!error <called with too many inputs> mustBeLessThanOrEqual (1, 2, 3)
%!error <must be less than or equal to 0> mustBeLessThanOrEqual (1, 0)
