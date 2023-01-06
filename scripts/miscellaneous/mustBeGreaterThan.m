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
## @deftypefn {} {} mustBeGreaterThan (@var{x}, @var{c})
##
## Require that input @var{x} is greater than @var{c}.
##
## Raise an error if any element of the input @var{x} is not greater than
## @var{c}, as determined by @code{@var{x} > @var{c}}.
##
## @seealso{mustBeGreaterThanOrEqual, mustBeLessThan, gt}
## @end deftypefn

function mustBeGreaterThan (x, c)

  if (nargin != 2)
    print_usage ();
  endif

  tf = (x > c)(:);
  if (! all (tf))
    label = inputname (1);
    if (isempty (label))
      label = "input";
    endif
    bad_idx = find (! tf);
    try
      bad_val = x(bad_idx);
      errmsg = sprintf ("%s must be greater than %f; found %d elements that were not: values %s", ...
                        label, c, numel (bad_idx), mat2str (bad_val));
    catch
      errmsg = sprintf ("%s must be greater than %f; found %d elements that were not: indexes %s", ...
                        label, c, numel (bad_idx), mat2str (bad_idx));
    end_try_catch
    error (errmsg);
  endif

endfunction


%!test
%! mustBeGreaterThan (42, 0);
%! mustBeGreaterThan (Inf, 9999);

%!error <Invalid call> mustBeGreaterThan ()
%!error <Invalid call> mustBeGreaterThan (1)
%!error <called with too many inputs> mustBeGreaterThan (1, 2, 3)
%!error <input must be greater than 42> mustBeGreaterThan (42, 42)
%!error <found 1 elements that were not> mustBeGreaterThan (Inf, Inf)
%!error <must be greater than 0> mustBeGreaterThan (NaN, 0)
