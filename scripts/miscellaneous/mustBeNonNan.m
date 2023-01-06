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
## @deftypefn {} {} mustBeNonNan (@var{x})
##
## Require that input @var{x} is non-@code{NaN}.
##
## Raise an error if any element of the input @var{x} is @code{NaN}, as
## determined by @code{isnan (@var{x})}.
##
## @seealso{mustBeFinite, mustBeNonempty, isnan}
## @end deftypefn

function mustBeNonNan (x)

  if (nargin < 1)
    print_usage ();
  endif

  tf = isnan (x(:));
  if (any (tf))
    label = inputname (1);
    if (isempty (label))
      label = "input";
    endif
    bad_idx = find (tf);
    errmsg = sprintf ("%s must be non-NaN; found %d elements that were not: indexes %s", ...
                      label, numel (bad_idx), mat2str (bad_idx));
    error (errmsg);
  endif

endfunction


%!test
%! mustBeNonNan (42);
%! mustBeNonNan ("foo");
%! mustBeNonNan (1:10);
%! mustBeNonNan (Inf);
%! mustBeNonNan (-Inf);

%!error <Invalid call> mustBeNonNan ()
%!error <must be non-NaN> mustBeNonNan (NaN)
%!error <input must be non-NaN> mustBeNonNan ([1 2 3 NaN])
