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
## @deftypefn {} {} mustBeFinite (@var{x})
##
## Require that input @var{x} is finite.
##
## Raise an error if any element of the input @var{x} is not finite, as
## determined by @code{isfinite (x)}.
##
## @seealso{mustBeNonNan, isfinite}
## @end deftypefn

function mustBeFinite (x)

  if (nargin < 1)
    print_usage ();
  endif

  tf = isfinite (x(:));
  if (! all (tf))
    label = inputname (1);
    if (isempty (label))
      label = "input";
    endif
    bad_idx = find (! tf);
    error ("%s must be finite; found %d non-finite elements: indexes %s",
           label, numel (bad_idx), mat2str (bad_idx));
  endif

endfunction


%!test
%! mustBeFinite ([]);
%! mustBeFinite (42);
%! mustBeFinite (-100:.1:100);
%! mustBeFinite (int32 (42));

%!error <Invalid call> mustBeFinite ()
%!error <found 1 non-finite> mustBeFinite (Inf)
%!error <indexes 4> mustBeFinite ([1 2 3 Inf])
%!error <indexes 1> mustBeFinite ([-Inf -1 0 1])
%!error <indexes 1> mustBeFinite ([NaN -1])
