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
## @deftypefn {} {} mustBeNonzero (@var{x})
##
## Require that input @var{x} is not zero.
##
## Raise an error if any element of the input @var{x} is zero, as determined
## by @code{@var{x} == 0}.
##
## @seealso{mustBeNonnegative, mustBePositive}
## @end deftypefn

function mustBeNonzero (x)

  if (nargin < 1)
    print_usage ();
  endif

  tf = (x(:) == 0);
  if (any (tf))
    label = inputname (1);
    if (isempty (label))
      label = "input";
    endif
    bad_idx = find (tf);
    errmsg = sprintf ("%s must be nonzero; found %d elements that were zero: indexes %s", ...
                      label, numel (bad_idx), mat2str (bad_idx));
    error (errmsg);
  endif

endfunction


%!test
%! mustBeNonzero (1);
%! mustBeNonzero (-1);
%! mustBeNonzero ([-5:-1 1:5]);
%! mustBeNonzero (Inf);
%! mustBeNonzero (-Inf);
%! mustBeNonzero (NaN);
%! mustBeNonzero (eps);

%!error <Invalid call> mustBeNonzero ()
%!error <found 1 elements> mustBeNonzero (-10:10)
%!error <found 2 elements> mustBeNonzero ([-1, 0, 0, 1])
