########################################################################
##
## Copyright (C) 2019-2020 The Octave Project Developers
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
## Requires that input @var{x} is finite.
##
## Raises an error if any element of the input @var{x} is not finite, as
## determined by @code{isfinite (x)}.
##
## @end deftypefn

function mustBeFinite (x)
  tf = isfinite (x);
  if ! all (tf)
    label = inputname (1);
    if isempty (label)
      label = "input";
    endif
    ix_bad = find (!tf);
    error ("%s must be finite; got Infs in %d elements: indexes %s", ...
      label, numel (ix_bad), mat2str (ix_bad));
  endif
endfunction

%!test
%! mustBeFinite ([]);
%! mustBeFinite (42);
%! mustBeFinite (-100:.1:100);
%! mustBeFinite (int32(42))

%!error mustBeFinite ();
%!error mustBeFinite (Inf);
%!error mustBeFinite ([1 2 3 Inf]);
%!error mustBeFinite ([-Inf -1 0 1]);
