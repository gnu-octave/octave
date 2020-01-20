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
## @deftypefn {} {} mustBeNonNan (@var{x})
##
## Requires that input @var{x} is non-NaN.
##
## Raises an error if any element of the input @var{x} is NaN, as determined
## by @code{isnan (@var{x})}.
##
## @end deftypefn

function mustBeNonNan (x)
  tf = ! isnan (x);
  tf = tf(:);
  if ! all (tf)
    label = inputname (1);
    if isempty (label)
      label = "input";
    endif
    ix_bad = find (! tf);
    errmsg = sprintf ( ...
      "%s must be non-NaN; got %d elements that were not: indexes %s", ...
      label, numel (ix_bad), mat2str (ix_bad));
    error (errmsg);
  endif
endfunction

%!test
%! mustBeNonNan (42)
%! mustBeNonNan ('foo')
%! mustBeNonNan (1:10)
%! mustBeNonNan (Inf)
%! mustBeNonNan (-Inf)

%!error mustBeNonNan ()
%!error mustBeNonNan (NaN)
%!error mustBeNonNan ([1 2 3 NaN])
