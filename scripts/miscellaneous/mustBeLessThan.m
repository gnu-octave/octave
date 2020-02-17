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
## @deftypefn {} {} mustBeLessThan (@var{x}, @var{c})
##
## Requires that input @var{x} is less than @var{c}.
##
## Raises an error if any element of the input @var{x} is not less than
## @var{c}, as determined by @code{@var{x} < @var{c}}.
##
## @end deftypefn

function mustBeLessThan (x, c)
  tf = x < c;
  tf = tf(:);
  if ! all (tf)
    label = inputname (1);
    if isempty (label)
      label = "input";
    endif
    ix_bad = find (! tf);
    try
      bad = x(ix_bad);
      errmsg = sprintf ( ...
        "%s must be less than %f; got %d elements that were not: values %s", ...
        label, c, numel (ix_bad), mat2str (bad));
    catch err
      errmsg = sprintf ( ...
        "%s must be less than %f; got %d elements that were not: indexes %s", ...
        label, c, numel (ix_bad), mat2str (ix_bad));
    end_try_catch
    error (errmsg);
  endif
endfunction

%!test
%! mustBeLessThan (0, 1)
%! mustBeLessThan (-Inf, 42)
%! mustBeLessThan (42, Inf)
%! mustBeLessThan (1:41, 42)

%!error mustBeLessThan ()
%!error mustBeLessThan (1, 0)
%!error mustBeLessThan (1, 1)
%!error mustBeLessThan (Inf, Inf)
%!error mustBeLessThan (1:42, 42)
