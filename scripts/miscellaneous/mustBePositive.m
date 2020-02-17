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
## @deftypefn {} {} mustBePositive (@var{x})
##
## Requires that input @var{x} is positive.
##
## Raises an error if any element of the input @var{x} is not positive, as
## determined by @code{@var{x} > 0}.
##
## @end deftypefn

function mustBePositive (x)
  tf = x > 0;
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
        "%s must be positive; got %d elements that were not: values %s", ...
        label, numel (ix_bad), mat2str (bad));
    catch err
      errmsg = sprintf ( ...
        "%s must be positive; got %d elements that were not: indexes %s", ...
        label, numel (ix_bad), mat2str (ix_bad));
    end_try_catch
    error (errmsg);
  endif
endfunction

%!test
%! mustBePositive (1)
%! mustBePositive (123.456)
%! mustBePositive (Inf)
%! mustBePositive (eps)

%!error mustBePositive ()
%!error mustBePositive (0)
%!error mustBePositive (0:10)
%!error mustBePositive (-1)
%!error mustBePositive ([0 1 2 3 -4])
%!error mustBePositive (-Inf)
%!error mustBePositive (NaN)
