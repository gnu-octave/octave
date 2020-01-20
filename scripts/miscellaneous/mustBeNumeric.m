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
## @deftypefn {} {} mustBeNumeric (@var{x})
##
## Requires that input @var{x} is numeric.
##
## Raises an error if the input @var{x} is not numeric, as determined by
## @code{isnumeric (@var{x})}.
##
## @end deftypefn

function mustBeNumeric (x)
  if ! isnumeric (x)
    label = inputname (1);
    if isempty (label)
      label = "input";
    endif
    error ("%s must be numeric; got a %s", label, class (x));
  endif
endfunction

%!test
%! mustBeNumeric ([])
%! mustBeNumeric (42)
%! mustBeNumeric (int32(42))
%! mustBeNumeric (NaN)

%!error mustBeNumeric ()
%!error mustBeNumeric ('foo')
%!error mustBeNumeric (struct)
%!error mustBeNumeric ({})
%!error mustBeNumeric (true)
