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
## @deftypefn {} {} mustBeReal (@var{x})
##
## Requires that input @var{x} is real.
##
## Raises an error if the input @var{x} is not real, as determined by
## @code{isreal (@var{x})}.
##
## @end deftypefn

function mustBeReal (x)
  if ! isreal (x)
    label = inputname (1);
    if isempty (label)
      label = "input";
    endif
    error ("%s must be real; got a complex value", label);
  endif
endfunction

%!test
%! mustBeReal ([])
%! mustBeReal (42)
%! mustBeReal (Inf)
%! mustBeReal (NaN)
%! mustBeReal (1:100)
%! mustBeReal (int32(42))

%!error mustBeReal ()
%!error mustBeReal (i)
%!error mustBeReal (2 + i)
