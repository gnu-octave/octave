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
## @deftypefn {} {} mustBeNumericOrLogical (@var{x})
##
## Require that input @var{x} is numeric or logical.
##
## Raise an error if the input @var{x} is not numeric or logical, as
## determined by @code{isnumeric (@var{x}) || islogical (@var{x})}.
##
## @seealso{mustBeNumeric, isnumeric, islogical}
## @end deftypefn

function mustBeNumericOrLogical (x)

  if (nargin < 1)
    print_usage ();
  endif

  if (! (isnumeric (x) || islogical (x)))
    label = inputname (1);
    if (isempty (label))
      label = "input";
    endif
    error ("%s must be numeric or logical; found a %s", label, class (x));
  endif

endfunction


%!test
%! mustBeNumericOrLogical ([]);
%! mustBeNumericOrLogical (true);
%! mustBeNumericOrLogical (false);
%! mustBeNumericOrLogical (42);
%! mustBeNumericOrLogical (int32 (42));

%!error <Invalid call> mustBeNumericOrLogical ()
%!error <found a char> mustBeNumericOrLogical ("foo")
%!error <found a struct> mustBeNumericOrLogical (struct ())
%!error <found a cell> mustBeNumericOrLogical (cell ())
