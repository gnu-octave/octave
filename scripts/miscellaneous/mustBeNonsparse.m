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
## @deftypefn {} {} mustBeNonsparse (@var{x})
##
## Require that input @var{x} is not sparse.
##
## Raise an error if the input @var{x} is sparse, as determined by
## @code{issparse (@var{x})}.
##
## @seealso{issparse}
## @end deftypefn

function mustBeNonsparse (x)

  if (nargin < 1)
    print_usage ();
  endif

  if (issparse (x))
    label = inputname (1);
    if (isempty (label))
      label = "input";
    endif
    error ("%s must be nonsparse", label);
  endif

endfunction


%!test
%! mustBeNonsparse ([]);
%! mustBeNonsparse (42);
%! mustBeNonsparse (1:100);
%! mustBeNonsparse ("Hello World");

%!error <Invalid call> mustBeNonsparse ()
%!error <input must be nonsparse> mustBeNonsparse (sparse (42))
