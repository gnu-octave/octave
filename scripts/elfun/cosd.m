########################################################################
##
## Copyright (C) 2006-2020 The Octave Project Developers
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
## @deftypefn {} {} cosd (@var{x})
## Compute the cosine for each element of @var{x} in degrees.
##
## Returns zero for elements where @code{(@var{x}-90)/180} is an integer.
## @seealso{acosd, cos}
## @end deftypefn

function y = cosd (x)

  if (nargin < 1)
    print_usage ();
  endif

  ## Advance phase by 90 degrees to turn sin in to cos and use sind. 
  y = sind (x + 90);

endfunction


%!assert (cosd (10:20:360), cos ([10:20:360] * pi/180), 5*eps)
%!assert (cosd ([-270, -90, 90, 270]) == 0)
%!assert (cosd ([-360, -180, 0, 180, 360]), [1, -1, 1, -1, 1])

%!error <Invalid call> cosd ()
