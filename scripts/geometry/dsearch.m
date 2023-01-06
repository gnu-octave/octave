########################################################################
##
## Copyright (C) 2007-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{idx} =} dsearch (@var{x}, @var{y}, @var{tri}, @var{xi}, @var{yi})
## @deftypefnx {} {@var{idx} =} dsearch (@var{x}, @var{y}, @var{tri}, @var{xi}, @var{yi}, @var{s})
## Return the index @var{idx} of the closest point in @code{@var{x}, @var{y}}
## to the elements @code{[@var{xi}(:), @var{yi}(:)]}.
##
## The variable @var{s} is accepted for compatibility but is ignored.
## @seealso{dsearchn, tsearch}
## @end deftypefn

function idx = dsearch (x, y, tri, xi, yi, s)

  if (nargin < 5)
    print_usage ();
  endif

  idx = __dsearchn__ ([x(:), y(:)], [xi(:), yi(:)]);

endfunction


%!shared x, y, tri
%! x = [-1;-1;1];
%! y = [-1;1;-1];
%! tri = [1,2,3];
%!assert (dsearch (x,y,tri,1,1/3), 3)
%!assert (dsearch (x,y,tri,1/3,1), 2)
