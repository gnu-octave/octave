## Copyright (C) 2008 Bill Denney
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {Function File} {@var{h} =} allchild (@var{handles})
## Find all children, including hidden children, of a graphics object.
##
## This function is similar to @code{get (h, "children")}, but also
## returns hidden objects.  If @var{handles} is a scalar,
## @var{h} will be a vector.  Otherwise, @var{h} will be a cell matrix
## of the same size as @var{handles} and each cell will contain a
## vector of handles.
## @seealso{get, set, findall, findobj}
## @end deftypefn

## Author: Bill Denney <bill@denney.ws>

function h = allchild (handles)

  shh = get (0, "showhiddenhandles");
  unwind_protect
    set (0, "showhiddenhandles", "on");
    if (isscalar (handles))
      h = get (handles, "children");
    else
      h = cell (size (handles));
      for i = 1:numel (handles)
        h{i} = get (handles, "children");
      endfor
    endif
  unwind_protect_cleanup
    set (0, "showhiddenhandles", shh);
  end_unwind_protect

endfunction
