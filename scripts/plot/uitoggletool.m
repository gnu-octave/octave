## Copyright (C) 2012 Michael Goffioul
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
## @deftypefn  {Function File} {@var{handle} =} uitoggletool ('Name', value, @dots{})
## @deftypefnx {Function File} {@var{handle} =} uitoggletool (@var{parent}, 'Name', value, @dots{})
## @end deftypefn

## Author: goffioul

function handle = uitoggletool (varargin)

  [h, args] = __uiobject_split_args__ ("uitoggletool", varargin, {"uitoolbar"}, 0);
  if (isempty (h))
    h = findobj (gcf, "-depth", 1, "type", "uitoolbar");
    if (isempty (h))
      h = uitoolbar ();
    else
      h = h(1);
    endif
  endif
  handle = __go_uitoggletool__ (h, args{:});

endfunction
