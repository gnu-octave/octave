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
## @deftypefn  {Function File} {@var{handle} =} uicontrol ('Name', value, @dots{})
## @deftypefnx {Function File} {@var{handle} =} uicontrol (@var{parent}, 'Name', value, @dots{})
## @deftypefnx {Function File} {} uicontrol (@var{handle})
## @end deftypefn

## Author: goffioul

function handle = uicontrol (varargin)

  if (nargin == 1 && ishandle (varargin{1}) && strcmpi (get (varargin{1}, "type"), "uicontrol"))
    error ("uicontrol focusing not implemented yet.");
  else
    [h, args] = __uiobject_split_args__ ("uicontrol", varargin, {"figure", "uipanel", "uibuttongroup"});
    handle = __go_uicontrol__ (h, args{:});
  endif

endfunction
