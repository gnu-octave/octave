
## Copyright (C) 2006,2007  Kai Habel
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Function File}  shading (@var{type})
## @deftypefnx {Function File}  shading (@var{ax}, ...)
##
## Sets the shading of surface or patch graphic objects. Valid arguments for
## @var{type} are "flat", "interp", or "faceted".
## If @var{ax} is given the shading is applied to axis @var{ax} instead of the 
## current axis.
##
## @example
## shading ("interp")
## @end example
##
## @end deftypefn

function shading(ax, mode)

  if (nargin == 1)
    mode = ax;
    ax = gca();
  end

  if ((nargin !=1 ) && (nargin != 2))
    print_usage();
  end

  obj = findobj(ax,"Type","patch");
  obj = [obj; findobj(ax,"Type","surface")];

  for n = 1 : length(obj)
    h = obj(n); 
    if strcmp(mode, "flat") 
      set(h,"FaceColor","flat");
      set(h,"EdgeColor","none");
    elseif strcmp(mode,"interp")
      set(h,"FaceColor","interp");
      set(h,"EdgeColor","none");
    elseif strcmp(mode,"faceted")
      set(h,"FaceColor","flat");
      set(h,"EdgeColor",[0 0 0]);
    else
      error("unknown argument")
    endif
  endfor
endfunction
