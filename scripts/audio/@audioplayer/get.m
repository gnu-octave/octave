## Copyright (C) 2013 Vytautas Jančauskas
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
## @deftypefn{Function File} @var{Value} = get (@var{playerObj}, @var{Name})
## Returns the @var{Value} of the property identified by @var{Name}.
## @end deftypefn
## @deftypefn{Function File} @var{Values} = get (@var{playerObj}, @{@var{Name1}, ... , @var{NameN}@})
## Returns the @var{Values} of the properties identified by @var{Name1} to @var{NameN}.
## @end deftypefn
## @deftypefn{Function File} @var{Values} = get (@var{playerObj})
## Returns a scalar structure with values of all properties of @var{playerObj}. 
## The field names correspond to property names.
## @end deftypefn

function result = get (varargin)
  player = varargin{1};
  properties = __get_properties__ (player);
  if nargin == 1
    result = properties;
  elseif nargin == 2
    if ischar (varargin{2})
      result = getfield (properties, varargin{2});
    else
      result = {};
      index = 1;
      for property = varargin{2}
        result{index} = getfield (properties, char(property));
        index = index + 1;
      endfor
    endif
  else
    error ('audioplayer: wrong number of arguments to the get method');
  endif
endfunction
