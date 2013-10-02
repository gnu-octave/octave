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
## @deftypefn{Function File} set (@var{playerObj}, @var{Name}, @var{Value})
## Set the value of property specified by @var{Name} to a given @var{Value}.
## @end deftypefn
## @deftypefn{Function File} set (@var{playerObj}, @var{CellOfNames}, @var{CellOfValues})
## Given a cell array of property names and a cell array of values, set each property to a corresponding value.
## @end deftypefn
## @deftypefn{Function File} set (@var{playerObj}, @var{StructOfProperties})
## Given a structure where fields are property names, set the value of those properties for an audioplayer object to corresponding values.
## @end deftypefn
## @deftypefn{Function File} @var{settableProperties} = set (@var{playerObj})
## Returns a structure of settable properties.
## @end deftypefn

function settable = set (varargin)
  if nargin < 1 || nargin > 3
    print_usage();
  endif
  player = struct (varargin{1}).player;
  if nargin == 1
    settable.SampleRate = {};
    settable.Tag = {};
    settable.UserData = {};
  elseif nargin == 2
    for [value, property] = varargin{2}
      setproperty (player, property, value);
    endfor
  elseif nargin == 3
    if iscell (varargin{2})
      index = 1;
      for property = varargin{2}
        setproperty (player, char(property), varargin{3}{index});
        index = index + 1;
      endfor
    else
      setproperty (player, varargin{2}, varargin{3});
    endif
  else
    error ('audioplayer: wrong number of arguments to the set method');
  endif
endfunction

function setproperty (player, property, value)
  switch property
    case 'SampleRate'
      __player_set_fs__ (player, value);
    case 'Tag'
      __player_set_tag__ (player, value);
    case 'UserData'
      __player_set_userdata__ (player, value);
    otherwise
      error ('audioplayer: no such property or the property specified is read-only');
  endswitch
endfunction
