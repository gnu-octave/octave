########################################################################
##
## Copyright (C) 2013-2023 The Octave Project Developers
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
## @deftypefn  {} {} set (@var{player}, @var{name}, @var{value})
## @deftypefnx {} {} set (@var{player}, @var{name_cell}, @var{value_cell})
## @deftypefnx {} {} set (@var{player}, @var{properties_struct})
## @deftypefnx {} {@var{properties} =} set (@var{player})
## Set the value of property specified by @var{name} to a given @var{value}.
##
## If @var{name} and @var{value} are cell arrays, set each property to the
## corresponding value.  Given a structure of properties with fields
## corresponding to property names, set the value of those properties to the
## corresponding field values.  Given only an audioplayer object, return a
## structure of configurable properties (i.e., writeable properties).
## @seealso{@audioplayer/get, @audioplayer/audioplayer}
## @end deftypefn

function properties = set (player, varargin)

  if (nargin > 3)
    print_usage ();
  endif

  hplayer = struct (player).player;

  if (nargin == 1)
    properties = struct ("SampleRate", {{}}, "Tag", {{}}, "UserData", {{}});
  elseif (nargin == 2)
    for [value, property] = varargin{1}
      setproperty (hplayer, property, value);
    endfor
  elseif (nargin == 3)
    if (iscell (varargin{1}))
      index = 1;
      for property = varargin{1}
        setproperty (hplayer, char (property), varargin{2}{index});
        index += 1;
      endfor
    else
      setproperty (hplayer, varargin{1}, varargin{2});
    endif
  endif

endfunction

function setproperty (player, property, value)

  switch (lower (property))
    case "samplerate"
      __player_set_fs__ (player, value);
    case "tag"
      __player_set_tag__ (player, value);
    case "userdata"
      __player_set_userdata__ (player, value);
    otherwise
      error ('@audioplayer/set: "%s" is not a valid property name or is read-only', property);
  endswitch

endfunction


%!testif HAVE_PORTAUDIO; audiodevinfo (0) > 0
%! player = audioplayer ([-1, 1], 44100, 8);
%! set (player, "SampleRate", 8800);
%! set (player, "Tag", "mytag");
%! ## Also test case insensitivity
%! set (player, "USERdata", [1, 2; 3, 4]);
%! assert (player.SampleRate, 8800);
%! assert (player.Tag, "mytag");
%! assert (player.UserData, [1, 2; 3, 4]);

%!testif HAVE_PORTAUDIO; audiodevinfo (0) > 0
%! player = audioplayer ([-1, 1], 44100, 8);
%! set (player, {"SampleRate", "Tag", "UserData"},
%!              {8800, "mytag", [1, 2; 3, 4]});
%! assert (player.SampleRate, 8800);
%! assert (player.Tag, "mytag");
%! assert (player.UserData, [1, 2; 3, 4]);

%!testif HAVE_PORTAUDIO; audiodevinfo (0) > 0
%! player = audioplayer ([-1, 1], 44100, 8);
%! props = set (player);
%! props.SampleRate = 8800;
%! props.Tag = "mytag";
%! props.UserData = [1, 2; 3, 4];
%! set (player, props);
%! assert (player.SampleRate, 8800);
%! assert (player.Tag, "mytag");
%! assert (player.UserData, [1, 2; 3, 4]);

## Test input validation
%!testif HAVE_PORTAUDIO; audiodevinfo (0) > 0
%! player = audioplayer ([-1, 1], 44100, 8);
%! fail ('set (player, "foobar", 1)', "not a valid property name");
%! fail ('set (player, "Running", 1)', "is read-only");
