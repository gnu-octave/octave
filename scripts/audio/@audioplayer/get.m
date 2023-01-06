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
## @deftypefn  {} {@var{value} =} get (@var{player}, @var{name})
## @deftypefnx {} {@var{values} =} get (@var{player}, @lbracechar{}@var{name1}, @var{name2}, @dots{}@rbracechar{})
## @deftypefnx {} {@var{values} =} get (@var{player})
## Return the @var{value} of the property identified by @var{name}.
##
## If @var{name} is a cell array return the values of the properties
## identified by the elements of the cell array.  Given only the player
## object, return a scalar structure with values for all properties of
## @var{player}.  The field names of the structure correspond to the property
## names.
## @seealso{@audioplayer/set, @audioplayer/audioplayer}
## @end deftypefn

function value = get (player, name)

  properties = __get_properties__ (player);

  if (nargin == 1)
    value = properties;
  elseif (nargin == 2)
    pnames = name;
    if (ischar (pnames))
      value = getproperty (properties, pnames);
    elseif (iscellstr (pnames))
      value = cell (size (pnames));
      for i = 1:numel (pnames)
        value{i} = getproperty (properties, pnames{i});
      endfor
    else
      error ("@audioplayer/get: NAME must be a string or cell array of strings");
    endif
  endif

endfunction

function value = getproperty (properties, pname)

  persistent valid_props;
  if (isempty (valid_props))
    valid_props = { "BitsPerSample", "CurrentSample", "DeviceID", ...
                    "NumberOfChannels", "Running", "SampleRate", ...
                    "TotalSamples", "Tag", "Type", "UserData" };
  endif

  idx = find (strcmpi (pname, valid_props), 1);
  if (isempty (idx))
    error ('@audioplayer/get: "%s" is not a valid property name', pname);
  endif

  value = properties.(valid_props{idx});

endfunction


%!testif HAVE_PORTAUDIO; audiodevinfo (0) > 0
%! player = audioplayer ([-1, 1], 44100, 8);
%! props = get (player);
%! assert (fieldnames (props), {"BitsPerSample"; "CurrentSample"; "DeviceID";
%!         "NumberOfChannels"; "Running"; "SampleRate"; "TotalSamples"; "Tag";
%!         "Type"; "UserData"});
%! value = get (player, "Running");
%! assert (value, "off");
%! value = get (player, "ruNNIng");  # test case insensitivity
%! assert (value, "off");
%! values = get (player, {"SampleRate", "BitsPerSample", "TotalSamples"});
%! assert (values, {44100, 8, 2});

## Test input validation
%!testif HAVE_PORTAUDIO; audiodevinfo (0) > 0
%! player = audioplayer ([-1, 1], 44100, 8);
%! fail ("get (player, 1)", "NAME must be a string");
%! fail ('get (player, "foobar")', '"foobar" is not a valid property');
