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
## @deftypefn  {} {@var{value} =} get (@var{recorder}, @var{name})
## @deftypefnx {} {@var{values} =} get (@var{recorder}, @lbracechar{}@var{name1}, @var{name2}, @dots{}@rbracechar{})
## @deftypefnx {} {@var{values} =} get (@var{recorder})
## Return the @var{value} of the property identified by @var{name}.
##
## If @var{name} is a cell array return the values of the properties
## identified by the elements of the cell array.  Given only the recorder
## object, return a scalar structure with values for all properties of
## @var{recorder}.  The field names of the structure correspond to the property
## names.
## @seealso{@audiorecorder/set, @audiorecorder/audiorecorder}
## @end deftypefn

function value = get (recorder, name)

  properties = __get_properties__ (recorder);

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
      error ("@audiorecorder/get: NAME must be a string or cell array of strings");
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
    error ('@audiorecorder/get: "%s" is not a valid property name', pname);
  endif

  value = properties.(valid_props{idx});

endfunction


%!testif HAVE_PORTAUDIO; audiodevinfo (1) > 0
%! recorder = audiorecorder (44100, 16, 2);
%! props = get (recorder);
%! assert (fieldnames (props), {"BitsPerSample"; "CurrentSample"; "DeviceID";
%!         "NumberOfChannels"; "Running"; "SampleRate"; "TotalSamples"; "Tag";
%!         "Type"; "UserData"});
%! value = get (recorder, "Running");
%! assert (value, "off");
%! values = get (recorder, {"SampleRate", "BitsPerSample", "NumberOfChannels"});
%! assert (values, {44100, 16, 2});

## Test input validation
%!testif HAVE_PORTAUDIO; audiodevinfo (1) > 0
%! recorder = audiorecorder (44100, 16, 2);
%! fail ("get (recorder, 1)", "NAME must be a string");
%! fail ('get (recorder, "foobar")', '"foobar" is not a valid property');
