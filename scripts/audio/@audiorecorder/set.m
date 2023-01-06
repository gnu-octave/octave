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
## @deftypefn  {} {} set (@var{recorder}, @var{name}, @var{value})
## @deftypefnx {} {} set (@var{recorder}, @var{name_cell}, @var{value_cell})
## @deftypefnx {} {} set (@var{recorder}, @var{properties_struct})
## @deftypefnx {} {@var{properties} =} set (@var{recorder})
## Set the value of property specified by @var{name} to a given @var{value}.
##
## If @var{name} and @var{value} are cell arrays, set each property to a
## corresponding value.  Given a structure of properties with fields
## corresponding to property names, set the value of those properties to the
## corresponding field values.  Given only a recorder object, return a
## structure of configurable properties (i.e., writeable properties).
## @seealso{@audiorecorder/get, @audiorecorder/audiorecorder}
## @end deftypefn

function properties = set (recorder, varargin)

  if (nargin > 3)
    print_usage ();
  endif

  hrecorder = struct (recorder).recorder;

  if (nargin == 1)
    properties = struct ("SampleRate", {{}}, "Tag", {{}}, "UserData", {{}});

  elseif (nargin == 2)
    for [value, property] = varargin{1}
      setproperty (hrecorder, property, value);
    endfor

  elseif (nargin == 3)
    if (iscell (varargin{1}))
      index = 1;
      for property = varargin{1}
        setproperty (hrecorder, char (property), varargin{2}{index});
        index += 1;
      endfor
    else
      setproperty (hrecorder, varargin{1}, varargin{2});
    endif
  endif

endfunction

function setproperty (recorder, property, value)

  switch (lower (property))
    case "samplerate"
      __recorder_set_fs__ (recorder, value);
    case "tag"
      __recorder_set_tag__ (recorder, value);
    case "userdata"
      __recorder_set_userdata__ (recorder, value);
    otherwise
      error ('@audiorecorder/set: "%s" is not a valid property name or is read-only', property);
  endswitch

endfunction


## Tests of audiorecorder must not actually record anything.
%!testif HAVE_PORTAUDIO; audiodevinfo (1) > 0
%! recorder = audiorecorder ();
%! set (recorder, "SampleRate", 8800);
%! set (recorder, "Tag", "mytag");
%! ## Also test case insensitivity
%! set (recorder, "USERdata", [1, 2; 3, 4]);
%! assert (recorder.SampleRate, 8800);
%! assert (recorder.Tag, "mytag");
%! assert (recorder.UserData, [1, 2; 3, 4]);

%!testif HAVE_PORTAUDIO; audiodevinfo (1) > 0
%! recorder = audiorecorder ();
%! set (recorder, {"SampleRate", "Tag", "UserData"},
%!                {8800, "mytag", [1, 2; 3, 4]});
%! assert (recorder.SampleRate, 8800);
%! assert (recorder.Tag, "mytag");
%! assert (recorder.UserData, [1, 2; 3, 4]);

%!testif HAVE_PORTAUDIO; audiodevinfo (1) > 0
%! recorder = audiorecorder ();
%! props = set (recorder);
%! props.SampleRate = 8800;
%! props.Tag = "mytag";
%! props.UserData = [1, 2; 3, 4];
%! set (recorder, props);
%! assert (recorder.SampleRate, 8800);
%! assert (recorder.Tag, "mytag");
%! assert (recorder.UserData, [1, 2; 3, 4]);

## Test input validation
%!testif HAVE_PORTAUDIO; audiodevinfo (1) > 0
%! recorder = audiorecorder ();
%! fail ('set (recorder, "foobar", 1)', "not a valid property name");
%! fail ('set (recorder, "Running", 1)', "is read-only");
