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
## @deftypefn  {} {@var{data} =} getaudiodata (@var{recorder})
## @deftypefnx {} {@var{data} =} getaudiodata (@var{recorder}, @var{datatype})
## Return audio data from audiorecorder object @var{recorder} as a double
## matrix with values between -1.0 and 1.0 and with as many columns as there
## are channels in @var{recorder}.
##
## Given the optional argument @var{datatype}, convert the recorded data
## to the specified type, which may be one of @qcode{"double"},
## @qcode{"single"}, @qcode{"int16"}, @qcode{"int8"} or @qcode{"uint8"}.
## @seealso{@audiorecorder/audiorecorder}
## @end deftypefn

function data = getaudiodata (recorder, datatype)

  hrecorder = struct (recorder).recorder;

  if (nargin == 1)
    data = __recorder_getaudiodata__ (hrecorder);
  else
    data = __recorder_getaudiodata__ (hrecorder);
    switch (datatype)
      case "double"
        ## Do nothing, data is already of type double
      case "single"
        data = single (data);
      case "int16"
        data = int16 (data * (2.0 ^ 15));
      case "int8"
        data = int8 (data * (2.0 ^ 7));
      case "uint8"
        data = uint8 ((data + 1.0) * 0.5 * (2.0 ^ 8 - 1));
      otherwise
        error ('@audiorecorder/getaudiodata: invalid DATATYPE "%s"', datatype);
    endswitch
  endif

  if (get (recorder, "NumberOfChannels") == 2)
    data = data.';
  else
    data = data(1,:).';
  endif

endfunction


## Tests of audiorecorder must not actually record anything.
%!testif HAVE_PORTAUDIO; audiodevinfo (1) > 0
%! recorder = audiorecorder (44100, 16, 2);
%! data = getaudiodata (recorder);
%! assert (isa (data, "double"));
%! data = getaudiodata (recorder, "double");
%! assert (isa (data, "double"));
%! data = getaudiodata (recorder, "single");
%! assert (isa (data, "single"));
%! data = getaudiodata (recorder, "int16");
%! assert (isa (data, "int16"));
%! data = getaudiodata (recorder, "int8");
%! assert (isa (data, "int8"));
%! data = getaudiodata (recorder, "uint8");
%! assert (isa (data, "uint8"));
%! assert (size (data)(1), recorder.TotalSamples);
%! assert (size (data)(2), 2);

%!testif HAVE_PORTAUDIO; audiodevinfo (1) > 0
%! recorder = audiorecorder (44100, 8, 1);
%! data = getaudiodata (recorder);
%! assert (size (data)(1), recorder.TotalSamples);
%! assert (size (data)(2), 1);

## Test input validation
%!testif HAVE_PORTAUDIO; audiodevinfo (1) > 0
%! recorder = audiorecorder (44100, 16, 2);
%! fail ("getaudiodata (recorder, 'foobar')", "invalid DATATYPE");
