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
## @deftypefn  {} {@var{recorder} =} audiorecorder ()
## @deftypefnx {} {@var{recorder} =} audiorecorder (@var{fs}, @var{nbits}, @var{nchannels})
## @deftypefnx {} {@var{recorder} =} audiorecorder (@var{fs}, @var{nbits}, @var{nchannels}, @var{id})
## Create an audiorecorder object recording 8-bit mono audio at 8000 Hz
## sample rate.
##
## The optional arguments @var{fs}, @var{nbits}, @var{nchannels}, and @var{id}
## specify the sample rate, number of bits per sample, number of channels, and
## recording device ID, respectively.  Device IDs may be found using the
## @code{audiodevinfo} function.
##
## The list of actions for an audiorecorder object are shown below.  All
## methods require an audiorecorder object as the first argument.
##
## @multitable @columnfractions 0.22 0.73
## @headitem Method @tab Description
## @item get @tab Read audiorecorder property values
## @item getaudiodata @tab Return audio data as a numeric matrix
## @item getplayer @tab Return audioplayer loaded with data from audiorecorder
## @item isrecording @tab Return true if audiorecorder is recording
## @item pause @tab Pause recording
## @item play @tab Play audio stored in audiorecorder object
## @item record @tab Record audio in audiorecorder object w/o blocking
## @item recordblocking @tab Record audio in audiorecorder object
## @item resume @tab Resume recording after pause
## @item set @tab Write audiorecorder property values
## @item stop @tab Stop recording
## @end multitable
## @end deftypefn
## @seealso{@audiorecorder/get, @audiorecorder/getaudiodata,
## @audiorecorder/getplayer, @audiorecorder/isrecording,
## @audiorecorder/pause, @audiorecorder/play, @audiorecorder/record,
## @audiorecorder/recordblocking, @audioplayer/resume, @audiorecorder/set,
## @audiorecorder/stop, audiodevinfo, @audioplayer/audioplayer, record}

################################################################################
## FIXME: callbacks don't work properly, apparently because portaudio
## will execute the callbacks in a separate thread, and calling Octave
## functions in a separate thread is likely to cause trouble with
## all of Octave's global data...
##
## @deftypefnx {} {@var{recorder} =} audiorecorder (@var{function}, @dots{})
##
## Given a function handle, use that function to process the audio.
################################################################################

function recorder = audiorecorder (varargin)

  if (nargin > 5)
    print_usage ();
  endif

  ## FIXME: Prevent use of callbacks until situation is fixed.
  if (nargin > 0 && (is_function_handle (varargin{1}) || ischar (varargin{1})))
    error ("audiorecorder: first argument cannot be a callback function");
  endif

  ## FIXME: Uncomment when callback functions are supported.
  ## if (nargin > 0 && ischar (varargin{1}))
  ##   varargin{1} = str2func (varargin{1});
  ## endif

  recorder.recorder = __recorder_audiorecorder__ (varargin{:});
  recorder = class (recorder, "audiorecorder");

endfunction


%!demo
%! ## Record 1 second of audio and play it back in two ways
%! recorder = audiorecorder (44100, 16, 2);
%! record (recorder, 1);
%! pause (2);
%! player1 = audioplayer (recorder);
%! player2 = getplayer (recorder);
%! play (player1);
%! pause (2);
%! play (player2);
%! pause (2);
%! stop (player1);
%! stop (player2);

## Tests of audiorecorder must not actually record anything.

## FIXME: Uncomment when callbacks are supported
%!#function status = callback_record (sound)
%!#  fid = fopen ("record.txt", "at");
%!#  for index = 1:rows(sound)
%!#    fprintf (fid, "%.4f, %.4f\n", sound(index, 1), sound(index, 2));
%!#  endfor
%!#  fclose (fid);
%!#  status = 0;
%!#endfunction

%!#testif HAVE_PORTAUDIO
%!# recorder = audiorecorder (@callback_record, 44100);
%!# unlink ("record.txt")
%!# record (recorder);
%!# pause (2);
%!# stop (recorder);
%!# s = stat ("record.txt");
%!# assert (s.size > 0);

%!#testif HAVE_PORTAUDIO
%!# recorder = audiorecorder (@callback_record, 44100);
%!# unlink ("record.txt")
%!# record (recorder);
%!# pause (2);
%!# stop (recorder);
%!# s = stat ("record.txt");
%!# assert (s.size > 0);

## Test input validation
%!error <first argument cannot be a callback> audiorecorder (@ls)
%!error <first argument cannot be a callback> audiorecorder ("ls")
