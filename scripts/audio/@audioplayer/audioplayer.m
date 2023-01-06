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
## @deftypefn  {} {@var{player} =} audioplayer (@var{y}, @var{fs})
## @deftypefnx {} {@var{player} =} audioplayer (@var{y}, @var{fs}, @var{nbits})
## @deftypefnx {} {@var{player} =} audioplayer (@var{y}, @var{fs}, @var{nbits}, @var{id})
## @deftypefnx {} {@var{player} =} audioplayer (@var{recorder})
## @deftypefnx {} {@var{player} =} audioplayer (@var{recorder}, @var{id})
## Create an audioplayer object that will play back data @var{y} at sample
## rate @var{fs}.
##
## The signal @var{y} can be a vector (mono audio) or a two-dimensional array
## (multi-channel audio).
##
## The optional arguments @var{nbits} and @var{id} specify the number of bits
## per sample and player device ID, respectively.  Device IDs may be found
## using the @code{audiodevinfo} function.
##
## Given an audiorecorder object @var{recorder}, use the data from the object
## to initialize the player.
##
## The list of actions for an audioplayer object are shown below.  All
## methods require an audioplayer object as the first argument.
##
## @multitable @columnfractions 0.2 0.75
## @headitem Method @tab Description
## @item get @tab Read audioplayer property values
## @item isplaying @tab Return true if audioplayer is playing
## @item pause @tab Pause audioplayer playback
## @item play @tab Play audio stored in audioplayer object w/o blocking
## @item playblocking @tab Play audio stored in audioplayer object
## @item resume @tab Resume playback after pause
## @item set @tab Write audioplayer property values
## @item stop @tab Stop playback
## @end multitable
##
## Example
##
## Create an audioplayer object that will play back one second of white noise
## at 44100 sample rate using 8 bits per sample.
##
## @example
## @group
## y = 0.25 * randn (2, 44100);
## player = audioplayer (y, 44100, 8);
## play (player);
## @end group
## @end example
## @seealso{@audioplayer/get, @audioplayer/isplaying, @audioplayer/pause,
## @audioplayer/play, @audioplayer/playblocking, @audioplayer/resume,
## @audioplayer/set, @audioplayer/stop, audiodevinfo,
## @audiorecorder/audiorecorder, sound, soundsc}
## @end deftypefn

################################################################################
## FIXME: callbacks don't work properly, apparently because portaudio
## will execute the callbacks in a separate thread, and calling Octave
## functions in a separate thread which is likely to cause trouble with
## all of Octave's global data...
##
## @deftypefnx {} {@var{player} =} audioplayer (@var{function}, @dots{})
##
## Given a function handle, use that function to process the audio.
#
## The following example will create and register a callback that generates
## a sine wave on both channels.
##
## @example
## @group
## function [ sound, status ] = callback_sine (frames)
##   global lphase = 0.0;
##   global rphase = 0.0;
##   incl = 440.0 / 44100.0;
##   incr = 443.0 / 44100.0;
##   nl = incl * frames;
##   nr = incr * frames;
##   left = sin (2.0 * pi * [lphase:incl:lphase+nl]);
##   right = sin (2.0 * pi * [rphase:incr:rphase+nr]);
##   sound = [left', right'];
##   status = 0;
##   lphase = lphase + nl;
##   rphase = rphase + nr;
## endfunction
## player = audioplayer (@@callback_sine, 44100);
## play (player);
## # play for as long as you want
## stop (player);
## @end group
################################################################################

function player = audioplayer (varargin)

  if (nargin < 1 || nargin > 4
      || (nargin < 2 && ! (is_function_handle (varargin{1})
                           || ischar (varargin{1}))))
    print_usage ();
  endif

  if (isa (varargin{1}, "audiorecorder"))
    if (nargin == 1)
      player = getplayer (varargin{1});
    elseif (nargin == 2)
      recorder = varargin{1};
      data = getaudiodata (recorder);
      player = audioplayer (data,
                            get (recorder, "SampleRate"),
                            get (recorder, "BitsPerSample"),
                            varargin{2});
    else
      print_usage ();
    endif
  else
    ## FIXME: Prevent use of callbacks until situation is fixed.
    if (is_function_handle (varargin{1}) || ischar (varargin{1}))
      error ("audioplayer: first argument cannot be a callback function");
    endif
    ## FIXME: Uncomment when callback functions are supported.
    ## if (ischar (varargin{1}))
    ##   varargin{1} = str2func (varargin{1});
    ## endif
    player.player = __player_audioplayer__ (varargin{:});
    player = class (player, "audioplayer");
  endif

endfunction


%!demo
%! ## Generate 2 seconds of white noise and play it back with a pause
%! fs = 44100;
%! audio = 0.25 * randn (2, 2*fs);
%! player = audioplayer (audio, fs);
%! play (player);
%! pause (1);
%! pause (player);
%! pause (1);
%! resume (player);
%! pause (1);
%! stop (player);

## Tests of audioplayer must not actually play anything.

%!testif HAVE_PORTAUDIO; audiodevinfo (0) > 0
%! mono = 0.25 * randn (1, 44100);
%! stereo = 0.25 * randn (2, 44100);
%! fs = 44100;
%! player1 = audioplayer (mono, fs);
%! player2 = audioplayer (stereo, fs);
%! assert (player1.NumberOfChannels, 1);
%! assert (player2.NumberOfChannels, 2);
%! assert (player1.SampleRate, 44100);
%! assert (player2.SampleRate, 44100);
%! assert (player1.TotalSamples, 44100);
%! assert (player2.TotalSamples, 44100);

%!testif HAVE_PORTAUDIO; audiodevinfo (0) > 0
%! audio = randn (8000, 1);
%! fs = 44100;
%! player = audioplayer (audio, fs, 16);
%! assert (player.NumberOfChannels, 1);
%! assert (player.SampleRate, 44100);
%! assert (player.BitsPerSample, 16);

## FIXME: Callbacks do not work currently (5/31/2020) so BIST tests commented.
%!#function [sound, status] = callback (samples)
%!#  sound = rand (samples, 2) - 0.5;
%!#  status = 0;
%!#endfunction

%!#testif HAVE_PORTAUDIO
%!# player = audioplayer (@callback, 44100);
%!# play (player);
%!# pause (2);
%!# stop (player);
%!# assert (1);

%!#testif HAVE_PORTAUDIO
%!# player = audioplayer ("callback", 44100, 16);
%!# play (player);
%!# pause (2);
%!# stop (player);
%!# assert (1);

## Verify input validation
%!testif HAVE_PORTAUDIO; audiodevinfo (0) > 0
%! ## Verify nbits option only accepts 8, 16, 24
%! fail ("audioplayer (1, 8e3, 9)", "NBITS must be 8, 16, or 24");
%! fail ("audioplayer (1, 8e3, 32)", "NBITS must be 8, 16, or 24");
%! player = audioplayer (1, 8e3, 8);
%! player = audioplayer (1, 8e3, 16);
%! player = audioplayer (1, 8e3, 24);

%!error <first argument cannot be a callback> audioplayer (@ls, 8000)
%!error <first argument cannot be a callback> audioplayer ("ls", 8000)
