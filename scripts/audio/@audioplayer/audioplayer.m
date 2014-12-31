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
## @deftypefn {Function File} {@var{player} =} audioplayer (@var{y}, @var{fs})
## @deftypefnx {Function File} {@var{player} =} audioplayer (@var{y}, @var{fs}, @var{nbits})
## @deftypefnx {Function File} {@var{player} =} audioplayer (@var{y}, @var{fs}, @var{nbits}, @var{id})
## @deftypefnx {Function File} {@var{player} =} audioplayer (@var{function}, @dots{})
## @deftypefnx {Function File} {@var{player} =} audioplayer (@var{recorder})
## @deftypefnx {Function File} {@var{player} =} audioplayer (@var{recorder}, @var{id})
## Create an audioplayer object that will play back data @var{y} at sample
## rate @var{fs}.  The optional arguments @var{nbits}, and @var{id}
## specify the bit depth and player device id, respectively.  Device IDs
## may be found using the audiodevinfo function.
## Given a function handle, use that function to process the audio.
## Given an audioplayer object, use the data from the object to
## initialize the player.
## @end deftypefn
##
## The signal @var{y} can be a vector or a two dimensional array.
##
## The following example will create an audioplayer object that will play
## back one second of white noise at 44100 sample rate using 8 bits per
## sample.
##
## @example
## @group
## y = randn (2, 44100) - 0.5;
## player = audioplayer (y, 44100, 8);
## play (player);
## @end group
## @end example
##
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
## @end example

function player = audioplayer (varargin)
  if (nargin < 1 || nargin > 4)
    print_usage ();
  endif
  if ((isa (varargin{1}, "function_handle")
       || ischar (varargin{1})) && nargin < 2)
    print_usage ();
  endif
  if isa (varargin{1}, "audiorecorder")
    if nargin == 1
      player = getplayer (varargin{1});
    elseif nargin == 2
      recorder = varargin{1};
      data = getaudiodata (recorder);
      player = audioplayer (data, get (recorder, "SampleRate"),
                            get (recorder, "BitsPerSample"), varargin{2});
    else
      print_usage ();
    endif
  else
    if (ischar (varargin{1}))
      varargin{1} = str2func (varargin{1});
    endif
    player.player = __player_audioplayer__ (varargin{:});
    player = class (player, "audioplayer");
  endif
endfunction

%!test
%!  mono = randn (1, 44100) - 0.5;
%!  stereo = randn (2, 44100) - 0.5;
%!  fs = 44100;
%!  player1 = audioplayer (mono, fs);
%!  player2 = audioplayer (stereo, fs);
%!  assert (player1.NumberOfChannels, 1);
%!  assert (player2.NumberOfChannels, 2);
%!  assert (player1.SampleRate, 44100);
%!  assert (player2.SampleRate, 44100);
%!  assert (player1.TotalSamples, 44100);
%!  assert (player2.TotalSamples, 44100);
%!  playblocking (player1);
%!  playblocking (player2);

%!test
%!  audio = randn (2, 88200) - 0.5;
%!  fs = 44100;
%!  player = audioplayer (audio, fs);
%!  assert (!isplaying (player));
%!  play (player);
%!  assert (isplaying (player));
%!  sleep (1);
%!  pause (player);
%!  assert (!isplaying (player));
%!  sleep (1);
%!  resume (player);
%!  assert (isplaying (player));
%!  sleep (1);

%!test
%!  audio = randn (2, 88200) - 0.5;
%!  fs = 44100;
%!  player = audioplayer (audio, fs);
%!  assert (!isplaying (player));
%!  play (player);
%!  assert (isplaying (player));
%!  sleep (1);
%!  stop (player);
%!  sleep (1);
%!  assert (!isplaying (player));
%!  assert (player.CurrentSample, 0);

%!test
%!  audio = randn (2, 44100) - 0.5;
%!  fs = 44100;
%!  player = audioplayer (audio, fs);
%!  set (player, {"SampleRate", "Tag", "UserData"}, {8000, "tag", [1, 2; 3, 4]});
%!  assert (player.SampleRate, 8000);
%!  assert (player.Tag, "tag");
%!  assert (player.UserData, [1, 2; 3, 4]);

%!test
%!  audio = randn (2, 44100) - 0.5;
%!  fs = 44100;
%!  player = audioplayer (audio, fs);
%!  settable = set (player);
%!  settable.SampleRate = 8000;
%!  settable.Tag = "tag";
%!  settable.UserData = [1, 2; 3, 4];
%!  set (player, settable);
%!  assert (player.SampleRate, 8000);
%!  assert (player.Tag, "tag");
%!  assert (player.UserData, [1, 2; 3, 4]);

%!test
%!  audio = randn (2, 44100) - 0.5;
%!  fs = 44100;
%!  player = audioplayer (audio, fs);
%!  player.SampleRate = 8000;
%!  player.Tag = "tag";
%!  player.UserData = [1, 2; 3, 4];
%!  properties = get (player, {"SampleRate", "Tag", "UserData"});
%!  assert (properties, {8000, "tag", [1, 2; 3, 4]});

%!function [ sound, status ] = callback (samples)
%!  sound = rand (samples, 2) - 0.5;
%!  status = 0;
%!endfunction

%!test
%!  player = audioplayer (@callback, 44100);
%!  play (player);
%!  sleep (2);
%!  stop (player);
%!  assert (1);

%!test
%!  player = audioplayer ("callback", 44100, 16);
%!  play (player);
%!  sleep (2);
%!  stop (player);
%!  assert (1);
