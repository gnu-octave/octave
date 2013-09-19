## -*- texinfo -*-
## @deftypefn{Function File} player = audioplayer (@var{Y}, @var{Fs})
## Create an audioplayer object that will play back data @var{Y} at sample rate @var{Fs}.
## @end deftypefn
## @deftypefn{Function File} player = audioplayer (@var{Y}, @var{Fs}, @var{nBytes})
## Create an audioplayer object that will play back data @var{Y} at sample rate @var{Fs} and bit depth @var{nBytes}.
## @end deftypefn
## @deftypefn{Function File} player = audioplayer (@var{Y}, @var{Fs}, @var{nBytes}, @var{ID})
## Create an audioplayer object that will play back data @var{Y} at sample rate @var{Fs}, bit depth @var{nBytes} and using a device with @var{ID} that you can get using the audiodevinfo function.
## @end deftypefn
## @deftypefn{Function File} player = audioplayer (@var{function}, @var{Fs})
## Argument @var{function} is a function handle, inline function or a string value of a function name that will get called to process audio. Audio will be played at @var{Fs} sampling rate.
## @end deftypefn
## @deftypefn{Function File} player = audioplayer (@var{function}, @var{Fs}, @var{nBytes})
## Same as above but also allows you to specify the number of bytes per sample.
## @end deftypefn
## @deftypefn{Function File} player = audioplayer (@var{function}, @var{Fs}, @var{nBytes}, @var{ID})
## Same as above but also allows you to specify device ID that will be used.
## @end deftypefn
## @deftypefn{Function File} player = audioplayer (@var{recorder})
## Create an audioplayer object that will use data and other information such as sample rate from an audiorecorder object.
## @end deftypefn
## @deftypefn{Function File} player = audioplayer (@var{recorder}, @var{ID})
## Create an audioplayer object that will use data and other information from an audiorecorder object and that will use a device with the given @var{ID}.
## @end deftypefn
##
## The signal @var{Y} can be a vector or a two dimensional array.
##
## The following example will create an audioplayer object that will play back one second of white noise at 44100 sample rate using 8 bits per sample.
##
## @example
## @group
## @code{y = randn (2, 44100) - 0.5;}
## @code{player = audioplayer (y, 44100, 8);}
## @code{play (player);}
## @end group
## @end example
##
## The following example will create and register a callback that generates a sine wave on both channels.
##
## @example
## @group
## @code{function [ sound, status ] = callback_sine (frames)}
## @code{  global lphase = 0.0;}
## @code{  global rphase = 0.0;}
## @code{  incl = 440.0 / 44100.0;}
## @code{  incr = 443.0 / 44100.0;}
## @code{  nl = incl * frames;}
## @code{  nr = incr * frames;}
## @code{  left = sin (2.0 * pi * [lphase:incl:lphase+nl]);}
## @code{  right = sin (2.0 * pi * [rphase:incr:rphase+nr]);}
## @code{  sound = [left', right'];}
## @code{  status = 0;}
## @code{  lphase = lphase + nl;}
## @code{  rphase = rphase + nr;}
## @code{endfunction}
## @code{player = audioplayer(@@callback_sine, 44100);}
## @code{play (player);}
## @code{# play for as long as you want}
## @code{stop (player);}
## @end group
## @end example

function player = audioplayer (varargin)
  if (nargin < 1 || nargin > 4)
    print_usage ();
  endif
  if ((isa(varargin{1}, 'function_handle') || ischar(varargin{1})) && nargin < 2)
    print_usage ();
  endif
  if isa (varargin{1}, 'audiorecorder')
    if nargin == 1
      player = getplayer(varargin{1});
    elseif nargin == 2
      recorder = varargin{1};
      data = getaudiodata (recorder);
      player = audioplayer (data, get (recorder, 'SampleRate'), get (recorder, 'BitsPerSample'), varargin{2});    
    else
      print_usage ();
    endif
  else
    if ischar(varargin{1})
      varargin{1} = str2func(varargin{1});
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
%!  set (player, {'SampleRate', 'Tag', 'UserData'}, {8000, 'tag', [1, 2; 3, 4]});
%!  assert (player.SampleRate, 8000);
%!  assert (player.Tag, 'tag');
%!  assert (player.UserData, [1, 2; 3, 4]);

%!test
%!  audio = randn (2, 44100) - 0.5;
%!  fs = 44100;
%!  player = audioplayer (audio, fs);
%!  settable = set (player);
%!  settable.SampleRate = 8000;
%!  settable.Tag = 'tag';
%!  settable.UserData = [1, 2; 3, 4];
%!  set (player, settable);
%!  assert (player.SampleRate, 8000);
%!  assert (player.Tag, 'tag');
%!  assert (player.UserData, [1, 2; 3, 4]);

%!test
%!  audio = randn (2, 44100) - 0.5;
%!  fs = 44100;
%!  player = audioplayer (audio, fs);
%!  player.SampleRate = 8000;
%!  player.Tag = 'tag';
%!  player.UserData = [1, 2; 3, 4];
%!  properties = get (player, {'SampleRate', 'Tag', 'UserData'});
%!  assert (properties, {8000, 'tag', [1, 2; 3, 4]});

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
%!  stop(player);
%!  assert (1);
