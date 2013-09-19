## -*- texinfo -*-
## @deftypefn{Function File} recorder = audiorecorder
## Create an audiorecorder object recording 8 bit mono audio at 8000 Hz sample rate.
## @end deftypefn
## @deftypefn{Function File} recorder = audiorecorder (@var{Fs}, @var{nBytes}, @var{Channels})
## Create an audiorecorder object recording at specified sample rate @var{Fs}, specified bit depth @var{nBytes} and specified number of @var{Channels}.
## @end deftypefn
## @deftypefn{Function File} recorder = audiorecorder (@var{Fs}, @var{nBytes}, @var{Channels}, @var{ID})
## Create an audiorecorder object recording at specified sample rate @var{Fs}, specified bit depth @var{nBytes}, number of @var{Channels} and recording on the device specified by @var{ID}. You can get device IDs by using the audiodevinfo function.
## @end deftypefn
## @deftypefn{Function File} recorder = audiorecorder (@var{function}, @var{Fs})
## Argument @var{function} is a function handle, inline function or a string value of a function name that will get called to process audio. Audio will be recorded at @var{Fs} sampling rate.
## @end deftypefn
## @deftypefn{Function File} recorder = audiorecorder (@var{function}, @var{Fs}, @var{nBytes})
## Same as above but also allows you to specify the number of bytes per sample.
## @end deftypefn
## @deftypefn{Function File} recorder = audiorecorder (@var{function}, @var{Fs}, @var{nBytes}, @var{ID})
## Same as above but also allows you to specify device ID that will be used.
## @end deftypefn

function recorder = audiorecorder (varargin)
  if (nargin > 5)
    print_usage ();
  endif
  if nargin > 0 && ischar(varargin{1})
    varargin{1} = str2func(varargin{1});
  endif
  recorder.recorder = __recorder_audiorecorder__ (varargin{:});
  recorder = class (recorder, "audiorecorder");
endfunction

%!test
%!  recorder = audiorecorder (44100, 16, 2);
%!  recordblocking (recorder, 1);
%!  data = getaudiodata (recorder, 'int16');
%!  assert (strcmp (class (data), 'int16'));
%!  data = getaudiodata (recorder, 'int8');
%!  assert (strcmp (class (data), 'int8'));
%!  data = getaudiodata (recorder, 'uint8');
%!  assert (strcmp (class (data), 'uint8'));
%!  assert (size (data)(1), recorder.TotalSamples);
%!  assert (size (data)(2), 2);
%!  assert (size (data)(1) != 0);

%!test
%!  recorder = audiorecorder (44100, 16, 2);
%!  record (recorder, 1)
%!  sleep (2);
%!  record (recorder, 1);
%!  sleep (2);
%!  data = getaudiodata(recorder);
%!  assert (size (data)(1) < 44100 * 2);

%!test
%!  recorder = audiorecorder (44100, 16, 2);
%!  record (recorder, 1);
%!  sleep (2);
%!  player1 = audioplayer (recorder);
%!  player2 = getplayer (recorder);
%!  play (player1);
%!  sleep (2);
%!  play (player2);
%!  sleep (2);
%!  assert (player1.TotalSamples, recorder.TotalSamples);
%!  assert (player2.TotalSamples, recorder.TotalSamples);

%!test
%!  recorder = audiorecorder;
%!  set (recorder, {'SampleRate', 'Tag', 'UserData'}, {8000, 'tag', [1, 2; 3, 4]});
%!  assert (recorder.SampleRate, 8000);
%!  assert (recorder.Tag, 'tag');
%!  assert (recorder.UserData, [1, 2; 3, 4]);

%!test
%!  recorder = audiorecorder;
%!  settable = set (recorder);
%!  settable.SampleRate = 8000;
%!  settable.Tag = 'tag';
%!  settable.UserData = [1, 2; 3, 4];
%!  set (recorder, settable);
%!  assert (recorder.SampleRate, 8000);
%!  assert (recorder.Tag, 'tag');
%!  assert (recorder.UserData, [1, 2; 3, 4]);

%!test
%!  recorder = audiorecorder;
%!  recorder.SampleRate = 8000;
%!  recorder.Tag = 'tag';
%!  recorder.UserData = [1, 2; 3, 4];
%!  properties = get (recorder, {'SampleRate', 'Tag', 'UserData'});
%!  assert (properties, {8000, 'tag', [1, 2; 3, 4]});

#%!function status = callback_record (sound)
#%!  fid = fopen('record.txt', 'at');
#%!  for index = 1:rows(sound)
#%!    fprintf(fid, "%.4f, %.4f\n", sound(index, 1), sound(index, 2));
#%!  endfor
#%!  fclose(fid);
#%!  status = 0;
#%!endfunction

#%!test
#%!  recorder = audiorecorder (@callback_record, 44100);
#%!  unlink('record.txt')
#%!  record (recorder);
#%!  sleep (2);
#%!  stop (player);
#%!  s = stat('record.txt');
#%!  assert (s.size > 0);

#%!test
#%!  recorder = audiorecorder (@callback_record, 44100);
#%!  unlink('record.txt')
#%!  record (recorder);
#%!  sleep (2);
#%!  stop (recorder);
#%!  s = stat('record.txt');
#%!  assert (s.size > 0);