## -*- texinfo -*-
## @deftypefn{Function File} player = play (@var{recorderObj})
## Play the audio recorded in @var{recorderObj} and return a corresponding audioplayer object.
## @deftypefnx{Function File} player = play (@var{recorderObj}, start)
## Play the audio recorded in @var{recorderObj} starting from @var{start} seconds in to the recording. Returns a corresponding audioplayer object.
## @deftypefnx{Function File} player = play (@var{recorderObj}, [start, end])
## Play the audio recorded in @var{recorderObj} starting from @var{start} seconds and ending at @var{end} seconds in the recording. Returns a corresponding audioplayer object.
## @end deftypefn

function player = play(varargin)
  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif 
  recorder = varargin{1};
  data = getaudiodata(recorder);
  player = audioplayer(data, get(recorder, 'SampleRate'), get(recorder, 'BitsPerSample'));
  if (nargin == 1)
    play(player);
  else
    play(player, varargin{2});
  endif
endfunction