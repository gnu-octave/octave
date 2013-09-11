## -*- texinfo -*-
## @deftypefn{Function File} getplayer (@var{recorderObj})
## Returns an audioplayer object with data recorded by the recorder.
## @end deftypefn

function player = getplayer(varargin)
  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif 
  recorder = varargin{1};
  data = getaudiodata(recorder);
  player = audioplayer(data, get(recorder, 'SampleRate'), get(recorder, 'BitsPerSample'));
endfunction