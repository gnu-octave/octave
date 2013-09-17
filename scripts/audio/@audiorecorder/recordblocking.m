## -*- texinfo -*-
## @deftypefn{Function File} recordblocking (@var{recorderObj}, @var{length})
## Record audio with blocking (synchronous I/O). You must specify the number of seconds
## that the recording will continue for.
## @end deftypefn

function recordblocking(varargin)
  if (nargin != 2)
    print_usage ();
  endif 
  __recorder_recordblocking__(struct(varargin{1}).recorder, varargin{2});
endfunction