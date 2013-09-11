## -*- texinfo -*-
## @deftypefn{Function File} record (@var{recorderObj})
## Record audio without blocking. The recording will continue until you use the stop method on @var{recorderObj}.
## @deftypefnx{Function File} record (@var{playerObj}, @var{length})
## Record audio without blocking. The recording will continue for @var{length} seconds.
## @end deftypefn

function record(varargin)
  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif 
  __recorder_record__(struct(varargin{1}).recorder, varargin{2:end});
endfunction