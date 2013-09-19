## -*- texinfo -*-
## @deftypefn{Function File} play (@var{playerObj})
## Play back audio stored in an audioplayer object without blocking.
## @end deftypefn
## @deftypefn{Function File} play  (@var{playerObj}, @var{start})
## Play back audio stored in an audioplayer object starting at the time in seconds specified by @var{start}.
## @end deftypefn
## @deftypefn{Function File} play (@var{playerObj}, [@var{start}, @var{end}])
## Play back audio stored in an audioplayer object starting at the time in seconds specified by @var{start} and ending at the time specified by @var{end}.
## @end deftypefn

function play(varargin)
  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif 
  __player_play__(struct(varargin{1}).player, varargin{2:end});
endfunction