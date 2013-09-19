## -*- texinfo -*-
## @deftypefn{Function File} playblocking (@var{playerObj})
## Play back audio stored in the audioplayer object with blocking.
## @end deftypefn
## @deftypefn{Function File} playblocking (@var{playerObj}, @var{start})
## Play back audio stored in the audioplayer object starting at the time in seconds specified by @var{start}.
## @end deftypefn
## @deftypefn{Function File} playblocking (@var{playerObj}, [@var{start}, @var{end}])
## Play back audio stored in the audioplayer object starting at the time in seconds specified by @var{start} and ending at the time specified by @var{end}.
## @end deftypefn

function playblocking(varargin)
  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif 
  __player_playblocking__(struct(varargin{1}).player, varargin{2:end});
endfunction