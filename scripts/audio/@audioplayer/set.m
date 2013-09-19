## -*- texinfo -*-
## @deftypefn{Function File} set (@var{playerObj}, @var{Name}, @var{Value})
## Set the value of property specified by @var{Name} to a given @var{Value}.
## @end deftypefn
## @deftypefn{Function File} set (@var{playerObj}, @var{CellOfNames}, @var{CellOfValues})
## Given a cell array of property names and a cell array of values, set each property to a corresponding value.
## @end deftypefn
## @deftypefn{Function File} set (@var{playerObj}, @var{StructOfProperties})
## Given a structure where fields are property names, set the value of those properties for an audioplayer object to corresponding values.
## @end deftypefn
## @deftypefn{Function File} @var{settableProperties} = set (@var{playerObj})
## Returns a structure of settable properties.
## @end deftypefn

function settable = set (varargin)
  if nargin < 1 || nargin > 3
    print_usage();
  endif
  player = struct (varargin{1}).player;
  if nargin == 1
    settable.SampleRate = {};
    settable.Tag = {};
    settable.UserData = {};
  elseif nargin == 2
    for [value, property] = varargin{2}
      setproperty (player, property, value);
    endfor
  elseif nargin == 3
    if iscell (varargin{2})
      index = 1;
      for property = varargin{2}
        setproperty (player, char(property), varargin{3}{index});
        index = index + 1;
      endfor
    else
      setproperty (player, varargin{2}, varargin{3});
    endif
  else
    error ('audioplayer: wrong number of arguments to the set method');
  endif
endfunction

function setproperty (player, property, value)
  switch property
    case 'SampleRate'
      __player_set_fs__ (player, value);
    case 'Tag'
      __player_set_tag__ (player, value);
    case 'UserData'
      __player_set_userdata__ (player, value);
    otherwise
      error ('audioplayer: no such property or the property specified is read-only');
  endswitch
endfunction