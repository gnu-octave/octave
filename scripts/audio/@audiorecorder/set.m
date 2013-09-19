## -*- texinfo -*-
## @deftypefn{Function File} set (@var{recorderObj}, @var{Name}, @var{Value})
## Set the value of property specified by @var{Name} to a given @var{Value}.
## @end deftypefn
## @deftypefn{Function File} set (@var{recorderObj}, @var{CellOfNames}, @var{CellOfValues})
## Given a cell array of property names and a cell array of values, set each property to a corresponding value.
## @end deftypefn
## @deftypefn{Function File} set (@var{recorderObj}, @var{StructOfProperties})
## Given a structure where fields are property names, set the value of those properties for an audiorecorder object to corresponding values.
## @end deftypefn
## @deftypefn{Function File} @var{settableProperties} = set (@var{recorderObj})
## Returns a structure of settable properties.
## @end deftypefn

function settable = set (varargin)
  if nargin < 1 || nargin > 3
    print_usage();
  endif
  recorder = struct (varargin{1}).recorder;
  if nargin == 1
    settable.SampleRate = {};
    settable.Tag = {};
    settable.UserData = {};
  elseif nargin == 2
    for [value, property] = varargin{2}
      setproperty (recorder, property, value);
    endfor
  elseif nargin == 3
    if iscell (varargin{2})
      index = 1;
      for property = varargin{2}
        setproperty (recorder, char(property), varargin{3}{index});
        index = index + 1;
      endfor
    else
      setproperty (recorder, varargin{2}, varargin{3});
    endif
  else
    error ('audiorecorder: wrong number of arguments to the set method');
  endif
endfunction

function setproperty (recorder, property, value)
  switch property
    case 'SampleRate'
      __recorder_set_fs__ (recorder, value);
    case 'Tag'
      __recorder_set_tag__ (recorder, value);
    case 'UserData'
      __recorder_set_userdata__ (recorder, value);
    otherwise
      error ('audiorecorder: no such property or the property specified is read-only');
  endswitch
endfunction