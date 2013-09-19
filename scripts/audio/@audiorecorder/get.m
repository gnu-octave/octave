## -*- texinfo -*-
## @deftypefn{Function File} @var{Value} = get (@var{recorderObj}, @var{Name})
## Returns the @var{Value} of the property identified by @var{Name}.
## @end deftypefn
## @deftypefn{Function File} @var{Values} = get (@var{recorderObj}, @{@var{Name1}, ... , @var{NameN}@})
## Returns the @var{Values} of the properties identified by @var{Name1} to @var{NameN}.
## @end deftypefn
## @deftypefn{Function File} @var{Values} = get (@var{recorderObj})
## Returns a scalar structure with values of all properties of @var{recorderObj}. 
## The field names correspond to property names.
## @end deftypefn

function result = get (varargin)
  recorder = varargin{1};
  properties = __get_properties__ (recorder);
  if nargin == 1
    result = properties;
  elseif nargin == 2
    if ischar (varargin{2})
      result = getfield (properties, varargin{2});
    else
      result = {};
      index = 1;
      for property = varargin{2}
        result{index} = getfield (properties, char(property));
        index = index + 1;
      endfor
    endif
  else
    error ('audiorecorder: wrong number of arguments to the get method');
  endif
endfunction