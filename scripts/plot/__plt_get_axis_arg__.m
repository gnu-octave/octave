## Copyright (C) 1996-2012 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {Function File} {[@var{h}, @var{varargin}, @var{narg}] =} __plt_get_axis_arg__ (@var{caller}, @var{varargin})
## Undocumented internal function.
## @end deftypefn

## Author: jwe

function [h, varargin, narg] = __plt_get_axis_arg__ (caller, varargin)

  if (islogical (caller))
    nogca = caller;
    caller = varargin{1};
    varargin(1) = [];
  else
    nogca = false;
  endif


  ## Search for parent property
  charargs = cellfun (@ischar,varargin);
  isparent = strcmp (lower (varargin(charargs)), "parent");
  
  ## Figure handles are integers, but object handles are non-integer,
  ## therefore ignore integer scalars.
  if (nargin > 1 && length (varargin) > 0 && isnumeric (varargin{1})
      && numel (varargin{1}) == 1 && ishandle (varargin{1}(1))
      && varargin{1}(1) != 0 && ! isfigure (varargin{1}(1)))
    tmp = varargin{1};
    obj = get (tmp);
    if ((strcmp (obj.type, "axes") && ! strcmp (obj.tag, "legend"))
        || strcmp (obj.type, "hggroup"))
      h = ancestor (tmp, "axes");
      varargin(1) = [];
      if (isempty (varargin))
        varargin = {};
      endif
    else
      error ("%s: expecting first argument to be axes handle", caller);
    endif
  elseif (numel (varargin) > 1 && any (isparent))
    tmp = find (charargs);
    idx = tmp(isparent)(1);
    if (idx < numel (varargin) && ishandle (varargin{idx+1}))
      tmp = varargin{idx+1};
      obj = get (tmp);
      if ((strcmp (obj.type, "axes") && ! strcmp (obj.tag, "legend"))
          || strcmp (obj.type, "hggroup"))
        h = ancestor (tmp, "axes");
        varargin(idx:idx+1) = [];
      else
        error ("%s: expecting parent value to be axes handle", caller);
      endif
    else
      error ("%s: expecting parent value to be axes handle", caller);
    endif
  else
    f = get (0, "currentfigure");
    if (isempty (f))
      h = [];
    else
      h = get (f, "currentaxes");
    endif
    if (isempty (h))
      if (nogca)
        h = NaN;
      else
        h = gca ();
      endif
    endif
    if (nargin < 2)
      varargin = {};
    endif
  endif

  if (ishandle (h) && strcmp (get (h, "nextplot"), "new"))
    h = axes ();
  endif

  narg = length (varargin);

endfunction


## No test needed for internal helper function.
%!assert (1)

