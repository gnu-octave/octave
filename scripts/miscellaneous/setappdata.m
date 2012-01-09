## Copyright (C) 2010-2012 Ben Abbott
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {Function File} {} setappdata (@var{h}, @var{name}, @var{value})
## Set the named application data to @var{value} for the object(s) with
## handle(s) @var{h}.  If the application data with the specified name does
## not exist, it is created.
## @end deftypefn

## Author: Ben Abbott <bpabbott@mac.com>
## Created: 2010-07-15

function setappdata (h, varargin)

  if (! (all (ishandle (h)) && mod (numel (varargin), 2) == 0))
    error ("setappdata: invalid input");
  endif

  for nh = 1:numel(h)
    if (! isfield (get (h(nh)), "__appdata__"))
      addproperty ("__appdata__", h(nh), "any", struct ());
    endif
    appdata = get (h(nh), "__appdata__");
    for narg = 1:2:numel(varargin)
      if (iscellstr (varargin{narg}))
        ## Handle cell arrays like set() does.
        set (h(nh), "__appdata__", appdata);
        setappdata (h(nh), vertcat (varargin{narg}', varargin{narg+1}'){:});
        appdata = get (h(nh), "__appdata__");
      elseif (ischar (varargin{narg}))
        appdata.(varargin{narg}) = varargin{narg+1};
      else
        error ("setappdata: invalid input");
      endif
    endfor
    set (h(nh), "__appdata__", appdata);
  endfor

endfunction

%!test
%! setappdata (0, "hello", "world")
%! assert (isappdata (0, "hello"), true)
%!assert (getappdata (0, "hello"), "world")

