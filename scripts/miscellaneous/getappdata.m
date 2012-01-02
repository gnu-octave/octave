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
## @deftypefn {Function File} {@var{value} =} getappdata (@var{h}, @var{name})
## Return the @var{value} for named application data for the object(s) with
## handle(s) @var{h}.
## @deftypefnx {Function File} {@var{appdata} =} getappdata (@var{h})
## Return a structure, @var{appdata}, whose fields correspond to the appdata
## properties.
## @end deftypefn

## Author: Ben Abbott <bpabbott@mac.com>
## Created: 2010-07-15

function val = getappdata (h, name)

  if (all (ishandle (h)) && nargin == 2 && ischar (name))
    ## FIXME - Is there a better way to handle non-existent appdata
    ## and missing fields?
    val = cell (numel (h), 1);
    appdata = struct();
    for nh = 1:numel(h)
      try
        appdata = get (h(nh), "__appdata__");
      end_try_catch
      if (! isfield (appdata, name))
        appdata.(name) = [];
      endif
      val(nh) = {appdata.(name)};
    endfor
    if (nh == 1)
      val = val{1};
    endif
  elseif (ishandle (h) && numel (h) == 1 && nargin == 1)
    try
      val = get (h, "__appdata__");
    catch
      val = struct ();
    end_try_catch
  else
    error ("getappdata: invalid input");
  endif

endfunction

