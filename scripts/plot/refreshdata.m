## Copyright (C) 2008, 2009 David Bateman
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
## @deftypefn  {Function File} {} refreshdata ()
## @deftypefnx {Function File} {} refreshdata (@var{h})
## @deftypefnx {Function File} {} refreshdata (@var{h}, @var{workspace})
## Evaluate any @samp{datasource} properties of the current figure and update
## the plot if the corresponding data has changed.  If called with one or more
## arguments @var{h} is a scalar or array of figure handles to refresh.  The
## optional second argument @var{workspace} can take the following values.
##
## @table @code
## @item "base"
## Evaluate the datasource properties in the base workspace.  (default).
## @item "caller"
## Evaluate the datasource properties in the workspace of the function
## that called @code{refreshdata}.
## @end table
##
## An example of the use of @code{refreshdata} is:
##
## @example
## @group
## x = 0:0.1:10;
## y = sin (x);
## plot (x, y, "ydatasource", "y");
## for i = 1 : 100
##   pause(0.1)
##   y = sin (x + 0.1 * i);
##   refreshdata();
## endfor
## @end group
## @end example
## @end deftypefn

function refreshdata (h, ws)

  if (nargin == 0)
    h = gcf ();
    ws = "base";
  else
    if (iscell (h))
      h = [h{:}];
    endif
    if (!all (ishandle (h)) || !all (strcmp (get (h, "type"), "figure")))
      error ("refreshdata: expecting a list of figure handles");
    endif
    if (nargin < 2)
      ws = "base";
    else
      if (!ischar (ws) || !(strcmpi (ws, "base") || strcmpi (ws, "caller")))
	error ("refreshdata: expecting workspace to be \"base\" or ""caller\"");
      else
	ws = tolower (ws);
      endif
    endif
  endif

  h = findall (h);
  objs = [];
  props = {};

  for i = 1 : numel (h)
    obj = get (h (i));
    fldnames = fieldnames (obj);
    m = regexpi (fieldnames(obj), "^.+datasource$", "match");
    idx = cellfun (@(x) !isempty(x), m);
    if (any (idx))
      tmp = m(idx);
      props = [props; {vertcat(tmp{:})}];
      objs  = [objs ; h(i)];
    endif
  endfor

  for i = 1 : length (objs)
    for j = 1 : length (props {i})
      expr = get (objs(i), props{i}{j});
      if (!isempty (expr))
	val = evalin (ws, expr);
	prop =  props{i}{j}(1:end-6);
        if (! isequal (get (objs(i), prop), val))
	  set (objs(i), props{i}{j}(1:end-6), val);
        endif
      endif
    endfor
  endfor
endfunction

%!demo
%! x = 0:0.1:10;
%! y = sin (x);
%! plot (x, y, "ydatasource", "y");
%! for i = 1 : 100
%!   pause(0.1)
%!   y = sin (x + 0.1 * i);
%!   refreshdata(gcf(), "caller");
%! endfor
