########################################################################
##
## Copyright (C) 2008-2025 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

## -*- texinfo -*-
## @deftypefn  {} {} refreshdata ()
## @deftypefnx {} {} refreshdata (@var{h})
## @deftypefnx {} {} refreshdata (@var{h}, @var{workspace})
## Evaluate any @samp{datasource} properties of the current figure and update
## the plot if the corresponding data has changed.
##
## If the first argument @var{h} is a list of graphics handles to figure, axes,
## or graphic objects with a DataSource property, then operate on these objects
## rather than the current figure returned by @code{gcf}.
##
## The optional second argument @var{workspace} can take the following values:
##
## @table @asis
## @item @qcode{"base"}
## Evaluate the datasource properties in the base workspace.  (default).
##
## @item @qcode{"caller"}
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
##   pause (0.1);
##   y = sin (x + 0.1*i);
##   refreshdata ();
## endfor
## @end group
## @end example
##
## Programming Note: For performance, specify @var{h} as the actual object(s)
## to be updated.  If no object is supplied then Octave must search through
## all graphic objects of the current figure and determine which ones have
## DataSource properties that are not empty.
## @end deftypefn

function refreshdata (h, workspace)

  if (nargin == 0)
    h = gcf ();
    workspace = "base";
  else
    ## Matlab compatibility requires accepting cell array of handles
    if (iscell (h))
      h = [h{:}];
    endif
    if (! all (ishghandle (h)))
      error ("refreshdata: H must be a list of graphic object handles");
    endif
    if (nargin == 1)
      workspace = "base";
    else
      if (! ischar (workspace)
          || ! any (strcmpi (workspace, {"base", "caller"})))
        error ('refreshdata: WORKSPACE must be "base" or "caller"');
      endif
      workspace = lower (workspace);
    endif
  endif

  h = findall (h);

  for hg = h(:).'
    obj = get (hg);
    flds = fieldnames (obj);
    ## regexp() is proper way to do searching, but is 3X slower.
    ## Pretty unlikely that people are going to be adding DataSource
    ## properties that are not, in fact, DataSources.
    ## m = regexp (flds, '^.+datasource$');
    idx = strfind (flds, 'datasource');
    dsources = flds(! cellfun ('isempty', idx));
    for i = 1 : numel (dsources)
      if (isempty (obj.(dsources{i})))
        continue;  # DataSource field doesn't point to anything
      endif
      expr = obj.(dsources{i});       # DataSource field
      val = evalin (workspace, expr);
      pdname = dsources{i}(1:end-6);  # property data name without "Source"
      set (hg, pdname, val);
    endfor
  endfor

endfunction


%!demo
%! clf;
%! x = 0:0.1:10;
%! y = sin (x);
%! h = plot (x, y, "ydatasource", "y");
%! title ("refreshdata() showing moving sine curve");
%! axis manual;
%! for i = 1 : 100
%!   pause (0.01);
%!   y = sin (x + 0.1 * i);
%!   refreshdata (h, "caller");
%! endfor
