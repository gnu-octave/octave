########################################################################
##
## Copyright (C) 2015-2023 The Octave Project Developers
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
## @deftypefn  {} {} profexport (@var{dir})
## @deftypefnx {} {} profexport (@var{dir}, @var{data})
## @deftypefnx {} {} profexport (@var{dir}, @var{name})
## @deftypefnx {} {} profexport (@var{dir}, @var{name}, @var{data})
##
## Export profiler data as HTML.
##
## Export the profiling data in @var{data} into a series of HTML files in
## the folder @var{dir}.  The initial file will be
## @file{@var{data}/index.html}.
##
## If @var{name} is specified, it must be a string that contains a ``name''
## for the profile being exported.  This name is included in the HTML.
##
## The input @var{data} is the structure returned by @code{profile ("info")}.
## If unspecified, @code{profexport} will use the current profile dataset.
##
## @seealso{profshow, profexplore, profile}
## @end deftypefn

function profexport (dir, name = "", data)

  if (nargin < 1)
    print_usage ();
  endif

  if (! ischar (dir))
    error ("profexport: DIR must be a string");
  endif

  if (nargin == 1)
    data = profile ("info");
  elseif (nargin == 2)
    if (isstruct (name))
      data = name;
      name = "";
    else
      if (! ischar (name))
        error ("profexport: NAME must be a string");
      endif
      data = profile ("info");
    endif
  endif

  if (! isfolder (dir))
    ok = mkdir (dir);
    if (! ok)
      error ("profexport: failed to create output directory '%s'", dir);
    endif
  endif

  if (! copyfile (__dataFilename ("style.css"), dir))
    error ("profexport: failed to copy data file to directory '%s'", dir);
  endif

  if (isempty (name))
    name = "Profile";
  else
    name = ["Profile - " __escapeHtml(name)];
  endif

  __writeFlat (fullfile (dir, "index.html"), name, data.FunctionTable);
  for i = 1 : length (data.FunctionTable)
    __writeFunc (fullfile (dir, sprintf ("function-%d.html", i)), name, ...
                 data.FunctionTable, i);
  endfor

  top = struct ("name", "Top");
  __writeHierarchical (dir, name, data.FunctionTable, ...
                       {top}, data.Hierarchical, 1);

endfunction

################################################################################
## Write flat profile.

function __writeFlat (file, name, table)

  template = __readTemplate ("flat.html");
  entryTemplate = __readTemplate ("flat_entry.html");

  ## Construct the entries string.
  ## This follows the same logic that is used in profshow.
  times = [ table.TotalTime ];
  totalTime = sum (times);
  [~, p] = sort (times, "descend");
  entries = "";
  for i = 1 : length (table)
    row = table(p(i));

    cur = entryTemplate;
    cur = strrep (cur, "%num", sprintf ("%d", p(i)));
    cur = strrep (cur, "%name", __escapeHtml (row.FunctionName));
    cur = strrep (cur, "%timeabs", sprintf ("%.3f", row.TotalTime));
    cur = strrep (cur, "%timerel", ...
                  sprintf ("%.2f", 100 * row.TotalTime / totalTime));
    cur = strrep (cur,  "%calls", sprintf ("%d", row.NumCalls));

    entries = [entries, cur];
  endfor

  ## Build full page content.
  res = template;
  res = strrep (res, "%title", name);
  res = strrep (res, "%entries", entries);

  ## Write out the file.
  __writeToFile (file, res);

endfunction

################################################################################
## Write "function profile" pages.

function __writeFunc (file, name, table, ind)

  template = __readTemplate ("function.html");
  row = table(ind);

  ## Fill in basic data.
  res = template;
  res = strrep (res, "%title", name);
  res = strrep (res, "%name", __escapeHtml (row.FunctionName));
  res = strrep (res, "%timeabs", sprintf ("%.3f", row.TotalTime));
  res = strrep (res, "%calls", sprintf ("%d", row.NumCalls));

  ## Build up attribute list.
  attr = "";
  if (row.IsRecursive)
    attr = "recursive";
  endif
  res = strrep (res, "%attr", attr);

  ## Add parent and child list.
  parents = __buildParentOrChildList (table, row.Parents);
  res = strrep (res, "%parents", parents);
  children = __buildParentOrChildList (table, row.Children);
  res = strrep (res, "%children", children);

  ## Write out the file.
  __writeToFile (file, res);

endfunction

function lst = __buildParentOrChildList (table, inds)

  if (length (inds) == 0)
    lst = "none";
    return;
  endif

  template = "<a href='function-%num.html'>%name</a>";

  lst = "";
  for i = 1 : length (inds)
    if (i > 1)
      lst = [lst, ", "];
    endif

    cur = template;
    cur = strrep (cur, "%num", sprintf ("%d", inds(i)));
    cur = strrep (cur, "%name", __escapeHtml (table(inds(i)).FunctionName));
    lst = [lst, cur];
  endfor

endfunction

################################################################################
## Write a hierarchical profile page.

## In order to generate unique filenames for the pages, we keep a running
## counter that is passed through and updated by the recursive calls.
## The function returns two counter values:  The one that is chosen
## for its own page (so that parent nodes can link down to them)
## and the next value to be passed to the next call.

function [mine, cnt] = __writeHierarchical (dir, name, funcs, ...
                                            parents, children, cnt)

  template = __readTemplate ("hierarchical.html");
  entryTemplate = __readTemplate ("hierarchical_entry.html");

  ## Fill in basic data and parent breadcrumbs.
  res = template;
  res = strrep (res, "%title", name);
  parentsStr = __hierarchicalParents (parents);
  res = strrep (res, "%parents", parentsStr);

  ## Set this page's counter and update parents struct with it.
  mine = cnt++;
  parents{end}.cnt = mine;
  file = sprintf ("%s/hierarchy-%d.html", dir, mine);

  ## Sort children by time.
  times = -[ children.TotalTime ];
  [~, p] = sort (times);
  children = children(p);

  ## Recurse on children and construct entry list.
  entries = "";
  for i = 1 : length (children)
    cur = children(i);
    curName = funcs(cur.Index).FunctionName;

    newParents = parents;
    newParents{end + 1} = struct ("name", curName);
    [childCnt, cnt] = __writeHierarchical (dir, name, funcs, ...
                                           newParents, cur.Children, cnt);

    str = entryTemplate;
    str = strrep (str, "%cnt", sprintf ("%d", childCnt));
    str = strrep (str, "%name", __escapeHtml (curName));
    str = strrep (str, "%total", sprintf ("%.3f", cur.TotalTime));
    str = strrep (str, "%self", sprintf ("%.3f", cur.SelfTime));
    str = strrep (str, "%calls", sprintf ("%d", cur.NumCalls));

    entries = [entries, str];
  endfor
  res = strrep (res, "%entries", entries);

  ## Write out the file.
  __writeToFile (file, res);

endfunction

function str = __hierarchicalParents (parents)

  ## We always have at least the "Top" entry!
  assert (length (parents) > 0);

  template = "<a href='hierarchy-%cnt.html'>%name</a>";
  lastTemplate = "<strong>%name</strong>";

  str = "";
  for i = 1 : length (parents) - 1
    cur = template;
    cur = strrep (cur, "%cnt", sprintf ("%d", parents{i}.cnt));
    cur = strrep (cur, "%name", __escapeHtml (parents{i}.name));
    str = [str, cur, " > "];
  endfor

  cur = lastTemplate;
  cur = strrep (cur, "%name", __escapeHtml (parents{end}.name));
  str = [str, cur];

endfunction

################################################################################
## General helper functions.

function __writeToFile (file, str)

  fid = fopen (file, "w");
  if (fid < 0)
    error ("profexport: failed to open '%s' for writing", file);
  endif
  fputs (fid, str);
  fclose (fid);

endfunction

function fn = __dataFilename (name)
  etcdir = __octave_config_info__ ("octetcdir");
  fn = fullfile (etcdir, "profiler", name);
endfunction

function str = __readTemplate (name)
  fn = __dataFilename (name);
  str = fileread (fn);
endfunction

function str = __escapeHtml (str)
  str = strrep (str, '&', "&amp;");
  str = strrep (str, '<', "&lt;");
  str = strrep (str, '>', "&gt;");
  str = strrep (str, '"', "&quot;");
endfunction

################################################################################
## Tests and demo.

%!demo
%! profile on;
%! A = rand (100);
%! B = expm (A);
%! profile off;
%! dir = tempname ();
%! profexport (dir, "Example Profile");
%! open (fullfile (dir, "index.html"));

## Test input validation
%!error <Invalid call> profexport ()
%!error profexport (1)
%!error profexport (1, 2, 3, 4)
%!error <DIR must be a string> profexport (5)
%!error <NAME must be a string> profexport ("dir", 5)
