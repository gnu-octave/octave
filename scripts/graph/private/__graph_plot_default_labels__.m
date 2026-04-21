########################################################################
##
## Copyright (C) 2026 The Octave Project Developers
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
## @deftypefn {} {@var{labels} =} __graph_plot_default_labels__ (@var{G}, @var{N})
## Return the default @code{NodeLabel} for @code{GraphPlot} when
## @code{NodeLabelMode} is @qcode{"auto"}.
##
## When @var{G} has named nodes (@code{G.Nodes.Name} non-empty), the
## default labels are that column cellstr.  Otherwise the defaults are
## the string node indices @qcode{"1"}, @qcode{"2"}, @dots{},
## @qcode{"N"}.
##
## The return value is always a column cell array of strings.
##
## This is a private helper for @file{GraphPlot.m}.
## @end deftypefn

function labels = __graph_plot_default_labels__ (G, N)

  if (nargin < 2)
    error ("__graph_plot_default_labels__: G and N required");
  endif

  if (N <= 0)
    labels = cell (0, 1);
    return;
  endif

  names = {};
  if (! isempty (G))
    try
      nt = G.Nodes;
      if (isfield (nt, "Name") && ! isempty (nt.Name))
        names = nt.Name;
      endif
    catch
      names = {};
    end_try_catch
  endif

  if (numel (names) == N && iscellstr (names))
    labels = names(:);
    return;
  endif

  labels = cell (N, 1);
  for ii = 1:N
    labels{ii} = num2str (ii);
  endfor

endfunction


## ---------------- BIST ----------------

## Unnamed digraph yields '1','2',...
%!test
%! L = __graph_plot_default_labels__ (digraph ([1 2 3], [2 3 1]), 3);
%! assert (L, {"1"; "2"; "3"});

## Unnamed graph yields '1','2',...
%!test
%! L = __graph_plot_default_labels__ (graph (5), 5);
%! assert (L, {"1"; "2"; "3"; "4"; "5"});

## Named digraph yields Names.
%!test
%! G = digraph ([1 2 3], [2 3 1], [], {"alpha", "beta", "gamma"});
%! L = __graph_plot_default_labels__ (G, 3);
%! assert (L, {"alpha"; "beta"; "gamma"});

## N=0 yields empty column cell.
%!test
%! L = __graph_plot_default_labels__ (digraph (), 0);
%! assert (L, cell (0, 1));

## Output is always a column cell.
%!test
%! L = __graph_plot_default_labels__ (digraph (4), 4);
%! assert (iscolumn (L));
%! assert (iscell (L));
