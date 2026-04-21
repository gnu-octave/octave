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
## @deftypefn  {} {@var{h} =} plot (@var{G})
## @deftypefnx {} {@var{h} =} plot (@var{G}, @var{name}, @var{value}, @dots{})
## Render a @code{graph} or @code{digraph} and return a
## @code{GraphPlot} handle.
##
## @code{plot (@var{G})} places the nodes of @var{G} using the default
## @qcode{"auto"} layout (subspace for graphs with fewer than 100 nodes,
## Fruchterman-Reingold force otherwise), draws the edges as line
## segments, draws the nodes as markers, and returns the resulting
## @code{GraphPlot} handle.  Trailing @var{name}/@var{value} pairs
## forward to the @code{GraphPlot} constructor; see @code{GraphPlot} for
## the full list of accepted options.
##
## When @var{G} is neither a @code{graph} nor a @code{digraph}, this
## file's free-function body is not used because Octave dispatches
## @code{plot} to its standard @file{scripts/plot/draw/plot} for
## numeric inputs.  The free-function body here exists primarily so
## @code{help plot} discovers the graph-overload docstring and so that
## explicit dispatch via @code{plot (G)} against a graph/digraph first
## argument resolves through this class file.
##
## @example
## @group
## G = digraph ([1 2 3], [2 3 1]);
## h = plot (G);
## @end group
## @end example
##
## @seealso{GraphPlot, graph, digraph}
## @end deftypefn

function h = plot (G, varargin)

  if (nargin < 1)
    print_usage ();
  endif

  if (! (isa (G, "graph") || isa (G, "digraph")))
    error ("Octave:invalid-input-arg", ...
           "plot: G must be a graph or digraph object");
  endif

  ## Defensive delegation: classdef method dispatch usually intercepts
  ## @code{plot (G, ...)} when G is a graph/digraph, but we keep the
  ## fallback explicit so the help text is self-contained.
  h = G.plot (varargin{:});

endfunction


## ---------------- BIST ----------------

## Basic call on a small digraph returns a GraphPlot handle.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = plot (G);
%!   assert (isa (h, "GraphPlot"));
%!   assert (h.NumNodes, 3);
%!   assert (h.NumEdges, 3);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Basic call on an undirected graph returns a GraphPlot handle.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = plot (G);
%!   assert (isa (h, "GraphPlot"));
%!   assert (h.NumNodes, 3);
%!   assert (h.NumEdges, 3);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Empty graph / digraph produces a GraphPlot with zero counts.
%!test
%! G = digraph ();
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = plot (G);
%!   assert (isa (h, "GraphPlot"));
%!   assert (h.NumNodes, 0);
%!   assert (h.NumEdges, 0);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Single-node digraph plots without error.
%!test
%! G = digraph (1);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = plot (G);
%!   assert (h.NumNodes, 1);
%!   assert (h.NumEdges, 0);
%!   assert (numel (h.XData), 1);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## 'auto' layout dispatches by size: small graphs get deterministic
## coordinates (stored in XData/YData as column vectors).
%!test
%! G = digraph ([1 2 3 4 5], [2 3 4 5 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = plot (G, "Layout", "auto");
%!   assert (iscolumn (h.XData));
%!   assert (iscolumn (h.YData));
%!   assert (numel (h.XData), 5);
%!   assert (numel (h.YData), 5);
%!   assert (all (isfinite (h.XData)));
%!   assert (all (isfinite (h.YData)));
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Plot works under the gnuplot headless toolkit as well.
%!test
%! gtk = graphics_toolkit ();
%! warn_state = warning ("off", "Octave:gnuplot-graphics");
%! hf = [];
%! unwind_protect
%!   graphics_toolkit ("gnuplot");
%!   hf = figure ("visible", "off");
%!   G = digraph ([1 2 3], [2 3 1]);
%!   h = plot (G);
%!   assert (isa (h, "GraphPlot"));
%!   assert (h.NumNodes, 3);
%! unwind_protect_cleanup
%!   if (! isempty (hf))
%!     close (hf);
%!   endif
%!   warning (warn_state);
%!   graphics_toolkit (gtk);
%! end_unwind_protect

## 'auto' layout with a 150-node digraph uses the large-graph branch
## and still produces finite coordinates.
%!test
%! N = 150;
%! s = 1:(N-1);
%! t = 2:N;
%! G = digraph (s, t);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = plot (G, "Layout", "auto");
%!   assert (numel (h.XData), N);
%!   assert (numel (h.YData), N);
%!   assert (all (isfinite (h.XData)));
%!   assert (all (isfinite (h.YData)));
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Custom XData / YData pass through to the GraphPlot.
%!test
%! G = digraph ([1 2], [2 3]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = plot (G, "XData", [0 1 2], "YData", [0 1 0]);
%!   assert (h.XData, [0; 1; 2]);
%!   assert (h.YData, [0; 1; 0]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## %!demo — small directed cycle under gnuplot (safe headless demo).
%!demo
%! G = digraph ([1 2 3 4], [2 3 4 1]);
%! h = plot (G);
%! title ("digraph 4-cycle");
