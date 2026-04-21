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
## @deftypefn  {} {} highlight (@var{h}, @var{nodes})
## @deftypefnx {} {} highlight (@var{h}, @var{nodes}, @var{name}, @var{value}, @dots{})
## @deftypefnx {} {} highlight (@var{h}, @var{s}, @var{t})
## @deftypefnx {} {} highlight (@var{h}, @var{s}, @var{t}, @var{name}, @var{value}, @dots{})
## Highlight the specified nodes or edges of a @code{GraphPlot}.
##
## @var{h} is a @code{GraphPlot} handle, typically the return value of
## @code{plot (@var{G})} for a @code{graph} or @code{digraph} @var{G}.
##
## In the node form, @var{nodes} is a numeric vector of node indices, a
## single node name (character row vector), a cell array of node names,
## or an empty array (silent no-op).  By default the selected nodes'
## color is set to red (@code{[1 0 0]}).
##
## In the edge form, @var{s} and @var{t} are equal-length vectors of
## node indices, cell arrays of node names, or single character-row
## vectors.  The edge matching each @code{(@var{s}(i),@var{t}(i))} pair
## is highlighted.  For undirected graphs, @code{(@var{s}, @var{t})} and
## @code{(@var{t}, @var{s})} refer to the same edge.  By default the
## selected edges' color is set to red.
##
## Trailing @var{name}/@var{value} pairs override the default.
## Recognised options (case-insensitive):
##
## @table @code
## @item NodeColor
## RGB triplet in @code{[0, 1]} or a MATLAB color name.  Node form.
## @item Marker
## Marker character such as @qcode{"o"}, @qcode{"s"}, @qcode{"d"},
## @qcode{"^"}, etc.  Node form.
## @item MarkerSize
## Positive real scalar.  Node form.
## @item EdgeColor
## RGB triplet in @code{[0, 1]} or a MATLAB color name.  Edge form.
## @item LineWidth
## Positive real scalar.  Edge form.
## @item LineStyle
## One of @qcode{"-"}, @qcode{"--"}, @qcode{":"}, @qcode{"-."},
## @qcode{"none"}.  Edge form.
## @end table
##
## @code{highlight} expands the corresponding scalar properties to
## per-node or per-edge form as needed (@code{NodeColor} becomes
## @code{Nx3}, @code{EdgeColor} becomes @code{Mx3}, @code{Marker} or
## @code{LineStyle} becomes a cellstr, etc.).  Nodes or edges not in
## the selection retain their current cosmetic values.
##
## @example
## @group
## G = digraph ([1 2 3], [2 3 1]);
## h = plot (G);
## highlight (h, [1 3], "NodeColor", "g", "MarkerSize", 10);
## highlight (h, [1 2], [2 3], "EdgeColor", "r", "LineWidth", 2);
## @end group
## @end example
##
## @seealso{GraphPlot, plot, graph, digraph}
## @end deftypefn

function highlight (h, varargin)

  if (nargin < 2)
    print_usage ();
  endif

  if (! isa (h, "GraphPlot"))
    error ("Octave:invalid-input-arg", ...
           "highlight: first argument must be a GraphPlot handle");
  endif

  ## Defensive delegation: classdef method dispatch usually intercepts
  ## @code{highlight (h, ...)} when h is a GraphPlot, but we keep the
  ## fallback explicit so the help text is self-contained and the
  ## free-function entry point works from any load path.
  h.highlight (varargin{:});

endfunction


## ---------------- BIST ----------------

## Basic smoke test: free-function entry point works and turns node red.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = plot (G);
%!   highlight (h, 1);
%!   assert (h.NodeColor(1, :), [1 0 0]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Name-value override through the free function.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = plot (G);
%!   highlight (h, 2, "NodeColor", "g", "MarkerSize", 9);
%!   assert (h.NodeColor(2, :), [0 1 0]);
%!   assert (h.MarkerSize(2), 9);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Non-GraphPlot first arg is rejected by the free-function.
%!error <GraphPlot> highlight (1, 1)
%!error <GraphPlot> highlight ("bogus", 1)

## Missing arguments: print_usage via the free-function entry.
%!error highlight ()
%!error highlight (GraphPlot ())
