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
## @deftypefn  {} {} labeledge (@var{h}, @var{idx}, @var{labels})
## @deftypefnx {} {} labeledge (@var{h}, @var{s}, @var{t}, @var{labels})
## Set labels on the edges of a @code{GraphPlot} @var{h}.
##
## @var{h} is a @code{GraphPlot} handle, typically the return value of
## @code{plot (@var{G})} for a @code{graph} or @code{digraph} @var{G}.
##
## In the edge-index form, @var{idx} is a numeric vector of 1-based edge
## indices into @code{@var{h}.Edges} (the same row order as
## @code{G.Edges.EndNodes}).
##
## In the @code{(s, t)} form, @var{s} and @var{t} are equal-length
## vectors of node indices, cell arrays of node names, or single
## character-row vectors selecting the edges whose endpoints match each
## @code{(@var{s}(i), @var{t}(i))} pair.  For undirected graphs
## @code{(@var{s}, @var{t})} and @code{(@var{t}, @var{s})} refer to the
## same edge.
##
## @var{labels} is either a cell array of strings (one per selected
## edge), a numeric vector (converted element-wise via @code{num2str}),
## a single character-row vector (broadcast to every selected edge), or
## a scalar numeric or single-cell cellstr (also broadcast).  The
## @code{EdgeLabelMode} property of @var{h} is set to @qcode{"manual"}.
##
## Selected edges receive the specified labels while the remaining
## edges keep their current labels.  Calling @code{labeledge} with
## empty selection is a silent no-op.
##
## @example
## @group
## G = digraph ([1 2 3], [2 3 1]);
## h = plot (G);
## labeledge (h, [1 3], @{"first", "third"@});
## labeledge (h, 2, 3, "two-three");
## labeledge (h, 1, 42);         # numeric label -> "42"
## @end group
## @end example
##
## @seealso{GraphPlot, plot, labelnode, highlight, graph, digraph}
## @end deftypefn

function labeledge (h, varargin)

  if (nargin < 3)
    print_usage ();
  endif

  if (! isa (h, "GraphPlot"))
    error ("Octave:invalid-input-arg", ...
           "labeledge: first argument must be a GraphPlot handle");
  endif

  ## Defensive delegation: classdef method dispatch usually intercepts
  ## @code{labeledge (h, ...)} when h is a GraphPlot, but we keep the
  ## fallback explicit so the help text is self-contained and the
  ## free-function entry point works from any load path.
  h.labeledge (varargin{:});

endfunction


## ---------------- BIST ----------------

## Basic smoke test: free-function entry point works and sets label.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = plot (G);
%!   labeledge (h, 1, "alpha");
%!   assert (h.EdgeLabel{1}, "alpha");
%!   assert (h.EdgeLabelMode, "manual");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## (s,t) form via free function with cellstr endpoints.
%!test
%! G = digraph ([1 2 3], [2 3 1], [], {"A", "B", "C"});
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = plot (G);
%!   labeledge (h, {"A", "B"}, {"B", "C"}, {"a1", "b2"});
%!   assert (h.EdgeLabel{1}, "a1");
%!   assert (h.EdgeLabel{2}, "b2");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Numeric labels via free function.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = plot (G);
%!   labeledge (h, [1 2 3], [10 20 30]);
%!   assert (h.EdgeLabel, {"10"; "20"; "30"});
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Non-GraphPlot first arg is rejected by the free-function.
%!error <GraphPlot> labeledge (1, 1, "x")
%!error <GraphPlot> labeledge ("bogus", 1, "x")

## Missing arguments: print_usage via the free-function entry.
%!error labeledge ()
%!error labeledge (GraphPlot ())
%!error labeledge (GraphPlot (), 1)
