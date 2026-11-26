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
## @deftypefn {} {} labelnode (@var{h}, @var{nodes}, @var{labels})
## Set labels on the nodes of a @code{GraphPlot} @var{h}.
##
## @var{h} is a @code{GraphPlot} handle, typically the return value of
## @code{plot (@var{G})} for a @code{graph} or @code{digraph} @var{G}.
##
## @var{nodes} selects the nodes whose labels are to be changed.  It
## may be a numeric vector of 1-based node indices (any shape), a
## character row vector holding a single node name, a cell array of
## node names, or an empty array (silent no-op).
##
## @var{labels} is either a cell array of strings (one per selected
## node), a numeric vector (converted element-wise via @code{num2str}),
## a single character-row vector (broadcast to every selected node), or
## a scalar numeric or single-cell cellstr (also broadcast).  The
## @code{NodeLabelMode} property of @var{h} is set to @qcode{"manual"}.
##
## Selected nodes receive the specified labels while the remaining
## nodes keep their current labels.  Calling @code{labelnode} with
## empty selection is a silent no-op.
##
## @example
## @group
## G = digraph ([1 2 3], [2 3 1]);
## h = plot (G);
## labelnode (h, [1 3], @{"first", "third"@});
## labelnode (h, 2, "middle");
## labelnode (h, 1, 42);         # numeric label -> "42"
## @end group
## @end example
##
## @seealso{GraphPlot, plot, labeledge, highlight, graph, digraph}
## @end deftypefn

function labelnode (h, varargin)

  if (nargin < 3)
    print_usage ();
  endif

  if (! isa (h, "GraphPlot"))
    error ("Octave:invalid-input-arg", ...
           "labelnode: first argument must be a GraphPlot handle");
  endif

  ## Defensive delegation: classdef method dispatch usually intercepts
  ## @code{labelnode (h, ...)} when h is a GraphPlot, but we keep the
  ## fallback explicit so the help text is self-contained and the
  ## free-function entry point works from any load path.
  h.labelnode (varargin{:});

endfunction


## ---------------- BIST ----------------

## Basic smoke test: free-function entry point works and sets label.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = plot (G);
%!   labelnode (h, 1, "alpha");
%!   assert (h.NodeLabel{1}, "alpha");
%!   assert (h.NodeLabelMode, "manual");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Cellstr nodes via free function with named nodes.
%!test
%! G = digraph ([1 2 3], [2 3 1], [], {"A", "B", "C"});
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = plot (G);
%!   labelnode (h, {"A", "C"}, {"first", "last"});
%!   assert (h.NodeLabel{1}, "first");
%!   assert (h.NodeLabel{3}, "last");
%!   ## Node 2 keeps its auto-generated default label.
%!   assert (h.NodeLabel{2}, "B");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Numeric labels via free function.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = plot (G);
%!   labelnode (h, [1 2 3], [10 20 30]);
%!   assert (h.NodeLabel, {"10"; "20"; "30"});
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Non-GraphPlot first arg is rejected by the free-function.
%!error <GraphPlot> labelnode (1, 1, "x")
%!error <GraphPlot> labelnode ("bogus", 1, "x")

## Missing arguments: print_usage via the free-function entry.
%!error labelnode ()
%!error labelnode (GraphPlot ())
%!error labelnode (GraphPlot (), 1)
