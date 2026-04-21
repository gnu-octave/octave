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
## @deftypefn {} {[@var{X}, @var{Y}] =} __graph_plot_auto_layout__ (@var{G}, @var{layout})
## Compute 2-D node coordinates for @code{plot}'ting a @code{graph} or
## @code{digraph}.
##
## @var{G} must be a @code{graph} or @code{digraph}.  @var{layout} is a
## lowercase string selecting the layout algorithm.  Recognised values:
##
## @table @code
## @item auto
## Default.  Dispatches by node count: fewer than 100 nodes use the
## @qcode{"subspace"} branch, the rest use the @qcode{"force"} branch.
## @item subspace
## Placeholder routed to a deterministic circle layout (to be replaced
## by the proper spectral subspace algorithm in a subsequent story).
## @item force
## Placeholder routed to a deterministic circle layout (to be replaced
## by Fruchterman-Reingold in a subsequent story).
## @item circle
## Unit-circle placement: @code{theta(k) = 2*pi*(k-1)/N}.
## @end table
##
## Returns @var{X} and @var{Y} as column vectors of length
## @code{numnodes (G)}.  Both are populated with finite double values
## for every @var{N} @code{>= 0}.
## @seealso{plot, GraphPlot}
## @end deftypefn

function [X, Y] = __graph_plot_auto_layout__ (G, layout)

  if (nargin != 2)
    print_usage ();
  endif
  if (! (isa (G, "graph") || isa (G, "digraph")))
    error ("Octave:invalid-input-arg", ...
           "__graph_plot_auto_layout__: G must be a graph or digraph");
  endif
  if (! (ischar (layout) && isrow (layout)))
    error ("Octave:invalid-input-arg", ...
           "__graph_plot_auto_layout__: LAYOUT must be a character vector");
  endif

  N = numnodes (G);
  layout = lower (layout);

  switch (layout)
    case "auto"
      ## US-GP01 scaffolding: auto picks "subspace" for small graphs
      ## and "force" for the rest.  Both placeholders share the circle
      ## fallback below; later stories will supply the real algorithms.
      if (N < 100)
        [X, Y] = __gp_layout_circle__ (N);
      else
        [X, Y] = __gp_layout_circle__ (N);
      endif
    case "subspace"
      [X, Y] = __gp_layout_circle__ (N);
    case "force"
      [X, Y] = __gp_layout_circle__ (N);
    case "circle"
      [X, Y] = __gp_layout_circle__ (N);
    otherwise
      error ("Octave:invalid-input-arg", ...
             "__graph_plot_auto_layout__: unknown layout '%s'", layout);
  endswitch

endfunction


## Local helper: unit-circle placement used as the deterministic
## fallback for every layout branch at the US-GP01 checkpoint.
function [X, Y] = __gp_layout_circle__ (N)

  if (N == 0)
    X = zeros (0, 1);
    Y = zeros (0, 1);
    return;
  elseif (N == 1)
    X = 0;
    Y = 0;
    return;
  endif

  theta = (2 * pi) * ((0:(N - 1)).') / N;
  X = cos (theta);
  Y = sin (theta);

endfunction


## ---------------- BIST ----------------

## Empty graph: both coordinate vectors are 0-by-1.
%!test
%! G = digraph ();
%! [X, Y] = __graph_plot_auto_layout__ (G, "auto");
%! assert (size (X), [0, 1]);
%! assert (size (Y), [0, 1]);

## Single-node graph: origin.
%!test
%! G = digraph (1);
%! [X, Y] = __graph_plot_auto_layout__ (G, "auto");
%! assert (X, 0);
%! assert (Y, 0);

## Small graph uses the subspace branch; finite coordinates, length N.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! [X, Y] = __graph_plot_auto_layout__ (G, "auto");
%! assert (numel (X), 3);
%! assert (numel (Y), 3);
%! assert (all (isfinite (X)));
%! assert (all (isfinite (Y)));

## Large graph uses the force branch; finite coordinates, length N.
%!test
%! N = 150;
%! G = digraph (1:(N-1), 2:N);
%! [X, Y] = __graph_plot_auto_layout__ (G, "auto");
%! assert (numel (X), N);
%! assert (numel (Y), N);
%! assert (all (isfinite (X)));
%! assert (all (isfinite (Y)));

## Explicit layout names all accepted.
%!test
%! G = digraph ([1 2], [2 3]);
%! for name = {"auto", "subspace", "force", "circle"}
%!   [X, Y] = __graph_plot_auto_layout__ (G, name{1});
%!   assert (numel (X), 3);
%!   assert (numel (Y), 3);
%!   assert (all (isfinite (X)));
%!   assert (all (isfinite (Y)));
%! endfor

## Layout names are case-insensitive.
%!test
%! G = digraph ([1 2], [2 3]);
%! [X1, Y1] = __graph_plot_auto_layout__ (G, "auto");
%! [X2, Y2] = __graph_plot_auto_layout__ (G, "AUTO");
%! assert (X1, X2);
%! assert (Y1, Y2);

## Circle layout places nodes on the unit circle.
%!test
%! G = digraph (6);
%! [X, Y] = __graph_plot_auto_layout__ (G, "circle");
%! assert (sqrt (X.^2 + Y.^2), ones (6, 1), 1e-12);

## Undirected graphs are handled identically.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! [X, Y] = __graph_plot_auto_layout__ (G, "auto");
%! assert (numel (X), 3);
%! assert (numel (Y), 3);

## Errors.
%!error <graph or digraph> __graph_plot_auto_layout__ (1, "auto")
%!error <character vector> __graph_plot_auto_layout__ (digraph (3), 1)
%!error <unknown layout> __graph_plot_auto_layout__ (digraph (3), "nope")
%!error <Invalid call> __graph_plot_auto_layout__ (digraph (3))
