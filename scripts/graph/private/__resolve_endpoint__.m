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
## @deftypefn {} {@var{idx} =} __resolve_endpoint__ (@var{x}, @var{names}, @var{argname})
## Private helper for the @code{digraph} and @code{graph} classdef
## constructors.
##
## Convert the user-supplied endpoint vector @var{x} into a column
## vector of 1-based node indices into @var{names} (a cellstr of
## unique node names).  @var{argname} is the display name used in
## error messages (typically @qcode{"S"} or @qcode{"T"}).
##
## If @var{x} is numeric, its entries must be positive integers in
## the range @code{1:numel (@var{names})}.  If @var{x} is a char row
## vector it is treated as a single node name.  If @var{x} is a cell
## array of strings each element is looked up in @var{names}; a
## missing name raises @samp{digraph: node name '@dots{}' not found
## in NODENAMES}.
##
## The helper returns a column vector @var{idx} of @code{double}
## indices with @code{numel (@var{idx}) == numel (@var{x})}.
## @seealso{digraph, graph}
## @end deftypefn

function idx = __resolve_endpoint__ (x, names, argname)

  if (nargin != 3)
    print_usage ();
  endif

  ## A bare char row vector is a single node name.  Promote to a
  ## 1-element cellstr so the lookup branch handles it.
  if (ischar (x))
    x = {x};
  endif

  if (iscellstr (x))
    idx = zeros (numel (x), 1);
    for i = 1:numel (x)
      match = find (strcmp (names, x{i}), 1);
      if (isempty (match))
        error ("Octave:invalid-input-arg", ...
               "digraph: node name '%s' not found in NODENAMES", x{i});
      endif
      idx(i) = match;
    endfor
  elseif (isnumeric (x) && isreal (x))
    if (! (isvector (x) || isempty (x)))
      error ("Octave:invalid-input-arg", ...
             "digraph: %s must be a vector", argname);
    endif
    x = double (x(:));
    if (! isempty (x))
      if (any (! isfinite (x)) || any (x < 1) || any (x != fix (x)))
        error ("Octave:invalid-input-arg", ...
               ["digraph: %s contains an invalid node index ", ...
                "(must be a positive integer <= numel (NODENAMES))"], ...
               argname);
      endif
      if (any (x > numel (names)))
        error ("Octave:invalid-input-arg", ...
               ["digraph: %s contains an invalid node index ", ...
                "(must be a positive integer <= numel (NODENAMES))"], ...
               argname);
      endif
    endif
    idx = x;
  else
    error ("Octave:invalid-input-arg", ...
           "digraph: %s must be a numeric or string node reference", ...
           argname);
  endif

endfunction


## Private-helper smoke tests.  (Private helpers are not loaded from a
## plain script context by default, so these tests only run when the
## private directory is on the load path -- i.e. inside the BIST runs
## of scripts/graph/digraph.m.)
%!test
%! idx = __resolve_endpoint__ ([1 3 2], {"a", "b", "c"}, "S");
%! assert (idx, [1; 3; 2]);

%!test
%! idx = __resolve_endpoint__ ({"b", "a", "c"}, {"a", "b", "c"}, "S");
%! assert (idx, [2; 1; 3]);

%!test
%! idx = __resolve_endpoint__ ("b", {"a", "b", "c"}, "S");
%! assert (idx, 2);

%!test
%! idx = __resolve_endpoint__ ([], {"a", "b"}, "S");
%! assert (idx, zeros (0, 1));

%!error <node name 'z' not found> ...
%! __resolve_endpoint__ ({"a", "z"}, {"a", "b"}, "S")

%!error <invalid node index> ...
%! __resolve_endpoint__ (3, {"a", "b"}, "S")

%!error <invalid node index> ...
%! __resolve_endpoint__ (0, {"a", "b"}, "S")

%!error <invalid node index> ...
%! __resolve_endpoint__ (1.5, {"a", "b"}, "S")

%!error <numeric or string> ...
%! __resolve_endpoint__ (true, {"a", "b"}, "S")
