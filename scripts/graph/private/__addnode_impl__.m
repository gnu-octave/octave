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
## @deftypefn {} {[@var{adj_out}, @var{names_out}, @var{attrs_out}] =} __addnode_impl__ (@var{adj_in}, @var{names_in}, @var{attrs_in}, @var{newnodes})
## Private helper shared by the @code{addnode} methods of the
## @code{graph} and @code{digraph} classes.
##
## Takes the current adjacency matrix @var{adj_in} (@code{N}-by-@code{N}
## sparse), node-name cellstr @var{names_in} (@code{N}-by-1 or empty
## when unnamed), and node-attribute struct @var{attrs_in}, plus the
## user's @code{newnodes} argument (non-negative integer scalar,
## cellstr, char row, or scalar struct NodeTable).
##
## Returns the updated triple @var{adj_out} (@code{(N+K)}-by-@code{(N+K)}
## sparse with the original upper-left submatrix preserved and zeros
## elsewhere), @var{names_out} (cellstr of length @code{N+K} or empty
## when nameless), and @var{attrs_out} (struct with each field of
## length @code{N+K}).
##
## The helper has no output for the edge arrays
## (@code{has_weights_}, @code{edge_attrs_}, @code{mg_endnodes_},
## @code{mg_weights_}, @code{is_multigraph_}) because @code{addnode}
## does not change them: new nodes have no incident edges by
## construction.
##
## Naming rules match MATLAB's @code{addnode}:
##
## @itemize
## @item
## If @var{newnodes} is a count and the input was named, new nodes
## receive auto-generated @qcode{"NodeK"} names starting at
## @code{K = numnodes + 1}, bumping past any collision.
## @item
## If @var{newnodes} supplies names and the input was unnamed,
## existing nodes are auto-named @qcode{"Node1"}, @dots{},
## @qcode{"NodeN"} before the new names are appended.
## @item
## New names must be unique among themselves and must not collide
## with existing names (or auto-generated names for existing nodes).
## @end itemize
##
## @seealso{addnode, graph, digraph}
## @end deftypefn

function [adj_out, names_out, attrs_out] = __addnode_impl__ (adj_in, names_in, attrs_in, newnodes)

  if (nargin != 4)
    print_usage ();
  endif

  N = size (adj_in, 1);
  had_names = ! isempty (names_in);

  ## Parse newnodes: derive K (count of new nodes), new_names (K-by-1
  ## cellstr or {}), and new_attrs (struct of extra columns).
  new_names = {};
  new_attrs = struct ();

  if (isnumeric (newnodes))
    if (! (isscalar (newnodes) && isreal (newnodes) ...
           && isfinite (newnodes) && newnodes >= 0 ...
           && newnodes == fix (newnodes)))
      error ("Octave:invalid-input-arg", ...
             "addnode: N must be a non-negative integer scalar");
    endif
    K = double (newnodes);
  elseif (ischar (newnodes))
    if (! isrow (newnodes))
      error ("Octave:invalid-input-arg", ...
             "addnode: character input must be a single row string");
    endif
    new_names = {newnodes};
    K = 1;
  elseif (iscell (newnodes))
    if (! iscellstr (newnodes))
      error ("Octave:invalid-input-arg", ...
             "addnode: cell input must be a cell array of strings (cellstr) of node names");
    endif
    new_names = newnodes(:);
    K = numel (new_names);
    if (numel (unique (new_names)) != K)
      error ("Octave:invalid-input-arg", ...
             "addnode: duplicate names in the new-node list");
    endif
  elseif (isstruct (newnodes))
    if (! isscalar (newnodes))
      error ("Octave:invalid-input-arg", ...
             "addnode: NodeTable must be a scalar struct");
    endif
    nt = newnodes;
    nf = fieldnames (nt);
    has_name = isfield (nt, "Name");
    ## Validate Name when present, and set new_names/K.
    if (has_name)
      nm = nt.Name;
      if (! iscellstr (nm))
        error ("Octave:invalid-input-arg", ...
               "addnode: NodeTable Name must be a cell array of strings");
      endif
      nm = nm(:);
      if (numel (unique (nm)) != numel (nm))
        error ("Octave:invalid-input-arg", ...
               "addnode: NodeTable Name contains duplicate values");
      endif
      new_names = nm;
      K = numel (new_names);
    else
      ## Infer K from the first non-empty column (or 0 if no columns).
      if (numel (nf) >= 1)
        K = size (nt.(nf{1}), 1);
      else
        K = 0;
      endif
    endif
    ## All columns must have K rows.
    for ii = 1:numel (nf)
      if (size (nt.(nf{ii}), 1) != K)
        error ("Octave:invalid-input-arg", ...
               "addnode: NodeTable columns must all have the same number of rows");
      endif
    endfor
    ## Extract extra (non-Name) columns.
    for ii = 1:numel (nf)
      if (strcmp (nf{ii}, "Name"))
        continue;
      endif
      new_attrs.(nf{ii}) = nt.(nf{ii});
    endfor
  else
    error ("Octave:invalid-input-arg", ...
           ["addnode: second argument must be a non-negative integer ", ...
            "count, a cellstr of names, or a NodeTable struct"]);
  endif

  ## Detect collisions between new_names and existing names.
  if (! isempty (new_names) && had_names)
    both = [names_in; new_names];
    if (numel (unique (both)) != numel (both))
      error ("Octave:invalid-input-arg", ...
             "addnode: a new node name already exists in the graph");
    endif
  endif

  ## Decide final_new_names (what gets appended to names_in).
  ## Three bookkeeping cases:
  ##   (A) had_names and new_names empty: auto-gen K "NodeK" names for
  ##       new nodes.
  ##   (B) ! had_names and new_names non-empty: auto-gen existing
  ##       "Node1"..."NodeN" names first, then append new_names.  Check
  ##       no collision between new_names and the auto names.
  ##   (C) had_names and new_names non-empty: just append (collision
  ##       already checked above).
  ##   (D) ! had_names and new_names empty: stays nameless, nothing to
  ##       append.
  names_out = names_in;
  final_new_names = new_names;
  if (K > 0 && had_names && isempty (new_names))
    ## Case (A).
    final_new_names = auto_gen_new_names (names_in, N, K);
  elseif (K > 0 && ! had_names && ! isempty (new_names))
    ## Case (B).
    existing_auto = cell (N, 1);
    for ii = 1:N
      existing_auto{ii} = sprintf ("Node%d", ii);
    endfor
    both = [existing_auto; new_names];
    if (numel (unique (both)) != numel (both))
      error ("Octave:invalid-input-arg", ...
             ["addnode: a new node name collides with an auto-generated ", ...
              "name ('NodeK') for an existing unnamed node"]);
    endif
    names_out = existing_auto;
  endif

  ## Resize adjacency matrix from NxN to (N+K)x(N+K).
  if (K > 0)
    if (N == 0)
      adj_out = sparse (K, K);
    else
      adj_out = adj_in;
      adj_out(N+K, N+K) = 0;
    endif
  else
    adj_out = adj_in;
  endif

  ## Append new names.
  if (! isempty (final_new_names))
    names_out = [names_out; final_new_names];
  endif

  ## Extend node_attrs_.  For each existing field extend by K rows of
  ## either new_attrs values or defaults.  For each new_attrs field not
  ## in existing, prepend N default rows.
  attrs_out = attrs_in;
  existing_fn = fieldnames (attrs_out);
  for ii = 1:numel (existing_fn)
    fn = existing_fn{ii};
    old_col = attrs_out.(fn);
    if (isfield (new_attrs, fn))
      ext = new_attrs.(fn);
      if (size (ext, 2) != size (old_col, 2))
        error ("Octave:invalid-input-arg", ...
               "addnode: NodeTable column '%s' column count does not match existing column", fn);
      endif
      try
        attrs_out.(fn) = [old_col; ext];
      catch err
        error ("Octave:invalid-input-arg", ...
               "addnode: NodeTable column '%s' is incompatible with the existing column type (%s)", ...
               fn, err.message);
      end_try_catch
    else
      attrs_out.(fn) = [old_col; default_rows(old_col, K)];
    endif
  endfor
  new_fn = fieldnames (new_attrs);
  for ii = 1:numel (new_fn)
    fn = new_fn{ii};
    if (isfield (attrs_out, fn))
      continue;   # already handled above
    endif
    new_col = new_attrs.(fn);
    if (N > 0)
      attrs_out.(fn) = [default_rows(new_col, N); new_col];
    else
      attrs_out.(fn) = new_col;
    endif
  endfor

endfunction

## Helper: generate K new auto-names "NodeJ" avoiding any names already
## in @var{existing}.  Start probing at J = N + 1 and bump until each
## name is unique.  Returns a K-by-1 cellstr.
function nm = auto_gen_new_names (existing, N, K)

  nm = cell (K, 1);
  taken = existing(:);
  j = N + 1;
  for i = 1:K
    while (true)
      candidate = sprintf ("Node%d", j);
      if (! any (strcmp (taken, candidate)))
        nm{i} = candidate;
        taken = [taken; {candidate}];
        j = j + 1;
        break;
      endif
      j = j + 1;
    endwhile
  endfor

endfunction

## Helper: construct K rows of default values that match the type of
## the existing column @var{col}.  Preserves column count.
function r = default_rows (col, K)

  cols = size (col, 2);
  if (K == 0)
    ## Preserve type via a zero-row slice of the same column width.
    r = col(1:0, :);
    return;
  endif
  if (iscellstr (col))
    r = repmat ({""}, K, cols);
  elseif (iscell (col))
    r = cell (K, cols);
  elseif (islogical (col))
    r = false (K, cols);
  elseif (isnumeric (col))
    r = zeros (K, cols, class (col));
  elseif (ischar (col))
    r = repmat (' ', K, cols);
  else
    error ("Octave:invalid-input-arg", ...
           "addnode: cannot generate default values for existing node column of class %s", ...
           class (col));
  endif

endfunction


## ------------------------------------------------------------------
## Private-helper smoke tests (run via the BIST of scripts/graph/addnode.m
## when the private directory is on the load path).
## ------------------------------------------------------------------

%!test
%! [A, nm, at] = __addnode_impl__ (sparse (0, 0), {}, struct (), 3);
%! assert (size (A), [3, 3]);
%! assert (nm, {});
%! assert (isequal (at, struct ()));

%!test
%! [A, nm, at] = __addnode_impl__ (sparse (3, 3), {"a"; "b"; "c"}, struct (), 2);
%! assert (size (A), [5, 5]);
%! assert (nm, {"a"; "b"; "c"; "Node4"; "Node5"});

%!test
%! [A, nm, at] = __addnode_impl__ (sparse (2, 2), {}, struct (), {"x", "y"});
%! assert (nm, {"Node1"; "Node2"; "x"; "y"});

%!error <non-negative integer> ...
%! __addnode_impl__ (sparse (0, 0), {}, struct (), -1);

%!error <duplicate> ...
%! __addnode_impl__ (sparse (0, 0), {}, struct (), {"a", "a"});
