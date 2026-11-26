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
## @deftypefn {} {[@var{s_idx}, @var{t_idx}, @var{w_vec}, @var{N_new}, @var{names_out}, @var{nattrs_out}, @var{hw_out}] =} __addedge_impl__ (@var{names_in}, @var{nattrs_in}, @var{hw_in}, @var{Nold}, @var{has_existing_edges}, @var{varargin})
## Private helper shared by the @code{addedge} methods of the
## @code{graph} and @code{digraph} classes.
##
## Resolves the user-supplied @code{(s, t, [w])} endpoint arguments
## (or the struct-form @code{EdgeTable}), appends any new named nodes
## implied by the endpoints, extends the numeric node range when
## necessary, and computes the weight vector for the new edges.
##
## Returns @var{s_idx} and @var{t_idx} as column @code{m}-by-1
## node-index vectors into the @emph{new} node set (after any
## auto-addition), @var{w_vec} as the column @code{m}-by-1 weight
## vector (or @code{[]} when the result should stay unweighted),
## @var{N_new} as the new node count, @var{names_out} as the updated
## node-name cellstr (empty when the graph has no names),
## @var{nattrs_out} as the updated node-attribute struct extended
## with default rows for any new nodes, and @var{hw_out} as the new
## @code{has_weights_} flag (which may become @code{true} when an
## unweighted edgeless graph is promoted by user-supplied weights).
##
## The helper does @emph{not} touch edge storage -- the class method
## owns the @code{adj_} / @code{mg_endnodes_} / @code{mg_weights_}
## state and decides whether parallel edges are permitted.
##
## Only @code{EndNodes} and @code{Weight} are honoured in the
## @code{EdgeTable} form; any other fields cause an error.  Future
## work (a follow-up US) may extend this to carry user-supplied
## edge-attribute columns.
##
## @seealso{addedge, graph, digraph}
## @end deftypefn

function [s_idx, t_idx, w_vec, N_new, names_out, nattrs_out, hw_out] = ...
  __addedge_impl__ (names_in, nattrs_in, hw_in, Nold, has_existing_edges, varargin)

  if (nargin < 6)
    print_usage ();
  endif

  narg = numel (varargin);
  if (narg < 1 || narg > 3)
    error ("Octave:invalid-fun-call", ...
           "addedge: expected 2, 3, or 4 total arguments");
  endif

  ## Distinguish EdgeTable form from (s, t[, w]).  The EdgeTable form
  ## is the sole single-extra-arg call.
  have_w_arg = false;
  w_arg = [];
  if (narg == 1)
    ET = varargin{1};
    if (! (isstruct (ET) && isscalar (ET)))
      error ("Octave:invalid-input-arg", ...
             "addedge: single extra argument must be a scalar EdgeTable struct");
    endif
    if (! isfield (ET, "EndNodes"))
      error ("Octave:invalid-input-arg", ...
             "addedge: EdgeTable must have an EndNodes field");
    endif
    EN = ET.EndNodes;
    if (! (isnumeric (EN) || iscellstr (EN)))
      error ("Octave:invalid-input-arg", ...
             ["addedge: EndNodes must be a numeric matrix or a ", ...
              "cell array of strings"]);
    endif
    if (ndims (EN) != 2 || size (EN, 2) != 2)
      error ("Octave:invalid-input-arg", ...
             "addedge: EndNodes must have exactly two columns");
    endif
    s_arg = EN(:, 1);
    t_arg = EN(:, 2);
    ## Reject any extra columns other than Weight (future work).
    ef = fieldnames (ET);
    for ii = 1:numel (ef)
      fn_i = ef{ii};
      if (! (strcmp (fn_i, "EndNodes") || strcmp (fn_i, "Weight")))
        error ("Octave:invalid-input-arg", ...
               ["addedge: EdgeTable extra column '%s' is not ", ...
                "supported (only EndNodes and Weight are recognised)"], ...
               fn_i);
      endif
    endfor
    if (isfield (ET, "Weight"))
      have_w_arg = true;
      w_arg = ET.Weight;
    endif
  else
    s_arg = varargin{1};
    t_arg = varargin{2};
    if (narg == 3)
      have_w_arg = true;
      w_arg = varargin{3};
    endif
  endif

  ## Normalise endpoint arg shapes (char row -> {v}; numeric column).
  s_arg = normalise_endpoint (s_arg, "S");
  t_arg = normalise_endpoint (t_arg, "T");
  if (numel (s_arg) != numel (t_arg))
    error ("Octave:invalid-input-arg", ...
           "addedge: S and T must have the same length");
  endif
  m = numel (s_arg);

  ## Resolve each endpoint array, auto-extending node set where needed.
  ## Start from the current graph state and update through the pair.
  names_out = names_in(:);   # column cellstr or empty
  nattrs_out = nattrs_in;
  N_cur = Nold;

  [s_idx, names_out, nattrs_out, N_cur] = ...
    resolve_one (s_arg, names_out, nattrs_out, N_cur, "S");
  [t_idx, names_out, nattrs_out, N_cur] = ...
    resolve_one (t_arg, names_out, nattrs_out, N_cur, "T");

  N_new = N_cur;

  ## Weight resolution.
  hw_out = hw_in;
  if (have_w_arg)
    if (! (isnumeric (w_arg) && isreal (w_arg)))
      error ("Octave:invalid-input-arg", ...
             "addedge: W must be a numeric real vector or scalar");
    endif
    if (! (isvector (w_arg) || isempty (w_arg) || isscalar (w_arg)))
      error ("Octave:invalid-input-arg", ...
             "addedge: W must be a vector or scalar");
    endif
    w_arg = double (w_arg(:));
    if (isscalar (w_arg))
      w_vec = repmat (w_arg, m, 1);
    elseif (isempty (w_arg))
      if (m == 0)
        w_vec = zeros (0, 1);
      else
        error ("Octave:invalid-input-arg", ...
               ["addedge: weight vector W must have length ", ...
                "numel (S) or be a scalar"]);
      endif
    else
      if (numel (w_arg) != m)
        error ("Octave:invalid-input-arg", ...
               ["addedge: weight vector W must have length ", ...
                "numel (S) or be a scalar"]);
      endif
      w_vec = w_arg;
    endif
    if (any (isnan (w_vec)))
      error ("Octave:invalid-input-arg", ...
             "addedge: weight vector W must not contain NaN");
    endif
    if (! hw_in)
      if (has_existing_edges)
        error ("Octave:invalid-input-arg", ...
               ["addedge: cannot add weighted edges to an ", ...
                "unweighted graph that already has edges"]);
      endif
      if (m > 0)
        hw_out = true;
      endif
    endif
  else
    if (hw_in)
      ## G is weighted; supply default weight 1 for new edges.
      w_vec = ones (m, 1);
    else
      w_vec = zeros (0, 1);
    endif
  endif

endfunction


## --------------------------------------------------------------------
## Helpers
## --------------------------------------------------------------------

## Normalise an endpoint argument into a column-vector shape suitable for
## resolution.  Char row vectors become 1-element cellstrs so downstream
## code sees a uniform shape.  Scalars stay scalar.  Returns either a
## column numeric vector or a column cellstr.
function v = normalise_endpoint (v, name)

  if (ischar (v))
    if (isempty (v))
      v = cell (0, 1);
      return;
    endif
    if (! isrow (v))
      error ("Octave:invalid-input-arg", ...
             "addedge: %s must be a numeric array, a char row vector, or a cellstr", name);
    endif
    v = {v};
    return;
  endif

  if (iscell (v))
    if (! iscellstr (v))
      error ("Octave:invalid-input-arg", ...
             "addedge: %s must be a numeric array, a char row vector, or a cellstr", name);
    endif
    v = v(:);
    return;
  endif

  if (isnumeric (v))
    if (! isreal (v))
      error ("Octave:invalid-input-arg", ...
             "addedge: %s must be a numeric array, a char row vector, or a cellstr", name);
    endif
    if (! (isvector (v) || isempty (v) || isscalar (v)))
      error ("Octave:invalid-input-arg", ...
             "addedge: %s must be a vector", name);
    endif
    v = double (v(:));
    return;
  endif

  error ("Octave:invalid-input-arg", ...
         "addedge: %s must be a numeric array, a char row vector, or a cellstr", name);

endfunction


## Resolve one endpoint vector (numeric or cellstr) into node indices,
## auto-extending @var{names} / @var{nattrs} / @var{N} for any new
## nodes the argument introduces.  Returns the resolved column-vector
## of indices plus the updated (names, nattrs, N) triple.
function [idx, names, nattrs, N] = resolve_one (v, names, nattrs, N, label)

  has_names = ! isempty (names);

  if (isnumeric (v))
    if (isempty (v))
      idx = zeros (0, 1);
      return;
    endif
    if (any (! isfinite (v)) || any (v < 1) || any (v != fix (v)))
      error ("Octave:invalid-input-arg", ...
             ["addedge: ", label, " entries must be positive integers"]);
    endif
    idx = v;
    max_v = max (v);
    if (max_v > N)
      ## Extend node set.  If G is named, auto-generate "NodeK" names
      ## for the new slots (with collision bumping against existing).
      K = max_v - N;
      if (has_names)
        names = [names; auto_gen_node_names(names, N, K)];
      endif
      nattrs = extend_nattrs_defaults (nattrs, K);
      N = max_v;
    endif
    return;
  endif

  ## cellstr
  if (! has_names)
    if (N > 0)
      error ("Octave:invalid-input-arg", ...
             ["addedge: ", label, " uses node names but the graph ", ...
              "has no node names.  Name the existing nodes first ", ...
              "(see addnode) or pass numeric endpoints"]);
    endif
    ## G is empty and nameless: promote to named by creating names
    ## from the unique set of v in first-appearance order.
    names = unique (v, "stable");
    names = names(:);
    N = numel (names);
    nattrs = extend_nattrs_defaults (nattrs, N);
  endif

  m_v = numel (v);
  idx = zeros (m_v, 1);
  for ii = 1:m_v
    nm = v{ii};
    match = find (strcmp (names, nm), 1);
    if (isempty (match))
      ## New node: append.
      names = [names; {nm}];
      N = N + 1;
      nattrs = extend_nattrs_defaults (nattrs, 1);
      idx(ii) = N;
    else
      idx(ii) = match;
    endif
  endfor

endfunction


## Append K default rows to every field of @var{nattrs}, preserving each
## column's type via default_rows().  Returns the extended struct.
function nattrs = extend_nattrs_defaults (nattrs, K)

  if (K <= 0)
    return;
  endif
  fn = fieldnames (nattrs);
  for ii = 1:numel (fn)
    f = fn{ii};
    col = nattrs.(f);
    nattrs.(f) = [col; default_rows(col, K)];
  endfor

endfunction


## Build K auto-generated node names "NodeJ" avoiding any in @var{existing}.
## Matches the convention used by __addnode_impl__.
function nm = auto_gen_node_names (existing, N, K)

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


## Construct K rows of default values that match the type of @var{col}.
function r = default_rows (col, K)

  cols = size (col, 2);
  if (K == 0)
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
           "addedge: cannot generate default values for existing node column of class %s", ...
           class (col));
  endif

endfunction


## ------------------------------------------------------------------
## Private-helper smoke tests.
## ------------------------------------------------------------------

## Form: (s, t) numeric on a 3-node unnamed graph.
%!test
%! [si, ti, wv, Nn, nm, na, hw] = __addedge_impl__ ({}, struct (), false, 3, true, 1, 2);
%! assert (si, 1);
%! assert (ti, 2);
%! assert (isequal (wv, zeros (0, 1)));
%! assert (Nn, 3);
%! assert (hw, false);

## Form: (s, t, w) on a weighted 3-node graph.
%!test
%! [si, ti, wv, Nn, nm, na, hw] = __addedge_impl__ ({}, struct (), true, 3, true, 2, 3, 10);
%! assert (si, 2);
%! assert (ti, 3);
%! assert (wv, 10);
%! assert (hw, true);

## Form: (s, t) string on a named graph appends new node.
%!test
%! names = {"a"; "b"; "c"};
%! [si, ti, wv, Nn, nm, na, hw] = __addedge_impl__ (names, struct (), false, 3, true, "c", "d");
%! assert (si, 3);
%! assert (ti, 4);
%! assert (Nn, 4);
%! assert (nm, {"a"; "b"; "c"; "d"});

## Form: numeric endpoint > N auto-extends unnamed graph.
%!test
%! [si, ti, wv, Nn, nm, na, hw] = __addedge_impl__ ({}, struct (), false, 3, true, 2, 5);
%! assert (si, 2);
%! assert (ti, 5);
%! assert (Nn, 5);
%! assert (isempty (nm));

## Form: empty G + named endpoints -> promote to named.
%!test
%! [si, ti, wv, Nn, nm, na, hw] = __addedge_impl__ ({}, struct (), false, 0, false, "a", "b");
%! assert (Nn, 2);
%! assert (nm, {"a"; "b"});

## Form: EdgeTable with EndNodes + Weight.
%!test
%! ET.EndNodes = [1 2; 2 3];
%! ET.Weight   = [10; 20];
%! [si, ti, wv, Nn, nm, na, hw] = __addedge_impl__ ({}, struct (), true, 3, true, ET);
%! assert (si, [1; 2]);
%! assert (ti, [2; 3]);
%! assert (wv, [10; 20]);

## Error: length mismatch between S and T.
%!error <same length> ...
%! __addedge_impl__ ({}, struct (), false, 3, true, [1 2], [3]);

## Error: weighted edges on unweighted non-empty graph.
%!error <unweighted> ...
%! __addedge_impl__ ({}, struct (), false, 3, true, 1, 2, 5);

## Error: string endpoint on unnamed non-empty graph.
%!error <no node names> ...
%! __addedge_impl__ ({}, struct (), false, 3, true, "a", "b");

## Error: EdgeTable missing EndNodes.
%!error <EndNodes> ...
%! ET.Weight = 5;
%! __addedge_impl__ ({}, struct (), false, 3, true, ET);

## Error: EdgeTable EndNodes wrong shape.
%!error <two columns> ...
%! ET.EndNodes = [1 2 3];
%! __addedge_impl__ ({}, struct (), false, 4, true, ET);

## Error: EdgeTable extra column not supported.
%!error <not supported> ...
%! ET.EndNodes = [1 2];
%! ET.Foo = 7;
%! __addedge_impl__ ({}, struct (), false, 3, true, ET);

## Error: non-integer endpoint.
%!error <positive integer> ...
%! __addedge_impl__ ({}, struct (), false, 3, true, 1.5, 2);
