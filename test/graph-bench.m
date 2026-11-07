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
## @c graph-bench.m is a performance benchmark script (not a function) for
## @c the Octave @code{graph}/@code{digraph} classes.  US-R03.
##
## Usage (from the Octave prompt with @file{scripts/graph} on the load
## path):
##
## @example
## @group
## run ("test/graph-bench.m")
## @end group
## @end example
##
## The script generates random Erdos--Renyi-style digraphs at a sequence
## of node counts and times four operations on each: @code{distances}
## (single-source), @code{shortestpath}, @code{centrality}
## (@qcode{"outdegree"}), and @code{plot} (only at modest sizes).  It
## populates a @code{bench_results} struct in the caller's workspace
## with fields @code{sizes}, @code{operations}, @code{times} (seconds,
## numel(sizes)-by-numel(operations)), @code{succeeded} (logical mask
## of the same shape), and metadata fields @code{avg_degree},
## @code{seed}, @code{octave_version}, and @code{date}.
##
## The following caller-scope variables override the defaults when
## present:
##
## @table @code
## @item sizes
## Node counts to benchmark.  Default @code{[1e3, 1e4, 1e5, 1e6]}.
## @item operations
## Cellstr of operations to time.  Default
## @code{@{"distances", "shortestpath", "centrality", "plot"@}}.
## @item verbose
## Logical; print progress to stdout.  Default @code{true}.
## @item seed
## Non-negative integer seed for the random generator.  Default 42.
## @item avg_degree
## Average outdegree per node.  Default 4.  The number of edges is
## @code{avg_degree * N}.
## @item max_plot_size
## Maximum N at which @code{plot} is actually run.  Default
## @code{1e4}; larger sizes are recorded as skipped.
## @item results_file
## When non-empty, a text file path that receives a human-readable
## table of timings.  Default empty (no file written).
## @end table
##
## Reference timings measured on the development machine are checked in
## at @file{doc/graph-bench-results.txt}.
##
## @seealso{digraph, graph, distances, shortestpath, centrality, plot}

## ------------------------------------------------------------------
## Resolve caller-overridable parameters.
## ------------------------------------------------------------------

if (! exist ("sizes", "var"))
  sizes = [1e3, 1e4, 1e5, 1e6];
endif
if (! exist ("operations", "var"))
  operations = {"distances", "shortestpath", "centrality", "plot"};
endif
if (! exist ("verbose", "var"))
  verbose = true;
endif
if (! exist ("seed", "var"))
  seed = 42;
endif
if (! exist ("avg_degree", "var"))
  avg_degree = 4;
endif
if (! exist ("max_plot_size", "var"))
  max_plot_size = 1e4;
endif
if (! exist ("results_file", "var"))
  results_file = "";
endif

## ------------------------------------------------------------------
## Parameter validation.
## ------------------------------------------------------------------

if (! (isnumeric (sizes) && isvector (sizes) && all (sizes > 0) ...
       && all (sizes == round (sizes)) && all (isfinite (sizes))))
  error ("graph-bench: SIZES must be a vector of positive integers");
endif
if (! iscellstr (operations))
  error ("graph-bench: OPERATIONS must be a cell array of strings");
endif
if (! isscalar (verbose) || ! (islogical (verbose) || isnumeric (verbose)))
  error ("graph-bench: VERBOSE must be a logical scalar");
endif
if (! (isnumeric (seed) && isscalar (seed) && isfinite (seed) ...
       && seed >= 0 && seed == round (seed)))
  error ("graph-bench: SEED must be a non-negative integer scalar");
endif
if (! (isnumeric (avg_degree) && isscalar (avg_degree) ...
       && avg_degree > 0 && isfinite (avg_degree)))
  error ("graph-bench: AVG_DEGREE must be a positive scalar");
endif
if (! (isnumeric (max_plot_size) && isscalar (max_plot_size) ...
       && max_plot_size >= 0 && isfinite (max_plot_size)))
  error ("graph-bench: MAX_PLOT_SIZE must be a non-negative scalar");
endif
if (! ischar (results_file))
  error ("graph-bench: RESULTS_FILE must be a string");
endif

## Force row shape for indexing ergonomics.
sizes = sizes(:).';
operations = operations(:).';

nS = numel (sizes);
nOp = numel (operations);

## ------------------------------------------------------------------
## Seed the random generator reproducibly.
## ------------------------------------------------------------------

rand ("state", seed);

## ------------------------------------------------------------------
## Allocate the results struct.
## ------------------------------------------------------------------

bench_results = struct ( ...
  "sizes",           sizes, ...
  "operations",      {operations}, ...
  "times",           NaN (nS, nOp), ...
  "succeeded",       false (nS, nOp), ...
  "build_times",     NaN (1, nS), ...
  "edges",           NaN (1, nS), ...
  "avg_degree",      avg_degree, ...
  "seed",            seed, ...
  "max_plot_size",   max_plot_size, ...
  "octave_version",  OCTAVE_VERSION, ...
  "date",            datestr (now));

## ------------------------------------------------------------------
## Main loop.
## ------------------------------------------------------------------

for i = 1:nS
  N = sizes(i);
  M = round (avg_degree * N);

  if (verbose)
    printf ("N=%d (M=%d): ", N, M);
    fflush (stdout);
  endif

  ## Build random edge endpoints in [1, N].  Deduplicate so the
  ## resulting digraph is simple (no parallel edges) -- the current
  ## digraph constructor rejects duplicate (s, t) pairs unless the
  ## "multigraph" flag is supplied, and centrality/shortestpath are
  ## meaningful on simple graphs.
  s_idx = double (randi (N, M, 1));
  t_idx = double (randi (N, M, 1));
  pair_keys = double (s_idx) * (double (N) + 1) + double (t_idx);
  [~, ui] = unique (pair_keys, "first");
  ui = sort (ui);
  s_idx = s_idx(ui);
  t_idx = t_idx(ui);

  t0 = tic;
  G = digraph (s_idx, t_idx, [], N);
  build_time = toc (t0);
  bench_results.build_times(i) = build_time;
  bench_results.edges(i) = numedges (G);

  if (verbose)
    printf ("build=%.4fs", build_time);
    fflush (stdout);
  endif

  for j = 1:nOp
    op = operations{j};

    ## Skip plot at sizes larger than max_plot_size -- it is genuinely
    ## impractical and would blow up most CI runners.
    if (strcmp (op, "plot") && N > max_plot_size)
      if (verbose)
        printf (" | %s=SKIP", op);
        fflush (stdout);
      endif
      continue;
    endif

    try
      t0 = tic;
      switch (op)
        case "distances"
          d_ = distances (G, 1);
        case "shortestpath"
          [p_, d_] = shortestpath (G, 1, N);
        case "centrality"
          c_ = centrality (G, "outdegree");
        case "plot"
          gp_ = plot (G);
          if (! isempty (get (0, "currentfigure")))
            close (gcf);
          endif
        otherwise
          error ("graph-bench: unknown operation %s", op);
      endswitch
      dt = toc (t0);
      bench_results.times(i, j) = dt;
      bench_results.succeeded(i, j) = true;
      if (verbose)
        printf (" | %s=%.4fs", op, dt);
        fflush (stdout);
      endif
    catch err
      bench_results.times(i, j) = NaN;
      bench_results.succeeded(i, j) = false;
      if (verbose)
        printf (" | %s=FAIL(%s)", op, err.message);
        fflush (stdout);
      endif
    end_try_catch
  endfor
  if (verbose)
    printf ("\n");
    fflush (stdout);
  endif
endfor

## ------------------------------------------------------------------
## Optional: write a human-readable results table.
## ------------------------------------------------------------------

if (! isempty (results_file))
  fid = fopen (results_file, "w");
  if (fid < 0)
    error ("graph-bench: cannot open %s for writing", results_file);
  endif
  fprintf (fid, "# Octave graph/digraph performance benchmark\n");
  fprintf (fid, "# Octave version: %s\n", bench_results.octave_version);
  fprintf (fid, "# Date:           %s\n", bench_results.date);
  fprintf (fid, "# avg_degree:     %g\n", avg_degree);
  fprintf (fid, "# seed:           %d\n", seed);
  fprintf (fid, "# max_plot_size:  %g\n", max_plot_size);
  fprintf (fid, "#\n");
  fprintf (fid, "# Timings in seconds.  SKIP = not run, FAIL = threw.\n");
  fprintf (fid, "#\n");
  fprintf (fid, "%-10s %-10s %-10s", "N", "edges", "build_s");
  for j = 1:nOp
    fprintf (fid, " %-12s", operations{j});
  endfor
  fprintf (fid, "\n");
  for i = 1:nS
    fprintf (fid, "%-10d %-10d %-10.4f", sizes(i), bench_results.edges(i), ...
             bench_results.build_times(i));
    for j = 1:nOp
      if (bench_results.succeeded(i, j))
        fprintf (fid, " %-12.4f", bench_results.times(i, j));
      elseif (strcmp (operations{j}, "plot") && sizes(i) > max_plot_size)
        fprintf (fid, " %-12s", "SKIP");
      else
        fprintf (fid, " %-12s", "FAIL");
      endif
    endfor
    fprintf (fid, "\n");
  endfor
  fclose (fid);
  if (verbose)
    printf ("Results written to %s\n", results_file);
  endif
endif

## ------------------------------------------------------------------
## BIST blocks (US-R03)
## ------------------------------------------------------------------
##
## The %!test blocks exercise the script against a small node count so
## the Octave test framework can run them as part of `make check'.  They
## locate graph-bench.m by probing pwd (same scheme the US-R02
## doc-examples.tst uses).

## Smoke test at tiny scale covering distances / shortestpath /
## centrality.
%!test <*US-R03>
%! sizes = [50];
%! operations = {"distances", "shortestpath", "centrality"};
%! verbose = false;
%! seed = 1;
%! avg_degree = 3;
%! max_plot_size = 1e4;
%! results_file = "";
%! p1 = fullfile (pwd (), "test", "graph-bench.m");
%! p2 = fullfile (pwd (), "graph-bench.m");
%! if (exist (p1, "file"))
%!   script = p1;
%! elseif (exist (p2, "file"))
%!   script = p2;
%! else
%!   error ("cannot locate graph-bench.m relative to pwd=%s", pwd ());
%! endif
%! source (script);
%! assert (exist ("bench_results", "var"));
%! assert (isstruct (bench_results));
%! assert (isfield (bench_results, "sizes"));
%! assert (isfield (bench_results, "operations"));
%! assert (isfield (bench_results, "times"));
%! assert (isfield (bench_results, "succeeded"));
%! assert (isfield (bench_results, "build_times"));
%! assert (isfield (bench_results, "edges"));
%! assert (isfield (bench_results, "octave_version"));
%! assert (isfield (bench_results, "date"));
%! assert (isequal (size (bench_results.times), [1, 3]));
%! assert (isequal (size (bench_results.succeeded), [1, 3]));
%! assert (all (bench_results.succeeded(:)));
%! assert (all (bench_results.times(:) >= 0));
%! assert (bench_results.edges(1) > 0);
%! assert (bench_results.edges(1) <= 150);

## Multiple sizes produce a rectangular timings matrix.
%!test <*US-R03>
%! sizes = [20, 50, 100];
%! operations = {"distances", "centrality"};
%! verbose = false;
%! seed = 7;
%! avg_degree = 2;
%! max_plot_size = 1e4;
%! results_file = "";
%! p1 = fullfile (pwd (), "test", "graph-bench.m");
%! p2 = fullfile (pwd (), "graph-bench.m");
%! if (exist (p1, "file"))
%!   script = p1;
%! elseif (exist (p2, "file"))
%!   script = p2;
%! else
%!   error ("cannot locate graph-bench.m relative to pwd=%s", pwd ());
%! endif
%! source (script);
%! assert (isequal (bench_results.sizes, [20, 50, 100]));
%! assert (isequal (size (bench_results.times), [3, 2]));
%! assert (all (bench_results.succeeded(:)));
%! assert (all (isfinite (bench_results.times(:))));
%! assert (all (bench_results.times(:) >= 0));

## Plot is skipped at sizes above max_plot_size.
%!test <*US-R03>
%! sizes = [50, 200];
%! operations = {"plot"};
%! verbose = false;
%! seed = 3;
%! avg_degree = 2;
%! max_plot_size = 100;
%! results_file = "";
%! p1 = fullfile (pwd (), "test", "graph-bench.m");
%! p2 = fullfile (pwd (), "graph-bench.m");
%! if (exist (p1, "file"))
%!   script = p1;
%! elseif (exist (p2, "file"))
%!   script = p2;
%! else
%!   error ("cannot locate graph-bench.m relative to pwd=%s", pwd ());
%! endif
%! source (script);
%! ## N=50 runs plot, N=200 is above max_plot_size and is skipped.
%! assert (isequal (size (bench_results.times), [2, 1]));
%! assert (bench_results.succeeded(1, 1), true);
%! assert (bench_results.succeeded(2, 1), false);
%! assert (isnan (bench_results.times(2, 1)));

## results_file path writes a readable text summary.
%!test <*US-R03>
%! sizes = [30];
%! operations = {"distances"};
%! verbose = false;
%! seed = 2;
%! avg_degree = 2;
%! max_plot_size = 1e4;
%! tmp = tempname ();
%! results_file = tmp;
%! p1 = fullfile (pwd (), "test", "graph-bench.m");
%! p2 = fullfile (pwd (), "graph-bench.m");
%! if (exist (p1, "file"))
%!   script = p1;
%! elseif (exist (p2, "file"))
%!   script = p2;
%! else
%!   error ("cannot locate graph-bench.m relative to pwd=%s", pwd ());
%! endif
%! unwind_protect
%!   source (script);
%!   assert (exist (tmp, "file"), 2);
%!   fid = fopen (tmp, "r");
%!   txt = fread (fid, Inf, "char=>char")';
%!   fclose (fid);
%!   assert (! isempty (txt));
%!   assert (! isempty (strfind (txt, "Octave graph/digraph performance benchmark")));
%!   assert (! isempty (strfind (txt, "distances")));
%!   assert (! isempty (strfind (txt, "30")));
%! unwind_protect_cleanup
%!   if (exist (tmp, "file"))
%!     unlink (tmp);
%!   endif
%! end_unwind_protect

## Reproducibility: same seed produces the same edge count.
%!test <*US-R03>
%! p1 = fullfile (pwd (), "test", "graph-bench.m");
%! p2 = fullfile (pwd (), "graph-bench.m");
%! if (exist (p1, "file"))
%!   script = p1;
%! elseif (exist (p2, "file"))
%!   script = p2;
%! else
%!   error ("cannot locate graph-bench.m relative to pwd=%s", pwd ());
%! endif
%! sizes = [40];
%! operations = {"distances"};
%! verbose = false;
%! seed = 99;
%! avg_degree = 3;
%! max_plot_size = 1e4;
%! results_file = "";
%! source (script);
%! e1 = bench_results.edges(1);
%! clear bench_results;
%! sizes = [40];
%! operations = {"distances"};
%! verbose = false;
%! seed = 99;
%! avg_degree = 3;
%! max_plot_size = 1e4;
%! results_file = "";
%! source (script);
%! e2 = bench_results.edges(1);
%! assert (e1, e2);

## Input validation: SIZES must be a positive-integer vector.
%!test <*US-R03>
%! p1 = fullfile (pwd (), "test", "graph-bench.m");
%! p2 = fullfile (pwd (), "graph-bench.m");
%! if (exist (p1, "file"))
%!   script = p1;
%! elseif (exist (p2, "file"))
%!   script = p2;
%! else
%!   error ("cannot locate graph-bench.m relative to pwd=%s", pwd ());
%! endif
%! sizes = [-1 2];
%! operations = {"distances"};
%! verbose = false;
%! seed = 0;
%! avg_degree = 2;
%! max_plot_size = 1e4;
%! results_file = "";
%! err_msg = "";
%! try
%!   source (script);
%! catch err
%!   err_msg = err.message;
%! end_try_catch
%! assert (! isempty (strfind (err_msg, "SIZES")));

## Input validation: OPERATIONS must be cellstr.
%!test <*US-R03>
%! p1 = fullfile (pwd (), "test", "graph-bench.m");
%! p2 = fullfile (pwd (), "graph-bench.m");
%! if (exist (p1, "file"))
%!   script = p1;
%! elseif (exist (p2, "file"))
%!   script = p2;
%! else
%!   error ("cannot locate graph-bench.m relative to pwd=%s", pwd ());
%! endif
%! sizes = [20];
%! operations = [1 2 3];
%! verbose = false;
%! seed = 0;
%! avg_degree = 2;
%! max_plot_size = 1e4;
%! results_file = "";
%! err_msg = "";
%! try
%!   source (script);
%! catch err
%!   err_msg = err.message;
%! end_try_catch
%! assert (! isempty (strfind (err_msg, "OPERATIONS")));
