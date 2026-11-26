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
## @deftypefn  {} {@var{summary} =} run_doc_examples ()
## @deftypefnx {} {@var{summary} =} run_doc_examples (@var{examples_dir})
## @deftypefnx {} {@var{summary} =} run_doc_examples (@var{examples_dir}, @var{verbose})
## Run every @file{*.m} MATLAB-doc-example script in
## @var{examples_dir} and collect pass/fail results.
##
## This is the runner referenced by @file{test/graph/doc-examples.tst}
## under the US-R02 story of the @code{graph}/@code{digraph} parity
## effort.  Each file in @file{doc-examples/} is a MATLAB R2024a
## documentation example that has been lightly augmented with
## @code{assert} calls so that a wrong result fails noisily rather
## than passing silently.
##
## If @var{examples_dir} is omitted, the default is the @file{doc-examples/}
## directory alongside this runner.  @var{verbose} (default: @code{false})
## prints one line per example to @code{stdout} as it runs.
##
## The returned @var{summary} is a struct with fields:
## @table @code
## @item total
## Total number of @file{.m} files discovered.
## @item npass
## Number of examples that ran to completion without throwing.
## @item nfail
## Number of examples that threw.
## @item ran
## Cell array of base names (no extension) in discovery order.
## @item failures
## Cell array of strings @code{"<basename>: <message>"} for each
## failed example, empty when every example passed.
## @end table
##
## Each example is executed inside a private subfunction's workspace
## so its local variables are isolated from the caller and from other
## examples.
##
## @seealso{run, assert, digraph, graph}
## @end deftypefn

function summary = run_doc_examples (examples_dir, verbose)

  if (nargin < 1 || isempty (examples_dir))
    examples_dir = fullfile (fileparts (mfilename ("fullpath")), ...
                             "doc-examples");
  endif
  if (nargin < 2 || isempty (verbose))
    verbose = false;
  endif

  if (! ischar (examples_dir))
    error ("run_doc_examples: EXAMPLES_DIR must be a string");
  endif
  if (! exist (examples_dir, "dir"))
    error ("run_doc_examples: directory not found: %s", examples_dir);
  endif

  files = dir (fullfile (examples_dir, "*.m"));
  ## Stable alphabetic order for reproducibility.
  [~, order] = sort ({files.name});
  files = files(order);

  total = numel (files);
  npass = 0;
  nfail = 0;
  ran = cell (1, total);
  failures = {};

  for k = 1:total
    name = files(k).name;
    base = name(1:end-2);           # strip ".m"
    full = fullfile (examples_dir, name);
    ran{k} = base;
    try
      __run_one_example__ (full);
      npass += 1;
      if (verbose)
        printf ("  ok    %s\n", base);
      endif
    catch err
      nfail += 1;
      failures{end+1} = sprintf ("%s: %s", base, err.message);
      if (verbose)
        printf ("  FAIL  %s: %s\n", base, err.message);
      endif
    end_try_catch
  endfor

  summary = struct ("total", total, ...
                    "npass", npass, ...
                    "nfail", nfail, ...
                    "ran", {ran}, ...
                    "failures", {failures});

endfunction

function __run_one_example__ (path)
  ## Run the script in this subfunction's workspace so each example is
  ## isolated from the caller and from its siblings.
  run (path);
endfunction

%!test
%! ## The runner discovers at least 15 examples from the default dir.
%! summary = run_doc_examples ();
%! assert (isstruct (summary));
%! assert (summary.total >= 15);
%! assert (summary.npass + summary.nfail == summary.total);

%!test
%! ## The "failures" list has an entry for every failure and no entry
%! ## for any pass.
%! summary = run_doc_examples ();
%! assert (numel (summary.failures), summary.nfail);

%!test
%! ## Passing a non-existent directory throws.
%! fail ("run_doc_examples ('/no/such/dir/exists')", ...
%!       "directory not found");

%!error <EXAMPLES_DIR must be a string> run_doc_examples (42)
