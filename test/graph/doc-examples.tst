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

## MATLAB R2024a doc-example suite for graph/digraph.  US-R02.
##
## Each .m file under ./doc-examples/ is a self-contained reproduction
## of an example from a MATLAB R2024a documentation page for graph,
## digraph, or one of their methods.  Each example ends with at least
## one assert() so that a wrongly-produced result surfaces as a failure
## rather than a silent pass.
##
## The runner run_doc_examples.m lives alongside this file (one level
## above doc-examples/).  It walks the directory, evals every script
## in its own isolated subfunction workspace, and reports a pass/fail
## summary plus per-file diagnostics.
##
## Each test block probes a couple of candidate directories relative
## to pwd() so it works when run via `test test/graph/doc-examples.tst`
## from the source root or via __run_test_suite__ from make check.

## Helper that returns the path to test/graph/ (containing
## run_doc_examples.m).  Uses a static local variable so the probing
## only happens once per test-suite invocation.
%!test <*US-R02>
%! p1 = fullfile (pwd (), "test", "graph");
%! p2 = fullfile (pwd (), "graph");
%! p3 = pwd ();
%! if (exist (fullfile (p1, "run_doc_examples.m"), "file"))
%!   d = p1;
%! elseif (exist (fullfile (p2, "run_doc_examples.m"), "file"))
%!   d = p2;
%! elseif (exist (fullfile (p3, "run_doc_examples.m"), "file"))
%!   d = p3;
%! else
%!   error ("cannot locate test/graph relative to pwd=%s", pwd ());
%! endif
%! assert (exist (fullfile (d, "run_doc_examples.m"), "file"), 2);

## The doc-examples/ directory has at least 15 example files.
%!test <*US-R02>
%! p1 = fullfile (pwd (), "test", "graph");
%! p2 = fullfile (pwd (), "graph");
%! p3 = pwd ();
%! if (exist (fullfile (p1, "run_doc_examples.m"), "file"))
%!   d = p1;
%! elseif (exist (fullfile (p2, "run_doc_examples.m"), "file"))
%!   d = p2;
%! else
%!   d = p3;
%! endif
%! examples_dir = fullfile (d, "doc-examples");
%! assert (exist (examples_dir, "dir"), 7);
%! files = dir (fullfile (examples_dir, "*.m"));
%! assert (numel (files) >= 15, ...
%!         "doc-examples must have >= 15 example files, found %d", ...
%!         numel (files));

## The runner returns a summary struct and every example passes.
%!test <*US-R02>
%! p1 = fullfile (pwd (), "test", "graph");
%! p2 = fullfile (pwd (), "graph");
%! p3 = pwd ();
%! if (exist (fullfile (p1, "run_doc_examples.m"), "file"))
%!   d = p1;
%! elseif (exist (fullfile (p2, "run_doc_examples.m"), "file"))
%!   d = p2;
%! else
%!   d = p3;
%! endif
%! addpath (d);
%! unwind_protect
%!   summary = run_doc_examples ();
%! unwind_protect_cleanup
%!   rmpath (d);
%! end_unwind_protect
%! assert (isstruct (summary));
%! assert (isfield (summary, "total"));
%! assert (isfield (summary, "npass"));
%! assert (isfield (summary, "nfail"));
%! assert (isfield (summary, "failures"));
%! assert (summary.total >= 15);
%! assert (summary.npass == summary.total, ...
%!         "doc-examples failures: %s", ...
%!         strjoin (summary.failures, "; "));
%! assert (summary.nfail == 0);
%! assert (iscell (summary.failures));
%! assert (isempty (summary.failures));

## Smoke-test: digraph_numeric_ids is discovered and run.
%!test <*US-R02>
%! p1 = fullfile (pwd (), "test", "graph");
%! p2 = fullfile (pwd (), "graph");
%! p3 = pwd ();
%! if (exist (fullfile (p1, "run_doc_examples.m"), "file"))
%!   d = p1;
%! elseif (exist (fullfile (p2, "run_doc_examples.m"), "file"))
%!   d = p2;
%! else
%!   d = p3;
%! endif
%! addpath (d);
%! unwind_protect
%!   summary = run_doc_examples ();
%!   assert (any (strcmp (summary.ran, "digraph_numeric_ids")), ...
%!           "expected digraph_numeric_ids in ran list, got: %s", ...
%!           strjoin (summary.ran, ", "));
%! unwind_protect_cleanup
%!   rmpath (d);
%! end_unwind_protect
