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
## @deftypefn  {} {@var{ref} =} __matlab_ref__ (@var{name})
## @deftypefnx {} {@var{ref} =} __matlab_ref__ (@var{name}, @var{fixture_dir})
## Load a MATLAB reference-output fixture for use in a @code{graph} or
## @code{digraph} BIST block.
##
## @var{name} is the base name (without @file{.json} extension) of a
## fixture file captured from MATLAB.  The fixture is read from
## @var{fixture_dir} (default: the sibling MatSlop tree's
## @file{tasks/matlab-reference/} directory).  The return value is a
## struct whose fields match the keys of the JSON object:
##
## @example
## @group
## ref = __matlab_ref__ ("numnodes");
## G = digraph (ref.input.s, ref.input.t);
## assert (numnodes (G), ref.expected);
## @end group
## @end example
##
## If the fixture is missing, the function returns an empty struct and
## prints a warning so the test can be skipped rather than failing
## spuriously when run outside the MatSlop development tree.
##
## @seealso{jsondecode, assert}
## @end deftypefn

function ref = __matlab_ref__ (name, fixture_dir)

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  if (! ischar (name) || isempty (name))
    error ("__matlab_ref__: NAME must be a non-empty string");
  endif

  if (nargin < 2)
    ## Default search path: sibling MatSlop repo's tasks/matlab-reference/.
    ## The Octave fork lives at .../projects/octave, MatSlop at
    ## .../projects/MatSlop — the fixture dir is two levels up then into
    ## MatSlop/tasks/matlab-reference.
    here = fileparts (mfilename ("fullpath"));
    octave_root = fullfile (here, "..", "..", "..");
    fixture_dir = fullfile (octave_root, "..", "MatSlop", ...
                            "tasks", "matlab-reference");
  endif

  fpath = fullfile (fixture_dir, [name ".json"]);
  if (! exist (fpath, "file"))
    warning ("Octave:missing-matlab-fixture", ...
             "__matlab_ref__: fixture '%s' not found at %s", name, fpath);
    ref = struct ();
    return;
  endif

  fid = fopen (fpath, "r");
  if (fid < 0)
    error ("__matlab_ref__: cannot open %s", fpath);
  endif
  raw = fread (fid, Inf, "uint8=>char")';
  fclose (fid);

  ref = jsondecode (raw);

endfunction

%!test
%! ## When the fixture is missing, return empty struct with a warning.
%! warning ("off", "Octave:missing-matlab-fixture", "local");
%! ref = __matlab_ref__ ("this-fixture-does-not-exist");
%! assert (isstruct (ref));
%! assert (isempty (fieldnames (ref)));

%!test
%! ## Round-trip a fixture we write inline.
%! tmpdir = tempname ();
%! mkdir (tmpdir);
%! unwind_protect
%!   fid = fopen (fullfile (tmpdir, "foo.json"), "w");
%!   fwrite (fid, '{"method":"foo","expected":42}');
%!   fclose (fid);
%!   ref = __matlab_ref__ ("foo", tmpdir);
%!   assert (ref.method, "foo");
%!   assert (ref.expected, 42);
%! unwind_protect_cleanup
%!   confirm_recursive_rmdir (false, "local");
%!   rmdir (tmpdir, "s");
%! end_unwind_protect

%!error <NAME must be a non-empty string> __matlab_ref__ ("")
%!error <NAME must be a non-empty string> __matlab_ref__ (42)
%!error <Invalid call> __matlab_ref__ ()
