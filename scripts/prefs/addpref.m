########################################################################
##
## Copyright (C) 2012-2023 The Octave Project Developers
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
## @deftypefn  {} {} addpref ("@var{group}", "@var{pref}", @var{val})
## @deftypefnx {} {} addpref ("@var{group}", @{"@var{pref1}", "@var{pref2}", @dots{}@}, @{@var{val1}, @var{val2}, @dots{}@})
## Add the preference @var{pref} and associated value @var{val} to the named
## preference group @var{group}.
##
## The named preference group must be a string.
##
## The preference @var{pref} may be a string or a cell array of strings.  An
## error will be issued if the preference already exists.
##
## The corresponding value @var{val} may be any Octave value, .e.g., double,
## struct, cell array, object, etc.  Or, if @var{pref} is a cell array of
## strings then @var{val} must be a cell array of values with the same size as
## @var{pref}.
## @seealso{setpref, getpref, ispref, rmpref}
## @end deftypefn

function addpref (group, pref, val)

  if (nargin != 3)
    print_usage ();
  endif

  if (! ischar (group))
    error ("addpref: GROUP must be a string");
  elseif (! (ischar (pref) || iscellstr (pref)))
    error ("addpref: PREF must be a string or cellstr");
  endif

  prefs = loadprefs ();

  if (ischar (pref))
    if (isfield (prefs, group) && isfield (prefs.(group), pref))
      error ("addpref: preference %s already exists in GROUP %s", pref, group);
    else
      prefs.(group).(pref) = val;
    endif
  else
    if (! size_equal (pref, val))
      error ("addpref: size mismatch for PREF and VAL");
    endif
    for i = 1:numel (pref)
      if (isfield (prefs, group) && isfield (prefs.(group), pref{i}))
        error ("addpref: preference %s already exists in GROUP %s",
               pref{i}, group);
      else
        prefs.(group).(pref{i}) = val{i};
      endif
    endfor
  endif

  saveprefs (prefs);

endfunction


%!test
%! HOME = getenv ("HOME");
%! tmpdir = tempname ();
%! save_default_options ("-binary", "local");
%! unwind_protect
%!   mkdir (tmpdir);
%!   setenv ("HOME", tmpdir);
%!
%!   addpref ("group1", "pref1", [1 2 3]);
%!   assert (getpref ("group1", "pref1"), [1 2 3]);
%!
%!   addpref ("group2", {"prefA", "prefB"}, {"StringA", {"StringB"}});
%!   assert (getpref ("group2", "prefA"), "StringA");
%!   assert (getpref ("group2", "prefB"), {"StringB"});
%!
%!   fail ('addpref ("group1", "pref1", 4)', ...
%!         "preference pref1 already exists in GROUP group1");
%!   fail ('setpref ("group1", {"p1", "p2"}, 1)', ...
%!         "size mismatch for PREF and VAL");
%!   fail ('addpref ("group2", {"prefC", "prefA"}, {1, 2})',
%!         "preference prefA already exists in GROUP group2");
%!
%! unwind_protect_cleanup
%!   unlink (fullfile (tmpdir, ".octave_prefs"));
%!   rmdir (tmpdir);
%!   if (isempty (HOME))
%!     unsetenv ("HOME");
%!   else
%!     setenv ("HOME", HOME);
%!   endif
%! end_unwind_protect

## Test input validation
%!error <Invalid call> addpref ()
%!error <Invalid call> addpref (1)
%!error <Invalid call> addpref (1,2)
%!error <GROUP must be a string> addpref (1, "pref1", 2)
%!error <PREF must be a string> addpref ("group1", 1, 2)
