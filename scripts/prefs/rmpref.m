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
## @deftypefn  {} {} rmpref ("@var{group}", "@var{pref}")
## @deftypefnx {} {} rmpref ("@var{group}", @{"@var{pref1}", "@var{pref2}", @dots{}@})
## @deftypefnx {} {} rmpref ("@var{group}")
## Remove the named preference @var{pref} from the preference group
## @var{group}.
##
## The named preference group must be a string.
##
## The preference @var{pref} may be a string or cell array of strings.
##
## If @var{pref} is not specified, remove the preference group @var{group}.
##
## It is an error to remove a nonexistent preference or group.
## @seealso{addpref, ispref, setpref, getpref}
## @end deftypefn

function rmpref (group, pref)

  if (nargin < 1)
    print_usage ();
  elseif (! ischar (group))
    error ("rmpref: GROUP must be a string");
  elseif (nargin == 2 && ! (ischar (pref) || iscellstr (pref)))
    error ("rmpref: PREF must be a string or cell array of strings");
  endif

  if (nargin == 1)
    if (! ispref (group))
      error ("rmpref: GROUP %s does not exist", group);
    endif
    prefs = loadprefs ();
    prefs = rmfield (prefs, group);
    saveprefs (prefs);
  else
    valid = ispref (group, pref);
    if (all (valid))
      prefs = loadprefs ();
      prefs.(group) = rmfield (prefs.(group), pref);
      saveprefs (prefs);
    else
      if (! ispref (group))
        error ("rmpref: GROUP %s does not exist", group);
      elseif (ischar (pref))
        error ("rmpref: preference %s does not exist", pref);
      else
        idx = find (! valid, 1);
        error ("rmpref: preference %s does not exist", pref{idx});
      endif
    endif
  endif

endfunction


%!test
%! HOME = getenv ("HOME");
%! save_default_options ("-binary", "local");
%! unwind_protect
%!   setenv ("HOME", P_tmpdir ());
%!   addpref ("group1", "pref1", [1 2 3]);
%!   addpref ("group2", {"prefA", "prefB", "prefC"}, {"strA", "strB", "strC"});
%!
%!   assert (ispref ("group1"));
%!   rmpref ("group1");
%!   assert (! ispref ("group1"));
%!
%!   assert (ispref ("group2", "prefB"));
%!   rmpref ("group2", "prefB");
%!   assert (! ispref ("group2", "prefB"));
%!
%!   fail ('rmpref ("group3")', ...
%!         "GROUP group3 does not exist");
%!   fail ('rmpref ("group3", "prefA")', ...
%!         "GROUP group3 does not exist");
%!   fail ('rmpref ("group2", "prefB")',
%!         "preference prefB does not exist");
%!   fail ('rmpref ("group2", {"prefA", "prefB"})',
%!         "preference prefB does not exist");
%!
%! unwind_protect_cleanup
%!   unlink (fullfile (P_tmpdir (), ".octave_prefs"));
%!   if (isempty (HOME))
%!     unsetenv ("HOME");
%!   else
%!     setenv ("HOME", HOME);
%!   endif
%! end_unwind_protect

## Test input validation
%!error <Invalid call> rmpref ()
%!error <GROUP must be a string> rmpref (1)
%!error <PREF must be a string> rmpref ("group1", 1)
