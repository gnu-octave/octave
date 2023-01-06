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
## @deftypefn  {} {@var{tf} =} ispref ("@var{group}", "@var{pref}")
## @deftypefnx {} {@var{tf} =} ispref ("@var{group}", @{"@var{pref1}", "@var{pref2"}, @dots{}@})
## @deftypefnx {} {@var{tf} =} ispref ("@var{group}")
## Return true if the named preference @var{pref} exists in the preference
## group @var{group}.
##
## The named preference group must be a string.
##
## The preference @var{pref} may be a string or a cell array of strings.
##
## If @var{pref} is not specified, return true if the preference group
## @var{group} exists.
## @seealso{getpref, addpref, setpref, rmpref}
## @end deftypefn

function tf = ispref (group, pref = "")

  if (nargin == 0)
    print_usage ();
  endif

  if (! ischar (group))
    error ("ispref: GROUP must be a string");
  endif
  if (! (ischar (pref) || iscellstr (pref)))
    error ("ispref: PREF must be a string or cellstr");
  endif

  if (nargin == 1)
    tf = isfield (loadprefs (), group);
  else
    prefs = loadprefs ();
    if (isfield (prefs, group))
      tf = isfield (prefs.(group), pref);
    else
      if (ischar (pref))
        tf = false;
      else
        tf = false (size (pref));
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
%!   addpref ("group2", {"prefA", "prefB"}, {"StringA", {"StringB"}});
%!
%!   assert (ispref ("group1"));
%!   assert (! ispref ("group3"));
%!
%!   assert (ispref ("group2", "prefB"));
%!   assert (! ispref ("group2", "prefC"));
%!
%!   assert (ispref ("group2", {"prefB", "prefC"}), [true, false]);
%!
%!   assert (ispref ("group3", "prefB"), false);
%!   assert (ispref ("group3", {"prefB", "prefC"}), [false, false]);
%!
%! unwind_protect_cleanup
%!   unlink (fullfile (P_tmpdir (), ".octave_prefs"));
%!   if (isempty (HOME))
%!     unsetenv ("HOME");
%!   else
%!     setenv ("HOME", HOME);
%!   endif
%! end_unwind_protect

%!error <Invalid call> ispref ()
%!error <GROUP must be a string> ispref (1, "pref1")
%!error <PREF must be a string> ispref ("group1", 1)
