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
## @deftypefn  {} {} setpref ("@var{group}", "@var{pref}", @var{val})
## @deftypefnx {} {} setpref ("@var{group}", @{"@var{pref1}", "@var{pref2}", @dots{}@}, @{@var{val1}, @var{val2}, @dots{}@})
## Set the preference @var{pref} to the given @var{val} in the named preference
## group @var{group}.
##
## The named preference group must be a string.
##
## The preference @var{pref} may be a string or a cell array of strings.
##
## The corresponding value @var{val} may be any Octave value, .e.g., double,
## struct, cell array, object, etc.  Or, if @var{pref} is a cell array of
## strings then @var{val} must be a cell array of values with the same size as
## @var{pref}.
##
## If the named preference or group does not exist, it is added.
## @seealso{addpref, getpref, ispref, rmpref}
## @end deftypefn

function setpref (group, pref, val)

  if (nargin != 3)
    print_usage ();
  endif

  if (! ischar (group))
    error ("setpref: GROUP must be a string");
  endif
  if (! (ischar (pref) || iscellstr (pref)))
    error ("setpref: PREF must be a string or cellstr");
  endif

  prefs = loadprefs ();

  if (ischar (pref))
    prefs.(group).(pref) = val;
  else
    if (! size_equal (pref, val))
      error ("setpref: size mismatch for PREF and VAL");
    endif

    for i = 1:numel (pref)
      prefs.(group).(pref{i}) = val{i};
    endfor
  endif

  saveprefs (prefs);

endfunction


%!test
%! HOME = getenv ("HOME");
%! tmp_home = tempname ();
%! unwind_protect
%!   mkdir (tmp_home);
%!   setenv ("HOME", tmp_home);
%!
%!   setpref ("group1", "pref1", [1 2 3]);
%!   assert (getpref ("group1", "pref1"), [1 2 3]);
%!
%!   setpref ("group2", {"prefA", "prefB"}, {"StringA", {"StringB"}});
%!   assert (getpref ("group2", "prefA"), "StringA");
%!   assert (getpref ("group2", "prefB"), {"StringB"});
%!
%!   setpref ("group1", {"pref1", "pref2"}, {1, 2});
%!   assert (getpref ("group1", "pref1"), 1);
%!   assert (getpref ("group1", "pref2"), 2);
%!
%!   fail ('setpref ("group1", {"p1", "p2"}, 1)', ...
%!         "size mismatch for PREF and VAL");
%! unwind_protect_cleanup
%!   unlink (fullfile (tmp_home, ".octave_prefs"));
%!   sts = rmdir (tmp_home);
%!   if (isempty (HOME))
%!     unsetenv ("HOME");
%!   else
%!     setenv ("HOME", HOME);
%!   endif
%! end_unwind_protect

## Test input validation
%!error <Invalid call> setpref ()
%!error <Invalid call> setpref (1)
%!error <Invalid call> setpref (1,2)
%!error <GROUP must be a string> setpref (1, "pref1", 2)
%!error <PREF must be a string> setpref ("group1", 1, 2)
