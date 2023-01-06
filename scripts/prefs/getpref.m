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
## @deftypefn  {} {@var{val} =} getpref ("@var{group}", "@var{pref}")
## @deftypefnx {} {@var{val} =} getpref ("@var{group}", "@var{pref}", @var{default})
## @deftypefnx {} {@{@var{val1}, @var{val2}, @dots{}@} =} getpref ("@var{group}", @{"@var{pref1}", "@var{pref2"}, @dots{}@})
## @deftypefnx {} {@var{prefstruct} =} getpref ("@var{group}")
## @deftypefnx {} {@var{prefstruct} =} getpref ()
## Return the preference value corresponding to the named preference @var{pref}
## in the preference group @var{group}.
##
## The named preference group must be a string.
##
## If @var{pref} does not exist in @var{group} and @var{default} is specified,
## create the preference with value @var{default} and return @var{default}.
##
## The preference @var{pref} may be a string or cell array of strings.  If it
## is a cell array of strings then a cell array of preferences is returned.
##
## The corresponding default value @var{default} may be any Octave value,
## .e.g., double, struct, cell array, object, etc.  Or, if @var{pref} is a cell
## array of strings then @var{default} must be a cell array of values with the
## same size as @var{pref}.
##
## If neither @var{pref} nor @var{default} are specified, return a structure
## of preferences for the preference group @var{group}.
##
## If no arguments are specified, return a structure containing all groups of
## preferences and their values.
## @seealso{addpref, setpref, ispref, rmpref}
## @end deftypefn

function retval = getpref (group, pref, default)

  if (nargin == 0)
    retval = loadprefs ();
  elseif (nargin == 1)
    if (! ischar (group))
      error ("getpref: GROUP must be a string");
    endif
    prefs = loadprefs ();
    if (isfield (prefs, group))
      retval = prefs.(group);
    else
      ## FIXME: Is this the right behavior, or should it produce an error?
      retval = [];
    endif
  else
    if (! (ischar (pref) || iscellstr (pref)))
      error ("getpref: PREF must be a string or cellstr");
    endif

    grp = getpref (group);

    if (ischar (pref))
      if (isfield (grp, pref))
        retval = grp.(pref);
      elseif (nargin == 3)
        addpref (group, pref, default);
        retval = default;
      else
        error ("getpref: preference %s does not exist in GROUP %s",
               pref, group);
      endif
    else
      if (nargin != 2 && ! size_equal (pref, default))
        error ("getpref: size mismatch for PREF and DEFAULT");
      endif

      for i = 1:numel (pref)
        if (isfield (grp, pref{i}))
          retval{i} = grp.(pref{i});
        elseif (nargin == 3)
          addpref (group, pref{i}, default{i});
          retval{i} = default{i};
        else
          error ("getpref: preference %s does not exist in GROUP %s",
                 pref{i}, group);
        endif
      endfor
    endif
  endif

endfunction


%!test
%! HOME = getenv ("HOME");
%! tmp_home = tempname ();
%! save_default_options ("-binary", "local");
%! unwind_protect
%!   mkdir (tmp_home);
%!   setenv ("HOME", tmp_home);
%!
%!   addpref ("group1", "pref1", [1 2 3]);
%!   addpref ("group2", {"prefA", "prefB"}, {"StringA", {"StringB"}});
%!
%!   exp.group1.pref1 = [1 2 3];
%!   exp.group2.prefA = "StringA";
%!   exp.group2.prefB = {"StringB"};
%!   obs = getpref ();
%!   assert (obs, exp);
%!
%!   assert (getpref ("group1"), exp.group1);
%!   assert (getpref ("group2"), exp.group2);
%!   assert (getpref ("group3"), []);
%!
%!   assert (getpref ("group1", "pref1"), [1 2 3]);
%!   assert (getpref ("group2", "prefA"), "StringA");
%!   assert (getpref ("group2", "prefB"), {"StringB"});
%!   assert (getpref ("group1", "pref2", "New_Value"), "New_Value");
%!   assert (getpref ("group1", "pref2"), "New_Value");
%!   fail ('getpref ("group1", "no_such_pref")', ...
%!         "preference no_such_pref does not exist in GROUP group1");
%!
%!   assert (getpref ("group2", {"prefA", "prefB"}), {"StringA", {"StringB"}});
%!   assert (getpref ("group2", {"prefA", "prefC"}, {1, "StringC"}),
%!           {"StringA", "StringC"});
%!   assert (getpref ("group2", "prefC"), "StringC");
%!   fail ('getpref ("group1", {"p1", "p2"}, 1)', ...
%!         "size mismatch for PREF and DEFAULT");
%!   fail ('getpref ("group2", {"prefA", "prefD"})',
%!         "preference prefD does not exist in GROUP group2");
%!
%! unwind_protect_cleanup
%!   unlink (fullfile (tmp_home, ".octave_prefs"));
%!   sts = rmdir (tmp_home);
%!   if (isempty (HOME))
%!     unsetenv ("HOME");
%!   else
%!     setenv ("HOME", HOME);
%!   endif
%! end_unwind_protect

%!error <GROUP must be a string> getpref (1)
%!error <PREF must be a string> getpref ("group1", 1, 2)
