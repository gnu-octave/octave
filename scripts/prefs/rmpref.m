## Copyright (C) 2012 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {Function File} {} rmpref (@var{group}, @var{pref})
## Remove the named preference @var{pref} from the preference group
## @var{group}.
##
## The named preference group must be a character string.
##
## The preference @var{pref} may be a character string or a cell array
## of character strings.
##
## If @var{pref} is not specified, remove the preference group
## @var{group}.
##
## It is an error to remove a nonexistent preference or group.
## @seealso{addpref, ispref, setpref, getpref}
## @end deftypefn

## Author: jwe

function retval = rmpref (group, pref)

  prefs = loadprefs ();

  if (nargin == 1)
    if (ischar (group))
      retval = isfield (prefs, group);
    else
      error ("expecting group to be a character array");
    endif
  elseif (nargin == 2)
    grp = getpref (group, pref);
    if (ischar (pref) || iscellstr (pref))
      retval = isfield (grp, pref);
    endif
  else
    print_usage ();
  endif

endfunction

%% Testing these functions will require some care to avoid wiping out
%% existing (or creating unwanted) preferences for the user running the
%% tests.
