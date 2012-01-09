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
## @deftypefn {Function File} {} prefsfile ()
## Undocumented internal function.
## @end deftypefn

## Author: jwe

function retval = prefsfile ()

  retval = "~/.octave_prefs";

  ## Transition users to new filename if necessary
  ## FIXME: Delete before 3.6.0 release
  oldname = tilde_expand ("~/.octave-prefs");
  if (exist (oldname, "file"))
    newname = tilde_expand (retval); 
    if (exist (newname, "file"))
      error (["Octave uses the file ~/.octave_prefs to store preferences.\n",...
              "       The old file name was ~/.octave-prefs.\n",...
              "       Both files exist."...
              "  User must manually delete one of the files.\n"]);
    endif
    status = movefile (oldname, newname);
    if (! status)
      error (["Octave uses the file ~/.octave_prefs to store preferences.\n",
             "        The old file name was ~/.octave-prefs.\n",
             "        User must manually rename the old file to the new name.\n"]);
    endif
  endif

endfunction

%% Testing these functions will require some care to avoid wiping out
%% existing (or creating unwanted) preferences for the user running the
%% tests.
