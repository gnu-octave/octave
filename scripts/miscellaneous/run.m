## Copyright (C) 2007  David Bateman
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} run (@var{f})
## @deftypefnx {Command} {} run @var{f}
## Run scripts in the current workspace that are not necessarily on the
## path. If @var{f} is the script to run, including its path, then @code{run}
## change the directory to the directory where @var{f} is found. @code{run}
## then executes the script, and returns to the original directory.
## @seealso{system}
## @end deftypefn

## PKG_ADD: mark_as_commnd run

function run (s)
  [d, f, ext] = fileparts (s);
  if (! isempty (d))
    if (exist (d, "dir"))
      wd = pwd ();
      unwind_protect
	cd (d);
	if (! exist (s, "file") ||  ! strcmp (ext, ".m"))
	  error ("run: file must exist and be a valid Octave script file");
	endif
	evalin ("caller", [f, ";"], "rethrow (lasterror ())");
      unwind_protect_cleanup
	cd (wd);
      end_unwind_protect
    else
      error ("run: the path %s doesn't exist", d);
    endif
  else
    if (exist (script, "file"))
      evalin ("caller", [script, ";"], "rethrow (lasterror ())");
    else
      error ("run: %s not found", s);
    endif
  endif
endfunction
