## Copyright (C) 2006, 2007 Daniel Sebald
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

## Undocumented internal function.

## Return the version of gnuplot we are using.  Note that we do not
## attempt to handle the case of the user switching to different
## versions of gnuplot during the same session.

function version = __gnuplot_version__ ()

  persistent __version__ = "";

  if (isempty (__version__))
    [status, output] = system (sprintf ("%s --version", gnuplot_binary ()));
    if (status != 0)
      error ("you must have gnuplot installed to display graphics; if you have gnuplot installed in a non-standard location, see the 'gnuplot_binary' function");
    endif
    pattern = "^[^\\s]*\\s*([0-9]+\\.[0-9]+)\\s*[^\\s]*\\s*([^\\s]*)";
    [d1, d2, d3, d4, matches] = regexp (output, pattern);
    if (iscell (matches) && numel (matches) > 0 && iscellstr (matches{1}))
      __version__ = matches{1}{1};
    endif
  endif

  version = __version__;

endfunction

