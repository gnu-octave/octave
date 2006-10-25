## Copyright (C) 2006 Daniel Sebald
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
## Software Foundation, 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {@var{version} =} __gnuplot_version__ (@var{gplt_exe})
## Return the version of gnuplot we are using.  Note that we do not
## attempt to handle the case of the user switching to different
## versions of gnuplot during the same session.
## @end deftypefn

function version = __gnuplot_version__ ()

  persistent __version__ = "";

  if (isempty (__version__))
    [status, output] = system (sprintf ("%s --version", gnuplot_binary ()));
    pattern = "^[^\\s]*\\s*([0-9]+\\.[0-9]+)\\s*[^\\s]*\\s*([^\\s]*)";
    [d1, d2, d3, d4, matches] = regexp (output, pattern);
    if (iscell (matches) && numel (matches) > 0 && iscellstr (matches{1}))
      __version__ = matches{1}{1};
    endif
  endif

  version = __version__;

endfunction

