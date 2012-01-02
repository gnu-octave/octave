## Copyright (C) 2007-2012 John W. Eaton
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
## @deftypefn {Function File} {} news (@var{package})
## Display the current NEWS file for Octave or installed package.
##
## If @var{package} is the name of an installed package, display the current
## NEWS file for that package.
## @end deftypefn

function news (package = "octave")

  if (ischar (package) && strcmpi (package, "octave"))
    octetcdir = octave_config_info ("octetcdir");
    newsfile  = fullfile (octetcdir, "NEWS");

  elseif (nargin == 1 && ischar (package))
    installed = pkg ("list");
    names     = cellfun (@(x) x.name, installed, "UniformOutput", false);
    ## we are nice and let the user use any case on the package name
    pos = strcmpi (names, package);
    if (!any (pos))
      error ("Package '%s' is not installed.", package);
    endif
    newsfile = fullfile (installed{pos}.dir, "packinfo", "NEWS");

  else
    print_usage;
  endif

  if (exist (newsfile, "file"))
    f = fopen (newsfile, "r");
    while (ischar (line = fgets (f)))
      puts (line);
    endwhile
  else
    if (strcmpi (package, "octave"))
      error ("news: unable to locate NEWS file");
    else
      error ("news: unable to locate NEWS file of %s package", package);
    endif
  endif

endfunction

## Remove from test statistics.  No real tests possible
%!assert (1)
