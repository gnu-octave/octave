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
## @deftypefn  {Function File} {} news
## @deftypefnx {Function File} {} news (@var{package})
## Display the current NEWS file for Octave or an installed package.
##
## When called without an argument, display the NEWS file for Octave.
## When given a package name @var{package}, display the current NEWS file for
## that package.
## @end deftypefn

function news (package = "octave")

  if (nargin > 1)
    print_usage ();
  elseif (! ischar (package))
    error ("news: PACKAGE must be a string");
  endif

  if (strcmpi (package, "octave"))
    octetcdir = octave_config_info ("octetcdir");
    newsfile  = fullfile (octetcdir, "NEWS");
  else
    installed = pkg ("list");
    names     = cellfun (@(x) x.name, installed, "UniformOutput", false);
    ## we are nice and let the user use any case on the package name
    pos = strcmpi (names, package);
    if (!any (pos))
      error ("Package '%s' is not installed.", package);
    endif
    newsfile = fullfile (installed{pos}.dir, "packinfo", "NEWS");
  endif

  if (! exist (newsfile, "file"))
    if (strcmpi (package, "octave"))
      error ("news: unable to locate NEWS file");
    else
      error ("news: unable to locate NEWS file for package %s", package);
    endif
  endif

  fid = fopen (newsfile, "r");
  while (ischar (line = fgets (fid)))
    puts (line);
  endwhile
  fclose (fid);

endfunction


%!error news (1, 2)
%!error <PACKAGE must be a string> news (1)
%!error <Package .* is not installed> news ("__NOT_A_VALID_PKG_NAME__")
