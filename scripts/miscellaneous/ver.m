## Copyright (C) 2005-2012 William Poetra Yoga Hadisoeseno
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
## @deftypefn {Function File} {} ver ()
## Display a header containing the current Octave version number, license
## string and operating system, followed by the installed package names,
## versions, and installation directories.
##
## @deftypefnx {Function File} {v =} ver ()
## Return a vector of structures, respecting Octave and each installed package.
## The structure includes the following fields.
##
## @table @code
## @item Name
## Package name.
##
## @item Version
## Version of the package.
##
## @item Revision
## Revision of the package.
##
## @item Date
## Date respecting the version/revision.
## @end table
##
## @deftypefnx {Function File} {v =} ver ("Octave")
## Return version information for Octave only.
##
## @deftypefnx {Function File} {v =} ver (@var{package})
## Return version information for @var{package}.
##
## @seealso{version, octave_config_info}
## @end deftypefn

## Author: William Poetra Yoga Hadisoeseno <williampoetra@gmail.com>

function varargout = ver (package = "")

  if (nargin > 1)
    print_usage ();
  endif

  ## Start with the version info for Octave
  ret = struct ("Name", "Octave", "Version", version,
                "Release", [], "Date", []);

  ## Add the installed packages
  lst = pkg ("list");
  for i = 1:length (lst)
    ret(end+1) = struct ("Name", lst{i}.name, "Version", lst{i}.version,
                         "Release", [], "Date", lst{i}.date);
  endfor

  if (nargout == 0)
    octave_license = license ();

    [unm, status] = uname ();

    if (status < 0)
      os_string = "unknown";
    else
      os_string = sprintf ("%s %s %s %s", unm.sysname, unm.release,
                           unm.version, unm.machine);
    endif

    hbar(1:70) = "-";
    ver_line1 = "GNU Octave Version ";
    ver_line2 = "GNU Octave License: ";
    ver_line3 = "Operating System: ";

    ver_desc = sprintf ("%s\n%s%s\n%s%s\n%s%s\n%s\n", hbar, ver_line1, version,
                        ver_line2, octave_license, ver_line3, os_string, hbar);

    puts (ver_desc);

    pkg ("list");
  else
    if (! isempty (package))
      n = [];
      for r = 1:numel(ret)
        if (strcmpi (ret(r).Name, package))
          n = r;
          break;
        endif
      endfor
      ret = ret(n);
    endif
    varargout{1} = ret;
  endif

endfunction

%!test
%! result = ver;
%! assert (result(1).Name, "Octave")
%! assert (result(1).Version, version)
%! result = ver ("octave");
%! assert (result(1).Name, "Octave")
%! assert (result(1).Version, version)

%!test
%! lst = pkg ("list");
%! for n=1:numel(lst)
%!   expected = lst{n}.name;
%!   result = ver (expected);
%!   assert (result.Name, expected);
%!   assert (isfield (result, "Version"), true);
%!   assert (isfield (result, "Release"), true);
%!   assert (isfield (result, "Date"), true);
%! endfor

