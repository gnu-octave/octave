## Copyright (C) 2008 Ben Abbott
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
## @deftypefn {Function File} {@var{val} =} pathdef ()
## Return the default path for Octave.
## The path information is extracted from one of three sources.
## In order of preference, those are;
##
## @enumerate
## @item @file{~/.octaverc}
## @item @file{<octave-home>/.../<version>/m/startup/octaverc}
## @item Octave's path prior to changes by any octaverc.
## @end enumerate
## @seealso{path, addpath, rmpath, genpath, savepath, pathsep}
## @end deftypefn

function val = pathdef ()

  ## Use Octave's orignal path as the default default.
  val = __pathorig__ ();

  ## Locate the site octaverc file.
  pathdir = octave_config_info ("localstartupfiledir");
  site_octaverc = fullfile (pathdir, "octaverc");

  ## locate the user ~\.octaverc file.
  user_octaverc = fullfile ("~", ".octaverc");

  ## Extract the specified paths from the site and user octaverc"s.
  site_pathscript = __extractpath__ (site_octaverc);
  if (exist (user_octaverc, "file"))
    user_pathscript = __extractpath__ (user_octaverc);
  else
    user_pathscript = "";
  endif

  ## A path definition in the user octaverc has precedence over the
  ## site.

  ## FIXME -- use a subfunction here to avoid code duplication?

  if (numel (user_pathscript))
    try
      if (numel (user_pathscript) == 1)
        n = strfind (user_pathscript{1}, "'");
        if (numel(n) == 1)
          n = strfind (user_pathscript{1}, "\"");
        endif
        val = user_pathscript{1}(n(1):n(end));
      else
        presentpath = path;
        eval (user_pathscript);
        val = path;
        path (presentpath);
      endif
    catch
      warning ("pathdef: invalid path found in `%s'", user_octaverc);
    end_try_catch
  elseif (numel (site_pathscript))
    try
      if (numel (site_pathscript) == 1)
        n = strfind (site_pathscript{1}, "'");
        if (numel(n) == 1)
          n = strfind (site_pathscript{1}, "\"");
        endif
        val = site_pathscript{1}(n(1):n(end));
      else
        presentpath = path;
        eval (site_pathscript);
        val = path;
        path (presentpath);
      endif
    catch
      warning ("pathdef: invalid path found in `%s'", site_octaverc);
    end_try_catch
  endif

endfunction
