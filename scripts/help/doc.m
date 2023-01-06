########################################################################
##
## Copyright (C) 2005-2023 The Octave Project Developers
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
## @deftypefn  {} {} doc @var{function_name}
## @deftypefnx {} {} doc
## Display documentation for the function @var{function_name} directly from an
## online version of the printed manual, using the GNU Info browser.
##
## If invoked without an argument, the manual is shown from the beginning.
##
## For example, the command @kbd{doc rand} starts the GNU Info browser at the
## @code{rand} node in the online version of the manual.
##
## Once the GNU Info browser is running, help for using it is available using
## the command @kbd{C-h}.
## @seealso{help}
## @end deftypefn

function retval = doc (function_name)

  if (nargin == 1)
    if (! ischar (function_name))
      error ("doc: FUNCTION_NAME must be a string");
    endif
    ftype = exist (function_name);
  else
    function_name = "";
    ftype = 0;
  endif

  ## Give event manager the first shot.

  status = ! __event_manager_show_documentation__ (function_name);

  if (status)

    if (ftype == 2 || ftype == 3)
      ffile = which (function_name);
    else
      ffile = "";
    endif

    if (isempty (ffile))
      info_dir = __octave_config_info__ ("infodir");
    else
      info_dir = fileparts (ffile);
    endif

    ## Determine if a file called doc.info exist in the same
    ## directory as the function.
    info_file_name = fullfile (info_dir, "doc.info");

    [~, err] = stat (info_file_name);

    if (err < 0)
      info_file_name = info_file ();

      if (! exist (info_file_name, "file")
          && ! exist ([info_file_name ".gz"], "file")
          && ! exist ([info_file_name ".bz2"], "file"))
        __gripe_missing_component__ ("doc", "info-file");
      endif
    endif

    ## FIXME: Don't change the order of the arguments below because
    ## the info-emacs-info script currently expects --directory DIR as
    ## the third and fourth arguments.  Someone should fix that.
    cmd = sprintf ('"%s" --file "%s" --directory "%s"',
                   info_program (), info_file_name, info_dir);

    have_fname = ! isempty (function_name);

    if (have_fname)
      status = system (sprintf ('%s --index-search "%s"', cmd, function_name));
    endif

    if (! (have_fname && status == 0))
      status = system (cmd);
      if (status == 127)
        warning ("doc: unable to find info program '%s'", info_program ());
      endif
    endif

  endif

  if (nargout > 0)
    retval = status;
  endif

endfunction


%!testif ENABLE_DOCS
%! ifile = info_file ();
%! if (exist (ifile) != 2 && exist (sprintf ("%s.gz", ifile)) != 2)
%!   error ("Info file %s or %s.gz does not exist!", ifile, ifile);
%! endif
