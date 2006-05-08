## Copyright (C) 2005 Soren Hauberg
## 
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.
## 
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
## 
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Command} doc @var{function_name}
## Displays documentation for the function @var{function_name}.
## For example, if you want to see the documentation for the Octave
## random number generator @code{rand}, type
## @example
## @code{doc rand}
## @end example
## @seealso{help}
## @end deftypefn

## Author: Soren Hauberg <soren@hauberg.org>
## Adapted-by: jwe

## PKG_ADD: mark_as_command doc

function retval = doc (fname)

  if (nargin != 1 || ! ischar (fname))
    usage ("doc function_name")
  endif

  ## Get the directory where the function lives.
  ## FIXME -- maybe we should have a better way of doing this.

  x = exist (fname);

  if (x == 2)
    ffile = file_in_loadpath (strcat (fname, "."));
  elseif (x == 3)
    ffile = file_in_loadpath (strcat (fname, "."));
  else
    ffile = "";
  endif

  if (! isempty (ffile))
    info_dir = fileparts (ffile);
  else
    info_dir = octave_config_info ("infodir");
  endif

  ## Determine if a file called doc.info exist in the same 
  ## directory as the function.

  info_file_name = fullfile (info_dir, "doc.info");

  if (! isstruct (stat (info_file_name)))
    info_file_name = info_file ();
  endif

  cmd = sprintf ("\"%s\" --directory \"%s\" --file \"%s\" --index-search %s",
		 info_program (), info_dir, info_file_name, fname);

  status = system (cmd);

  if (status == 127)
    warning ("unable to find info program `%s'", info_program ());
  endif

  if (nargout > 0)
    retval = status;
  endif

endfunction
