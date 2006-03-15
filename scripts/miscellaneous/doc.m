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
## along with this program; if not, write to the Free Software
## Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

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

function retval = doc (fname)

  if (nargin != 1 || ! ischar (fname))
    usage ("doc function_name")
  endif

  ## Get the directory where the function lives.
  ## XXX FIXME XXX -- maybe we should have a better way of doing this.

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

  info_file = fullfile (info_dir, "doc.info");

  if (! isstruct (stat (info_file)))
    info_file = INFO_FILE;
  endif

  cmd = sprintf ("\"%s\" --directory \"%s\" --file \"%s\" --index-search %s",
		 INFO_PROGRAM, info_dir, info_file, fname);

  status = system (cmd);

  if (status == 127)
    warning ("unable to find info program `%s'", INFO_PROGRAM);
  endif

  if (nargout > 0)
    retval = status;
  endif

endfunction
