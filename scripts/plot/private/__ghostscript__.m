## Copyright (C) 2010 Ben Abbott
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
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {Function File} {} __ghostscript__ (@var{@dots{}})
## Undocumented internal function.
## @end deftypefn

## Author: Ben Abbott <bpabbott@mac.com>
## Created: 2010-07-26

function status = __ghostscript__ (opts, varargin);

  if (nargin > 1)
    opts.name = varargin{1};
  endif
  if (nargin > 2)
    opts.ghostscript_device = varargin{2};
  endif
  if (nargin > 3)
    opts.ghostscript_output = varargin{3};
  endif

  if (strncmp (opts.devopt, "eps", 3))
    ## "eps" files
    gs_opts = "-q -dNOPAUSE -dBATCH -dSAFER -dEPSCrop";
  else
    ## "ps" or "pdf" files
    gs_opts = "-q -dNOPAUSE -dBATCH -dSAFER";
  endif

  cmd = sprintf ("%s %s -sDEVICE=%s -r%d -sOutputFile=%s %s", 
                 opts.ghostscript_binary, gs_opts, opts.ghostscript_device,
                 opts.resolution, opts.ghostscript_output, opts.name);

  if (opts.debug)
    fprintf ("Ghostscript command: %s\n", cmd);
  endif

  [status, output] = system (cmd);

  if (status != 0)
    warning ("print:ghostscripterror", 
             "print.m: gs failed to convert output to file '%s'.", opts.ghostscript_output)
  endif

endfunction
