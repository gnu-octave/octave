########################################################################
##
## Copyright (C) 2008-2022 The Octave Project Developers
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
## @deftypefn  {} {@var{output} =} python (@var{scriptfile})
## @deftypefnx {} {@var{output} =} python (@var{scriptfile}, @var{argument1}, @var{argument2}, @dots{})
## @deftypefnx {} {[@var{output}, @var{status}] =} python (@dots{})
## Invoke Python script @var{scriptfile}, possibly with a list of command line
## arguments.
##
## Return output in @var{output} and optional status in @var{status}.  If
## @var{scriptfile} is not an absolute filename it is searched for in the
## current directory and then in the Octave loadpath.
## @seealso{system, perl}
## @end deftypefn

function [output, status] = python (scriptfile = "-c ''", varargin)

  if (ischar (scriptfile)
      && (   (nargin == 1 && ! isempty (scriptfile))
          || (nargin != 1 && iscellstr (varargin))))
    if (! strcmp (scriptfile(1:2), "-c"))
      ## Attempt to find file in loadpath.  No effect for absolute filenames.
      scriptfile = file_in_loadpath (scriptfile);
    endif

    [status, output] = system (["python ", scriptfile, ...
                                sprintf(" %s", varargin{:})]);
  else
    error ("python: invalid arguments");
  endif

endfunction


%!error <invalid arguments> python (123)
