########################################################################
##
## Copyright (C) 2008-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{output} =} perl (@var{scriptfile})
## @deftypefnx {} {@var{output} =} perl (@var{scriptfile}, @var{argument1}, @var{argument2}, @dots{})
## @deftypefnx {} {[@var{output}, @var{status}] =} perl (@dots{})
## Invoke Perl script @var{scriptfile}, possibly with a list of command line
## arguments.
##
## Return output in @var{output} and optional status in @var{status}.  If
## @var{scriptfile} is not an absolute filename it is searched for in the
## current directory and then in the Octave loadpath.
## @seealso{system, python}
## @end deftypefn

function [output, status] = perl (scriptfile, varargin)

  if (nargin < 1)
    print_usage ();
  endif

  if (! ischar (scriptfile) || isempty (scriptfile))
    error ("perl: SCRIPTFILE must be a non-empty string");
  endif
  if (nargin > 1 && ! iscellstr (varargin))
    error ("perl: ARGUMENTS must be strings");
  endif

  if (numel (scriptfile) < 2 || ! strcmp (scriptfile(1:2), "-e"))
    ## Attempt to find file in loadpath.  No effect for absolute filenames.
    scriptfile = file_in_loadpath (scriptfile);
  endif

  [status, output] = system (["perl " scriptfile ...
                              sprintf(" %s", varargin{:})]);

endfunction


## Test input validation
%!error <Invalid call> perl ()
%!error <SCRIPTFILE must be a non-empty string> perl (123)
%!error <SCRIPTFILE must be a non-empty string> perl ("")
%!error <ARGUMENTS must be strings> perl ("perlfile", 123)
