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
## @deftypefn  {} {@var{output} =} python (@var{scriptfile})
## @deftypefnx {} {@var{output} =} python (@var{scriptfile}, @var{argument1}, @var{argument2}, @dots{})
## @deftypefnx {} {[@var{output}, @var{status}] =} python (@dots{})
## Invoke Python script @var{scriptfile}, possibly with a list of command line
## arguments.
##
## Return output in @var{output} and optional status in @var{status}.  If
## @var{scriptfile} is not an absolute filename it is searched for in the
## current directory and then in the Octave loadpath.
##
## Programming Note: On UNIX systems, the script will be executed by
## @command{python3} and on Windows by @command{python}.  You can override
## these defaults by setting the @env{PYTHON} environment variable, for example
## from within Octave using @code{setenv PYTHON /usr/local/bin/python3}.
##
## @seealso{system, perl}
## @end deftypefn

function [output, status] = python (scriptfile, varargin)

  persistent pyexec = get_python_executable ();

  if (nargin < 1)
    print_usage ();
  endif

  if (! ischar (scriptfile) || isempty (scriptfile))
    error ("python: SCRIPTFILE must be a non-empty string");
  endif
  if (nargin > 1 && ! iscellstr (varargin))
    error ("python: ARGUMENTS must be strings");
  endif

  if (numel (scriptfile) < 2 || ! strcmp (scriptfile(1:2), "-c"))
      ## Attempt to find file in loadpath.  No effect for absolute filenames.
      scriptfile = file_in_loadpath (scriptfile);
    endif

  [status, output] = system ([pyexec " " scriptfile, ...
                              sprintf(" %s", varargin{:})]);

endfunction

function pyexec = get_python_executable ()

  pyexec = getenv ("PYTHON");
  if (isempty (pyexec))
    ## PEP394 says Python 3 installs should all provide this command
    pyexec = "python3";

    if (ispc () && (! isunix ()))
      ## 2020-03: Python.org installer/Anaconda do not provide python3
      pyexec = "python";
    endif
  endif

endfunction


## Test input validation
%!error <Invalid call> python ()
%!error <SCRIPTFILE must be a non-empty string> python (123)
%!error <SCRIPTFILE must be a non-empty string> python ("")
%!error <ARGUMENTS must be strings> python ("pythonfile", 123)
