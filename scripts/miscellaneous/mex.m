########################################################################
##
## Copyright (C) 2006-2023 The Octave Project Developers
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
## @deftypefn  {} {} mex [-options] file @dots{}
## @deftypefnx {} {@code{status} =} mex (@dots{})
## Compile source code written in C, C++, or Fortran, to a MEX file.
##
## @var{status} is the return status of the @code{mkoctfile} function.
##
## If the compilation fails, and the output argument is not requested,
## an error is raised.  If the programmer requests @var{status}, however,
## Octave will merely issue a warning and it is the programmer's responsibility
## to verify the command was successful.
##
## This is equivalent to @code{mkoctfile --mex [-options] file}.
##
## @seealso{mkoctfile, mexext}
## @end deftypefn

function status = mex (varargin)

  [out, sts] = mkoctfile ("--mex", varargin{:});

  if (! isempty (out))
    printf ("%s", out);
  endif
  if (nargout > 0)
    status = sts;
  else
    if (sts != 0)
      error ("mex: building exited with failure status\n");
    endif
  endif

endfunction


## Remove from test statistics.  All real tests are in mkoctfile.
%!assert (1)
