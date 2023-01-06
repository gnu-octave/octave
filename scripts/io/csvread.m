########################################################################
##
## Copyright (C) 2001-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{x} =} csvread (@var{filename})
## @deftypefnx {} {@var{x} =} csvread (@var{filename}, @var{dlm_opt1}, @dots{})
## Read the comma-separated-value (CSV) file @var{filename} into the matrix
## @var{x}.
##
## Note: only CSV files containing numeric data can be read.
##
## This function is equivalent to
##
## @example
## @var{x} = dlmread (@var{filename}, "," , @var{dlm_opt1}, @dots{})
## @end example
##
## Any optional arguments are passed directly to @code{dlmread}
## (@pxref{XREFdlmread,,@code{dlmread}}).
## @seealso{dlmread, textscan, csvwrite, dlmwrite}
## @end deftypefn

function x = csvread (filename, varargin)
  x = dlmread (filename, ",", varargin{:});
endfunction


## Tests for csvread() are in csvwrite()
## Mark file as being tested
%!assert (1)
