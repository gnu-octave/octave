########################################################################
##
## Copyright (C) 2004-2023 The Octave Project Developers
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
## @deftypefn {} {@var{p} =} colperm (@var{s})
## Return the column permutations such that the columns of
## @code{@var{s}(:, @var{p})} are ordered in terms of increasing number of
## nonzero elements.
##
## If @var{s} is symmetric, then @var{p} is chosen such that
## @code{@var{s}(@var{p}, @var{p})} orders the rows and columns with
## increasing number of nonzero elements.
## @end deftypefn

function p = colperm (s)

  if (nargin < 1)
    print_usage ();
  endif

  [~, p] = sort (sum (s != 0, 1));

endfunction


%!assert <*59226> (colperm ([1,0;0,0]), [2, 1])
