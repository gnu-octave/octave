## Copyright (C) 2008-2012 David Bateman
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {Function File} {} symvar (@var{s})
## Identify the argument names in the function defined by a string.
## Common constant names such as @code{pi}, @code{NaN}, @code{Inf},
## @code{eps}, @code{i} or @code{j} are ignored.  The arguments that are
## found are returned in a cell array of strings.  If no variables are
## found then the returned cell array is empty.
## @end deftypefn

function args = symvar (s)
  args = argnames (inline (s));
endfunction

## This function is tested by the tests for argnames().
%!assert (1)
