## Copyright (C) 2005-2011 Paul Kienzle
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
## @deftypefn {Function File} {} runlength (@var{x})
## Find the lengths of all sequences of common values.  Return the
## vector of lengths and the value that was repeated.
##
## @example
## @group
## runlength ([2, 2, 0, 4, 4, 4, 0, 1, 1, 1, 1])
## @result{}  [2, 1, 3, 1, 4]
## @end group
## @end example
## @end deftypefn

function [count, value] = runlength (x)
  idx = [find(x(1:end-1) != x(2:end)), length(x)];
  value = x(idx);
  count = diff ([0 idx]);
endfunction

%!assert (runlength([2 2 0 4 4 4 0 1 1 1 1]), [2 1 3 1 4]);
