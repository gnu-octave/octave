## Copyright (C) 1997 Auburn University.  All rights reserved.
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by the
## Free Software Foundation; either version 2, or (at your option) any
## later version.
##
## Octave is distributed in the hope that it will be useful, but WITHOUT
## ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
## FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
## for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301 USA.

## Undocumented internal function.

## -*- texinfo -*-
## @deftypefn {Function File} {[@var{y}, @var{my}, @var{ny}] =} __abcddims__ (@var{x})
##
## Used internally in @code{abcddim}.  If @var{x} is a zero-size matrix,
## both dimensions are set to 0 in @var{y}.
## @var{my} and @var{ny} are the row and column dimensions of the result.
## @end deftypefn

## Author: A. S. Hodel <a.s.hodel@eng.auburn.edu>
## Created: February 1997

function [y, my, ny] = __abcddims__ (x)

  y = x;
  if (isempty (y))
    y = [];
  endif
  [my, ny] = size (y);

endfunction
