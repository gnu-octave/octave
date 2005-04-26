## Copyright (C) 1995, 1996, 1997  Kurt Hornik
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} dmult (@var{a}, @var{b})
## If @var{a} is a vector of length @code{rows (@var{b})}, return
## @code{diag (@var{a}) * @var{b}} (but computed much more efficiently).
## @end deftypefn

## Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description: Rescale the rows of a matrix

function M = dmult (a, B)

  if (nargin != 2)
    usage ("dmult (a, B)");
  endif
 if (! isvector (a))
    error ("dmult: a must be a vector of length rows (B)");
  endif
  a = a(:);
  sb = size (B);
  sb(1) = 1;
  M = repmat (a(:), sb) .* B;
endfunction
