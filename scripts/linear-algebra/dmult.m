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
## Software Foundation, 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.

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

  s = size (a);
  if ((min (s) > 1) || (max (s) != rows (B)))
    error ("dmult: a must be a vector of length rows (B)");
  endif

  M = (reshape (a, max (s), 1) * ones (1, columns (B))) .* B;

endfunction
