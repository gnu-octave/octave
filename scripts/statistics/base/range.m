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
## @deftypefn {Function File} {} range (@var{x})
## @deftypefnx {Function File} {} range (@var{x}, @var{dim})
## If @var{x} is a vector, return the range, i.e., the difference
## between the maximum and the minimum, of the input data.
##
## If @var{x} is a matrix, do the above for each column of @var{x}.
##
## If the optional argument @var{dim} is supplied, work along dimension
## @var{dim}.
## @end deftypefn

## Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description: Compute range

function y = range (x, varargin)

  if (nargin != 1 && nargin != 2)
    usage ("range (x, dim)");
  endif

  y = max (x, varargin{:}) - min (x, varargin{:});

endfunction
