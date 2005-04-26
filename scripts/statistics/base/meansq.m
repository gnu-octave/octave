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
## @deftypefn {Function File} {} meansq (@var{x})
## @deftypefnx {Function File} {} meansq (@var{x}, @var{dim})
## For vector arguments, return the mean square of the values.
## For matrix arguments, return a row vector contaning the mean square
## of each column. With the optional @var{dim} argument, returns the
## mean squared of the values along this dimension
## @end deftypefn

## Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description: Compute mean square

function y = meansq (x, varargin)

  if (nargin != 1 && nargin != 2)
    usage ("meansq (x)");
  endif

  y = mean (x.^2, varargin{:});

endfunction
