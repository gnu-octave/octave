## Copyright (C) 1995, 1996, 1997, 1998, 2000, 2002, 2004, 2005, 2006,
##               2007 Kurt Hornik
## Copyright (C) 2009 Jaroslav Hajek
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
## @deftypefn  {Function File} {} meansq (@var{x})
## @deftypefnx {Function File} {} meansq (@var{x}, @var{dim})
## For vector arguments, return the mean square of the values.
## For matrix arguments, return a row vector containing the mean square
## of each column.  With the optional @var{dim} argument, returns the
## mean squared of the values along this dimension.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Compute mean square

function y = meansq (x, dim)

  if (nargin != 1 && nargin != 2)
    print_usage ();
  endif

  if (nargin < 2)
    t = find (size (x) != 1);
    if (isempty (t))
      dim = 1;
    else
      dim = t(1);
    endif
  endif

  y = sumsq (x, dim) / size (x, dim);

endfunction
