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
## @deftypefn  {Function File} {} range (@var{x})
## @deftypefnx {Function File} {} range (@var{x}, @var{dim})
## If @var{x} is a vector, return the range, i.e., the difference
## between the maximum and the minimum, of the input data.
##
## If @var{x} is a matrix, do the above for each column of @var{x}.
##
## If the optional argument @var{dim} is supplied, work along dimension
## @var{dim}.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Compute range

function y = range (x, dim)

  if (nargin == 1)
    y = max (x) - min (x);
  elseif (nargin == 2)
    y = max (x, [], dim) - min (x, [], dim);
  else
    print_usage ();
  endif

endfunction
