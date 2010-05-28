## Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2002, 2004, 2005,
##               2006, 2007, 2008 Kurt Hornik
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
## @deftypefn  {Function File} {} statistics (@var{x})
## @deftypefnx {Function File} {} statistics (@var{x}, @var{dim})
## If @var{x} is a matrix, return a matrix with the minimum, first
## quartile, median, third quartile, maximum, mean, standard deviation,
## skewness, and kurtosis of the columns of @var{x} as its columns.
##
## If @var{x} is a vector, calculate the statistics along the first 
## non-singleton dimension.
##
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Compute basic statistics

function S = statistics (X, dim)

  if (nargin != 1 && nargin != 2)
    print_usage ();
  endif

  if (!ismatrix(X) || ischar(X))
    error ("statistics: X must be a numeric matrix or vector");
  endif

  nd = ndims (X);
  sz = size (X);
  if (nargin != 2)
    ## Find the first non-singleton dimension.
    dim = find (sz > 1, 1);
    if (isempty (dim))
      dim = 1;
    endif
  else
    if (!(isscalar (dim) && dim == round (dim)) || 
        !(1 <= dim && dim <= nd))
      error ("statistics: DIM must be an integer and a valid dimension");
    endif
  endif
  
  if (sz(dim) < 2)
    error ("statistics: dimension of X is too small (<2)");
  endif    

  emp_inv = quantile (X, [0.25; 0.5; 0.75], dim, 7);

  S = cat (dim, min (X, [], dim), emp_inv, max (X, [], dim), mean (X, dim),
           std (X, [], dim), skewness (X, dim), kurtosis (X, dim));

endfunction

%!test
%! x = rand(7,5);
%! s = statistics (x);
%! m = median (x);
%! assert (m, s(3,:), eps);
