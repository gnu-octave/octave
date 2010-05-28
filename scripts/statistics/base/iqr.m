## Copyright (C) 1995, 1996, 1997, 1998, 2000, 2002, 2003, 2004, 2005,
##               2006, 2007, 2009 Kurt Hornik
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
## @deftypefn {Function File} {} iqr (@var{x}, @var{dim})
## If @var{x} is a vector, return the interquartile range, i.e., the
## difference between the upper and lower quartile, of the input data.
##
## If @var{x} is a matrix, do the above for first non-singleton
## dimension of @var{x}.  If the option @var{dim} argument is given,
## then operate along this dimension.
## @end deftypefn

## Author KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Interquartile range

function y = iqr (x, dim)

  if (nargin != 1 && nargin != 2)
    print_usage ();
  endif

  if (!ismatrix(x) || ischar(x))
    error ("iqr: X must be a numeric matrix or vector");
  endif

  nd = ndims (x);
  sz = size (x);
  nel = numel (x);
  if (nargin != 2)
    ## Find the first non-singleton dimension.
    dim = find (sz > 1, 1);
    if (isempty (dim))
      dim = 1;
    endif
  else
    if (!(isscalar (dim) && dim == round (dim)) || 
        !(1 <= dim && dim <= nd))
      error ("iqr: DIM must be an integer and a valid dimension");
    endif
  endif

  ## This code is a bit heavy, but is needed until empirical_inv 
  ## takes other than vector arguments.
  c = sz(dim);
  sz(dim) = 1;
  y = zeros (sz);
  stride = prod (sz(1:dim-1));
  for i = 1 : nel / c;
    offset = i;
    offset2 = 0;
    while (offset > stride)
      offset -= stride;
      offset2++;
    endwhile
    offset += offset2 * stride * c;
    rng = [0 : c-1] * stride + offset;

    y (i) = empirical_inv (3/4, x(rng)) - empirical_inv (1/4, x(rng));
  endfor

endfunction
