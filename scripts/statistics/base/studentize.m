## Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2002, 2004, 2005,
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
## @deftypefn {Function File} {} studentize (@var{x}, @var{dim})
## If @var{x} is a vector, subtract its mean and divide by its standard
## deviation.
##
## If @var{x} is a matrix, do the above along the first non-singleton
## dimension.  If the optional argument @var{dim} is given then operate
## along this dimension.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Subtract mean and divide by standard deviation

function t = studentize (x, dim)

  if (nargin != 1 && nargin != 2)
    print_usage ();
  endif

  if (!ismatrix(x) || ischar(x))
    error ("studentize: X must be a numeric matrix or vector");
  endif

  nd = ndims (x);
  sz = size (x);
  if (nargin != 2)
    ## Find the first non-singleton dimension.
    dim = find (sz > 1, 1);
    if (isempty (dim))
      dim = 1;
    endif
  else
    if (!(isscalar (dim) && dim == round (dim))
        || !(1 <= dim && dim <= nd))
      error ("studentize: DIM must be an integer and a valid dimension");
    endif
  endif

  c = sz(dim);
  idx = ones (1, nd);
  idx(dim) = c;
  t = x - repmat (mean (x, dim), idx);
  t = t ./ repmat (max (cat (dim, std(t, [], dim), ! any (t, dim)), [], dim), idx);

endfunction
