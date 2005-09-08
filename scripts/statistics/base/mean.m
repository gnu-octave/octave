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
## @deftypefn {Function File} {} mean (@var{x}, @var{dim}, @var{opt})
## If @var{x} is a vector, compute the mean of the elements of @var{x}
## @iftex
## @tex
## $$ {\rm mean}(x) = \bar{x} = {1\over N} \sum_{i=1}^N x_i $$
## @end tex
## @end iftex
## @ifinfo
##
## @example
## mean (x) = SUM_i x(i) / N
## @end example
## @end ifinfo
## If @var{x} is a matrix, compute the mean for each column and return them
## in a row vector.
##
## With the optional argument @var{opt}, the kind of mean computed can be
## selected.  The following options are recognized:
##
## @table @code
## @item "a"
## Compute the (ordinary) arithmetic mean.  This is the default.
##
## @item "g"
## Computer the geometric mean.
##
## @item "h"
## Compute the harmonic mean.
## @end table
##
## If the optional argument @var{dim} is supplied, work along dimension
## @var{dim}.
##
## Both @var{dim} and @var{opt} are optional.  If both are supplied,
## either may appear first.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Compute arithmetic, geometric, and harmonic mean

function y = mean (x, opt1, opt2)

  need_dim = 0;

  if (nargin == 1)
    opt = "a";
    need_dim = 1;
  elseif (nargin == 2)
    if (ischar (opt1))
      opt = opt1;
      need_dim = 1;
    else
      dim = opt1;
      opt = "a";
    endif
  elseif (nargin == 3)
    if (ischar (opt1))
      opt = opt1;
      dim = opt2;
    elseif (ischar (opt2))
      opt = opt2;
      dim = opt1;
    else
      usage ("mean: expecting opt to be a string");
    endif
  else
    usage ("mean (x, dim, opt) or mean (x, dim, opt)");
  endif

  if (need_dim)
    t = find (size (x) != 1);
    if (isempty (t))
      dim = 1;
    else
      dim = t(1);
    endif
  endif

  if (dim > ndims (x))
    n = 1;
  else
    sz = size (x);
    n = sz (dim);
  endif

  if (strcmp (opt, "a"))
    y = sum (x, dim) / n;
  elseif (strcmp (opt, "g"))
    x(x <= 0) = NaN;
    y = exp (sum (log (x), dim) / n);
  elseif (strcmp (opt, "h"))
    x(x == 0) = NaN;
    y = n ./ sum (1 ./ x, dim);
  else
    error ("mean: option `%s' not recognized", opt);
  endif

endfunction
