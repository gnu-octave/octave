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
## @deftypefn {Function File} {} mean (@var{x}, @var{opt})
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
## @end deftypefn

## Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description: Compute arithmetic, geometric, and harmonic mean

function y = mean (x, opt)

  if ((nargin < 1) || (nargin > 2))
    usage ("mean (x, opt])");
  endif

  if isempty (x)
    error ("mean: x must not be empty");
  endif

  if (rows (x) == 1)
    x = x.';
  endif

  if (nargin == 1)
    opt = "a";
  endif

  [r, c] = size (x);

  if (strcmp (opt, "a"))
    y = sum (x) / r;
  elseif (strcmp (opt, "g"))
    y = NaN * ones (1, c);
    i = find (all (x > 0));
    if any (i)
      y(i) = exp (sum (log (x(:, i))) / r);
    endif
  elseif (strcmp (opt, "h"))
    y = NaN * ones (1, c);
    i = find (all (x != 0));
    if any (i)
      y(i) = r ./ sum (1 ./ x(:, i));
    endif
  else
    error ("mean: option `%s' not recognized", opt);
  endif

endfunction
