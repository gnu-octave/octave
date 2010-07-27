## Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2002, 2003, 2005,
##               2006, 2007, 2008, 2009 Kurt Hornik
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
## @deftypefn  {Function File} {} mean (@var{x})
## @deftypefnx {Function File} {} mean (@var{x}, @var{dim})
## @deftypefnx {Function File} {} mean (@var{x}, @var{opt})
## @deftypefnx {Function File} {} mean (@var{x}, @var{dim}, @var{opt})
## If @var{x} is a vector, compute the mean of the elements of @var{x}
## @tex
## $$ {\rm mean}(x) = \bar{x} = {1\over N} \sum_{i=1}^N x_i $$
## @end tex
## @ifnottex
##
## @example
## mean (x) = SUM_i x(i) / N
## @end example
##
## @end ifnottex
## If @var{x} is a matrix, compute the mean for each column and return them
## in a row vector.
##
## The optional argument @var{opt} selects the type of mean to compute.
## The following options are recognized:
##
## @table @code
## @item "a"
## Compute the (ordinary) arithmetic mean.  [default]
##
## @item "g"
## Compute the geometric mean.
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
## @seealso{median,mode}
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
      error ("mean: expecting opt to be a string");
    endif
  else
    print_usage ();
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
    y = prod (x, dim) .^ (1/n);
  elseif (strcmp (opt, "h"))
    y = n ./ sum (1 ./ x, dim);
  else
    error ("mean: option `%s' not recognized", opt);
  endif

endfunction

%!test
%! x = -10:10;
%! y = x';
%! z = [y, y+10];
%! assert(mean (x) == 0 && mean (y) == 0 && mean (z) == [0, 10]);

%!error mean ();

%!error mean (1, 2, 3);

