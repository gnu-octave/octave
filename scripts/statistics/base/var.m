## Copyright (C) 1995, 1996, 1997, 1998, 2000, 2002, 2004, 2005, 2006,
##               2007, 2009 Kurt Hornik
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
## @deftypefn {Function File} {} var (@var{x})
## For vector arguments, return the (real) variance of the values.
## For matrix arguments, return a row vector containing the variance for
## each column.
##
## The argument @var{opt} determines the type of normalization to use.
## Valid values are
##
## @table @asis 
## @item 0:
## Normalizes with @math{N-1}, provides the best unbiased estimator of the
## variance [default].
## @item 1:
## Normalizes with @math{N}, this provides the second moment around the mean.
## @end table
##
## The third argument @var{dim} determines the dimension along which the 
## variance is calculated.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Compute variance

function retval = var (x, opt, dim)

  if (nargin < 1 || nargin > 3)
    print_usage ();
  endif
  if (nargin < 3)
    dim = find (size (x) > 1, 1);
    if (isempty (dim))
      dim = 1;
    endif
  endif
  if (nargin < 2 || isempty (opt))
    opt = 0;
  endif

  sz = size (x);
  n = sz(dim);
  if (isempty (x))
    ## FIXME -- is there a way to obtain these results without all the
    ## special cases?
    if (ndims (x) == 2 && sz(1) == 0 && sz(2) == 0)
      retval = NaN;
    else
      sz(dim) = 1;
      if (n == 0)
        if (prod (sz) == 0)
          retval = zeros (sz);
        else
          retval = NaN (sz);
        endif
      else
        retval = zeros (sz);
      endif
    endif
  elseif (n == 1)
    retval = zeros (sz);
  else
    retval = sumsq (center (x, dim), dim) / (n + opt - 1);
  endif

endfunction

%!assert (var (13), 0)
%!assert (var ([]), NaN)
%!assert (var (ones (0, 0, 0), 0, 1), zeros (1, 0, 0))
%!assert (var (ones (0, 0, 0), 0, 2), zeros (0, 1, 0))
%!assert (var (ones (0, 0, 0), 0, 3), zeros (0, 0))
%!assert (var (ones (1, 2, 0)), zeros (1, 1, 0))
%!assert (var (ones (1, 2, 0, 0), 0, 1), zeros (1, 2, 0, 0))
%!assert (var (ones (1, 2, 0, 0), 0, 2), zeros (1, 1, 0, 0))
%!assert (var (ones (1, 2, 0, 0), 0, 3), zeros (1, 2, 1, 0))
