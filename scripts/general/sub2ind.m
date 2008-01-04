## Copyright (C) 2001, 2003, 2004, 2005, 2006, 2007, 2008 Paul Kienzle
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
## @deftypefn {Function File} {@var{ind} =} sub2ind (@var{dims}, @var{i}, @var{j})
## @deftypefnx {Function File} {@var{ind} =} sub2ind (@var{dims}, @var{s1}, @var{s2}, @dots{}, @var{sN})
## Convert subscripts into a linear index.
##
## The following example shows how to convert the two-dimensional
## index @code{(2,3)} of a 3-by-3 matrix to a linear index.  The matrix
## is linearly indexed moving from one column to next, filling up
## all rows in each column.
##
## @example
## linear_index = sub2ind ([3, 3], 2, 3)
## @result{} 8
## @end example
## @seealso{ind2sub}
## @end deftypefn

## Author: Paul Kienzle <pkienzle@kienzle.powernet.co.uk>
## Adapted-by: jwe

function ind = sub2ind (dims, varargin)

  if (nargin > 1)
    if (isvector (dims) && all (round (dims) == dims))
      nd = length (dims);
      vlen = length (varargin);
      dims(vlen) = prod (dims(vlen:nd));
      dims(vlen+1:nd) = [];
      scale = cumprod (dims(:));
      for i = 1:vlen
	arg = varargin{i};
	if (isnumeric (arg) && isequal (round (arg), arg))
	  if (i == 1)
	    if (all (arg(:) > 0 & arg(:) <= dims(i)))
	      ind = first_arg = arg;
	    else
	      error ("sub2ind: index out of range");
	    endif
	  else
	    if (size_equal (first_arg, arg))
	      if ((i > nd && arg == 1) || all (arg(:) > 0 & arg(:) <= dims(i)))
		ind += scale(i-1) * (arg - 1);
	      else
		error ("sub2ind: index out of range");
	      endif
	    else
	      error ("sub2ind: all index arguments must be the same size");
	    endif
	  endif
	else
	  error ("sub2ind: expecting integer-valued index arguments");
	endif
      endfor
    else
      error ("sub2ind: expecting dims to be an integer vector");
    endif
  else
    print_usage ();
  endif


endfunction

# Test input validation
%!error <sub2ind: expecting dims to be an integer vector> sub2ind([10 10.5], 1, 1);
%!error <sub2ind: expecting integer-valued index arguments> sub2ind([10 10], 1.5, 1);
%!error <sub2ind: expecting integer-valued index arguments> sub2ind([10 10], 1, 1.5);

# Test evaluation
%!shared s1, s2, s3, in
%! s1 = [   1   1   1   1 ;   2   2   2   2 ];
%! s2 = [   1   1   2   2 ;   1   1   2   2 ];
%! s3 = [   1   2   1   2 ;   1   2   1   2 ];
%! in = [   1 101  11 111 ;   2 102  12 112 ];
%!assert (sub2ind([10 10 10], s1, s2, s3), in);
%!shared

# Test low index
%!assert (sub2ind([10 10 10], 1, 1, 1), 1);
%!error <sub2ind: index out of range> sub2ind([10 10 10], 0, 1, 1);
%!error <sub2ind: index out of range> sub2ind([10 10 10], 1, 0, 1);
%!error <sub2ind: index out of range> sub2ind([10 10 10], 1, 1, 0);

# Test high index
%!assert (sub2ind([10 10 10], 10, 10, 10), 1000);
%!error <sub2ind: index out of range> sub2ind([10 10 10], 11, 10, 10);
%!error <sub2ind: index out of range> sub2ind([10 10 10], 10, 11, 10);
%!error <sub2ind: index out of range> sub2ind([10 10 10], 10, 10, 11);

# Test high index in the trailing dimensions
%!assert (sub2ind([10], 2, 1, 1), 2);
%!error <sub2ind: index out of range> sub2ind([10], 1, 2, 1);
%!error <sub2ind: index out of range> sub2ind([10], 1, 1, 2);
%!assert (sub2ind([10 10], 2, 2, 1), 12);
%!error <sub2ind: index out of range> sub2ind([10 10], 2, 1, 2);
%!error <sub2ind: index out of range> sub2ind([10 10], 1, 2, 2);

# Test handling of empty arguments
%!assert (sub2ind([10 10], zeros(0,0), zeros(0,0)), zeros(0,0));
%!assert (sub2ind([10 10], zeros(2,0), zeros(2,0)), zeros(2,0));
%!assert (sub2ind([10 10], zeros(0,2), zeros(0,2)), zeros(0,2));
%!error <sub2ind: all index arguments must be the same size> sub2ind([10 10 10], zeros(0,2), zeros(2,0));

# Test handling of arguments of different size
%!error <sub2ind: all index arguments must be the same size> sub2ind([10 10], ones(1,2), ones(1,3));
%!error <sub2ind: all index arguments must be the same size> sub2ind([10 10], ones(1,2), ones(2,1));

