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
## @deftypefn {Function File} {[@var{s1}, @var{s2}, @dots{}, @var{sN}] =} ind2sub (@var{dims}, @var{ind})
## Convert a linear index into subscripts.
##
## The following example shows how to convert the linear index @code{8}
## in a 3-by-3 matrix into a subscript.  The matrix is linearly indexed
## moving from one column to next, filling up all rows in each column.
## @example
## [r, c] = ind2sub ([3, 3], 8)
## @result{} r =  2
## c =  3
## @end example
## @seealso{sub2ind}
## @end deftypefn

## Author: Paul Kienzle <pkienzle@kienzle.powernet.co.uk>
## Adapted-by: jwe

function varargout = ind2sub (dims, ind)

  if (nargin == 2)
    if (isvector (dims) && all (round (dims) == dims))
      if (isnumeric (ind) && all (round (ind) == ind))
	ntot = prod (dims);
	if (all (ind > 0 & ind <= ntot))
	  nd = length (dims);
	  if (nargout > 0)
	    vlen = nargout;
	  else
	    vlen = 1;
	  endif
	  if (nd > vlen);
	    dims(vlen) = prod (dims(vlen:nd));
	    dims(vlen+1:nd) = [];
	  endif
	  nd = length (dims);
	  scale = [1; cumprod(dims(:))];
	  for i = nd:-1:2
	    k = (ind >= scale(i));
	    r = ones (size (ind));
	    t = zeros (size (ind));
	    t(k) = floor ((ind(k) - 1) / scale(i));
	    r(k) = t(k) + 1;
	    varargout{i} = r;
	    ind(k) -= t(k) * scale(i);
	  endfor
	  varargout{1} = ind;
	  for i = nd+1:vlen
	    varargout{i} = ones (size (ind));
	  endfor
	else
	  error ("ind2sub: index out of range");
	endif
      else
	error ("ind2sub: expecting integer-valued index argument");
      endif
    else
      error ("ind2sub: expecting dims to be an integer vector");
    endif
  else
    print_usage ();
  endif


endfunction
