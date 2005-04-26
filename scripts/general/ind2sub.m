## Copyright (C) 2001  Paul Kienzle <pkienzle@kienzle.powernet.co.uk>
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
## @deftypefn {Function File} {[@var{s1}, @var{s2}, @dots{}, @var{sN}] =} ind2sub (@var{dims}, @var{ind})
## Convert a linear index into subscripts.
## @end deftypefn
##
## @seealso{sub2ind}

## Author: Paul Kienzle <pkienzle@kienzle.powernet.co.uk>
## Adapted-by: jwe

function varargout = ind2sub (dims, ind)

  if (nargin == 2)
    if (isvector (dims) && round (dims) == dims)
      if (isnumeric (ind) && round (ind) == ind)
	ntot = prod (dims);
	if (ind > 0 & ind <= ntot)
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
      error ("sub2ind: expecting dims to be an integer vector");
    endif
  else
    usage ("ind2sub (dims, ind)");
  endif


endfunction
