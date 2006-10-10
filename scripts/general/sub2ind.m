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
## @deftypefn {Function File} {@var{ind} =} sub2ind (@var{dims}, @var{i}, @var{j})
## @deftypefnx {Function File} {@var{ind} =} sub2ind (@var{dims}, @var{s1}, @var{s2}, @dots{}, @var{sN})
## Convert subscripts into a linear index.
## @seealso{ind2sub}
## @end deftypefn

## Author: Paul Kienzle <pkienzle@kienzle.powernet.co.uk>
## Adapted-by: jwe

function ind = sub2ind (dims, varargin)

  if (nargin > 1)
    if (isvector (dims) && round (dims) == dims)
      nd = length (dims);
      vlen = length (varargin);
      dims(vlen) = prod (dims(vlen:nd));
      dims(vlen+1:nd) = [];
      scale = cumprod (dims(:));
      for i = 1:vlen
	arg = varargin{i};
	if (isnumeric (arg) && round (arg) == arg)
	  if (i == 1)
	    if (arg > 0 & arg <= dims(i))
	      ind = first_arg = arg;
	    else
	      error ("sub2ind: index out of range");
	    endif
	  else
            if (prod (size (first_arg)) == prod (size (arg)))
	      if ((i > nd && arg == 1) || (arg > 0 & arg <= dims(i)))
		ind(:) += scale(i-1) * (arg(:) - 1);
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
