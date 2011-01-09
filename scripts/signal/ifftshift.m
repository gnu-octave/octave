## Copyright (C) 1997, 2006, 2007, 2009 by Vincent Cautaerts
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
## @deftypefn  {Function File} {} ifftshift (@var{x})
## @deftypefnx {Function File} {} ifftshift (@var{x}, @var{dim})
## Undo the action of the @code{fftshift} function.  For even length 
## @var{x}, @code{fftshift} is its own inverse, but odd lengths differ 
## slightly.
## @end deftypefn

## Author: Vincent Cautaerts <vincent@comf5.comm.eng.osaka-u.ac.jp>
## Created: July 1997
## Adapted-By: jwe
## Modified-By: Paul Kienzle, converted from fftshift
## Modified-By: David Bateman, add NDArray capability and option dim arg

function retval = ifftshift (x, dim)

  retval = 0;

  if (nargin != 1 && nargin != 2)
    print_usage ();
  endif

  if (nargin == 2)
    if (! isscalar (dim))
      error ("ifftshift: dimension must be an integer scalar");
    endif
    nd = ndims (x);
    sz = size (x);
    sz2 = floor (sz(dim) / 2);
    idx = cell ();
    for i = 1:nd
      idx{i} = 1:sz(i);
    endfor
    idx{dim} = [sz2+1:sz(dim), 1:sz2];
    retval = x(idx{:});
  else
    if (isvector (x))
      x = length (x);
      xx = floor (x/2);
      retval = x([xx+1:x, 1:xx]);
    elseif (ismatrix (x))
      nd = ndims (x);
      sz = size (x);
      sz2 = floor (sz ./ 2);
      idx = cell ();
      for i = 1:nd
        idx{i} = [sz2(i)+1:sz(i), 1:sz2(i)];
      endfor
      retval = x(idx{:});
    else
      error ("ifftshift: expecting vector or matrix argument");
    endif
  endif

endfunction
