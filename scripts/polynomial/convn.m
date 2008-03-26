## Copyright (C) 2008 Soren Hauberg
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
## @deftypefn {Function File} {@var{c} =} convn (@var{a}, @var{b}, @var{shape})
## @math{N}-dimensional convolution of matrices @var{a} and @var{b}.
##
## The size of the output is determined by the @var{shape} argument.
## This can be any of the following character strings:
##
## @table @asis
## @item "full"
## The full convolution result is returned. The size out of the output is
## @code{size (@var{a}) + size (@var{b})-1}. This is the default behaviour.
## @item "same"
## The central part of the convolution result is returned. The size out of the
## output is the same as @var{a}.
## @item "valid"
## The valid part of the convolution is returned. The size of the result is
## @code{max (size (@var{a}) - size (@var{b})+1, 0)}.
## @end table
##
## @seealso{conv, conv2}
## @end deftypefn

function c = convn (a, b, shape = "full")

  if (nargin < 2)
    error ("convn: not enough input arguments");
  endif

  if (!ismatrix (a) || !ismatrix (b) || ndims (a) != ndims (b))
    error ("convn: first and second arguments must be matrices of the same dimensionality");
  endif

  if (!ischar (shape))
    error ("convn: third input argument must be a string");
  endif

  if (!any (strcmpi (shape, {"full", "same", "valid"})))
    error ("convn: invalid shape argument: '%s'", shape);
  endif
  
  ## Should we swap 'a' and 'b'?
  ## FIXME -- should we also swap in any of the non-full cases?
  if (numel (b) > numel (a) && strcmpi (shape, "full"))
    tmp = a;
    a = b;
    b = tmp;
  endif
  
  ## Pad A.
  switch (lower (shape))
    case "full"
      a = pad (a, size (b)-1, size (b)-1);
    case "same"
      a = pad (a, floor ((size (b)-1)/2), ceil ((size (b)-1)/2));
  endswitch
  
  ## Perform convolution.
  c = __convn__ (a, b);

endfunction

## Helper function that performs the padding.
function a = pad (a, left, right)
  cl = class (a);
  for dim = 1:ndims (a)
    l = r = size (a);
    l(dim) = left(dim);
    r(dim) = right(dim);
    a = cat (dim, zeros (l, cl), a, zeros (r, cl));
  endfor
endfunction

%!test
%! ## Compare to conv2
%! a = rand (100); 
%! b = ones (3);
%! c2 = conv2 (a, b, "full");
%! cn = convn (a, b, "full");
%! assert (max (abs (cn(:)-c2(:))), 0, 100*eps);

%!test
%! ## Compare to conv2
%! a = rand (100); 
%! b = ones (3);
%! c2 = conv2 (a, b, "same");
%! cn = convn (a, b, "same");
%! assert (max (abs (cn(:)-c2(:))), 0, 100*eps);

%!test
%! ## Compare to conv2
%! a = rand (100); 
%! b = ones (3);
%! c2 = conv2 (a, b, "valid");
%! cn = convn (a, b, "valid");
%! assert (max (abs (cn(:)-c2(:))), 0, 100*eps);

%!test
%! ## Real data
%! a = ones (10,10,10); 
%! b = ones (3,3,3);
%! c = convn (a, b, "valid");
%! assert (all (c == numel (b)));

%!test
%! ## Complex data
%! a = complex( ones (10,10,10), ones(10,10,10) ); 
%! b = complex( ones (3,3,3), ones(3,3,3) );
%! c = convn (a, b, "valid");
%! assert (all (c == 2*i*numel (b)));

