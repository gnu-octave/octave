## Copyright (C) 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2002, 2004,
##               2005, 2006, 2007, 2008, 2009 John W. Eaton
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
## @deftypefn {Function File} {} conv (@var{a}, @var{b}, @var{shape})
## Convolve two vectors.
##
## @code{y = conv (a, b)} returns a vector of length equal to
## @code{length (a) + length (b) - 1}.
## If @var{a} and @var{b} are polynomial coefficient vectors, @code{conv}
## returns the coefficients of the product polynomial.
##
## The optional @var{shape} parameter may be
##
## @table @asis
## @item @var{shape} = "full"
## Return the full convolution.
##
## @item @var{shape} = "same"
## Return central part of the convolution with the same size as @var{a}.
## @end table
##
## @noindent
## By default, @var{shape} is @samp{"full"}.
##
## @seealso{deconv, poly, roots, residue, polyval, polyderiv, polyint}
## @end deftypefn

## Author: Tony Richardson <arichard@stark.cc.oh.us>
## Created: June 1994
## Adapted-By: jwe

function y = conv (a, b, shape = "full")

  if (nargin < 2 || nargin > 3)
    print_usage ();
  endif

  if (! (isvector (a) && isvector (b)))
    error ("conv: both arguments must be vectors");
  endif

  la = length (a);
  lb = length (b);

  ly = la + lb - 1;

  if (ly == 0)
    y = zeros (1, 0);
  else
    ## Use the shortest vector as the coefficent vector to filter.
    ## Preserve the row/column orientation of the longer input.
    if (la <= lb)
      if (ly > lb)
        if (size (b, 1) <= size (b, 2))
          x = [b, (zeros (1, ly - lb))];
        else
          x = [b; (zeros (ly - lb, 1))];
        endif
      else
        x = b;
      endif
      y = filter (a, 1, x);
    else
      if (ly > la)
        if (size (a, 1) <= size (a, 2))
          x = [a, (zeros (1, ly - la))];
        else
          x = [a; (zeros (ly - la, 1))];
        endif
      else
        x = a;
      endif
      y = filter (b, 1, x);
    endif
    if (strcmp (shape, "same"))
      idx = ceil ((ly - la) / 2);
      y = y(idx+1:idx+la);
    endif
  endif

endfunction

%!test
%!  x = ones(3,1);
%!  y = ones(1,3);
%!  b = 2;
%!  c = 3;
%!  assert (conv (x, x), [1; 2; 3; 2; 1]);
%!  assert (conv (y, y), [1, 2, 3, 2, 1]);
%!  assert (conv (x, y), [1, 2, 3, 2, 1]);
%!  assert (conv (y, x), [1; 2; 3; 2; 1]);
%!  assert (conv (c, x), [3; 3; 3]);
%!  assert (conv (c, y), [3, 3, 3]);
%!  assert (conv (x, c), [3; 3; 3]);
%!  assert (conv (y, c), [3, 3, 3]);
%!  assert (conv (b, c), 6);
%!error conv ([1, 2; 3, 4], 3);
%!error conv (2, []);

%!test
%!  a = 1:10;
%!  b = 1:3;
%!  assert (size(conv(a,b)), [1, numel(a)+numel(b)-1])
%!  assert (size(conv(b,a)), [1, numel(a)+numel(b)-1])

%!test
%!  a = 1:10;
%!  b = 1:3;
%!  assert (size(conv(a,b)), [1, numel(a)+numel(b)-1])
%!  assert (size(conv(b,a)), [1, numel(a)+numel(b)-1])

%!  a = (1:10).';
%!  b = 1:3;
%!  assert (size(conv(a,b)), [numel(a)+numel(b)-1, 1])
%!  assert (size(conv(b,a)), [numel(a)+numel(b)-1, 1])

%!test
%!  a = 1:10;
%!  b = 1:3;
%!  assert (conv(a,b,'same'), [4, 10, 16, 22, 28, 34, 40, 46, 52, 47])
%!  assert (conv(b,a,'same'), [28, 34, 40])

%!test
%!  a = 1:10;
%!  b = (1:3).';
%!  assert (size(conv(a,b)), [1, numel(a)+numel(b)-1])
%!  assert (size(conv(b,a)), [1, numel(a)+numel(b)-1])
