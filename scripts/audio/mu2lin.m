########################################################################
##
## Copyright (C) 1995-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

## -*- texinfo -*-
## @deftypefn  {} {@var{y} =} mu2lin (@var{x})
## @deftypefnx {} {@var{y} =} mu2lin (@var{x}, @var{n})
## Convert audio data from mu-law to linear.
##
## Mu-law values are 8-bit unsigned integers in the range 0 @leq{} @var{y}
## @leq{} 255.  Linear values use floating point values in the range
## -@var{linmax} @leq{} @var{x} @var{linmax} (where
## @code{@var{linmax} = 32124/32768 =~ 0.98}) when @var{n} is zero (default).
## If @var{n} is 8 or 16 then @var{n}-bit signed integers are used instead.
##
## Programming Note: @code{mu2lin} maps maximum mu-law inputs to values
## slightly below the maximum ([-0.98, +0.98]) representable with a linear
## scale.  Because of this, @code{mu2lin (lin2mu (@var{x}))} might not
## reproduce the original input.
## @seealso{lin2mu}
## @end deftypefn

function y = mu2lin (x, n = 0)

  if (nargin < 1)
    print_usage ();
  endif

  if (nargin == 2)
    if (! isscalar (n) && ! isreal (n)
        || (n != 0 && n != 8 && n != 16))
      error ("lin2mu: N must be either 0, 8, or 16");
    elseif (isempty (n))
      n = 0;
    endif
  endif

  ulaw = [32124, 31100, 30076, 29052, 28028, 27004, 25980, 24956, ...
          23932, 22908, 21884, 20860, 19836, 18812, 17788, 16764, ...
          15996, 15484, 14972, 14460, 13948, 13436, 12924, 12412, ...
          11900, 11388, 10876, 10364,  9852,  9340,  8828,  8316, ...
           7932,  7676,  7420,  7164,  6908,  6652,  6396,  6140, ...
           5884,  5628,  5372,  5116,  4860,  4604,  4348,  4092, ...
           3900,  3772,  3644,  3516,  3388,  3260,  3132,  3004, ...
           2876,  2748,  2620,  2492,  2364,  2236,  2108,  1980, ...
           1884,  1820,  1756,  1692,  1628,  1564,  1500,  1436, ...
           1372,  1308,  1244,  1180,  1116,  1052,   988,   924, ...
            876,   844,   812,   780,   748,   716,   684,   652, ...
            620,   588,   556,   524,   492,   460,   428,   396, ...
            372,   356,   340,   324,   308,   292,   276,   260, ...
            244,   228,   212,   196,   180,   164,   148,   132, ...
            120,   112,   104,    96,    88,    80,    72,    64, ...
             56,    48,    40,    32,    24,    16,     8,     0 ];

  ulaw = [ -ulaw, ulaw ];

  ## Set the shape of y to that of x overwrites the contents of y with
  ## ulaw of x.
  y = x;
  y(:) = ulaw(x + 1);

  ## Convert to real or 8-bit.
  if (n == 0)
    ## [ -32768, 32767 ] -> [ -1, 1)
    y /= 32768;
  elseif (n == 8)
    ld = max (abs (y(:)));
    if (ld < 16384 && ld > 0)
      sc = 64 / ld;
    else
      sc = 1 / 256;
    endif
    y = fix (y * sc);
  endif

endfunction


## Test functionality
%!shared linmax
%! linmax = 32124 / 32768;

%!test
%! x = [0, 128, 255];
%! y = x';
%! assert (mu2lin (x), (mu2lin (y))');
%! assert (mu2lin (x), [-linmax, +linmax, 0]);

%!assert (mu2lin ([]), [])
%!assert (mu2lin (255), 0)
%!assert (mu2lin (255, 0), 0)
%!assert (mu2lin (255, 8), 0)
%!assert (mu2lin (255, 16), 0)
%!assert (mu2lin (128, []), linmax)
%!assert (mu2lin (128, 8), 125)
%!assert (mu2lin (128, 16), 32124)
%!assert (mu2lin (zeros (1, 1000), 0), repmat (-linmax, 1, 1000))
%!assert (mu2lin (255*ones (2, 2), 0), zeros (2, 2))

## Test input validation
%!error <Invalid call> mu2lin ()
%!error <N must be either 0, 8, or 16> mu2lin (1, 2)
%!error <N must be either 0, 8, or 16> mu2lin (1, [1,2])
%!error <N must be either 0, 8, or 16> mu2lin (1, ones (1, 2))
%!error mu2lin ({2:5})
