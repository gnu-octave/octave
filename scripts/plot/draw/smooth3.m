## Copyright (C) 2016 Markus Muetzel
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {} {@var{smoothed_data} =} smooth3 (@var{data})
## @deftypefnx {} {@var{smoothed_data} =} smooth3 (@var{data}, @var{method})
## @deftypefnx {} {@var{smoothed_data} =} smooth3 (@var{data}, @var{method}, @var{sz})
## @deftypefnx {} {@var{smoothed_data} =} smooth3 (@var{data}, @var{method}, @var{sz}, @var{std_dev})
## Smooth values of 3-dimensional matrix @var{data}.
##
## This function can be used, for example, to reduce the impact of noise in
## @var{data} before calculating isosurfaces.
##
## @var{data} must be a non-singleton 3-dimensional matrix.  The smoothed data
## from this matrix is returned in @var{smoothed_data} which is of the same
## size as @var{data}.
##
## The option input @var{method} determines which convolution kernel is used
## for the smoothing process.  Possible choices:
##
## @table @asis
## @item @qcode{"box"}, @qcode{"b"} (default)
## to use a convolution kernel with sharp edges.
##
## @item @qcode{"gaussian"}, @qcode{"g"}
## to use a convolution kernel that is represented by a non-correlated
## trivariate normal distribution function.
## @end table
##
## @var{sz} is either a vector of 3 elements representing the size of the
## convolution kernel in x-, y- and z-direction or a scalar, in which case
## the same size is used in all three dimensions.  The default value is 3.
##
## When @var{method} is @qcode{"gaussian"}, @var{std_dev} defines the standard
## deviation of the trivariate normal distribution function.  @var{std_dev} is
## either a vector of 3 elements representing the standard deviation of the
## Gaussian convolution kernel in x-, y- and z-directions or a scalar, in which
## case the same value is used in all three dimensions.  The default value is
## 0.65.
##
## @seealso{isosurface, isonormals, patch}
## @end deftypefn

## Author: mmuetzel

function smoothed_data = smooth3 (data, method = "box", sz = 3, std_dev = 0.65)

  if (nargin < 1 || nargin > 4)
    print_usage ();
  endif

  [data, conv_kernel, sz, std_dev] = ...
                        __get_check_smooth3_args__ (data, method, sz, std_dev);

  ## Manually pad data by replicating the values at the edges.
  ## (convn would pad with zeros)
  idx = cell (3, 1);
  for i_dim = 1:3
    sz_dim = size (data, i_dim);
    pad_vec = ones (1, (sz(i_dim)-1)/2);
    idx{i_dim} = [pad_vec 1:sz_dim sz_dim*pad_vec];
  endfor
  data_padded = data(idx{:});

  smoothed_data = convn (data_padded, conv_kernel, "valid");

endfunction

function [data, conv_kernel, sz, std_dev] = __get_check_smooth3_args__ (data, method, sz, std_dev)

  if (! isnumeric (data) || ndims (data) != 3)
    error ("smooth3: DATA must be a 3-D numeric matrix");
  endif

  if (! ischar (method))
    error ("smooth3: METHOD must be a string");
  endif

  if (! isreal (sz))
    error ("smooth3: SZ must be a real scalar or 3-element vector");
  elseif (isscalar (sz))
    sz(1:3) = sz;
  elseif (numel (sz) != 3)
    error (["smooth3: SZ of the convolution kernel must be " ...
            "a scalar or 3-element vector"]);
  endif
  if (any (sz < 1) || any (rem (sz, 2) != 1))
    error (["smooth3: SZ of the convolution kernel must consist " ...
            "of positive odd integers"]);
  endif

  switch (tolower (method))
    case {"g", "gaussian"}
      ## check std_dev
      if (! isreal (std_dev))
        error ("smooth3: STD_DEV must be a real scalar or 3-element vector");
      elseif (isscalar (std_dev))
        std_dev(1:3) = std_dev;
      elseif (numel (std_dev) != 3)
        error (["smooth3: STD_DEV of the Gaussian convolution kernel " ...
                "must be scalar or 3-element vector"]);
      endif

      conv_kernel = __smooth3_gaussian3__ (sz, std_dev);

    case {"b", "box"}
      conv_kernel = ones (sz) / prod (sz);

    otherwise
      error ("smooth3: invalid METHOD '%s'", method);

  endswitch

endfunction

function gaussian3 = __smooth3_gaussian3__ (sz, std_dev)
  ## trivariate non-correlated Gaussian distribution function

  x = (-(sz(2)-1)/2:(sz(2)-1)/2) / std_dev(2);
  y = (-(sz(1)-1)/2:(sz(1)-1)/2) / std_dev(1);
  z = (-(sz(3)-1)/2:(sz(3)-1)/2) / std_dev(3);

  [xx, yy, zz] = meshgrid (x, y, z);

  gaussian3 = exp (-(xx.^2 + yy.^2 + zz.^2)/2);

  gaussian3 /= sum (gaussian3(:));  # normalize

endfunction


%!demo
%! data = rand (10, 10, 10);
%! clf;
%! subplot (1, 2, 1);
%!  patch (isosurface (data, .5), ...
%!         "FaceColor", "blue", "EdgeColor", "k");
%!  title ("Original data");
%!  view (3);
%!  smoothed_data = smooth3 (data);
%! subplot (1, 2, 2);
%!  patch (isosurface (smoothed_data, .5), ...
%!         "FaceColor", "blue", "EdgeColor", "k");
%!  title ("Smoothed data");
%!  view (3);

## one input argument (method: "box")
%!test
%! a = rand (10, 8, 7);
%! b = smooth3 (a);
%! assert (size_equal (a, b), true);

## two input argument (method: "gaussian")
%!test
%! a = rand (5, 8, 7);
%! b = smooth3 (a, "gaussian");
%! assert (size_equal (a, b), true);

## three input argument (method: "box")
%!test
%! a = rand (3, 8, 7);
%! b = smooth3 (a, "box", 5);
%! assert (size_equal (a, b), true);

## three input argument (method: "gaussian")
%!test
%! a = rand (3, 8, 7);
%! b = smooth3 (a, "gaussian", 7);
%! assert (size_equal (a, b), true);

## size of convolution kernel = 1: no smoothing (method: "box")
%!test
%! a = rand (9, 8, 7);
%! b = smooth3 (a, "box", 1);
%! assert (a, b);

## size of convolution kernel = 1: no smoothing (method: "gaussian")
%!test
%! a = rand (9, 8, 7);
%! b = smooth3 (a, "gaussian", 1);
%! assert (a, b);

## four input arguments (method: "gaussian")
%!test
%! a = rand (3, 8, 7);
%! b = smooth3 (a, "gaussian", 7, .5);
%! assert (size_equal (a, b), true);

## size of convolution kernel is different in x, y and z (method: "box")
%!test
%! a = rand (3, 8, 7);
%! b = smooth3 (a, "box", [5 3 7]);
%! assert (size_equal (a, b), true);

## size of convolution kernel is different in x, y and z (method: "gaussian")
%!test
%! a = rand (3, 8, 7);
%! b = smooth3 (a, "gaussian", [5 3 7]);
%! assert (size_equal (a, b), true);

## size and width of gaussian convolution kernel is different in x, y and z
## (method: "gaussian")
%!test
%! a = rand (3, 8, 7);
%! b = smooth3 (a, "gaussian", [7 3 5], [.3 .5 .4]);
%! assert (size_equal (a, b), true);

## Test input validation
%!error smooth3 ()
%!error smooth3 (1,2,3,4,5)
%!error <DATA must be a 3-D numeric matrix> smooth3 (cell (2,2,2))
%!error <DATA must be a 3-D numeric matrix> smooth3 (1)
%!error <METHOD must be a string> smooth3 (ones (2,2,2), {3})
%!error <SZ must be a real> smooth3 (ones (2,2,2), "b", {3})
%!error <SZ .* must be .* 3-element vector> smooth3 (ones (2,2,2), :, [3 5])
%!error <SZ .* must .* positive .* integers> smooth3 (ones (2,2,2), :, [3 0 5])
%!error <SZ .* must .* odd integers> smooth3 (ones (2,2,2), :, [3 2 5])
%!error <STD_DEV must be a real> smooth3 (ones (2,2,2), "g", :, {0.65})
%!error <STD_DEV .* must be .* 3-element> smooth3 (ones (2,2,2), "g", :, [1 2])
%!error <invalid METHOD 'foobar'> smooth3 (ones (2,2,2), "foobar")

