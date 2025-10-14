## Copyright (C) 2025 Leonardo Araujo <leolca@gmail.com>
##
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; see the file COPYING. If not, see
## <https://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {Function File} {@var{X} = } dither (@var{RGB}, @var{map})
## @deftypefnx {Function File} {@var{X} = } dither (@var{RGB}, @var{map}, @var{Qm}, @var{Qe})
## @deftypefnx {Function File} {@var{BW} = } dither (@var{I})
## Quantize an image, using dithering to increase the apparent color resolution.
##
## @code{@var{X} = dither (@var{RGB},@var{map})} creates an indexed image 
## approximation. It uses the color provided in the colormap, and uses dithering
## to increase apparent color resolution. Floyd-Steinberg error filter is: 
## [   x  7]
## [3  5  1] / 16
## It uses a raster scan and no weight renormalization at boundaries.
## The default values are used: @var{Qm}=5, and @var{Qe}=8.
## 
## @var{RGB} is a mxnx3 array with values in [0, 1] (double) or [0, 255] (uint8).
##
## @var{map} is cx3 matrix holding RGB triplets in [0, 1] (double).
##
## @var{Qm} is the number of quantization bits per axis for inverse colormap (default: 5).
##
## @var{Qe} is the number of quantization bits for error diffusion (default: 8, max 16).
##
## @var{X} is a mxn indexed image (uint8 if c<=256, else uint16) for the 
## colormap @var{map} provided. 
##
## Example:
## @example
## X = dither (RGB, map);
## @end example
##
## @code{@var{X} = dither (@var{RGB}, @var{map}, @var{Qm}, @var{Qe})}
##
## @var{Qm} is the number of quantization bits along each color axis for the 
## inverse colormap.  @var{Qm} determines the resolution of this grid along each
## color axis (R, G, B).  @var{Qm} defines the precision of the color space 
## discretization used to map input RGB values to those colors available in the
## colormap.  @var{Qe} is the number of quantization bits for the color space
## error calculations in the Floyd-Steinberg error diffusion algorithm. 
## It controls the precision of the error values that are calculated and
## propagated during dithering.  If @var{Qe} < @var{Qm}, the error diffusion
## process may lose precision. Therefore dithering cannot be performed, and the
## function returns an undithered indexed image.
##
## @code{@var{BW} = dither (@var{I})} converts the grayscale input image @var{I}
## into binary applying dithering in the process.  The output image @var{BW} 
## is a black and white image where dithering creates the illusion of shades of
## gray.
##
## Ref [1] Floyd, R. W., and Steinberg, L., An Adaptive Algorithm for Spatial 
## Gray Scale, International Symposium Digest of Technical Papers, Society for 
## Information Displays, 1975, p. 36.
## Ref [2] Ulichney. R., Digital Halftoning, The MIT Press, 1987.
##
## @seealso{rgb2ind, imapprox}
## @end deftypefn

function X = dither (RGB, map, Qm = 5, Qe = 8)
  if (nargin < 1 || nargin > 4 || nargin == 3)
        print_usage;
  endif
  if ndims (RGB) == 2
    RGB = cat (3, RGB, RGB, RGB);  # Duplicate grayscale to RGB
    if nargin < 2,
      map = [0 0 0; 1 1 1]; % binary (black and white) colormap
      Qm = 1;
    endif
  endif
  if (ndims (RGB) != 3 || size (RGB, 3) != 3)
    error ('dither: RGB must be an m x n x 3 array.');
  end
  if (! ismatrix (map) || size (map, 2) != 3 || min (map(:)) < 0 || max (map(:)) > 1)
    error ('dither: Colormap must be a c x 3 matrix.');
  endif
  if nargin > 2,
    if (Qm < 1 || Qe < 1 || fix (Qm) != Qm || fix (Qe) != Qe)
      error ('Qm and Qe must be a positive integers.');
    elseif Qe < Qm
      warning ('dither: Qe < Qm, returning undithered image.');
      X = zeros (size (RGB, 1), size (RGB, 2), 'uint16');
      for i = 1:size (RGB, 1)
      for j = 1:size (RGB, 2)
        X(i, j) = rgb2indLUT (RGB(i, j, :), map, Qm);
      endfor
      endfor
      if size (map, 1) <= 256
        X = uint8 (X);
      endif
      return;
    endif
  endif
  Qe = min (Qe, 16); % Cap Qe to avoid excessive precision

  ## Scale RGB and map to [0, 1]
  if isa (RGB, 'uint8')
    RGB = double (RGB) / 255;
  elseif max (RGB(:)) > 1
    RGB = double (RGB) / 255;
  end
  RGB = max (0, min (1, RGB));
  if max (map(:)) > 1
    map = double (map) / 255;
  end
  map = max (0, min (1, map));

  % Initialize output
  [h, w, _] = size (RGB);
  X = zeros (h, w, 'uint16'); % Indices (1-based)

  % Floyd-Steinberg weights (raster scan, no renormalization)
  FSweights = [0 0 7; 3 5 1] / 16; % Sum = 1

  % neighbor offsets and weights
  offsets = [0 1; 1 -1; 1 0; 1 1];
  weights = [FSweights(1, 3), FSweights(2, 1), FSweights(2, 2), FSweights(2, 3)];

  % Quantization levels for error (Qe)
  n_levels = 2^Qe;
  error_scale = n_levels - 1;

  % Process pixels in raster order
  for i = 1:h
    for j = 1:w
      % Get current pixel (with accumulated errors)
      pixel = RGB(i, j, :);
      pixel = reshape (max (0, min (1, pixel)), 1, 3); % Clamp to [0, 1]

      % Quantize to nearest colormap color
      id = rgb2indLUT (pixel, map, Qm);
      X(i, j) = id;

      % Compute quantization error
      chosen_color = map(id+1, :);
      error = pixel - chosen_color; % 1x3
      error = round (error * error_scale) / error_scale; % Quantize to Qe bits

      % Diffuse error to neighboring pixels (no renormalization)
      for k = 1:length (weights)
        ni = i + offsets(k, 1);
        nj = j + offsets(k, 2);
        if ni <= h && nj >= 1 && nj <= w
          % Extract current pixel value as 1x3
          current = reshape (RGB(ni, nj, :), 1, 3);
          % Apply weighted error to each channel
          new_value = current + error * weights(k);
	  RGB(ni, nj, :) = reshape (new_value, 1, 3);
        endif
      endfor
    endfor
  endfor

  % Convert output to uint8 if colormap size allows
  if size (map, 1) <= 256
    X = uint8 (X);
  endif
endfunction

function id = rgb2indLUT (pixel, map, Qm = 5)
  % RGB2INDLUT Map an RGB pixel to the nearest colormap index using a lookup table.
  %   id = RGB2INDLUT (pixel, map) returns the 1-based index of the closest color
  %   in the colormap 'map' for the input RGB pixel (1x3 vector), using a quantized
  %   inverse colormap with 2^5 bins per RGB axis.
  %   id = RGB2INDLUT (pixel, map, Qm) uses Qm bits for quantization per RGB axis.
  %
  % Inputs:
  %   pixel: 1x3 vector [R, G, B], values in [0, 1] (double) or [0, 255] (uint8).
  %   map: c-by-3 matrix, each row an RGB triplet in [0, 1] (double).
  %   Qm: Number of quantization bits per axis (default: 5).
  %
  % Output:
  %   id: Index (1-based) into the colormap 'map' for the closest color.
  %
  % Notes:
  %   - Uses a persistent lookup table (LUT) for speed.
  %   - LUT is recomputed if map or Qm changes.
  %   - Warns if Qm is too large (>8) due to memory constraints.
  %   - Assumes input pixel and map are properly scaled (pixel auto-scaled if needed).

  % Validate inputs
  if nargin < 2
    error ('rgb2indLUT: Not enough input arguments. Pixel and colormap required.');
  endif
  if length (pixel) != 3
    error ('rgb2indLUT: Pixel must be a 1x3 RGB vector.');
    if !isvector (pixel)
      [s, i] = sort (size (pixel),'descend');
      pixel = permute (pixel, i);
    endif
  endif
  if !ismatrix (map) || size (map, 2) != 3
    error ('rgb2indLUT: Colormap must be a c-by-3 matrix.');
  endif
  if nargin < 3
    Qm = 5; % Default quantization bits
  end
  if !isscalar (Qm) || Qm < 1 || floor (Qm) != Qm
    error ('rgb2indLUT: Qm must be a positive integer.');
  end
  if Qm > 8
    warning ('rgb2indLUT: Qm > 8 may use excessive memory (%d^3 bins).', 2^Qm);
  endif

  % Scale pixel to [0, 1]
  if isa (pixel, 'uint8')
    pixel = double (pixel) / 255; % Convert to [0, 1]
  elseif max (pixel(:)) > 1
    pixel = double (pixel) / 255; % Assume [0, 255] if values exceed 1
  endif
  pixel = max (0, min (1, pixel)); % Clamp to [0, 1]

  % Ensure map is in [0, 1]
  if max (map(:)) > 1
    map = double(map) / 255;
  endif
  map = max (0, min (1, map));

  % Persistent variables for LUT
  persistent lut last_map last_Qm;

  % Check if we need to recompute the LUT
  recompute = isempty (lut) || Qm != last_Qm || !isequal (map, last_map);

  % Number of bins per axis
  n_bins = 2^Qm;

  % Scale pixel to [0, n_bins-1] for indexing
  bin_idx = round (pixel * (n_bins - 1)) + 1;

  if recompute
    % Initialize LUT: n_bins x n_bins x n_bins array of colormap indices
    lut = zeros (n_bins, n_bins, n_bins, 'uint16');

    % Compute bin centers for distance calculations
    bin_centers = (0:(n_bins-1))' / (n_bins-1); % [0, 1] range, column vector
    [R, G, B] = ndims_grid(n_bins, n_bins, n_bins); % Meshgrid for bin indices
    bin_rgb = [bin_centers(R(:)), bin_centers(G(:)), bin_centers(B(:))]; % n_bins^3 x 3

    % Compute Euclidean distances from each bin to each colormap color
    c = size (map, 1); % Number of colors
    distances = zeros (n_bins^3, c);
    for i = 1:c
      diff = bin_rgb - map(i, :); % n_bins^3 x 3
      distances(:, i) = sqrt (sum (diff.^2, 2)); % n_bins^3 x 1
    endfor

    % Find the nearest colormap index (1-based) for each bin
    [_, indices] = min (distances, [], 2);
    lut(:) = indices; % Assign to LUT

    % Update cached parameters
    last_map = map;
    last_Qm = Qm;
  endif

  % Look up the colormap index
  id = lut(bin_idx(1), bin_idx(2), bin_idx(3)) - 1;
endfunction

function [X, Y, Z] = ndims_grid (nx, ny, nz)
  % NDIMS_GRID Create 3D grid indices (emulates meshgrid for 3D).
  [x, y, z] = ind2sub ([nx, ny, nz], 1:(nx*ny*nz));
  X = reshape (x, nx, ny, nz);
  Y = reshape (y, nx, ny, nz);
  Z = reshape (z, nx, ny, nz);
endfunction

%!demo
%! ## Solid gray
%!
%! I = ones (256)/2;
%! X = dither (I);
%! figure;
%! subplot (121); imshow (I); title ('original'); 
%! subplot (122); imshow (double(X)); title ('dithered'); 

%!demo
%! ## Four solid gray levels
%!
%! I = [ones(256,64)/4, ones(256,64)/2, ones(256,64)*3/4, ones(256,64)*7/8];
%! X = dither (I);
%! figure;
%! subplot (121); imshow (I); title ('original');
%! subplot (122); imshow (double(X)); title ('dithered');

%!demo
%! ## Black-White Gradient
%! 
%! I = repmat ([0:255]./255,256,1);
%! X = dither (I);
%! figure;
%! subplot (121); imshow (I); title ('original');
%! subplot (122); imshow (double(X)); title ('dithered');

%!demo
%! ## Color Gradient
%!
%! width = 256; height = 256;
%! upperleft = [1, 0, 0];   % Red
%! upperright = [0, 1, 0];  % Green
%! lowerleft = [0, 0, 1];   % Blue
%! lowerright = [0, 0, 0];  % Black
%!
%! % Create a grid for interpolation
%! [x, y] = meshgrid(linspace(0, 1, width), linspace(0, 1, height));
%! % Initialize the 3D array for the image
%! image = zeros(height, width, 3);
%! % Calculate the interpolated colors for each point
%! % The logic is a bilinear interpolation of the four corner colors
%! % The first dimension of the `image` matrix is the height (y-axis) and the second is the width (x-axis)
%! image(:, :, 1) = (1 - x) .* (1 - y) * lowerleft(1) + x .* (1 - y) * lowerright(1) + (1 - x) .* y * upperleft(1) + x .* y * upperright(1);
%! image(:, :, 2) = (1 - x) .* (1 - y) * lowerleft(2) + x .* (1 - y) * lowerright(2) + (1 - x) .* y * upperleft(2) + x .* y * upperright(2);
%! image(:, :, 3) = (1 - x) .* (1 - y) * lowerleft(3) + x .* (1 - y) * lowerright(3) + (1 - x) .* y * upperleft(3) + x .* y * upperright(3);
%! 
%! % Use the corner colors to define the colormap
%! map = [upperleft; upperright; lowerleft; lowerright];
%! % Apply dither
%! X = dither (image, map);
%!
%! % Display the results
%! figure;
%! subplot (121); imshow (image); title ('original');
%! subplot (122); imshow (reshape(map(X(:)+1,:), [size(X) 3])); title ('dithered');

%!demo
%! # Lenna
%! url = 'https://upload.wikimedia.org/wikipedia/en/7/7d/Lenna_%28test_image%29.png';
%! rgb_image = imread(url);
%! map = [226 143 122; 199 127 124; 175 71 82; 230 191 168; 210 100 98; 132 50 81; 94 24 65; 149 97 139] / 255;
%! X = dither (rgb_image, map);
%! I = reshape(map(X(:)+1,:), [size(X) 3]);
%! figure;
%! subplot (121); imshow (rgb_image); title ('original'); 
%! subplot (122); imshow (I); title ('dithered'); 

## Test input validation
%!error dither ()
%!error dither (permute (1:3,[1 3 2]))
%!error dither (1, 1)
%!error dither (1, 1:3)
%!error dither (1, [0 0 0]')
%!error dither (1, [0 0 0], 0)
%!error dither (1, [0 0 0], 0, 0)
%!error dither (1, [0 0 0], -1, 1)
%!error dither (1, [0 0 0], 1, -1)

%!test
%! X = dither (0, [0 0 0; 1 1 1], 1, 1);
%! assert (X, uint8(0))

%!test
%! X = dither (1, [0 0 0; 1 1 1], 1, 1);
%! assert (X, uint8(1))

%!test
%! X = dither (repmat(ones(3)/2,1,1,3), [0 0 0; 1 1 1], 4, 4);
%! assert (X, uint8([1 0 1; 0 1 0; 1 0 1]))

%!test
%! X = dither (repmat(ones(3)/4,1,1,3), [0 0 0; 1 1 1], 4, 4);
%! assert (X, uint8([0 0 0; 0 1 0; 0 0 0]))

%!test
%! X = dither (repmat(ones(3)*3/4,1,1,3), [0 0 0; 1 1 1], 4, 4);
%! assert (X, uint8([1 1 1; 1 0 1; 1 1 1]))
