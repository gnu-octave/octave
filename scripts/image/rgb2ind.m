## Copyright (C) 1994-2012 John W. Eaton
## Copyright (C) 2012 Carnë Draug
## Copyright (C) 2013 Adam H Aitkenhead
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
## @deftypefn  {Function File} {[@var{x}, @var{map}] =} rgb2ind (@var{rgb})
## @deftypefnx {Function File} {[@var{x}, @var{map}] =} rgb2ind (@var{rgb}, @var{map})
## @deftypefnx {Function File} {[@var{x}, @var{map}] =} rgb2ind (@var{rgb}, @var{n})
## @deftypefnx {Function File} {[@var{x}, @var{map}] =} rgb2ind (@var{rgb}, @var{tol})
## @deftypefnx {Function File} {[@var{x}, @var{map}] =} rgb2ind (@var{rgb}, @var{map}, @var{dither_option})
## @deftypefnx {Function File} {[@var{x}, @var{map}] =} rgb2ind (@var{rgb}, @var{n}, @var{dither_option})
## @deftypefnx {Function File} {[@var{x}, @var{map}] =} rgb2ind (@var{rgb}, @var{tol}, @var{dither_option})
## @deftypefnx {Function File} {[@var{x}, @var{map}] =} rgb2ind (@var{R}, @var{G}, @var{B})
## @deftypefnx {Function File} {[@var{x}, @var{map}] =} rgb2ind (@var{R}, @var{G}, @var{B}, @var{map})
## @deftypefnx {Function File} {[@var{x}, @var{map}] =} rgb2ind (@var{R}, @var{G}, @var{B}, @var{n})
## @deftypefnx {Function File} {[@var{x}, @var{map}] =} rgb2ind (@var{R}, @var{G}, @var{B}, @var{tol})
## @deftypefnx {Function File} {[@var{x}, @var{map}] =} rgb2ind (@var{R}, @var{G}, @var{B}, @var{map}, @var{dither_option})
## @deftypefnx {Function File} {[@var{x}, @var{map}] =} rgb2ind (@var{R}, @var{G}, @var{B}, @var{n}, @var{dither_option})
## @deftypefnx {Function File} {[@var{x}, @var{map}] =} rgb2ind (@var{R}, @var{G}, @var{B}, @var{tol}, @var{dither_option})
## Convert an image in red-green-blue (RGB) color space to an indexed image.
##
## The input image @var{rgb} must be an N-dimensional RGB image (MxNxO...x3 array) where M,N,O... are the
## image dimensions, and the final dimension contains the values in the red, green and blue
## channels.  Alternatively, the red, green and blue color channels can be input as separate arrays @var{R}, @var{G} and  @var{B}.
##
## @var{map} defines the colormap to be used.  Alternatively, @var{n} or @var{tol} may be used to define
## the maximum number of colors to use in an automatically generated colormap.  @var{n} is related to @var{tol}
## by:  @var{n} = (floor (1/@var{tol}) + 1)^3;
## @var{tol} must be >0 and <=1.
##
## @var{dither_option} is a string which enables or disables dithering: 'dither' (default) or 'nodither'.
##
## @seealso{ind2rgb, rgb2hsv, rgb2ntsc}
## @end deftypefn

## Author: Tony Richardson <arichard@stark.cc.oh.us>
## Created: July 1994
## Adapted-By: jwe

function [x, map] = rgb2ind (varargin)

  ## Gather the inputs
  if (nargin < 1 || nargin > 6)
    print_usage ();
  else

    ## Test for dither_option, by checking if the final input is a string
    if ischar (varargin{end})
      dither_option = varargin{end};
      dither_check  = true;
    else
      dither_option = 'dither';
      dither_check  = false;
    endif

    ## Read the rgb input
    if (nargin-dither_check==1 || nargin-dither_check==2)

      rgb = varargin{1};
      if (size (rgb)(end) != 3)
        error ("rgb2ind: The input image must be an RGB image (MxNxO...x3 array).");
      elseif (min (rgb(:)) < 0 || max (rgb(:)) > 1)
        error ("rgb2ind: The input image must contain values between 0 and 1.");
      endif
      if (nargin-dither_check==2)
        option = varargin{2};
      else
        dither_option = 'nodither';
      endif

    ## Read the R,G,B inputs
    elseif (nargin-dither_check==3 || nargin-dither_check==4)

      R = varargin{1};
      G = varargin{2};
      B = varargin{3};
      if (! size_equal (R, G, B))
        error ("rgb2ind: R, G, and B must have the same size");
      endif
      if (nargin-dither_check==4)
        option = varargin{4};
      else
        dither_option = 'nodither';
      endif
      
      rgb = reshape ([R(:), G(:), B(:)], [size(R), 3]);

    endif
  endif
      
  ## Apply a limited colormap if required
  if (exist ('option','var'))

    if (size (option,1)==1)

      if option>0 && option<=1
        ## option: tol
        tol = option;
        n   = (floor (1/option) + 1)^3;
      else
        ## option: n
        n   = option;
      endif
      optionstr = sprintf ('-colors %d',n);
      
    else

      ## option: map
      map = option;
      if (isequal (map(:,1),map(:,2)) || isequal (map(:,1),map(:,3)) || isequal (map(:,2),map(:,3)))
        error ('rgb2ind: The colormap cannot contain matching R,G, or B channels.')
      endif
      fnmap = tmpnam;
      map = reshape (map, size (map, 1), 1, 3);
      imwrite (map, fnmap, 'tiff');
      optionstr = sprintf ('-map %s', fnmap);
      
    endif
  
    ## Prepare the Graphicsmagick dithering option
    if strcmp (dither_option, 'nodither')
      ditherstr = '+dither';
    elseif strcmp (dither_option, 'dither')
      ditherstr = '-dither';
    endif
      
    ## Perform the image processing using Graphicsmagick
    fna = tmpnam;
    fnb = tmpnam;
    imwrite (rgb, fna, 'tiff');
    gmstr = sprintf ('gm convert %s %s %s %s', fna, ditherstr, optionstr, fnb);
    system (gmstr);
    rgb = imread (fnb);
    
  endif

  ## Conversion of rgb image to x,map
  sz = size (rgb);
  pr = prod (sz(1:end-1));
  x = zeros (sz(1:end-1));
  [map,~,x(:)] = unique (reshape(rgb, [pr, 3]), "rows");

  ## a colormap is of class double and values between 0 and 1
  switch (class (rgb))
    case {"single", "double", "logical"}
      ## do nothing, return the same
    case {"uint8", "uint16"}
      map = double (map) / double (intmax (class (rgb)));
    case "int16"
      map = (double (im) + 32768) / 65535;
    otherwise
      error ("unsupported image class %s", im_class);
  endswitch

  ## we convert to the smallest class necessary to encode the image. Matlab
  ## documentation does not mention what it does when uint16 is not enough...
  ## When an indexed image is of integer class, there's a -1 offset to the
  ## colormap, hence the adjustment
  if (rows (map) < 256)
    x = uint8 (x - 1);
  elseif (rows (map) < 65536)
    x = uint16 (x - 1);
  else
    ## leave it as double
  endif

endfunction


%% FIXME: Need some functional tests or %!demo blocks

%% Test input validation
%!error rgb2ind ()
%!error rgb2ind (1,2)
%!error rgb2ind (1,2,3,4)

