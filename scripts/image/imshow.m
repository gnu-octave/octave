## Copyright (C) 1996, 1997 John W. Eaton
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
## @deftypefn {Function File} {} imshow (@var{i})
## @deftypefnx {Function File} {} imshow (@var{x}, @var{map})
## @deftypefnx {Function File} {} imshow (@var{i}, @var{n})
## @deftypefnx {Function File} {} imshow (@var{r}, @var{g}, @var{b})
## Display an image.
##
## @code{imshow (@var{x})} displays an image @var{x}.
## The numerical class of the image determines its bit-depth: 1 for
## @code{logical}, 8 for @code{uint8} and @code{logical}, and 16 for
## @code{double} or @code{uint16}.  If @var{x} has dimensions MxNx3, the
## three matrices represent the red, green and blue components of the
## image.
##
## @code{imshow (@var{x}, @var{map})} displays an indexed image using the
## specified colormap.
##
## @code{imshow (@var{i}, @var{n})} displays a gray scale intensity image of
## N levels.
##
## @code{imshow (@var{r}, @var{g}, @var{b})} displays an RGB image.
##
## The character string @code{"truesize"} can always be used as an
## optional final argument to prevent automatic zooming of the image.
## @end deftypefn
## @seealso{image, imagesc, colormap, gray2ind, and rgb2ind}

## Author: Tony Richardson <arichard@stark.cc.oh.us>
## Created: July 1994
## Adapted-By: jwe

function imshow (varargin)

  usage_str = "imshow (x) or imshow (x, map) or imshow (i, N) or imshow (r, g, b)";

  if (nargin == 0 || nargin > 4)
    usage (usage_str);
  endif
  
  ## Count nr of matrix arguments.
  mvars = 0;
  while (mvars < nargin && ismatrix (varargin{mvars+1}))
    mvars++;
  endwhile
  
  if (mvars < 1 || mvars > 3)
    usage (usage_str);
  endif
    
  ## Determine image depth
  imclass = class (varargin{1});
  s = __im_numeric_limits__ (imclass);
  if (!isfield (s, "max"))
    error ("imshow: cannot handle image class '%s'", imclass);
  endif

  ## Maximum bit-depth is 16
  if (s.max > 65535)
    s.max = 65535;
  endif

  imdepth = log (s.max+1) / log (2);
  if (imdepth - floor (imdepth) != 0)
    error ("imshow: cannot determine image colour depth");
  endif
  
  ## Remove complex parts of arguments
  realwarning = false;
  for i = 1:mvars
    if (iscomplex (varargin{i}))
      if (!realwarning)
        warning ("imshow: displaying real part of complex image");
        realwarning = true;
      endif
      varargin{i} = real (varargin{i});
    endif
  endfor
  
  ## Pack r,g,b image into ND-matrix if necessary
  if (mvars == 3)
    I = [];
    try
      I = cat (3, varargin{1:3});
    catch
      error ("imshow: r, g and b matrix dimensions must agree");
    end_try_catch
  else
    I = varargin{1};
  endif
  I = double (I);
  
  ## Is the image specified as MxNx3 colour?
  iscolour = false;
  if (size (I,3) == 3)
    iscolour = true;
  endif

  ## Is the image indexed?
  isindexed = false;
  if (mvars == 2)
    isindexed = true;
    if (iscolour)
      error ("imshow: cannot provide colour image and colourmap");
    endif
  endif
  
  ## Scale images of class "double" appropriately
  if (!isindexed)
    if (strcmp (imclass, "double") == 1)
      if (max (I(:)) <= 1)
        ## image in [0-1]; scale to [0 - 2^imdepth]
        I = I * 2^imdepth;
      else
        ## image outside [0-1]; this is unexpected: scale to [0 - 2^imdepth]
        I = I / max (I(:)) * 2^imdepth;
      endif
    endif
  endif
  
  ## Generate colour map
  if (isindexed)
    M = varargin{2};
    if (isscalar (M))
      M = gray (M);
    endif
  elseif (iscolour)
    I = I / 2^imdepth;
    [I, M] = rgb2ind (I(:,:,1), I(:,:,2), I(:,:,3));
  else
    I = I+1; ## index into colourmap
    M = gray (2^imdepth);
  endif
  
  ## Check for "truesize".
  zoom = [];
  for i = mvars+1:nargin
    if (ischar (varargin{i}) && strcmp (varargin{i}, "truesize"))
      zoom = 1;
    endif
  endfor

  colormap (M);
  image (I, zoom);

endfunction

function s = __im_numeric_limits__ (cname)  
  s = struct ();
  switch (cname)
    case ("double")
      s.max = realmax;
    case ("char")
      s.max = 255;
    case ("logical")
      s.max = 1;
    otherwise
      try
        s.max = double (intmax (cname));
      catch
      end_try_catch
  endswitch 
endfunction

%!error imshow ()                           # no arguments
%!error imshow (1, 2, 3, 4, 5)              # too many arguments
%!error imshow ([1,2], [2,3], [3,4], [4,5]) # too many matrix arguments
%!error imshow ("image.png")                # filename not accepted as argument

%!demo
%!  imshow (loadimage ("default.img"));

%!demo
%!  I = loadimage ("default.img");
%!  imshow (I, "truesize")

%!demo
%!  [I, M] = loadimage ("default.img");
%!  imshow (I, M);

%!demo
%!  [I, M] = loadimage ("default.img");
%!  imshow (I, I*0.5, I*0.8);

%!demo
%!  [I, M] = loadimage ("default.img");
%!  X = [];
%!  X = cat (3, X, I*0.8);
%!  X = cat (3, X, I*0.8);
%!  X = cat (3, X, I);
%!  imshow (X);

