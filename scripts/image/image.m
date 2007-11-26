## Copyright (C) 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2002, 2003,
##               2004, 2005, 2006, 2007 John W. Eaton
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
## @deftypefn {Function File} {} image (@var{img})
## @deftypefnx {Function File} {} image (@var{x}, @var{y}, @var{img})
## Display a matrix as a color image.  The elements of @var{x} are indices
## into the current colormap, and the colormap will be scaled so that the
## extremes of @var{x} are mapped to the extremes of teh colormap.
##
## It first tries to use @code{gnuplot}, then @code{display} from 
## @code{ImageMagick}, then @code{xv}, and then @code{xloadimage}.
## The actual program used can be changed using the @code{image_viewer}
## function.
##
## The axis values corresponding to the matrix elements are specified in
## @var{x} and @var{y}. If you're not using gnuplot 4.2 or later, these
## variables are ignored.
## @seealso{imshow, imagesc, colormap, image_viewer}
## @end deftypefn

## Author: Tony Richardson <arichard@stark.cc.oh.us>
## Created: July 1994
## Adapted-By: jwe

function retval = image (varargin)

  if (nargin < 2)
    print_usage ();
  elseif (isscalar (varargin{1}) && ishandle (varargin{1}))
    h = varargin {1};
    if (! strcmp (get (h, "type"), "axes"))
      error ("image: expecting first argument to be an axes object");
    endif
    oldh = gca ();
    unwind_protect
      axes (h);
      tmp = __image__ (h, varargin{2:end});
    unwind_protect_cleanup
      axes (oldh);
    end_unwind_protect
  else
    tmp = __image__ (gca (), varargin{:});
  endif

  if (nargout > 0)
    retval = tmp;
  endif

endfunction

function h = __image__ (ax, x, y, img)

  ## Deprecated zoom.  Remove this hunk of code if old zoom argument
  ## is outmoded.
  if ((nargin == 3 && isscalar (y)) || nargin == 5)
    warning ("image: zoom argument ignored -- use GUI features");
  endif
  if (nargin == 5)
    nargin = 4;
  endif
  if (nargin == 3 && isscalar (y))
    nargin = 2;
  endif

  if (nargin == 1)
    ## Load Bobbie Jo Richardson (Born 3/16/94)
    img = loadimage ("default.img");
    x = y = [];
  elseif (nargin == 2)
    img = x;
    x = y = [];
  elseif (nargin == 3 || nargin > 4)
    print_usage ();
  endif

  h = __img__ (x, y, img);

endfunction
