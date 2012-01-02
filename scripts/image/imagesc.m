## Copyright (C) 1994-2012 John W. Eaton
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
## @deftypefn  {Function File} {} imagesc (@var{A})
## @deftypefnx {Function File} {} imagesc (@var{x}, @var{y}, @var{A})
## @deftypefnx {Function File} {} imagesc (@dots{}, @var{limits})
## @deftypefnx {Function File} {} imagesc (@var{h}, @dots{})
## @deftypefnx {Function File} {@var{h} =} imagesc (@dots{})
## Display a scaled version of the matrix @var{A} as a color image.  The
## colormap is scaled so that the entries of the matrix occupy the entire
## colormap.  If @var{limits} = [@var{lo}, @var{hi}] are given, then that
## range is set to the 'clim' of the current axes.
##
## The axis values corresponding to the matrix elements are specified in
## @var{x} and @var{y}, either as pairs giving the minimum and maximum
## values for the respective axes, or as values for each row and column
## of the matrix @var{A}.
##
## The optional return value @var{h} is a graphics handle to the image.
## @seealso{image, imshow, caxis}
## @end deftypefn

## Author: Tony Richardson <arichard@stark.cc.oh.us>
## Created: July 1994
## Adapted-By: jwe

function retval = imagesc (varargin)

  if (nargin < 1)
    print_usage ();
  elseif (isscalar (varargin{1}) && ishandle (varargin{1}))
    h = varargin{1};
    if (! strcmp (get (h, "type"), "axes"))
      error ("imagesc: expecting first argument to be an axes object");
    endif
    oldh = gca ();
    unwind_protect
      axes (h);
      tmp = __imagesc__ (h, varargin{2:end});
    unwind_protect_cleanup
      axes (oldh);
    end_unwind_protect
  else
    tmp = __imagesc__ (gca (), varargin{:});
  endif

  if (nargout > 0)
    retval = tmp;
  endif

endfunction

function ret = __imagesc__ (ax, x, y, A, limits, DEPRECATEDZOOM)

  ## Deprecated zoom.  Remove this hunk of code if old zoom argument
  ## is outmoded.
  if ((nargin == 3 && isscalar (y))
      || (nargin == 4 && (isscalar (y) || isscalar (A)))
      || (nargin == 5 && isscalar (limits))
      || nargin == 6)
    warning ("image: zoom argument ignored -- use GUI features");
  endif
  if (nargin == 6)
    if (isscalar (limits))
      limits = DEPRECATEDZOOM;
    endif
    nargin = 5;
  endif
  if (nargin == 5 && isscalar (limits))
    nargin = 4;
  endif
  if (nargin == 4 && (isscalar (y) || isscalar (A)))
    if (isscalar (y))
      y = A;
    endif
    nargin = 3;
  endif
  if (nargin == 3 && isscalar (y))
    nargin = 2;
  endif

  if (nargin < 2 || nargin > 5)
    print_usage ();
  elseif (nargin == 2)
    A = x;
    x = y = limits = [];
  elseif (nargin == 3)
    A = x;
    limits = y;
    x = y = [];
  elseif (nargin == 4 && ! isscalar (x) && ! isscalar (y) && ! isscalar (A))
    limits = [];
  endif

  ret = image (ax, x, y, A);
  set (ret, "cdatamapping", "scaled");

  ## use given limits or guess them from the matrix
  if (length (limits) == 2 && limits(2) >= limits(1))
    set (ax, "clim", limits);
  elseif (! isempty (limits))
    error ("imagesc: expected data LIMITS to be [lo, hi]");
  endif

endfunction
