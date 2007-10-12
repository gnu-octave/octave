## Copyright (C) 1994, 1995, 1996, 1997, 2000, 2002, 2005, 2006, 2007
##               John W. Eaton
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

## Undocumented internal function.

## Author: jwe

function retval = __plt2__ (h, x1, x2, options, properties)

  if (nargin < 3 || nargin > 5)
    print_usage ();
  endif

  if (nargin < 4 || isempty (options))
    options = __default_plot_options__ ();
  endif

  if (nargin < 5)
    properties = {};
  endif

  if (! isstruct (options))
    error ("__plt1__: options must be a struct array");
  endif

  if (any (any (imag (x1))))
    x1 = real (x1);
  endif

  if (any (any (imag (x2))))
    x2 = real (x2);
  endif

  h_set = false;
  if (isscalar (x1))
    if (isscalar (x2))
      retval = __plt2ss__ (h, x1, x2, options, properties);
    else
      error ("__plt2__: invalid data for plotting");
    endif
  elseif (isvector (x1))
    if (isvector (x2))
      retval = __plt2vv__ (h, x1, x2, options, properties);
    elseif (ismatrix (x2))
      retval = __plt2vm__ (h, x1, x2, options, properties);
    else
      error ("__plt2__: invalid data for plotting");
    endif
  elseif (ismatrix (x1))
    if (isvector (x2))
      retval = __plt2mv__ (h, x1, x2, options, properties);
    elseif (ismatrix (x2))
      retval = __plt2mm__ (h, x1, x2, options, properties);
    else
      error ("__plt2__: invalid data for plotting");
    endif
  elseif (isempty (x1) && isempty (x2))
    retval = zeros (0, 1);
  else
    error ("__plt2__: invalid data for plotting");
  endif

endfunction
