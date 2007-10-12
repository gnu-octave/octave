## Copyright (C) 1994, 1995, 1996, 1997, 2000, 2005, 2006, 2007
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

function retval = __plt1__ (h, x1, options, properties)

  if (nargin < 2 || nargin > 4)
    print_usage ();
  endif

  if (nargin < 3 || isempty (options))
    options = __default_plot_options__ ();
  endif

  if (nargin < 4)
    properties = {};
  endif

  if (! isstruct (options))
    error ("__plt1__: options must be a struct array");
  endif

  [nr, nc] = size (x1);
  if (nr == 1)
    x1 = x1.';
    tmp = nr;
    nr = nc;
    nc = tmp;
  endif
  x1_i = imag (x1);
  if (any (any (x1_i)))
    x2 = x1_i;
    x1 = real (x1);
  else
    x2 = x1;
    x1 = (1:nr)';
  endif

  retval = __plt2__ (h, x1, x2, options, properties);

endfunction
