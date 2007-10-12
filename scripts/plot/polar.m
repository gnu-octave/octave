## Copyright (C) 1993, 1994, 1995, 1996, 1997, 1999, 2000, 2004, 2005,
##               2006, 2007 John W. Eaton
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
## @deftypefn {Function File} {} polar (@var{theta}, @var{rho}, @var{fmt})
## Make a two-dimensional plot given polar the coordinates @var{theta} and
## @var{rho}.
##
## The optional third argument specifies the line type.
## @seealso{plot}
## @end deftypefn

## Author: jwe

function retval = polar (varargin)

  newplot ();

  ## [h, varargin] = __plt_get_axis_arg__ ("semilogx", varargin{:});
  h = gca ();

  nargs = numel (varargin);

  if (nargs == 3)
    if (! ischar (varargin{3}))
      error ("polar: third argument must be a string");
    endif
    tmp = __plr2__ (h, varargin{:});
  elseif (nargin == 2)
    if (ischar (varargin{2}))
      tmp = __plr1__ (h, varargin{:});
    else
      fmt = "";
      tmp = __plr2__ (h, varargin{:}, fmt);
    endif
  elseif (nargin == 1)
    fmt = "";
    tmp = __plr1__ (h, varargin{:}, fmt);
  else
    print_usage ();
  endif

  if (nargout > 0)
    retval = tmp;
  endif

endfunction
