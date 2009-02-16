## Copyright (C) 2001, 2005, 2006, 2007 Paul Kienzle
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
## @deftypefn {Function File} {} orient (@var{orientation})
## Set the default print orientation.  Valid values for
## @var{orientation} include @code{"landscape"} and @code{"portrait"}.
## If called with no arguments, return the default print orientation.
## @end deftypefn

## Author: Paul Kienzle
## Adapted-By: jwe

function retval = orient (varargin)

  nargs = nargin;

  if (nargs > 0 && numel (varargin{1}) == 1 && isfigure (varargin{1}))
    cf = varargin{1};
    varargin(1) = [];
    nargs--;
  else
    cf = gcf ();
  endif

  if (nargs == 0)
    retval = get (cf, "paperorientation");
  elseif (nargin == 1)
    orientation = varargin{1};
    if (strcmpi (orientation, "landscape") || strcmpi (orientation, "portrait"))
      set (cf, "paperorientation", orientation)
    else
      error ("orient: unknown orientation");
    endif
  else
    print_usage ();
  endif

endfunction

%!shared
%!  set (gcf (), "visible", "off")
%!assert(orient,"portrait") # default
%!test orient('landscape')
%!assert(orient,"landscape") # change to landscape
%!test orient('portrait')
%!assert(orient,"portrait")  # change to portrait
%!fail("orient('nobody')","unknown orientation")
%!assert(orient,"portrait") # errors don't change the state
