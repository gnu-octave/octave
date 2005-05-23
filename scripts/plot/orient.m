## Copyright (C) 2001 Paul Kienzle
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

## orient("landscape"|"portrait")
##    Set default print orientation
##
## ret = orient
##    Return default print orientation

function ret = orient(orientation)

  static __print_orientation = "landscape";

  if (nargin == 0)
    ret = __print_orientation;

  elseif (nargin == 1)

    if strcmp(orientation,"landscape") || strcmp(orientation,"portrait")
      __print_orientation = orientation;
    else
      error ("orient: unknown orientation");
    endif

  else

    usage("orient(['portrait' | 'landscape'])  OR  ret=orient");

  endif

endfunction

%!assert(orient,"landscape") # default
%!test orient('portrait')
%!assert(orient,"portrait")  # change to portrait
%!test orient('landscape')
%!assert(orient,"landscape") # change to landscape
%!fail("orient('nobody')","unknown orientation")
%!assert(orient,"landscape") # errors don't change the state
