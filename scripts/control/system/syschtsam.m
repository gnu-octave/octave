## Copyright (C) 1996, 2000, 2002, 2003, 2005, 2006, 2007
##               Auburn University.  All rights reserved.
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
## @deftypefn {Function File} {} syschtsam (@var{sys}, @var{tsam})
## This function changes the sampling time (tsam) of the system.  Exits with
## an error if sys is purely continuous time.
## @end deftypefn

## Author: John Ingram <ingraje@eng.auburn.edu>
## Created: August 1996

function retsys = syschtsam (sys, tsam)

  if (nargin != 2)
    print_usage ();
  elseif (! isstruct (sys))
    error ("sys must be in system data structure form");
  elseif (! isscalar (tsam))
    disp ("syschtsam:")
    tsam
    error ("tsam must be a scalar")
  elseif (! (is_sample (tsam) || tsam <= 0))
    error ("tsam must be real, scalar, and greater than zero");
  elseif (sysgettsam (sys) == 0)
    [nc, nz, mm, pp] = sysdimensions (sys);
    warning ("syschtsam: continuous system (nc=%d, nz=%d, mm=%d, pp=%d)",
	     nc, nz, mm, pp);
    warning ("syschtsam: The system is continuous, use c2d to make the system discrete");
  endif

  retsys = sys;
  retsys.tsam = tsam;

endfunction
