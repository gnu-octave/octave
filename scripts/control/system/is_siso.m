## Copyright (C) 1996 Auburn University.  All rights reserved.
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by the
## Free Software Foundation; either version 2, or (at your option) any
## later version.
##
## Octave is distributed in the hope that it will be useful, but WITHOUT
## ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
## FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
## for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301 USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} is_siso (@var{sys})
## Returns nonzero if the system data structure
## @var{sys} is single-input, single-output.
## @end deftypefn

## Author: A. S. Hodel <a.s.hodel@eng.auburn.edu>
## Created: July 1996, 1998

function  SISO = is_siso (sys)

  if (nargin != 1)
    usage ("SISO = is_siso (sys)");
  elseif (! isstruct (sys))
    error ("input must be a system structure (see ss, tf, zp)");
  endif

  [n, nz, m, p] = sysdimensions (sys);

  SISO = (m == 1 && p == 1);

endfunction
