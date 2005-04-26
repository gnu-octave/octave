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
## @deftypefn {Function File} {[@var{c}, @var{tsam}, @var{input}, @var{output}] =} sys2fir (@var{sys})
##
## Extract @acronym{FIR} data from system data structure; see @command{fir2sys} for
## parameter descriptions.
## @end deftypefn
##
## @seealso{fir2sys}

## Author: A. S. Hodel <a.s.hodel@eng.auburn.edu>
## Created: July 1996

function [c, tsam, inname, outname] = sys2fir (sys)

  ## let sys2tf do most of the work

  [num, den, tsam, inname, outname] = sys2tf (sys);

  alph = den(1);                        # scale to get monic denominator
  den /= alph;
  num /= alph;
  l = length (den);
  m = length (num);
  if (norm (den(2:l)))
    sysout (sys, "tf");
    error ("denominator has poles away from origin");
  elseif (! is_digital (sys))
    error ("system must be discrete-time to be FIR");
  elseif (m != l)
    warning ("sys2fir: deg(num) - deg(den) = %d; coefficients must be shifted",
	     m-l);
  endif
  c = num;

endfunction

