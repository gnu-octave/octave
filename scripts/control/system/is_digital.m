## Copyright (C) 1996, 1999, 2000, 2004, 2005, 2006, 2007
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
## @deftypefn {Function File} {@var{digital} =} is_digital (@var{sys}, @var{eflg})
## Return nonzero if system is digital.
##
## @strong{Inputs}
## @table @var
## @item sys
## System data structure.
## @item eflg
## When equal to 0 (default value), exits with an error if the system 
## is mixed (continuous and discrete components); when equal to 1, print
## a warning if the system is mixed (continuous and discrete); when equal
## to 2, operate silently.
## @end table
##
## @strong{Output}
## @table @var
## @item digital
## When equal to 0, the system is purely continuous; when equal to 1, the
## system is purely discrete; when equal to -1, the system is mixed continuous 
## and discrete.
## @end table
## Exits with an error if @var{sys} is a mixed (continuous and discrete) system.
## @end deftypefn

## Author: A. S. Hodel <a.s.hodel@eng.auburn.edu>
## Created: July 1996

function DIGITAL = is_digital (sys, eflg)

  switch(nargin)
  case(1),  eflg = 0;
  case(2),
    if( isempty(find(eflg == [0, 1, 2])) )
      error("invalid value of eflg=%d (%e)",eflg,eflg);
    endif
  otherwise,
    print_usage ();
  endswitch

  ## checked for sampled data system (mixed)
  ## discrete system
  sysyd = sysgetsignals(sys,"yd");
  [nn,nz] = sysdimensions(sys);
  cont = sum(sysyd == 0) + nn;
  tsam = sysgettsam(sys);
  dig = sum(sysyd != 0) + nz + tsam;

  ## check for mixed system
  if( cont*dig != 0)
   switch(eflg)
   case(0),
     error("continuous/discrete system; use syscont, sysdisc, or c2d first");
   case(1),
     warning("is_digital: mixed continuous/discrete system");
   endswitch
   dig_sign = -1;
  else
   dig_sign = 1;
  endif

  DIGITAL = dig_sign*(tsam > 0);

endfunction
