# Copyright (C) 1996 A. Scottedward Hodel
#
# This file is part of Octave. 
#
# Octave is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 2, or (at your option) any
# later version.
#
# Octave is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public License
# along with Octave; see the file COPYING.  If not, write to the Free
# Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

function DEMOcontrol()
# Controls toolbox demo.
# Demo programs: bddemo.m, frdemo.m, analdemo.m, moddmeo.m, rldemo.m
#  
# Written by David Clem August 15, 1994
# $Revision: 2.0.0.0 $    

  disp(' O C T A V E    C O N T R O L   S Y S T E M S   T O O L B O X')

  while (1)
    clc
    k = 0;
    while (k > 8 || k < 1),
      k = menu("Octave Controls System Toolbox Demo", ...
	'System representation', ...
    	'Block diagram manipulations ', ...
    	'Frequency response functions ', ...
    	'State space analysis functions ', ...
    	'System model manipulations ', ...
    	'Root locus functions ', ...
	'LQG/H2/Hinfinity functions ', ...
    	'End');

    endwhile
    if(k == 1)
      sysrepdemo
    elseif (k == 2)
      bddemo
    elseif (k == 3)
      frdemo
    elseif (k == 4)
      analdemo
    elseif (k == 5)
      moddemo
    elseif (k == 6)
      rldemo
    elseif (k == 7)
      dgkfdemo
    elseif (k == 8)
      return
    endif
  endwhile
endfunction
