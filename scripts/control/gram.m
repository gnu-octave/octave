# Copyright (C) 1996 Auburn University.  All Rights Reserved
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
# Software Foundation, 59 Temple Place, Suite 330, Boston, MA 02111 USA. 

## -*- texinfo -*-
## @deftypefn {Function File } { @var{m} =} gram (@var{a}, @var{b})
##  Return controllability grammian @var{m} of the continuous time system
## @math{ dx/dt = a x + b u}.  
## 
## @var{m} satisfies @math{ a m + m a' + b b' = 0 }.
## @end deftypefn

function m = gram(a,b)
  # Written by A. S. Hodel 

  # let lyap do the error checking...
  m = lyap(a,b*b');
endfunction
