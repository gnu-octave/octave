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
 
function [wmin,wmax] = bode_bounds(zer,pol,DIGITAL,tsam)
# function [wmin,wmax] = bode_bounds(zer,pol,DIGITAL{,tsam})
# get default range of frequencies for system zeros and poles
#
# frequency range is the interval [10^wmin,10^wmax]
#
# used internally in freqresp

# $Revision: 1.4 $
# $Log: bode_bounds.m,v $
# Revision 1.4  1998/10/05 17:12:56  hodelas
# various bug changes
#
# Revision 1.2  1998/08/18 21:21:19  hodelas
# updated for simpler interface
#
#
# Revision 1.2  1997/11/24  15:39:38  mueller
# floating overflow on digital systems fixed
# The overflow occurs if the system has poles or zeros at 0 (log(0)/tsamp)
#
# Revision 1.1  1997/11/24  15:36:31  mueller
# Initial revision
#

  # make sure zer,pol are row vectors
  if(!isempty(pol)) pol = reshape(pol,1,length(pol)); endif
  if(!isempty(zer)) zer = reshape(zer,1,length(zer)); endif

# check for natural frequencies away from omega = 0
  if (DIGITAL)
    # The 2nd conditions prevents log(0) in the next log command
    iiz = find(abs(zer - 1) > norm(zer) * eps && abs(zer) > norm(zer) * eps);
    iip = find(abs(pol - 1) > norm(pol) * eps && abs(pol) > norm(pol) * eps);

    # avoid dividing empty matrices, it would work but looks nasty
    if (!isempty(iiz)) czer = log(zer(iiz))/tsam;
    else               czer = [];                 endif

    if (!isempty(iip)) cpol = log(pol(iip))/tsam;
    else 	       cpol = [];                 endif

  else
    # continuous
    iip = find((abs(pol)) > (norm(pol) * eps));
    iiz = find((abs(zer)) > (norm(zer) * eps));

    if(!isempty(zer)) czer = zer(iiz);
    else              czer = [];                endif
    if(!isempty(pol)) cpol = pol(iip);
    else              cpol = [];                endif
  endif

  if(max(size(iip)) + max(size(iiz)) )
    wmin = floor(log10(min(abs([cpol,czer]))));
    wmax = ceil(log10(max(abs([cpol,czer]))));
  else
    # no poles/zeros away from omega = 0; pick defaults
    wmin = -1;
    wmax = 3;
  endif

  # expand to show the entirety of the "interesting" portion of the plot
  wmin--;
  wmax++;

  # run digital frequency all the way to pi
  if (DIGITAL) wmax = log10(pi/tsam); endif
endfunction
