## Copyright (C) 1998 Auburn University.  All rights reserved.
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
## Software Foundation, 59 Temple Place, Suite 330, Boston, MA 02111 USA.

## -*- texinfo -*-
## @deftypefn{Function File} {@var{axvec} =} axis2dlim (@var{axdata})
## determine axis limits for 2-d data(column vectors); leaves a 10% margin
## around the plots.
## puts in margins of +/- 0.1 if data is one dimensional (or a single point)
##
## @strong{Inputs}
## @var{axdata} nx2 matrix of data [x,y]
##
## @strong{Outputs}
## @var{axvec} vector of axis limits appropriate for call to axis() function
## @end deftypefn

function axvec = axis2dlim (axdata)

  if(isempty(axdata))
    axdata = 0;
  endif

  ## compute axis limits
  minv = min(axdata);
  maxv = max(axdata);
  delv = (maxv-minv)/2;      # breadth of the plot
  midv = (minv + maxv)/2;    # midpoint of the plot
  axmid = [midv(1), midv(1), midv(2), midv(2)];
  axdel = [-0.1, 0.1,-0.1,0.1];   # default plot width (if less than 2-d data)
  if(max(delv) == 0)
    if(midv(1) != 0)
      axdel(1:2) = [-0.1*midv(1),0.1*midv(1)];
    endif
    if(midv(2) != 0)
      axdel(3:4) = [-0.1*midv(2),0.1*midv(2)];
    endif
  else
    ## they're at least one-dimensional
    if(delv(1) != 0)
      axdel(1:2) = 1.1*[-delv(1),delv(1)];
    endif
    if(delv(2) != 0)
      axdel(3:4) = 1.1*[-delv(2),delv(2)];
    endif
  endif
  axvec = axmid + axdel;
endfunction

