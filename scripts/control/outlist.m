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
 
function outlist(lmat,tabchar,yd,ilist)
# function outlist(lmat[,tabchar,yd,ilist])
#
# internal use only; minimal argument checking performed
#
# print an enumerated list of strings
# inputs:
#	lmat: matrix of strings (one per row)
#	tabchar: tab character (default: none)
#       yd: indices of strings to append with the string "(discrete)"
#           (used by sysout; minimal checking of this argument)
#	   yd = [] => all continuous
#       ilist: index numbers to print with names
#	  default: 1:rows(lmat)
# outputs:
#   prints the list to the screen, numbering each string in order.

# A. S. Hodel Dec. 1995
# $Revision: 1.1.1.1 $

save_val = implicit_str_to_num_ok;	# save for later
implicit_str_to_num_ok = 1;

#save for restore later
save_empty = empty_list_elements_ok;
empty_list_elements_ok = 1;

if( (nargin < 1) || (nargin > 4) )
  usage("outlist(x[,tabchar,yd,ilist])");
endif

[m,n] = size(lmat);
if(nargin < 4)
  ilist = 1:m;
endif
if(nargin ==1)
  empty_list_elements_ok = 1;
  tabchar = "";
endif

if(nargin < 3)
  yd = zeros(1,m);
elseif(isempty(yd))
  yd = zeros(1,m);
endif

if((m >= 1) && (isstr(lmat)))
  for ii=1:m
    str = dezero([lmat(ii,:),setstr((yd(ii)*" (discrete)"))]);
    #disp(["length(str)=",num2str(length(str))])
    disp([tabchar,num2str(ilist(ii)),": ",str])
  endfor
else
  disp([tabchar,"None"])
endif

empty_list_elements_ok = save_empty;
implicit_str_to_num_ok = save_val;	# restore value
endfunction
