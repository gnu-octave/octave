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
 
function names = sysgroupn(names,kind)
# names = sysgroupn(names)
# locate and mark duplicate names
#
#  used internally in sysgroup

# $Revision: 1.1 $

  #disp("sysgroupn: entry")
  #names
  #[lmatrws,lmatcls] = size(names)
  #disp("/sysgroupn")

  # check for duplicate names
  l = rows(names);
  if(l > 1)
    for ii = 1:(l-1);
      #disp(["sysgroupn: ii=",num2str(ii)])
      #names
      #[lmatrws,lmatcls] = size(names)
      #disp("/sysgroupn")
      st1 = dezero(names(ii,:));
      for jj = (ii+1):l
	st2 = dezero(names(jj,:));
        if(strcmp(st1,st2))
          suffix = ["_",num2str(jj)];
          warning(["sysgroup: Appending ",suffix," to duplicate ",kind,...
		" name '",st2,"'."]);
          strval = [st2,suffix];

          #disp(["sysgroupn: length(strval)=",num2str(length(strval))]);
	  #disp(["sysgroupn: length(st2)=",num2str(length(st2))]);

	  names(jj,(1:length(strval))) = strval;
        endif
      endfor
    endfor
  endif
endfunction
