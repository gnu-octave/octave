# Copyright (C) 1996 Auburn University.  All Rights Reserved.
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
## @deftypefn {Function File } { } syschnamesl 
##  used internally in syschnames
##  item olist: index list 
##  old_names: original list names
##  inames: new names
##  listname: name of index list
## 
##  combines the two string lists old_names and inames
## @end deftypefn
 
function old_names = syschnamesl(olist,old_names,inames,listname)
  # $Revision: 2.1.14.5 $
  # $Log: syschnamesl.m,v $
# Revision 2.1.14.5  1999/09/22  21:55:46  scotte
# Auburn copyright fixed; krylov.m patched to fix bug
#
# Revision 2.1.14.4  1999/07/21  19:49:21  scotte
# sysgroup, sysadd, sysmult, syssub accept variable # of input args
#
  # Revision 1.3  1998/07/17 15:08:50  hodelas
  # use isempty instead of max(size(...))
  #
  # Revision 1.2  1998/07/01 16:23:39  hodelas
  # Updated c2d, d2c to perform bilinear transforms.
  # Updated several files per bug updates from users.
  #
  # Revision 1.5  1997/02/28 23:47:26  hodel
  # fixed bug in checking parameters; (inames dimension not checked against olist)
  # a.s.hodel@eng.auburn.edu
  #
  # Revision 1.4  1997/02/13 15:56:37  hodel
  # added code to convert zeros in the name matrix to blanks
  # a.s.hodel@eng.auburn.edu
  #
  # Revision 1.3  1997/02/13 15:11:17  hodel
  # fixed bug when len_my < len_out a.s.hodel@eng.auburn.edu
  #
  # Revision 1.2  1997/02/12 22:54:50  hodel
  # fixed string matrix <-> numerical matrix problem
  #
  
  probstr = [];
  if( max(olist) > rows(old_names) )
    probstr = ["index list value(s) exceed(s) number of signals (", ...
      num2str(rows(old_names)),")"];

  elseif( length(olist) > rows(inames) )
    probstr = ["index list dimension exceeds number of replacement names (", ...
      num2str(rows(inames)),")"];

  elseif(isempty(olist))
    probstr = [];    # do nothing, no changes

  elseif(min(size(olist)) != 1 )
    probstr = "index list must be either a vector or an empty matrix";

  elseif(max(olist) > rows(old_names))
    probstr = ["max(",listname,")=",num2str(max(olist))," > ", ...
	num2str(rows(old_names)),", too big"];

  elseif(min(olist) < 1)
    probstr = ["min(",listname,")=",num2str(min(olist))," < 1, too small"];

  else
    if( length(olist)  == 1)
	len_in = columns(inames);
	len_out = columns(old_names);

      if (len_in < len_out)
        inames(1,(len_in+1):(len_out)) = zeros(1,(len_out - len_in));
      endif

      old_names(olist,1:length(inames)) = inames;
    elseif(length(olist) > 1)
      for ii=1:length(olist)
        mystr = inames(ii,:);
	len_my = columns(mystr);
        len_out = columns(old_names);
       
        if (len_my < len_out)
          mystr(1,(len_my+1):(len_out)) = " "*ones(1,(len_out - len_my));
	  len_my = len_out;
        endif

        old_names(olist(ii),1:len_my) = mystr;
      endfor
    endif
  endif
  if(!isempty(probstr))
    # the following lines are NOT debugging code!
    disp("Problem in syschnames: old names are")
    outlist(old_names,"	")
    disp("new names are")
    outlist(inames,"	")
    disp("list indices are")
    disp(olist)
    error(sprintf("syschnames: \"%s\" dim=(%d x %d)--\n\t%s\n", ...
	listname, rows(olist), columns(olist),probstr));
  endif

  # change zeros  to blanks
  if( find(old_names == 0) )
    #disp("syschnamesl: old_names contains zeros ")
    #old_names
    #disp("/syschnamesl");

    [ii,jj] = find(old_names == 0);
    for idx=1:length(ii)
      old_names(ii(idx),jj(idx)) = " ";
    endfor

    #disp("syschnamesl: old_names fixed zeros ")
    #old_names
    #disp("/syschnamesl");
  endif

  # just in case it's not a string anymore
  if( !isstr(old_names) )
    old_names = setstr(old_names);
  endif
  
  #disp("syschnamesl: exit, old_names=")
  #old_names
  #disp("/syschnamesl: exiting")
  
endfunction
