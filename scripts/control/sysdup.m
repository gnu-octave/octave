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
 
function retsys = sysdup(Asys,output_list,input_list)
# function retsys = sysdup(Asys,output_list,input_list)
# Duplicate specified input/output connections of a system
# 
#
# inputs:
#   Asys: system data structure (see ss2sys)
#   output_list,input_list: list of connections indices; 
#       duplicates are made of y(output_list(ii)) and u(input_list(ii)).
# output: retsys: resulting closed loop system:
#    duplicated i/o names are appended with a "+" suffix.
#
# Operation: sysdup creates copies of selected inputs and outputs as
# shown below.  u1/y1 is the set of original inputs/outputs, and 
# u2,y2 is the set of duplicated inputs/outputs in the order specified
# in input_list,output_list, respectively
#                      ____________________
#                      |                  |
#     u1         ----->|                  |----> y1
#                      |       Asys       |
#                      |                  |
#     u2 ------------->|                  |----->y2 
#     (input_list)     |                  |      (output_list)
#                      --------------------     

# A. S. Hodel August 1995
# modified by John Ingram July 1996
# $Revision: 1.2 $

  save_val = implicit_str_to_num_ok;	# save for later
  implicit_str_to_num_ok = 1;

  if( nargin != 3)
    usage("retsys = sysdup(Asys,output_list,input_list)");
  endif

  if( !is_struct(Asys))
    error("Asys must be a system data structure (see ss2sys, tf2sys, or zp2sys)")
  endif

  if (Asys.sys(4) != 1)
    Asys = sysupdate(Asys,'ss');
  endif

  mm = rows(Asys.inname);
  pp = rows(Asys.outname);

  # first duplicate inputs
  if(is_vector(input_list))
    for ii=1:length(input_list);
      Asys.b(:,mm+ii) = Asys.b(:,input_list(ii));
      Asys.d(:,mm+ii) = Asys.d(:,input_list(ii));
    end
  elseif(!isempty(input_list))
    error("input_list must be a vector or empty");
  endif


  # now duplicate outputs
  osize = min(size(output_list));
  if(osize == 1)
    for ii=1:length(output_list);
      Asys.c(pp+ii,:) = Asys.c(output_list(ii),:);
      Asys.d(pp+ii,:) = Asys.d(output_list(ii),:);
    end
  elseif(osize != 0)
    error("output_list must be a vector or empty");
  endif
  
  yd = Asys.yd(output_list);
  Asys.yd = [Asys.yd yd];

  # give default names to the added inputs
  for ii=(mm+1):(mm+length(input_list))
    orig_name = Asys.inname(input_list(ii-mm),:);

    #disp("sysdup: orig_name=")
    #orig_name
    #disp("/sysdup")

    strval = [dezero(orig_name),"(dup)"];
     
    #disp("sysdup: strval=")
    #strval
    #disp("/sysdup")

    Asys.inname(ii,1:length(strval)) = [strval];
     
    #disp("sysdup: resulting Asys.inname:")
    #Asys.inname
    #disp("/sysdup");

  endfor

  # give default names to the added outputs
  for jj=(pp+1):(pp+length(output_list))
    if(isstr(Asys.outname))
      orig_name =Asys.outname;
    else
      orig_name = Asys.outname(output_list(jj-pp),:);
    endif
    strval = [dezero(orig_name),"(dup)"];
    Asys.outname(jj,1:length(strval)) = [strval];

  endfor

    

  if(max(size(Asys.d)) > 1 )
    Asys.sys = [2 0 0 1];	# change default form to state space
				# tf and zp are no longer relevant
  endif

  retsys = Asys;

  implicit_str_to_num_ok = save_val;	# restore value

endfunction
