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
 
function sys = sysprune(sys,output_list,input_list)
# function retsys = sysprune(Asys,output_list,input_list)
# Extract specified inputs/outputs from a system
#
# inputs:
#   Asys: system data structure 
#   output_list,input_list: list of connections indices; the new
#       system has outputs y(output_list(ii)) and inputs u(input_list(ii)).
#       May select as [] (empty matrix) to specify all outputs/inputs.
#
# output: retsys: resulting system:
#                      ____________________
#                      |                  |
#     u1         ----->|                  |----> y1
#    (input_list)      |       Asys       | (output_list)
#                      |                  |
#   u2 (deleted) |---->|                  |----| y2  (deleted)
#                      |                  |    
#                      --------------------    

# A. S. Hodel August 1995
# Updated by John Ingram 7-15-96
# $Revision: 1.2 $

  if( nargin != 3  )
    usage("retsys = sysprune(sys,output_list,input_list)");
  endif

  # default: no action
  if(isempty(output_list))
    outputlist = 1:rows(sys.outname);
  endif
  if(isempty(input_list))
    input_list = 1:rows(sys.inname);
  endif

  # check dimensions
  if( !(
	(is_vector(output_list) | isempty(output_list)) 
	& (is_vector(input_list) | isempty(input_list)) 
  ))
    error("sysprune: output_list and input_list must be vectors");
  else
    lo = length(output_list);
    li = length(input_list);
    
    if(is_duplicate_entry(output_list) || is_duplicate_entry(input_list) )
      error("sysprune: duplicate entry in input and/or output list");
    endif
  endif
  
  m = rows(sys.inname);
  p = rows(sys.outname);

  if( !is_struct(sys))
    error("Asys must be a system data structure (see ss2sys, tf2sys, or zp2sys)")
  elseif(p < lo)
    error([num2str(lo)," output_list entries, system has only ", ...
	num2str(p)," outputs"]);
  elseif(m < li)
    error([num2str(li)," input_list entries, system has only ", ...
	num2str(m)," inputs"]);
  endif

  sys = sysupdate(sys,"ss");

  sys.b = sys.b(:,input_list);
  sys.c = sys.c(output_list,:);
  sys.d = sys.d(output_list,input_list);

  sys.inname = sys.inname(input_list,:);
  sys.outname = sys.outname(output_list,:); 
  sys.yd = sys.yd(output_list); 
  
endfunction
