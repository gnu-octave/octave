# Copyright (C) 1996,1998 A. Scottedward Hodel 
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
 
function retsys = syschnames(sys,opt,list,names)
# retsys = syschnames(sys,opt,list,names)
# change the names of selected inputs, outputs and states.
# inputs:
# 	sys: system data structure
#	opt: []: change default name (output)
#	     "out": change selected output names
#	     "in": change selected input names
#	     "st": change selected state names	 
#	     "yd": change selected outputs from discrete to continuous or 
#		   from continuous to discrete.
#
#     	list: vector of indices of outputs, yd, inputs, or
#             states whose respective names should be changed
#
#    	names: strings or string arrays containing
#              names corresponding to the lists above.  To
# 	       change yd, use a vector.  Set the name to 0 for continuous, 
#	       or 1 for discrete.
# outputs:
#    retsys=sys with appropriate signal names changed 
#            (or yd values, where appropriate)

# Written by John Ingram August 1996; updated by A. S. Hodel 1998
# $Revision: 2.0.0.2 $

  retsys = syssetsignals(sys,opt,names,list);

endfunction
