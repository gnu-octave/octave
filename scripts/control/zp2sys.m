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
 
function  outsys = zp2sys (zer,pol,k,tsam,inname,outname)
  # sys = zp2sys (zer,pol,k{,tsam,inname,outname})
  # Create system data structure from zero-pole data
  # inputs:
  #   zer: vector of system zeros
  #   pol: vector of system poles
  #   k: scalar leading coefficient
  #   tsam: sampling period. default: 0 (continuous system)
  #   inname, outname: input/output signal names (strings)
  # outputs: sys: system data structure

  #  Modified by John Ingram  July 20, 1996  
  # $Revision: 1.2 $

  save_val = implicit_str_to_num_ok;	# save for restoring later
  implicit_str_to_num_ok = 1;

  #  Test for the correct number of input arguments
  if ((nargin < 3) || (nargin > 6))
    usage("outsys = zp2sys(zer,pol,k[,tsam,inname,outname])");
  endif

  # check input format 
  if( ! (is_vector(zer) | isempty(zer) ) )
    error("zer must be a vector or empty");
  endif
  zer = reshape(zer,1,length(zer));		# make it a row vector

  if( ! (is_vector(pol) | isempty(pol)))
    error("pol must be a vector");
  endif
  pol = reshape(pol,1,length(pol));

  if (! is_scalar(k))
     error('k must be a scalar');
  endif

  #  Test proper numbers of poles and zeros.  The number of poles must be 
  #  greater than or equal to the number of zeros.
  if (length(zer) >  length(pol))
    error(["number of poles (", num2str(length(pol)), ...
	") < number of zeros (", num2str(length(zer)),")"]);
  endif

  #  Set the system transfer function
  outsys.zer = zer;
  outsys.pol = pol;
  outsys.k = k;

  #  Set the system vector:  active = 1, updated = [0 1 0];
  outsys.sys = [1 0 1 0];

  #  Set defaults
  outsys.tsam = 0;
   outsys.n = length(pol);
  outsys.nz = 0;
  outsys.yd = 0;	# assume (for now) continuous time outputs

  #  Set the type of system
  if (nargin > 3)
    if( !is_scalar(tsam) )
      error("tsam must be a nonnegative scalar");
    endif
    if (tsam < 0)
      error("sampling time must be positve")
    elseif (tsam > 0)
      [outsys.n,outsys.nz] = swap(outsys.n, outsys.nz);
      outsys.yd = 1;		# discrete-time output
    endif

    outsys.tsam = tsam;
  endif

  outsys.inname = sysdefioname(1,"u");
  outsys.outname = sysdefioname(1,"y");
  outsys.stname = sysdefstname(outsys.n,outsys.nz);

  #  Set name of input
  if (nargin > 4)
    if (rows(inname) > 1)
      warning("zp2sys: ",num2str(rows(inname))," input names given, 1st used");
      inname = inname(1,:);
    endif
    outsys.inname(1,1:length(inname)) = inname;
  endif

  #  Set name of output
  if (nargin > 5)
    if (rows(outname) > 1)
      warning("zp2sys: ",num2str(rows(outname)), ...
	" output names given, 1st used");
      outname = outname(1,:);  
    endif
    outsys.outname(1,1:length(outname)) = outname;  
  endif 

  implicit_str_to_num_ok = save_val;
endfunction
