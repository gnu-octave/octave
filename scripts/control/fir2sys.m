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
 
function sys = fir2sys (num,tsam,inname,outname)
  #
  # outsys = fir2sys(num,{tsam,inname,outname})
  # construct a system data structure from FIR description
  # inputs:
  #   num: vector of coefficients [c0 c1 ... cn] of the SISO FIR transfer
  #        function C(z) = c0 + c1*z^{-1} + c2*z^{-2} + ... + znz^{-n}
  #   tsam: sampling time (default: 1)
  #   inname: name of input signal 
  #   outname: name of output signal
  # outputs:  sys (system data structure)
   
  #  Written by R. Bruce Tenison  July 29, 1994
  #  Name changed to TF2SYS July 1995
  #  updated for new system data structure format July 1996
  # adapted from tf2sys july 1996
  # $Revision: 1.1.1.1 $

  save_val = implicit_str_to_num_ok;
  implicit_str_to_num_ok = 1;

  #  Test for the correct number of input arguments
  if ((nargin < 2) || (nargin > 4))
    usage('sys=fir2sys(num[,tsam,inname,outname])');
    return
  endif

  # check input format 
  if( !is_vector(num) )
    error(['num (',num2str(rows(num)),'x',num2str(columns(num)), ...
	') must be a vector'])
  endif

  den = [1,zeros(1,length(num)-1)];

  # check sampling interval (if any)
  if(nargin <= 1)
    tsam = 1;		# default
  elseif (isempty(tsam))
    tsam = 1;
  endif
  if ( (! (is_scalar(tsam) && (imag(tsam) == 0) )) || (tsam <= 0) )
    error('fir tsam must be a positive real scalar')
  endif

  #  Set name of input
  if(nargin < 3)
    inname = "u";
  elseif(isempty(inname))
    inname = "u";
  endif
  if (rows(inname) > 1)
    warning(['fir2sys:,' num2str(rows(inname)),' input names given, 1st used.'])
    inname = (inname(1,:));
  endif

  #  Set name of output
  if(nargin < 4)
    outname = "y";
  elseif(isempty(outname))
    outname = "y";
  endif
  if (rows(outname) > 1)
    warning(['fir2sys: ',num2str(rows(outname)),...
      ' output names given, 1st used.'])
    outname = (outname(1,:));
  endif
  
  sys.num = num;
  sys.den = den;

  #  Set the system vector:  active = 0(tf), updated = [1 0 0];
  sys.sys = [0 1 0 0];

  #  Set defaults
  sys.tsam = tsam;
  sys.inname = inname;
  sys.outname = outname;
  sys.nz = length(den)-1;
  sys.n = 0;
  sys.yd = 1;

  implicit_str_to_num_ok = save_val;
endfunction
