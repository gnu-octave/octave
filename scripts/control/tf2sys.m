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
 
function outsys = tf2sys(num,den,tsam,inname,outname)
  #
  # sys = tf2sys(num,den{,tsam,inname,outname})
  # build system data structure from transfer function format data
  # inputs:
  #   num, den: coefficients of numerator/denominator polynomials
  #   tsam: sampling interval. default: 0 (continuous time)
  #   inname, outname: input/output signal names (string variables)
  # outputs: sys = system data structure
   
  #  Written by R. Bruce Tenison  July 29, 1994
  #  Name changed to TF2SYS July 1995
  #  updated for new system data structure format July 1996
  # $Revision: 1.4 $
  # $Log: tf2sys.m,v $
  # Revision 1.4  1998/08/24 15:50:30  hodelas
  # updated documentation
  #
  # Revision 1.2  1998/07/01 16:23:39  hodelas
  # Updated c2d, d2c to perform bilinear transforms.
  # Updated several files per bug updates from users.
  #
  # Revision 1.2  1997/02/12 22:45:57  hodel
  # added debugging code (commented out)
  #

  save_val = implicit_str_to_num_ok;
  implicit_str_to_num_ok = 1;

  #  Test for the correct number of input arguments
  if ((nargin < 2) || (nargin > 5))
    usage('outsys=tf2sys(num,den[,tsam,inname,outname])');
    return
  endif

  # check input format 
  if( ! ( (is_vector(num) || is_scalar(num)) && ...
	(is_vector(den) || is_scalar(den))) )
    error(['num (',num2str(rows(num)),'x',num2str(columns(num)), ...
      ') and den (',num2str(rows(den)),'x',num2str(columns(den)), ...
      ') must be vectors'])
  endif
  
  # strip leading zero coefficients
  num = tf2sysl(num);
  den = tf2sysl(den);

  if (length(num) >  length(den))
    error([ 'number of poles (', num2str(length(den)-1), ...
	') < number of zeros (', num2str(length(num)-1),')']);
  endif

  # check sampling interval (if any)
  if(nargin <= 2)
    tsam = 0;		# default
  elseif (isempty(tsam))
    tsam = 0;
  endif
  if ( (! (is_scalar(tsam) && (imag(tsam) == 0) )) || (tsam < 0) )
    error('tsam must be a positive real scalar')
  endif

  outsys.num = num;
  outsys.den = den;

  #  Set the system vector:  active = 0(tf), updated = [1 0 0];
  outsys.sys = [0 1 0 0];

  #  Set defaults
  outsys.tsam = tsam;
  outsys.n = length(den)-1;
  outsys.nz = 0;
  outsys.yd = 0;	# assume discrete-time
  # check discrete time
  if(tsam > 0)
    [outsys.n,outsys.nz] = swap(outsys.n, outsys.nz);
    outsys.yd = 1;
  endif

  outsys.inname = sysdefioname(1,"u");
  outsys.outname = sysdefioname(1,"y");
  outsys.stname = sysdefstname(outsys.n,outsys.nz);

  #  Set name of input
  if (nargin > 3)
    if (rows(inname) > 1)
      warning(["tf2sys: ",num2str(rows(inname))," input names given, 1st used"]);
      inname = inname(1,:);
    endif
    outsys.inname(1,1:length(inname)) = inname;
  endif

  #  Set name of output
  if (nargin > 4)
    if (rows(outname) > 1)
      warning(["tf2sys: ",num2str(rows(outname)), ...
	" output names given, 1st used"]);
      outname = outname(1,:);  
    endif
    outsys.outname(1,1:length(outname)) = outname;  
  endif 

  #disp("tf2sys: returning")
  #outsys
  #disp("/tf2sys")
  
  implicit_str_to_num_ok = save_val;
endfunction
