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
 
function  outsys = ss2sys  (a,b,c,d,tsam,n,nz,stname,inname,outname,outlist)
  # outsys = ss2sys  (a,b,c{,d,tsam,n,nz,stname,inname,outname,outlist})
  # Create system structure from state-space data.   May be continous,
  # discrete, or mixed (sampeld-data)
  # inputs:
  #   a, b, c, d: usual state space matrices.
  #               default: d = zero matrix
  #   tsam: sampling rate.  Default: tsam = 0 (continuous system)
  #   n, nz: number of continuous, discrete states in the system
  #        default: tsam = 0: n = rows(a), nz = 0
  #                 tsam > 0: n = 0,       nz = rows(a), n 
  #        see below for system partitioning
  #   stname: string matrix of state signal names
  #           default (stname=[] on input): x_n for continuous states,
  #                    xd_n for discrete states
  #   inname: string matrix of input signal names
  #           default (inname = [] on input): u_n
  #   outname: string matrix of input signal names
  #           default (outname = [] on input): y_n
  #   outlist: list of indices of outputs y that are sampled
  #           default: (tsam = 0)  outlist = []
  #                    (tsam > 0)  outlist = 1:rows(c)
  #           Unlike states, discrete/continous outputs may appear
  #           in any order.
  #           Note: sys2ss returns a vector yd where
  #                 yd(outlist) = 1; all other entries of yd are 0.
  #
  #  System partitioning: Suppose for simplicity that outlist specified
  #  that the first several outputs were continuous and the remaining outputs
  #  were discrete.  Then the system is partitioned as
  #   x = [ xc ]  (n x 1)
  #       [ xd ]  (nz x 1 discrete states)
  #   a = [ acc acd ]    b = [ bc ]
  #       [ adc add ]        [ bd ]
  #   c = [ ccc  ccd  ]    d = [ dc ]
  #       [ cdc  cdd  ]    d = [ dd ]  (cdc = c(outlist,1:n), etc.)
  # 
  # with dynamic equations:
  #   
  #  d/dt xc(t)     = acc*xc(t)      + acd*xd(k*tsam) + bc*u(t)
  #  xd((k+1)*tsam) = adc*xc(k*tsam) + add*xd(k*tsam) + bd*u(k*tsam)
  #  yc(t)      = ccc*xc(t)      + ccd*xd(k*tsam) + dc*u(t)
  #  yd(k*tsam) = cdc*xc(k*tsam) + cdd*xd(k*tsam) + dd*u(k*tsam)
  #  
  #  signal partitions: 
  #            | continuous      | discrete               |
  #  ------------------------------------------------------
  #  states    | stname(1:n,:)   | stname((n+1):(n+nz),:) |
  #  ------------------------------------------------------
  #  outputs   | outname(cout,:) | outname(outlist,:)     |
  #  ------------------------------------------------------
  #
  #  where cout = list if indices in 1:rows(p) not contained in outlist.
  #

  #  Written by John Ingram (ingraje@eng.auburn.edu)  July 20, 1996
  # $Revision: 1.3 $
  # $Log: ss2sys.m,v $
  # Revision 1.3  1998/07/01 20:55:08  hodelas
  # Updated sysgroup, sys2ss, ss2sys to use system structure interface
  #
  # Revision 1.4  1997/03/11 15:19:27  scotte
  # fixed warning message about inname dimensions a.s.hodel@eng.auburn.edu
  #

  save_val = implicit_str_to_num_ok;	# save for later
  implicit_str_to_num_ok = 1;

  #  Test for correct number of inputs
  if ((nargin < 3) | (nargin > 11))
    error("Incorrect number of arguments");
  endif

  # verify A, B, C, D arguments
  #  If D is not specified, set it to a zero matrix of appriate dimension.
  if (nargin == 3) 
    d = zeros(rows(c) , columns(b));
  elseif (isempty(d))
    d = zeros(rows(c) , columns(b));
  endif

  #  Check the dimensions
  [na,m,p] = abcddim(a,b,c,d);

  #  If dimensions are wrong, exit function
  if (m == -1)
    error("a,b,c,d matrix dimensions are not compatible");
  endif

  # check for tsam input
  if(nargin < 5)
    tsam = 0;
  elseif( !( is_sample(tsam) | (tsam == 0) ) )
    error("tsam must be a nonnegative real scalar");
  endif

  # check for continuous states
  if( (nargin < 6) & (tsam == 0) )
    n = na;
  elseif(nargin < 6)
    n = 0;
  elseif( (!is_scalar(n)) | (n < 0 ) | (n != round(n)) )
    if(is_scalar(n))
      error(["illegal value of n=",num2str(n)]);
    else
      error(["illegal value of n=(",num2str(rows(n)),"x", ...
	num2str(columns(n)),")"]);
    endif
  endif

  # check for num discrete states
  if( (nargin < 7) & (tsam == 0))
    nz = 0;
  elseif(nargin < 7)
    nz = na - n;
  elseif( (!is_scalar(nz)) | (nz < 0 ) | (nz != round(nz)) )
    if(is_scalar(nz))
      error(["illegal value of nz=",num2str(nz)]);
    else
      error(["illegal value of nz=(",num2str(rows(nz)),"x", ...
	num2str(columns(nz)),")"]);
    endif
  endif

  #check for total number of states
  if( (n + nz) != na )
    error(["Illegal: a is ",num2str(na),"x",num2str(na),", n=", ...
	num2str(n),", nz=",num2str(nz)]);
  endif

  # check for state names
  if(nargin < 8)
    stname = sysdefstname(n,nz);
  else
    nr = rows(stname);
    if(na != nr)
      error(["ss2sys: ",num2str(na),"system states,", ...
	num2str(nr)," state names provided"]);
    endif
  endif

  #check for input names
  if(nargin < 9)
    inname = sysdefioname(m,"u");
  elseif( !isstr(inname) )
    warning("ss2sys: inname=")
    disp(inname);
    error("inname must be a string or string matrix.");
  elseif(rows(inname) != m )
    warning("ss2sys: inname=")
    disp(inname);
    error(["inname has ",num2str(rows(inname))," rows, sys has ", ...
      num2str(m)," inputs."]);
  endif

  #check for output names
  if(nargin < 10)
    outname = sysdefioname(p,"y");
  elseif( !isstr(outname) )
    warning("ss2sys: outname=")
    disp(outname);
    error("outname must be a string or string matrix.");
  elseif(rows(outname) != p )
    warning("ss2sys: outname=")
    disp(outname);
    error(["outname has ",num2str(rows(outname))," rows, sys has ", ...
      num2str(p)," outputs."]);
  endif

  # set up yd
  if(nargin < 11)
    yd = ones(1,p)*(tsam > 0);
  else
    yd = zeros(1,p);
    yd(outlist) = ones(1,length(outlist));
    if(max(outlist) > p)
      error(["max outlist index=",num2str(max(outlist)), ...
	" exceeds number of outputs=",num2str(p)]);
    endif
  endif

  # Construct the state space system
  outsys.a = a; 
  outsys.b = b; 
  outsys.c = c; 
  outsys.d = d;

  outsys.n = n;
  outsys.nz = nz;
  outsys.tsam = tsam;
  outsys.yd = yd;

  outsys.stname = stname;
  outsys.inname = inname;
  outsys.outname = outname;

  #  Set the system vector:  active = 2(ss), updated = [0 0 1];
  outsys.sys = [2 0 0 1]; 

  implicit_str_to_num_ok = save_val;	# restore value
endfunction
