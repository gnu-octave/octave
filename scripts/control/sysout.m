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
 
function retsys = sysout(sys,opt)
# function sysout(sys[,opt])
# print out a system data structure in desired format
#
# sys: system data structure
# opt: []: primary system form (default)
#      "ss": state space form
#      "tf": transfer function form
#      "zp": zero-pole form
#      "all": all of the above

# Written by A S Hodel: 1995-1996
# $Revision: 1.2 $

# save for restoring at end of routine
save_val = implicit_str_to_num_ok;
implicit_str_to_num_ok = 1;

if( (nargin < 1) || (nargin > 2) )
  usage("sysout(sys[,opt])");
endif

if(isempty(sys))
  retsys = sys;
  warning("sysout: empty system")
  return;
endif

if(! is_struct(sys))
  disp("sysout: input must be a system structure")
endif

# set up output type array
farray = ["tf";"zp";"ss"];

if( nargin == 1 )
  opt = farray(sys.sys(1)+1,:);
else
  if( ! (strcmp(opt,"ss") + strcmp(opt,"tf") + ...
    strcmp(opt,"zp") + strcmp(opt,"all") ) )
    error("opt must be one of [], \"ss\", \"tf\", \"zp\", or \"all\"");
  endif
endif

# now check output for each form:
if( !isempty(sys.inname) )
  disp("Input(s)")
  outlist(sys.inname,"	")
else
  disp("Input(s): none");
endif
if ( ! isempty(sys.outname) )
  disp("Output(s):")
  outlist(sys.outname,"	",sys.yd)
else
  disp("Output(s): none");
endif
if(sys.tsam > 0)
  disp(["Sampling interval: ",num2str(sys.tsam)]);
  str = "z";
else
  str = "s";
endif

# transfer function form
if( strcmp(opt,"tf") + strcmp(opt,"all") )
  sys = sysupdate(sys,"tf");		#make sure tf is up to date
  disp("transfer function form:")
  tfout(sys.num,sys.den,str);
endif

if( strcmp(opt,"zp") + strcmp(opt,"all") )
  sys = sysupdate(sys,"zp");		#make sure zp is up to date
  disp("zero-pole form:")
  zpout(sys.zer, sys.pol,sys.k,str)
endif

if( strcmp(opt,"ss") + strcmp(opt,"all") )
  sys = sysupdate(sys,"ss");
  disp("state-space form:");
  disp([num2str(sys.n)," continuous states, ",  ...
    num2str(sys.nz)," discrete states"]);
  if( !isempty(sys.stname) )
    disp("State(s):")
    xi = (sys.n+1):(sys.n+sys.nz);
    xd = zeros(1,rows(sys.a));
    if(!isempty(xi))
      xd(xi) = 1;
    endif
    outlist(sys.stname,"	",xd);
  else
    disp("State(s): none");
  endif

  # display matrix values?
  dmat = (max( [ size(sys.a) size(sys.b) size(sys.c) size(sys.d) ] ) <= 32);

  disp(sprintf("A matrix: %d x %d",rows(sys.a),columns(sys.a)))
  if(dmat)
    disp(sys.a)
  endif

  disp(sprintf("B matrix: %d x %d",rows(sys.b),columns(sys.b)))
  if(dmat)
    disp(sys.b)
  endif

  disp(sprintf("C matrix: %d x %d",rows(sys.c),columns(sys.c)))
  if(dmat)
    disp(sys.c)
  endif
  disp(sprintf("D matrix: %d x %d",rows(sys.d),columns(sys.d)))
  if(dmat)
    disp(sys.d)
  endif
endif

if(nargout >= 1)
  retsys = sys;
endif 
  
# restore global variable
implicit_str_to_num_ok = save_val;

endfunction
