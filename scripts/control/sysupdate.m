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
 
function sys = sysupdate(sys,opt)
# function retsys = sysupdate(sys,opt)
# Update the internal representation of a system.
# inputs:
#  sys: system data structure
#  opt: string:  "tf" -> update transfer function
#                "zp" -> update zero-pole form
#                "ss" -> update state space form
#                "all" -> all of the above
# outputs: retsys: contains union of data in sys and requested data.
#     if requested data in sys is already up to date then retsys=sys.
#
# conversion to tf or zp exits with an error if the system is
# mixed continuous/digital
#
# see also: tf2sys, ss2sys, zp2sys, sysout, sys2ss, sys2tf, sys2zp

# Written by John Ingram  7-9-96
# $Revision: 1.1.1.1 $

  # check for correct number of inputs 
  if (nargin != 2)
    usage("newsys = sysupdate(sys,opt)");
  elseif(! is_struct(sys) )
   error("1st argument must be system data structure")
  elseif(! (strcmp(opt,"tf") + strcmp(opt,"zp") + ...
	strcmp(opt,"ss") + strcmp(opt,"all")) )
    error("2nd argument must be \"tf\", \"zp\", \"ss\", or \"all\"");
  endif

  # check to make sure not trying to make a SISO system out of a MIMO sys
  if ( (strcmp(opt,"tf") + strcmp(opt,"zp") + strcmp(opt,"all")) ...
	& (sys.sys(1) == 2) &  (! is_siso(sys) ) )
    error("MIMO -> SISO update requested");
  endif

  # update transfer function if desired
  if ( (strcmp(opt, "tf") + strcmp(opt,"all"))&&  (!sys.sys(2)))
    # check to make sure the system is not discrete and continuous
    is_digital(sys);

    # if original system zero-pole
    if (sys.sys(1) == 1)
      [sys.num,sys.den] = zp2tf(sys.zer,sys.pol,sys.k);
      sys.sys(2) = 1;
    # if original system is state-space
    elseif(sys.sys(1) == 2)
      [sys.num,sys.den] = ss2tf(sys.a,sys.b,sys.c,sys.d);
      sys.sys(2) = 1; 
    endif
  endif


  # update zero-pole if desired
  if ( (strcmp(opt, "zp") + strcmp(opt,"all")) && (! sys.sys(3)) )
    # check to make sure the system is not discrete and continuous
    is_digital(sys);

    # original system is transfer function
    if (sys.sys(1) == 0)
      [sys.zer,sys.pol,sys.k] = tf2zp(sys.num,sys.den);
      sys.sys(3) = 1;
    # original system is state-space

    elseif(sys.sys(1) == 2)
      [sys.zer,sys.pol,sys.k] = ss2zp(sys.a,sys.b,sys.c,sys.d);
      sys.sys(3) = 1; 
    endif

  endif

  # update state-space if desired
  if ( (strcmp(opt, "ss") + strcmp(opt,"all")) && (! sys.sys(4)) )
    # original system is transfer function
    if (sys.sys(1) == 0)
      [sys.a,sys.b,sys.c,sys.d] = tf2ss(sys.num,sys.den);
      sys.sys(4) = 1;
    # original system is zero-pole
    elseif(sys.sys(1) == 1)
      [sys.a,sys.b,sys.c,sys.d] = zp2ss(sys.zer,sys.pol,sys.k);
      sys.sys(4) = 1; 
    endif

    # create new state names
    sys.stname = sysdefstname(sys.n, sys.nz);
  endif
  

endfunction
