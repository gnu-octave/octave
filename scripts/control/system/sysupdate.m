## Copyright (C) 1996, 2000, 2002, 2003, 2004, 2005, 2006, 2007
##               Auburn University.  All rights reserved.
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {Function File} {} sysupdate (@var{sys}, @var{opt})
## Update the internal representation of a system.
##
## @strong{Inputs}
## @table @var
## @item sys:
## system data structure
## @item opt
## string:
## @table @code
## @item "tf"
## update transfer function form
## @item "zp"
## update zero-pole form
## @item "ss"
## update state space form
## @item "all"
## all of the above
## @end table
## @end table
##
## @strong{Outputs}
## @table @var
## @item retsys
## Contains union of data in sys and requested data.
## If requested data in @var{sys} is already up to date then @var{retsys}=@var{sys}.
## @end table
##
## Conversion to @command{tf} or @command{zp} exits with an error if the system is
## mixed continuous/digital.
## @seealso{tf, ss, zp, sysout, sys2ss, sys2tf, sys2zp}
## @end deftypefn

## Author: John Ingram <ingraje@eng.auburn.edu>
## Created: July 9, 1996

function sys = sysupdate (sys, opt)

  ## check for correct number of inputs
  if (nargin != 2)
    print_usage ();
  elseif(! isstruct(sys) )
   error("1st argument must be system data structure")
  elseif(! (strcmp(opt,"tf") + strcmp(opt,"zp") + ...
        strcmp(opt,"ss") + strcmp(opt,"all")) )
    error("2nd argument must be \"tf\", \"zp\", \"ss\", or \"all\"");
  endif

  ## check to make sure not trying to make a SISO system out of a MIMO sys
  if ( (strcmp(opt,"tf") + strcmp(opt,"zp") + strcmp(opt,"all")) ...
        & strcmp(sysgettype(sys),"ss") &  (! is_siso(sys) ) )
    error("MIMO -> SISO update requested");
  endif

  ## update transfer function if desired
  if ( (strcmp(opt, "tf") + strcmp(opt,"all"))&&  (!sys.sys(2)))
    ## check to make sure the system is not discrete and continuous
    is_digital(sys);

    ## if original system zero-pole
    if strcmp(sysgettype(sys),"zp")
      [sys.num,sys.den] = zp2tf(sys.zer,sys.pol,sys.k);
      sys.sys(2) = 1;
    ## if original system is state-space
    elseif(sys.sys(1) == 2)
      [sys.num,sys.den] = ss2tf(sys.a,sys.b,sys.c,sys.d);
      sys.sys(2) = 1;
    endif
  endif


  ## update zero-pole if desired
  if ( (strcmp(opt, "zp") + strcmp(opt,"all")) && (! sys.sys(3)) )
    ## check to make sure the system is not discrete and continuous
    is_digital(sys);

    ## original system is transfer function
    if (sys.sys(1) == 0)
      [sys.zer,sys.pol,sys.k] = tf2zp(sys.num,sys.den);
      sys.sys(3) = 1;
    ## original system is state-space

    elseif(sys.sys(1) == 2)
      [sys.zer,sys.pol,sys.k] = ss2zp(sys.a,sys.b,sys.c,sys.d);
      sys.sys(3) = 1;
    endif

  endif

  ## update state-space if desired
  if ( (strcmp(opt, "ss") + strcmp(opt,"all")) && (! sys.sys(4)) )
    ## original system is transfer function
    if (sys.sys(1) == 0)
      [sys.a,sys.b,sys.c,sys.d] = tf2ss(sys.num,sys.den);
      sys.sys(4) = 1;
    ## original system is zero-pole
    elseif(sys.sys(1) == 1)
      [sys.a,sys.b,sys.c,sys.d] = zp2ss(sys.zer,sys.pol,sys.k);
      sys.sys(4) = 1;
    endif

    ## create new state names
    sys.stname = __sysdefstname__ (sys.n, sys.nz);
  endif

endfunction
