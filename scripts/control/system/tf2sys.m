## Copyright (C) 1996, 1998 Auburn University.  All rights reserved.
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by the
## Free Software Foundation; either version 2, or (at your option) any
## later version.
##
## Octave is distributed in the hope that it will be useful, but WITHOUT
## ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
## FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
## for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, 59 Temple Place, Suite 330, Boston, MA 02111 USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} tf2sys (@var{num}, @var{den}, @var{tsam}, @var{inname}, @var{outname})
## build system data structure from transfer function format data
##
## @strong{Inputs}
## @table @var
## @item  num
## @itemx den
## coefficients of numerator/denominator polynomials
## @item tsam
## sampling interval. default: 0 (continuous time)
## @item inname
## @itemx outname
## input/output signal names; may be a string or list with a single string
## entry.
## @end table
##
## @strong{Outputs}
## @var{sys} = system data structure
##
## @strong{Example}
## @example
## octave:1> sys=tf2sys([2 1],[1 2 1],0.1);
## octave:2> sysout(sys)
## Input(s)
##         1: u_1
## Output(s):
##         1: y_1 (discrete)
## Sampling interval: 0.1
## transfer function form:
## 2*z^1 + 1
## -----------------
## 1*z^2 + 2*z^1 + 1
## @end example
## @end deftypefn

## Author: R. Bruce Tenison <btenison@eng.auburn.edu>
## Created: July 29, 1994
## Name changed to TF2SYS July 1995
## updated for new system data structure format July 1996

function outsys = tf2sys (num, den, tsam, inname, outname)

  ## Test for the correct number of input arguments
  if ((nargin < 2) || (nargin > 5))
    usage ("outsys = tf2sys (num, den [, tsam, inname, outname])");
    return
  endif

  ## check input format
  if( ! ( (is_vector(num) || is_scalar(num)) && ...
        (is_vector(den) || is_scalar(den))) )
    error(["num (",num2str(rows(num)),"x",num2str(columns(num)), ...
      ") and den (",num2str(rows(den)),"x",num2str(columns(den)), ...
      ") must be vectors"])
  endif

  ## strip leading zero coefficients
  num = __tf2sysl__ (num);
  den = __tf2sysl__ (den);

  if (length(num) >  length(den))
    error("# of poles (%d) < # of zeros (%d)",length(den)-1, length(num)-1);
  endif

  ## check sampling interval (if any)
  if(nargin <= 2)           tsam = 0;           # default
  elseif (isempty(tsam))    tsam = 0;           endif
  if ( (! (is_scalar(tsam) && (imag(tsam) == 0) )) || (tsam < 0) )
    error("tsam must be a positive real scalar")
  endif

  outsys.num = num;
  outsys.den = den;

  ## Set the system vector:  active = 0(tf), updated = [1 0 0];
  outsys.sys = [0, 1, 0, 0];

  ## Set defaults
  outsys.tsam = tsam;
  outsys.n = length(den)-1;
  outsys.nz = 0;
  outsys.yd = 0;        # assume discrete-time
  ## check discrete time
  if(tsam > 0)
    [outsys.n,outsys.nz] = swap(outsys.n, outsys.nz);
    outsys.yd = 1;
  endif

  outsys.inname  = __sysdefioname__ (1, "u");
  outsys.outname = __sysdefioname__ (1, "y");
  outsys.stname  = __sysdefstname__ (outsys.n, outsys.nz);

  ## Set name of input
  if (nargin > 3)
    ## make sure its a list of a single string
    if(!isempty(inname))
      if(!is_list(inname))  inname = list(inname);  endif
      if( !is_signal_list(inname) )
        error("inname must be a string or list of strings");
      endif
      if(length(inname) > 1)
        warning("tf2sys: %d input names provided; first used",length(inname));
        inname = inname(1);
      endif
      outsys = syssetsignals(outsys,"in",inname);
    endif
  endif

  ## Set name of output
  if (nargin > 4)
    if(!isempty(outname))
      if(!is_list(outname))  outname = list(outname);  endif
      if(!is_signal_list(outname))
        error("outname must be a string or a list of strings");
      endif
      if(length(outname) > 1)
        warning("tf2sys: %d output names provided; first used",length(outname));
        outname = outname(1);
      endif
      outsys = syssetsignals(outsys,"out",outname);
    endif
  endif

endfunction
