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
## @deftypefn {Function File} {} fir2sys (@var{num}, @var{tsam}, @var{inname}, @var{outname})
## construct a system data structure from @acronym{FIR} description
##
## @strong{Inputs}
## @table @var
## @item num
## vector of coefficients 
## @ifinfo
## [c0, c1, ..., cn]
## @end ifinfo
## @iftex
## @tex
## $ [c_0, c_1, \ldots, c_n ]$
## @end tex
## @end iftex
## of the @acronym{SISO} @acronym{FIR} transfer function
## @ifinfo
## C(z) = c0 + c1*z^(-1) + c2*z^(-2) + ... + cn*z^(-n)
## @end ifinfo
## @iftex
## @tex
## $$ C(z) = c_0 + c_1z^{-1} + c_2z^{-2} + \ldots + c_nz^{-n} $$
## @end tex
## @end iftex
##
## @item tsam
## sampling time (default: 1)
##
## @item inname
## name of input signal;  may be a string or a list with a single entry.
##
## @item outname
## name of output signal; may be a string or a list with a single entry.
## @end table
##
## @strong{Output}
## @table @var
## @item sys
## system data structure
## @end table
##
## @strong{Example}
## @example
## octave:1> sys = fir2sys([1 -1 2 4],0.342,\
## > "A/D input","filter output");
## octave:2> sysout(sys)
## Input(s)
##         1: A/D input
##
## Output(s):
##         1: filter output (discrete)
##
## Sampling interval: 0.342
## transfer function form:
## 1*z^3 - 1*z^2 + 2*z^1 + 4
## -------------------------
## 1*z^3 + 0*z^2 + 0*z^1 + 0
## @end example
## @end deftypefn

## Author: R. Bruce Tenison <btenison@eng.auburn.edu>
## Created: July 29, 1994
## Name changed to TF2SYS July 1995
## updated for new system data structure format July 1996
## adapted from tf2sys july 1996

function sys = fir2sys (num, tsam, inname, outname)

  ## Test for the correct number of input arguments
  if (nargin < 1 | nargin > 4)
    usage ("sys = fir2sys(num [, tsam, inname, outname])");
  endif

  ## let tf do the argument checking
  den = [1,zeros(1,length(num)-1)];

  ## check sampling interval (if any)
  if (nargin <= 1)
    tsam = 1;           # default
  elseif (isempty(tsam))
    tsam = 1;
  endif

  ## Set name of input
  if (nargin < 3)
    inname = __sysdefioname__ (1, "u");
  endif

  ## Set name of output
  if (nargin < 4)
    outname = __sysdefioname__ (1, "y");
  endif

  sys = tf (num, den, tsam, inname, outname);

endfunction
