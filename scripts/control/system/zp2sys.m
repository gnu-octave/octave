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
## @deftypefn {Function File} {} zp2sys (@var{zer}, @var{pol}, @var{k}, @var{tsam}, @var{inname}, @var{outname})
## Create system data structure from zero-pole data.
##
## @strong{Inputs}
## @table @var
## @item   zer
## Vector of system zeros.
## @item   pol
## Vector of system poles.
## @item   k
## Scalar leading coefficient.
## @item   tsam
## Sampling period; default: 0 (continuous system).
## @item   inname
## @itemx  outname
## Input/output signal names (lists of strings).
## @end table
##
## @strong{Output}
## @table @var
## @item sys
## System data structure.
## @end table
##
## @strong{Example}
## @example
## octave:1> sys=zp2sys([1 -1],[-2 -2 0],1);
## octave:2> sysout(sys)
## Input(s)
##         1: u_1
## Output(s):
##         1: y_1
## zero-pole form:
## 1 (s - 1) (s + 1)
## -----------------
## s (s + 2) (s + 2)
## @end example
## @end deftypefn

## Modified by John Ingram  July 20, 1996

function outsys = zp2sys ( varargin )

  warning("zp2sys is deprecated.  Use zp() instead.");
  outsys = zp(varargin{:});

endfunction
