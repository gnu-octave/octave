## Copyright (C) 1996, 1998, 2000, 2002, 2004, 2005, 2007
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
## @deftypefn {Function File} {} tf2sys (@var{num}, @var{den}, @var{tsam}, @var{inname}, @var{outname})
## Build system data structure from transfer function format data.
##
## @strong{Inputs}
## @table @var
## @item  num
## @itemx den
## Coefficients of numerator/denominator polynomials.
## @item tsam
## Sampling interval; default: 0 (continuous time).
## @item inname
## @itemx outname
## Input/output signal names; may be a string or cell array with a single string
## entry.
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
## name changed to tf Feb 2004

function outsys = tf2sys (varargin)

  warning ("tf2sys is deprecated.  Use tf instead.");
  outsys = tf (varargin{:});

endfunction
