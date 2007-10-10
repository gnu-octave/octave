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
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301 USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} ss (@var{a}, @var{b}, @var{c}, @var{d}, @var{tsam}, @var{n}, @var{nz}, @var{stname}, @var{inname}, @var{outname}, @var{outlist})
## Create system structure from state-space data.   May be continuous,
## discrete, or mixed (sampled data)
##
## @strong{Inputs}
## @table @var
## @item a
## @itemx b
## @itemx c
## @itemx d
## usual state space matrices.
##
## default: @var{d} = zero matrix
##
## @item   tsam
## sampling rate.  Default: @math{tsam = 0} (continuous system)
##
## @item n
## @itemx nz
## number of continuous, discrete states in the system
##
## If @var{tsam} is 0, @math{n = @code{rows}(@var{a})}, @math{nz = 0}.
##
## If @var{tsam} is greater than zero, @math{n = 0},
## @math{nz = @code{rows}(@var{a})}
##
## see below for system partitioning
##
## @item  stname
## cell array of strings of state signal names
##
## default (@var{stname}=[] on input): @code{x_n} for continuous states,
##                     @code{xd_n} for discrete states
##
## @item inname
## cell array of strings of input signal names
##
## default (@var{inname} = [] on input): @code{u_n}
##
## @item outname
## cell array of strings of input signal names
##
## default (@var{outname} = [] on input): @code{y_n}
##
## @item   outlist
##
## list of indices of outputs y that are sampled
##
## If @var{tsam} is 0, @math{outlist = []}.
##
## If @var{tsam} is greater than 0, @math{outlist = 1:@code{rows}(@var{c})}.
## @end table
##
## Unlike states, discrete/continuous outputs may appear in any order.
##
## @code{sys2ss} returns a vector @var{yd} where
## @var{yd}(@var{outlist}) = 1; all other entries of @var{yd} are 0.
##
## @strong{Outputs}
## @var{outsys} = system data structure
##
## @strong{System partitioning}
##
## Suppose for simplicity that outlist specified
## that the first several outputs were continuous and the remaining outputs
## were discrete.  Then the system is partitioned as
## @example
## @group
## x = [ xc ]  (n x 1)
##     [ xd ]  (nz x 1 discrete states)
## a = [ acc acd ]  b = [ bc ]
##     [ adc add ]      [ bd ]
## c = [ ccc ccd ]  d = [ dc ]
##     [ cdc cdd ]      [ dd ]
##
##     (cdc = c(outlist,1:n), etc.)
## @end group
## @end example
## with dynamic equations:
## @ifinfo
## @math{d/dt xc(t)     = acc*xc(t)      + acd*xd(k*tsam) + bc*u(t)}
##
## @math{xd((k+1)*tsam) = adc*xc(k*tsam) + add*xd(k*tsam) + bd*u(k*tsam)}
##
## @math{yc(t)      = ccc*xc(t)      + ccd*xd(k*tsam) + dc*u(t)}
##
## @math{yd(k*tsam) = cdc*xc(k*tsam) + cdd*xd(k*tsam) + dd*u(k*tsam)}
## @end ifinfo
## @iftex
## @tex
## $$\eqalign{
## {d \over dt} x_c(t)
##   & =   a_{cc} x_c(t)      + a_{cd} x_d(k*t_{sam}) + bc*u(t) \cr
## x_d((k+1)*t_{sam})
##   & =   a_{dc} x_c(k t_{sam}) + a_{dd} x_d(k t_{sam}) + b_d u(k t_{sam}) \cr
## y_c(t)
##  & =  c_{cc} x_c(t) + c_{cd} x_d(k t_{sam}) + d_c u(t) \cr
## y_d(k t_{sam})
##   & =  c_{dc} x_c(k t_{sam}) + c_{dd} x_d(k t_{sam}) + d_d u(k t_{sam})
## }$$
## @end tex
## @end iftex
##
## @strong{Signal partitions}
## @example
## @group
##         | continuous      | discrete               |
## ----------------------------------------------------
## states  | stname(1:n,:)   | stname((n+1):(n+nz),:) |
## ----------------------------------------------------
## outputs | outname(cout,:) | outname(outlist,:)     |
## ----------------------------------------------------
## @end group
## @end example
## where @math{cout} is the list of in 1:@code{rows}(@var{p})
## that are not contained in outlist. (Discrete/continuous outputs
## may be entered in any order desired by the user.)
##
## @strong{Example}
## @example
## octave:1> a = [1 2 3; 4 5 6; 7 8 10];
## octave:2> b = [0 0 ; 0 1 ; 1 0];
## octave:3> c = eye (3);
## octave:4> sys = ss (a, b, c, [], 0, 3, 0, @{"volts", "amps", "joules"@});
## octave:5> sysout(sys);
## Input(s)
##         1: u_1
##         2: u_2
##
## Output(s):
##         1: y_1
##         2: y_2
##         3: y_3
##
## state-space form:
## 3 continuous states, 0 discrete states
## State(s):
##         1: volts
##         2: amps
##         3: joules
##
## A matrix: 3 x 3
##    1   2   3
##    4   5   6
##    7   8  10
## B matrix: 3 x 2
##   0  0
##   0  1
##   1  0
## C matrix: 3 x 3
##   1  0  0
##   0  1  0
##   0  0  1
## D matrix: 3 x 3
##   0  0
##   0  0
##   0  0
## @end example
## Notice that the @math{D} matrix is constructed  by default to the
## correct dimensions.  Default input and output signals names were assigned
## since none were given.
## @end deftypefn

## Author: John Ingram <ingraje@eng.auburn.edu>
## Created: July 20, 1996

function retsys = ss2sys (varargin )
  
  warning("ss2sys is deprecated.  Use ss() instead.");
  retsys = ss(varargin{:});

endfunction
