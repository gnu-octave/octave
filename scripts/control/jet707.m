## Copyright (C) 1997 Kai P. Mueller
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
## @deftypefn {Function File } { @var{outsys}  =} jet707 ( ) 
##  Creates linearized state space model of a Boeing 707-321 aircraft
##  at v=80m/s. (M = 0.26, Ga0 = -3 deg, alpha0 = 4 deg, kappa = 50 deg)
##  System inputs:   (1) thrust   and (2) elevator angle
##  System outputs:  (1) airspeed and (2) pitch angle
##  Ref: R. Brockhaus: Flugregelung (Flight Control), Springer, 1994
## 
##  see also: ord2
## 
## Contributed by Kai Mueller
## @end deftypefn
 
function outsys = jet707()

  ## Written by Kai P. Mueller September 28, 1997
  ## Updates

  if (nargin != 0)
    usage("outsys = jet707()")
  endif
  if (nargin > 1)
    usage("outsys = jet707()")
  endif

  a = [ -0.46E-01,            0.10681415316, 0.0,   -0.17121680433;
        -0.1675901504661613, -0.515,         1.0,    0.6420630320636088E-02;
         0.1543104215347786, -0.547945,     -0.906, -0.1521689385990753E-02;
         0.0,                 0.0,           1.0,    0.0 ];
  b = [ 0.1602300107479095,      0.2111848453E-02;
        0.8196877780963616E-02, -0.3025E-01;
        0.9173594317692437E-01, -0.75283075;
        0.0,                     0.0 ];
  c = [ 1.0,  0.0,  0.0,  0.0;
        0.0,  0.0,  0.0,  1.0 ];
  d=zeros(2,2);
  inam = ["thrust"; "rudder"];
  onam = ["speed"; "pitch"];
  snam = ["x1"; "x2"; "x3"; "x4"];
  outsys = ss2sys(a, b, c, d, 0.0, 4, 0, snam, inam, onam);
endfunction
