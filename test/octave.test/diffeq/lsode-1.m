## dassl-1.m
##
## Test lsode() function
##
## Author: David Billinghurst (David.Billinghurst@riotinto.com.au)
##         Comalco Research and Technology
##         20 May 1998
##
## Problem
##
##    y1' = -y2,   y1(0) = 1
##    y2' =  y1,   y2(0) = 0
##
## Solution
##
##    y1(t) = cos(t)
##    y2(t) = sin(t)

x0 = [1; 0];
xdot0 = [0; 1];
t = (0:1:10)';

tol = 500 * lsode_options ("absolute tolerance");

function xdot = f (x, t)
  xdot = [-x(2); x(1)];
endfunction

x = lsode ("f", x0, t);

y = [cos(t), sin(t)];

all (all (abs (x - y) < tol))
