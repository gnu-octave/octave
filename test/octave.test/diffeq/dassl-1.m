## dassl-1.m
##
## Test dassl() function
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

tol = 100 * dassl_options ("absolute tolerance");

function res = f (x, xdot, t)
  res = [xdot(1)+x(2); xdot(2)-x(1)];
endfunction

[x, xdot] = dassl ("f", x0, xdot0, t);

y = [cos(t), sin(t)];

all (all (abs (x - y) < tol))
