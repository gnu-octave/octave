## dassl-2.m
##
## Test dassl() function
##
## Author: David Billinghurst (David.Billinghurst@riotinto.com.au)
##         Comalco Research and Technology
##         20 May 1998
##
## Based on SLATEC quick check for DASSL by Linda Petzold
##
## Problem
##
##   x1' + 10*x1 = 0,   x1(0) = 1
##   x1  + x2    = 1,   x2(0) = 0
## 
##
## Solution
##
##  x1(t) = exp(-10*t)
##  x2(t) = 1 - x(1)

x0 = [1; 0];
xdot0 = [-10; 10];
t = (0:0.2:1)';

tol = 500 * dassl_options ("absolute tolerance");

function res = f (x, xdot, t)
  res = [xdot(1)+10*x(1); x(1)+x(2)-1];
endfunction

[x, xdot] = dassl ("f", x0, xdot0, t);

y = [exp(-10*t), 1-exp(-10*t)];

all (all (abs (x - y) < tol))
