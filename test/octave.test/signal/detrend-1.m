## detrend-1.m
##
## Author: David Billinghurst (David.Billinghurst@riotinto.com.au)
##         Comalco Research and Technology
##         02 May 2000
N=32;
x = (0:1:N-1)/N + 2;
y = detrend(x);
all (all (abs (y) < 10*eps))
