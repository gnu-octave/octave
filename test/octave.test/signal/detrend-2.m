## detrend-2.m
##
## Author: David Billinghurst (David.Billinghurst@riotinto.com.au)
##         Comalco Research and Technology
##         02 May 2000
N=32;
t = (0:1:N-1)/N;
x = t .* t + 2;
y = detrend(x,2);
all (all (abs (y) < 30*eps))
