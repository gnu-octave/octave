## ifft-1.m
##
## Author: David Billinghurst (David.Billinghurst@riotinto.com.au)
##         Comalco Research and Technology
##         02 May 2000
N=64;
n=7;
t = 2*pi*(0:1:N-1)/N;
s = cos(n*t);

S = 0*t;
S(n+1) = N/2;
S(N-n+1) = N/2;

all( abs(ifft(S)-s) < 4*N*eps )
