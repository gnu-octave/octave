## fft-1.m
##
## Author: David Billinghurst (David.Billinghurst@riotinto.com.au)
##         Comalco Research and Technology
##         02 May 2000
N=64;
n=4;
t = 2*pi*(0:1:N-1)/N;
s = cos(n*t);
S = fft(s);

answer = 0*t;
answer(n+1) = N/2;
answer(N-n+1) = N/2;

all( abs(S-answer) < 4*N*eps )
