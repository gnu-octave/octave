## ifft2-1.m
##
## Author: David Billinghurst (David.Billinghurst@riotinto.com.au)
##         Comalco Research and Technology
##         02 May 2000
M=12;
N=7;

m=3;
n=2;

x = 2*pi*(0:1:M-1)/M;
y = 2*pi*(0:1:N-1)/N;

sx = cos(m*x);
sy = cos(n*y);

S = kron(fft(sx)',fft(sy));
answer=kron(sx',sy);
s = ifft2(S);

all( all( abs(s-answer) < 30*eps ) )
