## fft2-1.m
##
## Author: David Billinghurst (David.Billinghurst@riotinto.com.au)
##         Comalco Research and Technology
##         02 May 2000
M=16;
N=8;

m=5;
n=3;

x = 2*pi*(0:1:M-1)/M;
y = 2*pi*(0:1:N-1)/N;
sx = cos(m*x);
sy = sin(n*y);
s=kron(sx',sy);
S = fft2(s);
answer = kron(fft(sx)',fft(sy));
all( all( abs(S-answer) < 4*M*N*eps ) )
