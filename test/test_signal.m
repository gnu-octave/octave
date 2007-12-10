## Copyright (C) 2006, 2007 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

%% Automatically generated from DejaGNU files

%% test/octave.test/signal/fft-1.m
%% fft-1.m
%%
%% Author: David Billinghurst (David.Billinghurst@riotinto.com.au)
%%         Comalco Research and Technology
%%         02 May 2000
%!test
%! N=64;
%! n=4;
%! t = 2*pi*(0:1:N-1)/N;
%! s = cos(n*t);
%! S = fft(s);
%! 
%! answer = 0*t;
%! answer(n+1) = N/2;
%! answer(N-n+1) = N/2;
%! 
%! assert(all( abs(S-answer) < 4*N*eps ));

%% test/octave.test/signal/ifft-1.m
%% ifft-1.m
%%
%% Author: David Billinghurst (David.Billinghurst@riotinto.com.au)
%%         Comalco Research and Technology
%%         02 May 2000
%!test
%! N=64;
%! n=7;
%! t = 2*pi*(0:1:N-1)/N;
%! s = cos(n*t);
%! 
%! S = 0*t;
%! S(n+1) = N/2;
%! S(N-n+1) = N/2;
%! 
%! assert(all( abs(ifft(S)-s) < 4*N*eps ));

%% test/octave.test/signal/fft2-1.m
%% fft2-1.m
%%
%% Author: David Billinghurst (David.Billinghurst@riotinto.com.au)
%%         Comalco Research and Technology
%%         02 May 2000
%!test
%! M=16;
%! N=8;
%! 
%! m=5;
%! n=3;
%! 
%! x = 2*pi*(0:1:M-1)/M;
%! y = 2*pi*(0:1:N-1)/N;
%! sx = cos(m*x);
%! sy = sin(n*y);
%! s=kron(sx',sy);
%! S = fft2(s);
%! answer = kron(fft(sx)',fft(sy));
%! assert(all( all( abs(S-answer) < 4*M*N*eps ) ));

%% test/octave.test/signal/ifft2-1.m
%% ifft2-1.m
%%
%% Author: David Billinghurst (David.Billinghurst@riotinto.com.au)
%%         Comalco Research and Technology
%%         02 May 2000
%!test
%! M=12;
%! N=7;
%! 
%! m=3;
%! n=2;
%! 
%! x = 2*pi*(0:1:M-1)/M;
%! y = 2*pi*(0:1:N-1)/N;
%! 
%! sx = cos(m*x);
%! sy = cos(n*y);
%! 
%! S = kron(fft(sx)',fft(sy));
%! answer=kron(sx',sy);
%! s = ifft2(S);
%! 
%! assert(all( all( abs(s-answer) < 30*eps ) ));

%% test/octave.test/signal/unwrap-1.m
%!function t = xassert(a,b,tol)
%!  if (nargin == 1)
%!    t = all(a(:));
%!  else
%!    if (nargin == 2)
%!      tol = 0;
%!    endif
%!    if (any (size(a) != size(b)))
%!      t = 0;
%!    elseif (any (abs(a(:) - b(:)) > tol))
%!      t = 0;
%!    else
%!      t = 1;
%!    endif
%!  endif
%!
%!test
%! 
%! i = 0;
%! t = [];
%! 
%! r = [0:100];                        # original vector
%! w = r - 2*pi*floor((r+pi)/(2*pi));  # wrapped into [-pi,pi]
%! tol = 1e3*eps;                      # maximum expected deviation
%! 
%! t(++i) = xassert(r, unwrap(w), tol);               #unwrap single row
%! t(++i) = xassert(r', unwrap(w'), tol);             #unwrap single column
%! t(++i) = xassert([r',r'], unwrap([w',w']), tol);   #unwrap 2 columns
%! t(++i) = xassert([r;r], unwrap([w;w],[],2), tol);  #verify that dim works
%! t(++i) = xassert(r+10, unwrap(10+w), tol);         #verify that r(1)>pi works
%! 
%! t(++i) = xassert(w', unwrap(w',[],2));  #unwrap col by rows should not change it
%! t(++i) = xassert(w, unwrap(w,[],1));    #unwrap row by cols should not change it
%! t(++i) = xassert([w;w], unwrap([w;w])); #unwrap 2 rows by cols should not change them
%! 
%! ## verify that setting tolerance too low will cause bad results.
%! t(++i) = xassert(any(abs(r - unwrap(w,0.8)) > 100));
%! 
%! assert(all(t));

