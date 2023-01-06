########################################################################
##
## Copyright (C) 2014-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

## -*- texinfo -*-
## @deftypefn  {} {[@var{num}, @var{den}] =} padecoef (@var{T})
## @deftypefnx {} {[@var{num}, @var{den}] =} padecoef (@var{T}, @var{N})
## Compute the @var{N}th-order Pad@'e approximant of the continuous-time
## delay @var{T} in transfer function form.
##
## @tex
## The Pad\'e approximant of $e^{-sT}$ is defined by the following equation
## $$ e^{-sT} \approx {P_n(s) \over Q_n(s)} $$
## where both $P_n(s)$ and $Q_n(s)$ are $N^{th}$-order rational functions
## defined by the following expressions
## $$ P_n(s)=\sum_{k=0}^N {(2N - k)!N!\over (2N)!k!(N - k)!}(-sT)^k $$
## $$ Q_n(s) = P_n(-s) $$
## @end tex
## @ifnottex
## The Pad@'e approximant of @nospell{@code{exp (-sT)}} is defined by the
## following equation
##
## @example
## @group
##              Pn(s)
## exp (-sT) ~ -------
##              Qn(s)
## @end group
## @end example
##
## Where both @nospell{Pn(s) and Qn(s)} are @var{N}th-order rational functions
## defined by the following expressions
##
## @example
## @group
##          N    (2N - k)!N!        k
## Pn(s) = SUM --------------- (-sT)
##         k=0 (2N)!k!(N - k)!
##
## Qn(s) = Pn(-s)
## @end group
## @end example
##
## @end ifnottex
##
## The inputs @var{T} and @var{N} must be non-negative numeric scalars.  If
## @var{N} is unspecified it defaults to 1.
##
## The output row vectors @var{num} and @var{den} contain the numerator and
## denominator coefficients in descending powers of s.  Both are
## @var{N}th-order polynomials.
##
## For example:
##
## @smallexample
## @group
## t = 0.1;
## n = 4;
## [num, den] = padecoef (t, n)
## @result{} num =
##
##       1.0000e-04  -2.0000e-02   1.8000e+00  -8.4000e+01   1.6800e+03
##
## @result{} den =
##
##       1.0000e-04   2.0000e-02   1.8000e+00   8.4000e+01   1.6800e+03
## @end group
## @end smallexample
## @end deftypefn

function [num, den] = padecoef (T, N = 1)

  if (nargin < 1)
    print_usage ();
  endif

  if (! (isscalar (T) && isnumeric (T) && T >= 0))
    error ("padecoef: T must be a non-negative scalar");
  elseif (! (isscalar (N) && isnumeric (N) && N >= 0))
    error ("padecoef: N must be a non-negative scalar");
  endif

  N = round (N);
  k = N : -1 : 0;
  num = prod (linspace ((N - k + 1), (2 * N - k), N)', ones (1, N)) ...
        / prod (N + 1 : 2 * N) ./ factorial (k);
  num /= num(1);
  den = num .* (T .^ k);
  num .*= ((-T) .^ k);

endfunction


%!test
%! T = 1;
%! [n_obs, d_obs] = padecoef (T);
%! n_exp = [1, 2] .* [-T, 1];
%! d_exp = [1, 2] .* [T, 1];
%! assert ([n_obs, d_obs], [n_exp, d_exp], eps);

%!test
%! T = 0.1;
%! [n_obs, d_obs] = padecoef (T);
%! n_exp = [1, 2] .* [-T, 1];
%! d_exp = [1, 2] .* [T, 1];
%! assert ([n_obs, d_obs], [n_exp, d_exp], eps);

%!test
%! T = 1;
%! N = 2;
%! k = N : -1 : 0;
%! [n_obs, d_obs] = padecoef (T, N);
%! n_exp = [1, 6, 12] .* ((-T) .^ k);
%! d_exp = [1, 6, 12] .* (T .^ k);
%! assert ([n_obs, d_obs], [n_exp, d_exp], eps);

%!test
%! T = 0.25;
%! N = 2;
%! k = N : -1 : 0;
%! [n_obs, d_obs] = padecoef (T, 2);
%! n_exp = [1, 6, 12] .* ((-T) .^ k);
%! d_exp = [1, 6, 12] .* (T .^ k);
%! assert ([n_obs, d_obs], [n_exp, d_exp], eps);

%!test
%! T = 0.47;
%! N = 3;
%! k = N : -1 : 0;
%! [n_obs, d_obs] = padecoef (T, N);
%! n_exp = [1, 12, 60, 120] .* ((-T) .^ k);
%! d_exp = [1, 12, 60, 120] .* (T .^ k);
%! assert ([n_obs, d_obs], [n_exp, d_exp], eps);

%!test
%! T = 1;
%! N = 7;
%! i = 0 : 2 * N;
%! b = ((-T) .^ i) ./ factorial (i);
%! A = [[eye(N + 1); zeros(N, N + 1)], ...
%!      [zeros(1, N); toeplitz(-b(1 : 2 * N), [-b(1), zeros(1, N-1)])]];
%! x = A \ b';
%! k = N : -1 : 0;
%! d_exp = [flipud(x(N + 2 : 2 * N + 1)); 1]';
%! n_exp = flipud (x(1 : N + 1))';
%! n_exp ./= d_exp(1);
%! d_exp ./= d_exp(1);
%! [n_obs, d_obs] = padecoef (T, N);
%! assert ([n_obs, d_obs], [n_exp, d_exp], 1e-2);

## For checking in Wolfram Alpha (look at Alternate forms -> more):
## PadeApproximant[Exp[-x * T], {x, 0, {n, n}}]

## Test input validation
%!error <Invalid call> padecoef ()
%!error <T must be a non-negative scalar> padecoef ([1,2])
%!error <T must be a non-negative scalar> padecoef ({1})
%!error <T must be a non-negative scalar> padecoef (-1)
%!error <N must be a non-negative scalar> padecoef (1, [1,2])
%!error <N must be a non-negative scalar> padecoef (1, {1})
%!error <N must be a non-negative scalar> padecoef (1, -1)
