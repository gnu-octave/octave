########################################################################
##
## Copyright (C) 1993-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{c} =} cond (@var{A})
## @deftypefnx {} {@var{c} =} cond (@var{A}, @var{p})
## Compute the @var{p}-norm condition number of a matrix with respect to
## inversion.
##
## @code{cond (@var{A})} is defined as
## @tex
## $ {\parallel A \parallel_p * \parallel A^{-1} \parallel_p .} $
## @end tex
## @ifnottex
## @code{norm (@var{A}, @var{p}) * norm (inv (@var{A}), @var{p})}.
## @end ifnottex
##
## By default, @code{@var{p} = 2} is used which implies a (relatively slow)
## singular value decomposition.  Other possible selections are
## @code{@var{p} = 1, Inf, "fro"} which are generally faster.  For a full
## discussion of possible @var{p} values, @pxref{XREFnorm,,@code{norm}}.
##
## The condition number of a matrix quantifies the sensitivity of the matrix
## inversion operation when small changes are made to matrix elements.  Ideally
## the condition number will be close to 1.  When the number is large this
## indicates small changes (such as underflow or round-off error) will produce
## large changes in the resulting output.  In such cases the solution results
## from numerical computing are not likely to be accurate.
## @seealso{condest, rcond, condeig, norm, svd}
## @end deftypefn

function c = cond (A, p = 2)

  if (nargin < 1)
    print_usage ();
  endif

  if (ndims (A) > 2)
    error ("cond: A must be a 2-D matrix");
  endif

  if (p == 2)
    if (isempty (A))
      c = 0.0;
    else
      try
        sigma = svd (A);
      catch
        error ("cond: A must not contain Inf or NaN values");
      end_try_catch
      sigma_1 = sigma(1);
      sigma_n = sigma(end);
      if (sigma_1 == 0 || sigma_n == 0)
        c = Inf;
      else
        c = sigma_1 / sigma_n;
      endif
    endif
  else
    c = norm (A, p) * norm (inv (A), p);
  endif

endfunction


%!test
%! y = [7, 2, 3; 1, 3, 4; 6, 4, 5];
%! tol = 1e-6;
%! type = {1, 2, "fro", "inf", inf};
%! for n = 1:numel (type)
%!   rcondition(n) = 1 / cond (y, type{n});
%! endfor
%! assert (rcondition, [0.017460, 0.019597, 0.018714, 0.012022, 0.012022], tol);

%!assert (cond ([1, 2; 2, 1]), 3, sqrt (eps))
%!assert (cond ([1, 2, 3; 4, 5, 6; 7, 8, 9]) > 1.0e+16)

%!error <Invalid call> cond ()
%!error <A must be a 2-D matrix> cond (ones (1,3,3))
%!error <A must not contain Inf or NaN value> cond ([1, 2;Inf 4])
%!error <A must not contain Inf or NaN value> cond ([1, 2;NaN 4])
