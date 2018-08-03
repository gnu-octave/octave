## Copyright (C) 2018 Marco Caliari
## Copyright (C) 2018 Sébastien Villemot <sebastien@debian.org>
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {} {@var{lambda} =} ordeig (@var{A})
## @deftypefnx {} {@var{lambda} =} ordeig (@var{A}, @var{B})
## Return the eigenvalues of quasi-triangular matrices in their order
## of appearance on the matrix @var{A}.  The matrix @var{A} is usually
## the result of a Schur factorization.  If @var{B} is given, then the
## generalized eigenvalues of the pair @var{A}, @var{B} are computed,
## in their order of appearance on the matrix
## @code{@var{A}-@var{lambda}*@var{B}}. The pair @var{A}, @var{B} is
## usually the result of a QZ decomposition.
##
## @seealso{eig, schur, qz}
## @end deftypefn

function lambda = ordeig (A, B)

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  if (! isnumeric (A) || ! issquare (A))
    error ("ordeig: A must be a square matrix")
  endif

  n = length (A);

  if (nargin == 1)
    B = eye (n);
    if (isreal (A))
      if (! isquasitri (A))
        error ("ordeig: A must be quasi-triangular (i.e. upper block triangular with 1x1 or 2x2 blocks on the diagonal)")
      endif
    else
      if (! istriu (A))
        error ("ordeig: A must be upper-triangular when it is complex")
      endif
    endif
  elseif (! isquasitri (B))
    if (! isnumeric (B) || ! issquare (B))
      error ("ordeig: B must be a square matrix")
    endif
    if (length (B) != n)
      error ("ordeig: A and B must be of the same size")
    endif
    if (isreal (A) && isreal (B))
      if (! isquasitri (A) || ! isquasitri (B))
        error ("ordeig: A and B must be quasi-triangular (i.e. upper block triangular with 1x1 or 2x2 blocks on the diagonal)")
      endif
    else
      if (! istriu (A) || ! istriu (B))
        error ("ordeig: A and B must be both upper-triangular if any of the two is complex")
      endif
    endif
  endif

  lambda = zeros (n, 1);

  i = 1;
  while (i <= n)
    if (i == n || (A(i+1,i) == 0 && B(i+1,i) == 0))
      lambda(i) = A(i,i) / B(i,i);
    else
      a = B(i,i) * B(i+1,i+1);
      b = - (A(i,i) * B(i+1,i+1) + A(i+1,i+1) * B(i,i));
      c = A(i,i) * A(i+1,i+1) - ...
          (A(i,i+1) - B(i,i+1)) * (A(i+1,i) - B(i+1,i));
      if (b > 0)
        lambda(i) = 2*c / (-b - sqrt (b^2 - 4*a*c));
        i = i + 1;
        lambda(i) = (-b - sqrt (b^2 - 4*a*c)) / 2 / a;
      else
        lambda(i) = (-b + sqrt (b^2 - 4*a*c)) / 2 / a;
        i = i + 1;
        lambda(i) = 2*c / (-b + sqrt (b^2 - 4*a*c));
      endif
    endif
    i = i + 1;
  endwhile

endfunction

# Checks whether a matrix is quasi-triangular
function retval = isquasitri (A)
  v = diag (A, -1) != 0;
  retval = all (tril (A, -2)(:) == 0) && all (v(1:end-1) + v(2:end) < 2);
endfunction

%!test
%! A = toeplitz ([0, 1, 0, 0], [0, -1, 0, 0]);
%! T = schur (A);
%! lambda = ordeig (T);
%! assert (lambda, [1.61803i; -1.61803i; 0.61803i; -0.61803i], 1e-4)

%!test
%! A = toeplitz ([0, 1, 0, 0], [0, -1, 0, 0]);
%! B = toeplitz ([0, 0, 0, 1], [0, -1, 0, 2]);
%! [AA, BB] = qz (A, B);
%! assert (isreal (AA) && isreal (BB))
%! lambda = ordeig (AA, BB);
%! assert (lambda, [0.5+0.86603i; 0.5-0.86603i; i; -i], 1e-4)
%! [AA, BB] = qz (complex (A), complex (B));
%! assert (iscomplex (AA) && iscomplex (BB))
%! lambda = ordeig (AA, BB);
%! assert (lambda, diag (AA) ./ diag (BB))
