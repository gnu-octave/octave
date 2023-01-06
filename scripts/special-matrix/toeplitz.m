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
## @deftypefn  {} {@var{T} =} toeplitz (@var{c})
## @deftypefnx {} {@var{T} =} toeplitz (@var{c}, @var{r})
## Return the Toeplitz matrix constructed from the first column @var{c},
## and optionally the first row @var{r}.
##
## If the second argument is omitted, the first row is taken to be the
## same as the first column.  If the first element of @var{r} is not the same
## as the first element of @var{c}, the first element of @var{c} is used.
##
## A Toeplitz, or diagonal-constant, matrix has the same value along each
## diagonal.  Although it need not be square, it often is.  An @nospell{MxN}
## Toeplitz matrix has the form:
## @tex
## $$
## \left[\matrix{c_1    & r_2     & r_3      & \cdots & r_n\cr
##               c_2    & c_1     & r_2      & \cdots & r_{n-1}\cr
##               c_3    & c_2     & c_1      & \cdots & r_{n-2}\cr
##               \vdots & \vdots  & \vdots   & \ddots & \vdots\cr
##               c_m    & c_{m-1} & c_{m-2} & \ldots & c{m-n+1}}\right]
## $$
## @end tex
## @ifnottex
##
## @example
## @group
## c(1)  r(2)   r(3)  @dots{}  r(n)
## c(2)  c(1)   r(2)  @dots{} r(n-1)
## c(3)  c(2)   c(1)  @dots{} r(n-2)
##  .     .      .   .      .
##  .     .      .     .    .
##  .     .      .       .  .
## c(m) c(m-1) c(m-2) @dots{} c(m-n+1)
## @end group
## @end example
##
## @end ifnottex
## @seealso{hankel}
## @end deftypefn

function T = toeplitz (c, r)

  if (nargin < 1)
    print_usage ();
  endif

  if (nargin == 1)
    if (! isvector (c))
      error ("toeplitz: C must be a vector");
    endif

    r = c;
    nr = length (c);
    nc = nr;
  else
    if (! (isvector (c) && isvector (r)))
      error ("toeplitz: C and R must be vectors");
    elseif (r(1) != c(1))
      warning ("toeplitz: column wins diagonal conflict");
    endif

    nr = length (c);
    nc = length (r);
  endif

  if (nr == 0 || nc == 0)
    ## Empty matrix.
    T = zeros (nr, nc, class (c));
    return;
  endif

  ## If we have a single complex argument, we want to return a
  ## Hermitian-symmetric matrix (actually, this will really only be
  ## Hermitian-symmetric if the first element of the vector is real).
  if (nargin == 1 && iscomplex (c))
    c = conj (c);
    c(1) = conj (c(1));
  endif

  if (issparse (c) && issparse (r))
    c = c(:).';  # enforce row vector
    r = r(:).';  # enforce row vector
    cidx = find (c);
    ridx = find (r);

    ## Ignore the first element in r.
    ridx = ridx(ridx > 1);

    ## Form matrix.
    T = spdiags (repmat (c(cidx),nr,1),1-cidx,nr,nc) + ...
                 spdiags (repmat (r(ridx),nr,1),ridx-1,nr,nc);
  else
    ## Concatenate data into a single column vector.
    data = [r(end:-1:2)(:); c(:)];

    ## Get slices.
    slices = cellslices (data, nc:-1:1, nc+nr-1:-1:nr);

    ## Form matrix.
    T = horzcat (slices{:});
  endif

endfunction


%!assert (toeplitz (1), [1])
%!assert (toeplitz ([1, 2, 3], [1; -3; -5]), [1, -3, -5; 2, 1, -3; 3, 2, 1])
%!assert (toeplitz ([1, 2, 3], [1; -3i; -5i]),
%!        [1, -3i, -5i; 2, 1, -3i; 3, 2, 1])

## Test input validation
%!error <Invalid call> toeplitz ()
%!error <C must be a vector> toeplitz ([1, 2; 3, 4])
%!error <C and R must be vectors> toeplitz ([1, 2; 3, 4], 1)
%!error <C and R must be vectors> toeplitz (1, [1, 2; 3, 4])
