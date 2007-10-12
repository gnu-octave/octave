## Copyright (C) 1996, 1997 John W. Eaton
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

## -*- texinfo -*-
## @deftypefn {Function File} {} toeplitz (@var{c}, @var{r})
## Return the Toeplitz matrix constructed given the first column @var{c},
## and (optionally) the first row @var{r}.  If the first element of @var{c}
## is not the same as the first element of @var{r}, the first element of
## @var{c} is used.  If the second argument is omitted, the first row is
## taken to be the same as the first column.
##
## A square Toeplitz matrix has the form:
## @iftex
## @tex
## $$
## \left[\matrix{c_0    & r_1     & r_2      & \cdots & r_n\cr
##               c_1    & c_0     & r_1      & \cdots & r_{n-1}\cr
##               c_2    & c_1     & c_0      & \cdots & r_{n-2}\cr
##               \vdots & \vdots  & \vdots   & \ddots & \vdots\cr
##               c_n    & c_{n-1} & c_{n-2} & \ldots & c_0}\right]
## $$
## @end tex
## @end iftex
## @ifinfo
##
## @example
## @group
## c(0)  r(1)   r(2)  ...  r(n)
## c(1)  c(0)   r(1)  ... r(n-1)
## c(2)  c(1)   c(0)  ... r(n-2)
##  .     ,      ,   .      .
##  .     ,      ,     .    .
##  .     ,      ,       .  .
## c(n) c(n-1) c(n-2) ...  c(0)
## @end group
## @end example
## @end ifinfo
## @seealso{hankel, vander, sylvester_matrix, hilb, invhilb}
## @end deftypefn

## Author: jwe

function retval = toeplitz (c, r)

  if (nargin == 1)
    r = c;
  elseif (nargin != 2)
    print_usage ();
  endif

  [c_nr, c_nc] = size (c);
  [r_nr, r_nc] = size (r);

  if ((c_nr != 1 && c_nc != 1) || (r_nr != 1 && r_nc != 1))
    error ("toeplitz: expecting vector arguments");
  endif

  if (c_nc != 1)
    c = c.';
  endif

  if (r_nr != 1)
    r = r.';
  endif

  if (r (1) != c (1))
    warning ("toeplitz: column wins diagonal conflict");
  endif

  ## If we have a single complex argument, we want to return a
  ## Hermitian-symmetric matrix (actually, this will really only be
  ## Hermitian-symmetric if the first element of the vector is real).

  if (nargin == 1)
    c = conj (c);
    c(1) = conj (c(1));
  endif

  ## This should probably be done with the colon operator...

  nc = length (r);
  nr = length (c);

  retval = resize (resize (c, 0), nr, nc);

  for i = 1:min (nc, nr)
    retval (i:nr, i) = c (1:nr-i+1);
  endfor

  for i = 1:min (nr, nc-1)
    retval (i, i+1:nc) = r (2:nc-i+1);
  endfor

endfunction
