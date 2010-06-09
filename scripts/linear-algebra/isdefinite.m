## Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008 Gabriele Pannocchia
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
## @deftypefn  {Function File} {} isdefinite (@var{x})
## @deftypefnx {Function File} {} isdefinite (@var{x}, @var{tol})
## Return 1 if @var{x} is symmetric positive definite within the
## tolerance specified by @var{tol} or 0 if @var{x} is symmetric
## positive semidefinite.  Otherwise, return -1.  If @var{tol}
## is omitted, use a tolerance of 
## @code{100 * eps * norm (@var{x}, "fro")}
## @seealso{issymmetric}
## @end deftypefn

## Author: Gabriele Pannocchia <g.pannocchia@ing.unipi.it>
## Created: November 2003
## Adapted-By: jwe

function retval = isdefinite (x, tol)

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  if (! ishermitian (x))
    error ("isdefinite: x must be a hermitian matrix");
  endif

  if (! isfloat (x))
    x = double (x);
  endif

  if (nargin == 1)
    tol = 100 * eps(class (x)) * norm (x, "fro");
  endif

  e = tol * eye (rows (x));
  [r, p] = chol (x - e);
  if (p == 0)
    retval = 1;
  else
    [r, p] = chol (x + e);
    if (p == 0)
      retval = 0;
    else
      retval = -1;
    endif
  endif

endfunction
