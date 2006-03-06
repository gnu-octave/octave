## Copyright (C) 1993, 1994, 1995 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by the
## Free Software Foundation; either version 2, or (at your option) any
## later version.
##
## Octave is distributed in the hope that it will be useful, but WITHOUT
## ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
## FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
## for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301 USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} damp (@var{p}, @var{tsam})
## Displays eigenvalues, natural frequencies and damping ratios
## of the eigenvalues of a matrix @var{p} or the @math{A} matrix of a
## system @var{p}, respectively.
## If @var{p} is a system, @var{tsam} must not be specified.
## If @var{p} is a matrix and @var{tsam} is specified, eigenvalues
## of @var{p} are assumed to be in @var{z}-domain.
## @seealso{eig}
## @end deftypefn

## Author: Kai P. Mueller <mueller@ifr.ing.tu-bs.de>
## Created: September 29, 1997.

function damp (p, tsam)

  ## assume a continuous system
  DIGITAL = 0;
  if(nargin < 1 || nargin > 2)
    usage("damp(p,[ tsamp])")
  endif
  if(isstruct(p))
    if (nargin != 1)
      error("damp: when p is a system, tsamp parameter is not allowed.");
    endif
    [aa, b, c, d, t_samp] = sys2ss(p);
    DIGITAL = is_digital(p);
  else
    aa = p;
    if (nargin == 2)
        DIGITAL = 1;
        t_samp = tsam;
    endif
  endif
  if (!issquare(aa))
    error("damp: Matrix p is not square.")
  endif
  if (DIGITAL && t_samp <= 0.0)
    error("damp: Sampling time tsam must not be <= 0.")
  endif

  ## all checks done.
  e = eig(aa);
  [n, m] = size(aa);
  if (DIGITAL)
    printf("  (discrete system with sampling time %f)\n", t_samp);
  endif
  printf("............... Eigenvalue ...........     Damping     Frequency\n");
  printf("--------[re]---------[im]--------[abs]----------------------[Hz]\n");
  for i = 1:n
    pole = e(i);
    cpole = pole;
    if (DIGITAL)
      cpole = log(pole) / t_samp;
    endif
    d0 = -cos(atan2(imag(cpole), real(cpole)));
    f0 = 0.5 / pi * abs(cpole);
    if (abs(imag(cpole)) < eps)
      printf("%12f         ---  %12f  %10f  %12f\n",
             real(pole), abs(pole), d0, f0);
    else
      printf("%12f %12f %12f  %10f  %12f\n",
             real(pole), imag(pole), abs(pole), d0, f0);
    endif
  endfor

endfunction
