## Copyright (C) 1995, 1996, 1997  Kurt Hornik
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## This program is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this file.  If not, write to the Free Software Foundation,
## 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

## usage:  lognormal_rnd (a, v [, r, c])
##
## lognormal_rnd (a, v) returns a matrix of random samples from the
## lognormal distribution with parameters a and v.  The size of the
## matrix is the common size of a and v.
##
## lognormal_rnd (a, v, r, c) returns an r by c matrix of random samples
## from the lognormal distribution with parameters a and v. Both a and v
## must be scalar or of size r by c.

## Author:  KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description:  Random deviates from the log normal distribution

function rnd = lognormal_rnd (a, v, r, c)

  if (nargin == 4)
    if ( !(is_scalar (r) && (r > 0) && (r == round (r))) )
      error ("lognormal_rnd:  r must be a positive integer");
    endif
    if ( !(is_scalar (c) && (c > 0) && (c == round (c))) )
      error ("lognormal_rnd:  c must be a positive integer");
    endif
    [retval, a, v] = common_size (a, v, zeros (r, c));
    if (retval > 0)
      error (strcat("lognormal_rnd:  ",
                    "a and v must be scalar or of size ",
                    sprintf ("%d by %d", r, c)));
    endif
  elseif (nargin == 2)
    [retval, a, v] = common_size (a, v);
    if (retval > 0)
      error (strcat("lognormal_rnd:  ",
                    "a and v must be of common size or scalar"));
    endif
  else
    usage ("lognormal_rnd (a, v [, r, c])");
  endif

  [r, c] = size (a);
  s = r * c;
  a = reshape (a, 1, s);
  v = reshape (v, 1, s);
  rnd = zeros (1, s);

  k = find (!(a > 0) | !(a < Inf) | !(v > 0) | !(v < Inf));
  if any (k)
    rnd(k) = NaN * ones (1, length (k));
  endif

  k = find ((a > 0) & (a < Inf) & (v > 0) & (v < Inf));
  if any (k)
    rnd(k) = a(k) .* exp (sqrt (v(k)) .* randn (1, length (k)));
  endif

  rnd = reshape (rnd, r, c);

endfunction