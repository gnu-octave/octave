### Copyright (C) 1996 John W. Eaton
###
### This file is part of Octave.
###
### Octave is free software; you can redistribute it and/or modify it
### under the terms of the GNU General Public License as published by
### the Free Software Foundation; either version 2, or (at your option)
### any later version.
###
### Octave is distributed in the hope that it will be useful, but
### WITHOUT ANY WARRANTY; without even the implied warranty of
### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
### General Public License for more details.
###
### You should have received a copy of the GNU General Public License
### along with Octave; see the file COPYING.  If not, write to the Free
### Software Foundation, 59 Temple Place - Suite 330, Boston, MA
### 02111-1307, USA.

function y = gammai (a, x)
  
  ## usage: gammai (a, x)
  ##
  ## Computes the incomplete gamma function
  ##
  ##   gammai (a, x) 
  ##     = (integral from 0 to x of exp(-t) t^(a-1) dt) / gamma(a).
  ##
  ## If a is scalar, then gammai(a, x) is returned for each element of x
  ## and vice versa.
  ##
  ## If neither a nor x is scalar, the sizes of a and x must agree, and
  ## gammai is applied pointwise.
  
  ## Written by KH (Kurt.Hornik@ci.tuwien.ac.at) on Aug 13, 1994

  if (nargin != 2)
    usage ("gammai (a, x)");
  endif
  
  [r_a, c_a] = size (a);
  [r_x, c_x] = size (x);
  e_a = r_a * c_a;
  e_x = r_x * c_x;
  
  ## The following code is rather ugly.  We want the function to work
  ## whenever a and x have the same size or a or x is scalar.  
  ## We do this by reducing the latter cases to the former.
  
  if (e_a == 0 || e_x == 0)
    error ("gammai: both a and x must be nonempty");
  endif
  if (r_a == r_x && c_a == c_x)
    n   = e_a;
    a   = reshape (a, 1, n);
    x   = reshape (x, 1, n);
    r_y = r_a;
    c_y = c_a;
  elseif (e_a == 1)
    n   = e_x;
    a   = a * ones (1, n);
    x   = reshape (x, 1, n);
    r_y = r_x;
    c_y = c_x;
  elseif (e_x == 1)
    n   = e_a;
    a   = reshape (a, 1, n);
    x   = x * ones (1, n);
    r_y = r_a;
    c_y = c_a;
  else
    error ("gammai: a and x must have the same size if neither is scalar"); 
  endif

  ## Now we can do sanity checking ...
  
  if (any (a <= 0) || any (a == Inf))
    error ("gammai: all entries of a must be positive anf finite");
  endif
  if (any (x < 0))
    error ("gammai: all entries of x must be nonnegative");
  endif
  
  y = zeros (1, n);

  ## For x < a + 1, use summation.  The below choice of k should ensure
  ## that the overall error is less than eps ... 

  S = find ((x > 0) & (x < a + 1));
  s = length (S);
  if (s > 0)
    k   = ceil (- max ([a(S), x(S)]) * log (eps));
    K   = (1:k)';
    M   = ones (k, 1);
    A   = cumprod ((M * x(S)) ./ (M * a(S) + K * ones(1, s)));
    y(S) = exp (-x(S) + a(S) .* log (x(S))) .* (1 + sum (A)) ./ gamma (a(S)+1);
  endif

  ## For x >= a + 1, use the continued fraction.
  ## Note, however, that this converges MUCH slower than the series
  ## expansion for small a and x not too large!

  S = find ((x >= a + 1) & (x < Inf));
  s = length (S);
  if (s > 0)
    t1 = zeros (1, s);
    t2 = ones (1, s);
    u   = [t1; t2];
    v   = [t2; x(S)];
    c_old = 0;
    c_new = v(1,:) ./ v(2,:);
    n   = 1;
    while (max (abs (c_old ./ c_new - 1)) > 10 * eps)
      c_old = c_new;
      u = v + u .* (ones (2, 1) * (n - a(S)));
      v = u .* (ones (2, 1) * x(S)) + n * v;
      c_new = v(1,:) ./ v(2,:);
      n = n + 1;
    endwhile
    y(S) = 1 - exp (-x(S) + a(S) .* log (x(S))) .* c_new ./ gamma (a(S));
  endif
  
  y (find (x == Inf)) = ones (1, sum (x == Inf));
  
  y = reshape (y, r_y, c_y);

endfunction
