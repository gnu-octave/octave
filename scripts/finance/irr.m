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

## -*- texinfo -*-
## @deftypefn {Function File} {} irr (@var{p}, @var{i})
## Return the internal rate of return of a series of payments @var{p}
## from an initial investment @var{i} (i.e., the solution of
## @code{npv (r, p) = i}.  If the second argument is omitted, a value of
## 0 is used.
## @end deftypefn
## @seealso{npv, pv, and rate}

## Author:  KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description:  Internal rate of return of an investment

function r = irr (p, i)

  if (nargin == 1)
    i = 0;
  elseif !(nargin == 2)
    usage ("irr (p [, i])");
  endif

  tmp = output_precision;
  output_precision = 15;
  if !(is_vector (p))
    error ("irr:  p must be a vector");
  else
    p_string = type p;
  endif

  if !is_scalar (i)
    error ("irr:  i must be a scalar");
  endif

  string = ["function delta = f (r) ", ...
      "delta = npv (r, %s) - %g;  end"];
  eval (sprintf (string, p_string, i));

  r = fsolve ("f", 0.01);

  output_precision = tmp;

endfunction
