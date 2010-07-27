## Copyright (C) 1995, 1996, 1997, 1998, 2000, 2002, 2005, 2006, 2007, 2009
##               Kurt Hornik
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
## @deftypefn {Function File} {} logit (@var{p})
## For each component of @var{p}, return the logit of @var{p} defined as
## @tex
## $$
## {\rm logit}(p) = \log\Big({p \over 1-p}\Big)
## $$
## @end tex
## @ifnottex
##
## @example
## logit(@var{p}) = log (@var{p} / (1-@var{p}))
## @end example
##
## @end ifnottex
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Logit transformation

function y = logit (p)

  if (nargin == 1)
    y = logistic_inv (p);
  else
    print_usage ();
  endif

endfunction
