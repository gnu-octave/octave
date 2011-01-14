## Copyright (C) 1998-2011 Nicol N. Schraudolph
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
## @deftypefn {Mapping Function} {} betaln (@var{a}, @var{b})
## Return the natural logarithm of the Beta function,
## @tex
## $$
##  B (a, b) = \log {\Gamma (a) \Gamma (b) \over \Gamma (a + b)}.
## $$
## @end tex
## @ifnottex
##
## @example
## betaln (a, b) = gammaln (a) + gammaln (b) - gammaln (a + b)
## @end example
## 
## @end ifnottex
## @seealso{beta, betainc, gammaln}
## @end deftypefn

## Author:   Nicol N. Schraudolph <nic@idsia.ch>
## Created:  06 Aug 1998
## Keywords: log beta special function

function retval = betaln (a, b)
  if (nargin != 2)
    print_usage ();
  endif

  retval = gammaln (a) + gammaln (b) - gammaln (a + b);
endfunction

%!assert (betaln(3,4),log(beta(3,4)),eps)

%!error (betaln(1.))
%!error (betaln(1.,1.,1.))
