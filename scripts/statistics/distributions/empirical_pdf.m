## Copyright (C) 2012 Rik Wehbring
## Copyright (C) 1996-2012 Kurt Hornik
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
## @deftypefn {Function File} {} empirical_pdf (@var{x}, @var{data})
## For each element of @var{x}, compute the probability density function
## (PDF) at @var{x} of the empirical distribution obtained from the
## univariate sample @var{data}.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: PDF of the empirical distribution

function pdf = empirical_pdf (x, data)

  if (nargin != 2)
    print_usage ();
  endif

  if (! isvector (data))
    error ("empirical_pdf: DATA must be a vector");
  endif

  pdf = discrete_pdf (x, data, ones (size (data)));

endfunction


%!shared x,v,y
%! x = [-1 0.1 1.1 1.9 3];
%! v = 0.1:0.2:1.9;
%! y = [0 0.1 0.1 0.1 0];
%!assert(empirical_pdf (x, v), y);

%% Test class of input preserved
%!assert(empirical_pdf (single(x), v), single (y));
%!assert(empirical_pdf (x, single(v)), single (y));

%% Test input validation
%!error empirical_pdf ()
%!error empirical_pdf (1)
%!error empirical_pdf (1,2,3)
%!error empirical_inv (1, ones(2))

