## Copyright (C) 2013, Roberto Porcu' <roberto.porcu@polimi.it>
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
## @deftypefn {Command} {[@var{sum}] =} kahan (@var{sum},
## @var{comp}, @var{temp})
## @deftypefnx {Command} {[@var{sum}, @var{comp}] =} kahan (@var{sum},
## @var{comp}, @var{temp})
##
## This function is the implementation of the Kahan summation algorithm,
## also known as compensated summation. It significantly reduces the numerical
## error in the total obtained by adding a sequence of finite precision
## floating point numbers, compared to the obvious approach. For more details
## see @url{http://en.wikipedia.org/wiki/Kahan_summation_algorithm}.
## This function is called in @command{integrate_adaptive} and in
## @command{integrate_const} to better catch equality comparisons.
##
## The first input argument is the variable that will contain the summation,
## so that is also returned as first output argument in order to reuse it in
## next calls to kahan function.
##
## The second input argument contains the compensation term and it is returned
## as second output argument so that it can be reused in the next calls
## of the same computation.
##
## The third input argument is the variable that contains the term to
## be added to @var{Sum}.
## @end deftypefn

function [Sum, comp] = kahan (Sum, comp, term)

    y = term - comp;
    t = Sum + y;
    comp = (t - Sum) - y;
    Sum = t;

endfunction

## Local Variables: ***
## mode: octave ***
## End: ***
