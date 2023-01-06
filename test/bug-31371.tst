########################################################################
##
## Copyright (C) 2012-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

%!test <*31371>
%! % Work around MATLAB bug where f(x)(y) is invalid syntax
%! % (This bug does not apply to Octave)
%!
%! C = @(fcn,x) fcn(x);
%! C2 = @(fcn,x,y) fcn(x,y);
%!
%! % Church Booleans
%! T = @(t,f) t;
%! F = @(t,f) f;
%!
%! % Church Numerals
%! Zero  = @(fcn,x) x;
%! One   = @(fcn,x) fcn(x);
%! Two   = @(fcn,x) fcn(fcn(x));
%! Three = @(fcn,x) fcn(fcn(fcn(x)));
%! Four  = @(fcn,x) fcn(fcn(fcn(fcn(x))));
%!
%! % Arithmetic Operations
%! Inc = @(a) @(f,x) f(a(f,x)); % Increment
%! Add = @(a,b) @(f,x) a(f,b(f,x));
%! Mult = @(a,b) @(f,x) a(@(x) b(f,x),x);
%! Dec = @(a) @(f,x) C(a(@(g) @(h) h(g(f)), @(u) x), @(u) u); % Decrement
%! Sub = @(a,b) b(Dec, a);
%!
%! % Renderer - Convert church numeral to "real" number
%! Render = @(n) n(@(n) n+1,0);
%!
%! % Predicates
%! Iszero = @(n) n(@(x) F, T);
%!
%! % Y combinator implements recursion
%! Ycomb = @(f) C(@(g) f(@(x) C(g(g), x)), ...
%!                @(g) f(@(x) C(g(g), x)));
%!
%! Factorial = Ycomb(@(f) @(n) C(C2(Iszero(n), ...
%!                   @(d) One, @(d) Mult(n, f(Dec(n)))),0));
%!
%! assert (Render (Factorial (Two)), 2);
%! assert (Render (Factorial (Three)), 6);
%! assert (Render (Factorial (Four)), 24);
