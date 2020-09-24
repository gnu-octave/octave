########################################################################
##
## Copyright (C) 2008-2020 The Octave Project Developers
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

## -*- texinfo -*-
## @deftypefn {} {} idivide (@var{x}, @var{y}, @var{op})
## Integer division with different rounding rules.
##
## The standard behavior of integer division such as @code{@var{a} ./ @var{b}}
## is to round the result to the nearest integer.  This is not always the
## desired behavior and @code{idivide} permits integer element-by-element
## division to be performed with different treatment for the fractional
## part of the division as determined by the @var{op} flag.  @var{op} is
## a string with one of the values:
##
## @table @asis
## @item @qcode{"fix"}
## Calculate @code{@var{a} ./ @var{b}} with the fractional part rounded
## towards zero.
##
## @item @qcode{"round"}
## Calculate @code{@var{a} ./ @var{b}} with the fractional part rounded
## towards the nearest integer.
##
## @item @qcode{"floor"}
## Calculate @code{@var{a} ./ @var{b}} with the fractional part rounded
## towards negative infinity.
##
## @item @qcode{"ceil"}
## Calculate @code{@var{a} ./ @var{b}} with the fractional part rounded
## towards positive infinity.
## @end table
##
## @noindent
## If @var{op} is not given it defaults to @qcode{"fix"}.
## An example demonstrating these rounding rules is
##
## @example
## @group
## idivide (int8 ([-3, 3]), int8 (4), "fix")
##   @result{}   0   0
## idivide (int8 ([-3, 3]), int8 (4), "round")
##   @result{}  -1   1
## idivide (int8 ([-3, 3]), int8 (4), "floor")
##   @result{}  -1   0
## idivide (int8 ([-3, 3]), int8 (4), "ceil")
##   @result{}   0   1
## @end group
## @end example
##
## @seealso{ldivide, rdivide}
## @end deftypefn

function z = idivide (x, y, op)

  if (nargin < 2)
    print_usage ();
  endif

  if (nargin == 2)
    op = "fix";
  else
    op = tolower (op);
  endif

  if (isfloat (x))
    if (isinteger (y))
      typ = class (y);
    else
      error ("idivide: at least one input (X or Y) must be an integer type");
    endif
  elseif (isfloat (y))
    if (isinteger (x))
      typ = class (x);
    else
      error ("idivide: at least one input (X or Y) must be an integer type");
    endif
  else
    typ = class (x);
    if (! strcmp (typ, class (y)))
      error ("idivide: integer type of X (%s) must match integer type of Y (%s)", typ, class (y));
    endif
  endif

  if (strcmp (op, "fix"))
    z = cast (fix (double (x) ./ double (y)), typ);
  elseif (strcmp (op, "round"))
    z = x ./ y;
  elseif (strcmp (op, "floor"))
    z = cast (floor (double (x) ./ double (y)), typ);
  elseif (strcmp (op, "ceil"))
    z = cast (ceil (double (x) ./ double (y)), typ);
  else
    error ('idivide: unrecognized rounding type "%s"', op);
  endif

endfunction


%!shared a, af, b, bf
%! a = int8 (3);
%! af = 3;
%! b = int8 ([-4, 4]);
%! bf = [-4, 4];

%!assert (idivide (a, b), int8 ([0, 0]))
%!assert (idivide (a, b, "floor"), int8 ([-1, 0]))
%!assert (idivide (a, b, "ceil"), int8 ([0, 1]))
%!assert (idivide (a, b, "round"), int8 ([-1, 1]))

%!assert (idivide (af, b), int8 ([0, 0]))
%!assert (idivide (af, b, "floor"), int8 ([-1, 0]))
%!assert (idivide (af, b, "ceil"), int8 ([0, 1]))
%!assert (idivide (af, b, "round"), int8 ([-1, 1]))

%!assert (idivide (a, bf), int8 ([0, 0]))
%!assert (idivide (a, bf, "floor"), int8 ([-1, 0]))
%!assert (idivide (a, bf, "ceil"), int8 ([0, 1]))
%!assert (idivide (a, bf, "round"), int8 ([-1, 1]))

## Test input validation
%!error idivide (uint8 (1))
%!error idivide (uint8 (1), 2, 3)
%!error <at least one input> idivide (1, 2)
%!error <at least one input> idivide ({1}, 2)
%!error <X \(int8\) must match.* Y \(uint8\)> idivide (int8 (1), uint8 (2))
%!error <unrecognized rounding type "foo"> idivide (int8 (1), 2, "foo")
