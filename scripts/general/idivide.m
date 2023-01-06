########################################################################
##
## Copyright (C) 2008-2023 The Octave Project Developers
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
## @deftypefn {} {@var{C} =} idivide (@var{A}, @var{B}, @var{op})
## Integer division with different rounding rules.
##
## The standard behavior of integer division such as @code{@var{A} ./ @var{B}}
## is to round the result to the nearest integer.  This is not always the
## desired behavior and @code{idivide} permits integer element-by-element
## division to be performed with different treatment for the fractional
## part of the division as determined by the @var{op} flag.  @var{op} is
## a string with one of the values:
##
## @table @asis
## @item @qcode{"fix"}
## Calculate @code{@var{A} ./ @var{B}} with the fractional part rounded
## towards zero.
##
## @item @qcode{"round"}
## Calculate @code{@var{A} ./ @var{B}} with the fractional part rounded
## towards the nearest integer.
##
## @item @qcode{"floor"}
## Calculate @code{@var{A} ./ @var{B}} with the fractional part rounded
## towards negative infinity.
##
## @item @qcode{"ceil"}
## Calculate @code{@var{A} ./ @var{B}} with the fractional part rounded
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
## @seealso{ceil, floor, fix, round, ldivide, rdivide}
## @end deftypefn

function C = idivide (A, B, op)

  if (nargin < 2)
    print_usage ();
  endif

  if (nargin == 2)
    op = "fix";
  else
    op = tolower (op);
  endif

  if (! isinteger (A) && ! isinteger (B))
    error ("idivide: at least one input (A or B) must be an integer type");
  elseif (isinteger (A) && isinteger (B) && ! strcmp (class (A), class (B)))
    error ("idivide: integer type of A (%s) must match integer type of B (%s)",
           class (A), class (B));
  endif

  C = A ./ B;
  if (strcmp (op, "fix"))
    ## The following is an optimized version of `C -= (C .* B > A) .* sign (B)`.
    if (isscalar (B))
      if (B > 0)
        C -= (C * B > A);
      else
        C += (C * B > A);
      endif
    else
      y_sel = (B > 0);
      if (isscalar (A))
        C(y_sel) -= (C(y_sel) .* B(y_sel) > A);
        y_sel = ! y_sel;
        C(y_sel) += (C(y_sel) .* B(y_sel) > A);
      else
        C(y_sel) -= (C(y_sel) .* B(y_sel) > A(y_sel));
        y_sel = ! y_sel;
        C(y_sel) += (C(y_sel) .* B(y_sel) > A(y_sel));
      endif
    endif
  elseif (strcmp (op, "round"))
    return;
  elseif (strcmp (op, "floor"))
    ## The following is an optimized version of `C -= (C .* abs (B) > sign (B) .* A)`.
    if (isscalar (B))
      if (B > 0)
        C -= (C * B > A);
      else
        C -= (C * B < A);
      endif
    else
      y_sel = (B > 0);
      if (isscalar (A))
        C(y_sel) -= (C(y_sel) .* B(y_sel) > A);
        y_sel = ! y_sel;
        C(y_sel) -= (C(y_sel) .* B(y_sel) < A);
      else
        C(y_sel) -= (C(y_sel) .* B(y_sel) > A(y_sel));
        y_sel = ! y_sel;
        C(y_sel) -= (C(y_sel) .* B(y_sel) < A(y_sel));
      endif
    endif
  elseif (strcmp (op, "ceil"))
    ## The following is an optimized version of `C += (C .* abs (B) < sign (B) .* A)`.
    if (isscalar (B))
      if (B > 0)
        C += (C * B < A);
      else
        C += (C * B > A);
      endif
    else
      y_sel = (B > 0);
      if (isscalar (A))
        C(y_sel) += (C(y_sel) .* B(y_sel) < A);
        y_sel = ! y_sel;
        C(y_sel) += (C(y_sel) .* B(y_sel) > A);
      else
        C(y_sel) += (C(y_sel) .* B(y_sel) < A(y_sel));
        y_sel = ! y_sel;
        C(y_sel) += (C(y_sel) .* B(y_sel) > A(y_sel));
      endif
    endif
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

%!shared c, d
%! c = int64 (4e16);
%! d = int64 ([-2e8, 2e8]);

%!assert <*61319> (idivide (c, d + int64 (1)), d + int64 ([-1, -1]))
%!assert <*61319> (idivide (c, d + int64 (1), "floor"), d + int64 ([-2, -1]))
%!assert <*61319> (idivide (c, d + int64 (1), "ceil"), d + int64 ([-1, 0]))
%!assert <*61319> (idivide (c, d + int64 (1), "round"), d + int64 ([-1, -1]))

%!assert <*61319> (idivide (c + int64 (1), d), d)
%!assert <*61319> (idivide (c + int64 (1), d, "floor"), d + int64 ([-1, 0]))
%!assert <*61319> (idivide (c + int64 (1), d, "ceil"), d + int64 ([0, 1]))
%!assert <*61319> (idivide (c + int64 (1), d, "round"), d)

## Test input validation
%!error idivide (uint8 (1))
%!error idivide (uint8 (1), 2, 3)
%!error <at least one input> idivide (1, 2)
%!error <at least one input> idivide ({1}, 2)
%!error <A \(int8\) must match.* B \(uint8\)> idivide (int8 (1), uint8 (2))
%!error <unrecognized rounding type "foo"> idivide (int8 (1), 2, "foo")
