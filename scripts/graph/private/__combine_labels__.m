########################################################################
##
## Copyright (C) 2026 The Octave Project Developers
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
## @deftypefn {} {@var{c} =} __combine_labels__ (@var{data}, @var{n})
## Private helper: combine an ordered list of per-row label columns
## into a single column vector of positive integer color codes such
## that equal row-tuples receive equal codes.
##
## @var{data} is a cell row of @math{K} columns.  Each column has
## @var{n} rows and may be a numeric vector, a cellstr, a char matrix
## with one row per item, or a logical vector.  The @math{k}-th entry
## in every column is considered the @math{k}-th component of row
## @math{k}'s label tuple.
##
## The return value @var{c} is an @math{n}-by-@math{1} positive
## integer vector.  Tuples are serialised to @qcode{NUL}-delimited
## strings and fed through @code{unique} so that colors are assigned
## in ascending lexicographic order of the tuple representation.
##
## This helper is used by @code{__isomorphism_parse_opts__} when
## converting Node / Edge table variables into VF2 color vectors.
## @seealso{isomorphism, __isomorphism_parse_opts__}
## @end deftypefn

function c = __combine_labels__ (data, n)

  if (nargin != 2)
    error ("Octave:invalid-fun-call", ...
           "__combine_labels__: expected 2 arguments (data, n)");
  endif

  if (! iscell (data))
    error ("Octave:invalid-input-arg", ...
           "__combine_labels__: DATA must be a cell array");
  endif

  K = numel (data);
  if (K == 0)
    c = zeros (n, 1);
    return;
  endif

  if (n == 0)
    c = zeros (0, 1);
    return;
  endif

  keys = cell (n, 1);
  for i = 1 : n
    parts = cell (1, K);
    for k = 1 : K
      v = data{k};
      if (iscell (v))
        parts{k} = v{i};
      elseif (ischar (v))
        parts{k} = v(i, :);
      elseif (islogical (v))
        if (v(i))
          parts{k} = "1";
        else
          parts{k} = "0";
        endif
      else
        ## Numeric -- %.17g gives a round-trip-safe representation of
        ## a double-precision value (17 significant digits), so two
        ## bit-identical doubles produce the same key and distinct
        ## doubles produce distinct keys.
        parts{k} = sprintf ("%.17g", double (v(i)));
      endif
    endfor
    keys{i} = strjoin (parts, char (1));
  endfor

  [~, ~, c] = unique (keys);
  c = c(:);

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## Single cellstr column: matching strings get matching colors.
%!test
%! c = __combine_labels__ ({{"a"; "b"; "a"; "c"}}, 4);
%! assert (size (c), [4, 1]);
%! assert (c(1), c(3));
%! assert (c(1) != c(2));
%! assert (c(2) != c(4));

## Single numeric column.
%!test
%! c = __combine_labels__ ({[10; 20; 10; 30]}, 4);
%! assert (c(1), c(3));
%! assert (c(1) != c(2));

## Two columns: tuple (a, 1) vs (a, 2) differ even though first matches.
%!test
%! c = __combine_labels__ ({{"a"; "a"; "b"}, [1; 2; 1]}, 3);
%! assert (c(1) != c(2));

## Two columns: matching tuples share a color.
%!test
%! c = __combine_labels__ ({{"a"; "b"; "a"}, [1; 2; 1]}, 3);
%! assert (c(1), c(3));
%! assert (c(1) != c(2));

## Logical column.
%!test
%! c = __combine_labels__ ({[true; false; true]}, 3);
%! assert (c(1), c(3));
%! assert (c(1) != c(2));

## n=0 returns zeros (0, 1).
%!test
%! c = __combine_labels__ ({{}}, 0);
%! assert (size (c), [0, 1]);

## Missing 2 args.
%!error __combine_labels__ ({{"a"; "b"}})

## data not a cell.
%!error <cell> ...
%! __combine_labels__ (42, 3)

## Color codes start from 1 (no zero colors).
%!test
%! c = __combine_labels__ ({{"x"; "y"; "z"}}, 3);
%! assert (min (c), 1);
