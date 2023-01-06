########################################################################
##
## Copyright (C) 2006-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{sout} =} orderfields (@var{s1})
## @deftypefnx {} {@var{sout} =} orderfields (@var{s1}, @var{s2})
## @deftypefnx {} {@var{sout} =} orderfields (@var{s1}, @{@var{cellstr}@})
## @deftypefnx {} {@var{sout} =} orderfields (@var{s1}, @var{p})
## @deftypefnx {} {[@var{sout}, @var{p}] =} orderfields (@dots{})
## Return a @emph{copy} of @var{s1} with fields arranged alphabetically, or as
## specified by the second input.
##
## Given one input struct @var{s1}, arrange field names alphabetically.
##
## If a second struct argument is given, arrange field names in @var{s1} as
## they appear in @var{s2}.  The second argument may also specify the order
## in a cell array of strings @var{cellstr}.  The second argument may also
## be a permutation vector.
##
## The optional second output argument @var{p} is the permutation vector which
## converts the original name order to the new name order.
##
## Examples:
##
## @example
## @group
## s = struct ("d", 4, "b", 2, "a", 1, "c", 3);
## t1 = orderfields (s)
##   @result{} t1 =
##        scalar structure containing the fields:
##          a =  1
##          b =  2
##          c =  3
##          d =  4
## @end group
## @end example
##
## @example
## @group
## t = struct ("d", @{@}, "c", @{@}, "b", @{@}, "a", @{@});
## t2 = orderfields (s, t)
##   @result{} t2 =
##        scalar structure containing the fields:
##          d =  4
##          c =  3
##          b =  2
##          a =  1
## @end group
## @end example
##
## @example
## @group
## t3 = orderfields (s, [3, 2, 4, 1])
##   @result{} t3 =
##        scalar structure containing the fields:
##          a =  1
##          b =  2
##          c =  3
##          d =  4
## @end group
## @end example
##
## @example
## @group
## [t4, p] = orderfields (s, @{"d", "c", "b", "a"@})
##   @result{} t4 =
##        scalar structure containing the fields:
##          d =  4
##          c =  3
##          b =  2
##          a =  1
##      p =
##         1
##         4
##         2
##         3
## @end group
## @end example
##
## @seealso{fieldnames, getfield, setfield, rmfield, isfield, isstruct, struct}
## @end deftypefn

function [sout, p] = orderfields (s1, s2)

  if (nargin < 1)
    print_usage ();
  endif

  if (! isstruct (s1))
    error ("orderfields: S1 must be a struct");
  endif

  names = fieldnames (s1);

  if (nargin == 1)
    ## One structure: return the fields in alphabetical order.
    [~, p] = sort (names);
  elseif (nargin == 2)

    if (isstruct (s2))
      ## Two structures: return the fields in the order of s2.
      names2 = fieldnames (s2);
      [ns1, idx1] = sort (names);
      [ns2, idx2] = sort (names2);
      if (! isequal (ns1, ns2))
        error ("orderfields: structures S1 and S2 do not have the same fields");
      endif
      p = eye (numel (idx2))(idx2,:).' * idx1;

    elseif (iscellstr (s2))
      ## A structure and a list of fields: order by the list of fields.
      names2 = s2(:);
      [ns1, idx1] = sort (names);
      [ns2, idx2] = sort (names2);
      if (! isequal (ns1, ns2))
        error ("orderfields: CELLSTR list does not match structure fields");
      endif
      p = eye (numel (idx2))(idx2,:).' * idx1;

    elseif (isnumeric (s2))
      ## A structure and a permutation vector: permute the order of s1.
      p = s2(:);
      if (! isequal (sort (p), (1:numel (names)).'))
        error ("orderfields: invalid permutation vector P");
      endif

    else
      error ("orderfields: second argument must be structure, cellstr, or permutation vector");
    endif
  endif

  ## Permute the names in the structure.
  names = names(p);
  C = struct2cell (s1);
  C = C(p,:);
  sout = cell2struct (C, names);
  ## Inherit dimensions.
  sout = reshape (sout, size (s1));

endfunction


%!shared a, b, c
%! a = struct ("C", {1, 2}, "A", {3, 4}, "B", {5, 6});
%! b = struct ("A", 1, "B", 2, "C", 3);
%! c = struct ("B", {7, 8}, "C", 9, "A", 10);
%!test
%! x = orderfields (b, a);
%! assert (fieldnames (x), {"C"; "A"; "B"});
%! assert (x.A, 1);
%! assert (x.B, 2);
%! assert (x.C, 3);
%!test
%! [x, p] = orderfields (b, [3 2 1]);
%! assert (fieldnames (x), {"C"; "B"; "A"});
%! assert (p, [3; 2; 1]);
%! assert (x.A, 1);
%! assert (x.B, 2);
%! assert (x.C, 3);
%!test
%! x = orderfields (b, {"B", "C", "A"});
%! assert (fieldnames (x), {"B"; "C"; "A"});
%! assert (x.A, 1);
%! assert (x.B, 2);
%! assert (x.C, 3);
%!test
%! x(1:2) = orderfields (c, {"C", "A", "B"});
%! assert (fieldnames (x), {"C"; "A"; "B"});
%! assert (x(2).A, 10);
%! assert (x(2).B, 8);
%! assert (x(2).C, 9);

%!test
%! aa.x = {1, 2};
%! aa.y = 3;
%! aa(2).x = {4, 5};
%! bb.y = {6, 7};
%! bb.x = 8;
%! aa(2) = orderfields (bb, aa);
%! assert (aa(2).x, 8);
%! assert (aa(2).y{1}, 6);

## Corner case of empty struct
%!assert <*40224> (orderfields (struct ()), struct ())
%!test
%! s(2,2).a = 1;
%! s(1,1).b = 2;
%! s = resize (s, [1 0]);
%! s2 = orderfields (s, {"b", "a"});
%! assert (fieldnames (s2), {"b"; "a"});
%! assert (size_equal (s, s2));

## Test input validation
%!error <Invalid call> orderfields ()
%!error <S1 must be a struct> orderfields (1)
%!error <S1 and S2 do not have the same fields>
%! s1.a = 1;
%! s2.b = 2;
%! orderfields (s1, s2);
%!error <CELLSTR list does not match structure fields>
%! s1.a = 1;
%! orderfields (s1, {"b"});
%!error <invalid permutation vector P>
%! s1.a = 1;
%! orderfields (s1, [2 1]);
%!error <invalid permutation vector P>
%! s1.a = 1;
%! orderfields (s1, ones (2,2));
%!error <second argument must be structure, cellstr, or permutation vector>
%! s1.a = 1;
%! orderfields (s1, "foobar");
