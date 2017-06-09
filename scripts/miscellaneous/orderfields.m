## Copyright (C) 2006-2017 Paul Kienzle
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

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
##      @result{} t1 =
##         @{
##           a =  1
##           b =  2
##           c =  3
##           d =  4
##         @}
## @end group
## @end example
##
## @example
## @group
## t = struct ("d", @{@}, "c", @{@}, "b", @{@}, "a", @{@});
## t2 = orderfields (s, t)
##      @result{} t2 =
##         @{
##           d =  4
##           c =  3
##           b =  2
##           a =  1
##         @}
## @end group
## @end example
##
## @example
## @group
## t3 = orderfields (s, [3, 2, 4, 1])
##      @result{} t3 =
##         @{
##           a =  1
##           b =  2
##           c =  3
##           d =  4
##         @}
## @end group
## @end example
##
## @example
## @group
## [t4, p] = orderfields (s, @{"d", "c", "b", "a"@})
##      @result{} t4 =
##         @{
##           d =  4
##           c =  3
##           b =  2
##           a =  1
##         @}
##         p =
##            1
##            4
##            2
##            3
## @end group
## @end example
##
## @seealso{fieldnames, getfield, setfield, rmfield, isfield, isstruct, struct}
## @end deftypefn

## Author: Paul Kienzle <pkienzle@users.sf.net>
## Adapted-By: jwe

function [sout, p] = orderfields (s1, s2)

  if (nargin < 1 || nargin > 2)
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
      p = idx1(idx2);

    elseif (iscellstr (s2))
      ## A structure and a list of fields: order by the list of fields.
      names2 = s2(:);
      [ns1, idx1] = sort (names);
      [ns2, idx2] = sort (names2);
      if (! isequal (ns1, ns2))
        error ("orderfields: CELLSTR list does not match structure fields");
      endif
      p = idx1(idx2);

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
%! a = struct ("foo", {1, 2}, "bar", {3, 4});
%! b = struct ("bar", 6, "foo", 5);
%! c = struct ("bar", {7, 8}, "foo", 9);
%!test
%! a(2) = orderfields (b, a);
%! assert (a(2).foo, 5);
%! assert (a(2).bar, 6);
%!test
%! [a(2), p] = orderfields (b, [2 1]);
%! assert (a(2).foo, 5);
%! assert (a(2).bar, 6);
%! assert (p, [2; 1]);
%!test
%! a(2) = orderfields (b, fieldnames (a));
%! assert (a(2).foo, 5);
%! assert (a(2).bar, 6);
%!test
%! a(1:2) = orderfields (c, fieldnames (a));
%! assert (a(2).foo, 9);
%! assert (a(2).bar, 8);

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
%!error orderfields ()
%!error orderfields (1,2,3)
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
