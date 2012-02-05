## Copyright (C) 2006-2012 Paul Kienzle
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
## @deftypefn  {Function File} {[@var{t}, @var{p}] =} orderfields (@var{s1})
## @deftypefnx {Function File} {[@var{t}, @var{p}] =} orderfields (@var{s1}, @var{s2})
## Return a copy of @var{s1} with fields arranged alphabetically or
## as specified by @var{s2}.
##
## Given one struct, arrange field names in @var{s1} alphabetically.
##
## If the second argument is a struct, arrange field names in @var{s1}
## as they appear in @var{s2}.  The second argument may also specify the
## order in a permutation vector or a cell array of strings containing
## the fieldnames of @var{s1} in the desired order.
##
## The optional second output argument @var{p} is assigned the permutation
## vector
## which converts the original name order into the new name order.
##
## Examples:
##
## @example
## @group
## s = struct("d", 4, "b", 2, "a", 1, "c", 3);
## t1 = orderfields (s)
##      @result{} t1 =
##         @{
##           a =  1
##           b =  2
##           c =  3
##           d =  4
##         @}
## @end group
## @group
## t = struct("d", @{@}, "c", @{@}, "b", "a", @{@});
## t2 = orderfields (s, t)
##      @result{} t2 =
##         @{
##           d =  4
##           c =  3
##           b =  2
##           a =  1
##         @}
## @end group
## @group
## t3 = orderfields (s, [3, 2, 4, 1]);
##      @result{} t3 =
##         @{
##           a =  1
##           b =  2
##           c =  3
##           d =  4
##         @}
## @end group
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
## @seealso{getfield, rmfield, isfield, isstruct, fieldnames, struct}
## @end deftypefn

## Author: Paul Kienzle <pkienzle@users.sf.net>
## Adapted-By: jwe

function [t, p] = orderfields (s1, s2)

  if (nargin == 1 || nargin == 2)
    if (! isstruct (s1))
      error ("orderfields: expecting argument to be a struct");
    endif
  else
    print_usage ();
  endif

  if (nargin == 1)
    ## One structure: return the fields in alphabetical order.
    if (isstruct (s1))
      names = sort (fieldnames (s1));
    endif
  elseif (nargin == 2)
    if (isstruct (s2))
      ## Two structures: return the fields in the order of s2.
      names = fieldnames (s2);
      if (! isequal (sort (fieldnames (s1)), sort (names)))
        error ("orderfields: structures do not have same fields");
      endif
    elseif (iscellstr (s2))
      ## A structure and a list of fields: order by the list of fields.
      t1 = sort (fieldnames (s1));
      t2 = sort (s2(:));
      if (! isequal (t1, t2))
        error ("orderfields: name list does not match structure fields");
      endif
      names = s2;
    elseif (isvector (s2))
      ## A structure and a permutation vector: permute the order of s1.
      names = fieldnames (s1);
      t1 = sort (s2);
      t1 = t1(:)';
      t2 = 1:numel (names);
      if (! isequal (t1, t2))
        error ("orderfields: invalid permutation vector");
      endif
      names = names (s2);
    endif
  endif

  ## Find permutation vector which converts the original name order
  ## into the new name order.  Note: could save a couple of sorts
  ## in some cases, but performance isn't critical.

  if (nargout == 2)
    [oldel, oldidx] = sort (fieldnames (s1));
    [newel, newidx] = sort (names);
    p = oldidx(newidx);
  endif

  ## Permute the names in the structure.
  if (numel (s1) == 0)
    args = cell (1, 2 * numel (names));
    args(1:2:end) = names;
    args(2:2:end) = {[]};
    t = struct (args{:});
  else
    n = numel (s1);
    for i = 1:numel (names)
      el = names(i);
      [t(1:n).(el)] = s1(:).(el);
    endfor
    ## inherit dimensions
    t = reshape (t, size (s1));
  endif

endfunction

%!shared a, b, c
%! a = struct ("foo", {1, 2}, "bar", {3, 4});
%! b = struct ("bar", 6, "foo", 5);
%! c = struct ("bar", {7, 8}, "foo", 9);
%!test
%! a(2) = orderfields (b, a);
%! assert (a(2).foo, 5)
%! assert (a(2).bar, 6)
%!test
%! [a(2), p] = orderfields (b, [2 1]);
%! assert (a(2).foo, 5)
%! assert (a(2).bar, 6)
%! assert (p, [2; 1]);
%!test
%! a(2) = orderfields (b, fieldnames (a));
%! assert (a(2).foo, 5)
%! assert (a(2).bar, 6)
%!test
%! a(1:2) = orderfields (c, fieldnames (a));
%! assert (a(2).foo, 9)
%! assert (a(2).bar, 8)

%!test
%! aa.x = {1, 2};
%! aa.y = 3;
%! aa(2).x = {4, 5};
%! bb.y = {6, 7};
%! bb.x = 8;
%! aa(2) = orderfields (bb, aa);
%! assert (aa(2).x, 8);
%! assert (aa(2).y{1}, 6);
