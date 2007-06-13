## Copyright (C) 2006  Paul Kienzle
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {[@var{t}, @var{p}] =} orderfields (@var{s1}, @var{s2})
## Return a struct with fields arranged alphabetically or as specified
## by @var{s2} and a corresponding permutation vector.
##
## Given one struct, arrange field names in @var{s1} alphabetically.
##
## Given two structs, arrange field names in @var{s1} as they appear
## in @var{s2}.  The second argument may also specify the order in
## a permutation vector or a cell array of strings.
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
    if (isstruct(s1))
      names = sort (fieldnames (s1));
    endif
  elseif (nargin == 2)
    if (isstruct(s2))
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
      t2 = 1:length (names);
      if (! isequal (t1, t2))
	error ("orderfields: invalid permutation vector");
      endif
      names = names(s2);
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
  for i = 1:length (names)
    el = names(i);
    t(:).(el) = s1(:).(el);
  endfor

endfunction
