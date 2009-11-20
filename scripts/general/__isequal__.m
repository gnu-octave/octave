## Copyright (C) 2000, 2005, 2006, 2007, 2009 Paul Kienzle
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

## Undocumented internal function.

## -*- texinfo -*-
## @deftypefn {Function File} {} __isequal__ (@var{nans_compare_equal}, @var{x1}, @var{x2}, @dots{})
## Undocumented internal function.
## @end deftypefn

## Return true if @var{x1}, @var{x2}, @dots{} are all equal and
## @var{nans_compare_equal} evaluates to false.
##
## If @var{nans_compare_equal} evaluates to true, then assume NaN == NaN.

## Modified by: William Poetra Yoga Hadisoeseno

## Algorithm:
##
## 1. Determine the class of x
## 2. If x is of the struct, cell, list or char class, for each
##    argument after x, determine whether it has the same class
##    and size as x.
##    Otherwise, for each argument after x, verify that it is not
##    of the struct, cell, list or char class, and that it has
##    the same size as x.
## 3. For each argument after x, compare it for equality with x:
##    a. struct     compare each member by name, not by order (recursive)
##    b. cell/list  compare each member by order (recursive)
##    c. char       compare each member with strcmp
##    d. <other>    compare each nonzero member, and assume NaN == NaN
##                  if nans_compare_equal is nonzero.

function t = __isequal__ (nans_compare_equal, x, varargin)

  if (nargin < 3)
    print_usage ();
  endif

  l_v = nargin - 2;

  ## Generic tests.

  ## Give an error for a list (that will make the code simpler and lists
  ## are deprecated anyway.
  if (islist (x))
    error ("__isequal__: list objects are deprecated and cannot be tested for equality here; use cell arrays instead");
  endif

  ## All arguments must either be of the same class or they must be
  ## numeric values.
  t = (all (strcmp (class(x),
		   cellfun (@class, varargin, "UniformOutput", false)))
       || ((isnumeric (x) || islogical (x))
	   && all ((cellfun (@isnumeric, varargin) | cellfun (@islogical, varargin)))));

  if (t)
    ## Test that everything has the same number of dimensions.
    s_x = size (x);
    s_v = cellfun (@size, varargin, "UniformOutput", false);
    t = all (length (s_x) == cellfun (@length, s_v));
  endif

  if (t)
    ## Test that everything is the same size since it has the same
    ## dimensionality.
    l_x = length (s_x);
    s_v = reshape ([s_v{:}], length (s_x), []);
    idx = 0;
    while (t && idx < l_x)
      idx++;
      t = all (s_x(idx) == s_v(idx, :));
    endwhile
  endif

  if (t)
    ## Check individual classes.
    if (isstruct (x))
      ## Test the number of fields.
      fn_x = fieldnames (x);
      l_fn_x = length (fn_x);
      fn_v = cellfun (@fieldnames, varargin, "UniformOutput", false);
      t = all (l_fn_x == cellfun (@length, fn_v));

      ## Test that all the names are equal.
      idx = 0;
      s_fn_x = sort (fn_x);
      while (t && idx < l_v)
	idx++;
	## We'll allow the fieldnames to be in a different order.
	t = all (strcmp (s_fn_x, sort (fn_v{idx})));
      endwhile

      idx = 0;
      while (t && idx < l_fn_x)
	## Test that all field values are equal.
	idx++;
	args = {nans_compare_equal, {x.(fn_x{idx})}};
	for argn = 1:l_v
	  args{argn+2} = {varargin{argn}.(fn_x{idx})};
	endfor
	## Minimize function calls by calling for all the arguments at
	## once.
        t = __isequal__ (args{:});
      endwhile

    elseif (iscell (x))
      ## Check that each element of a cell is equal.
      l_x = numel (x);
      idx = 0;
      while (t && idx < l_x)
	idx++;
	args = {nans_compare_equal, x{idx}};
	for p = 1:l_v
	  args{p+2} = varargin{p}{idx};
	endfor
        t = __isequal__ (args{:});
      endwhile

    elseif (ischar (x))

      ## Sizes are equal already, so we can just make everything into a
      ## row and test the rows.
      for i = 1:l_v
	strings{i} = reshape (varargin{i}, 1, []);
      endfor
      t = all (strcmp (reshape (x, 1, []), strings));

    else
      ## Check the numeric types.

      if (issparse (x))
	f_x = spfind (x);
      else
	f_x = find (x);
      endif
      l_f_x = length (f_x);
      x = x(f_x);
      for argn = 1:l_v
	y = varargin{argn};
	if (issparse (y))
          f_y = spfind (y);
	else
          f_y = find (y);
	endif

	t = (l_f_x == length (f_y)) && all (f_x == f_y);
	if (!t)
          return;
	endif

	y = y(f_y);
	m = (x == y);
	t = all (m);

	if (!t)
          if (nans_compare_equal)
            t = isnan (x(!m)) && isnan (y(!m));
          else
            return;
          endif
	endif
      endfor

    endif
  endif

endfunction

## test size and shape
%!assert(__isequal__(0,[1,2,3,4],[1,2,3,4]), true)
%!assert(__isequal__(0,[1;2;3;4],[1;2;3;4]), true)
%!assert(__isequal__(0,[1,2,3,4],[1;2;3;4]), false)
%!assert(__isequal__(0,[1,2,3,4],[1,2;3,4]), false)
%!assert(__isequal__(0,[1,2,3,4],[1,3;2,4]), false)

%!test
%! A = 1:8;
%! B = reshape (A, 2, 2, 2);
%! assert (__isequal__ (0, A, B), false);

%!test
%! A = reshape (1:8, 2, 2, 2);
%! B = A;
%! assert (__isequal__ (0, A, B), true);

%!test
%! A = reshape (1:8, 2, 4);
%! B = reshape (A, 2, 2, 2);
%! assert (__isequal__ (0, A, B), false);

## test for equality
%!assert(__isequal__(0,[1,2,3,4],[1,2,3,4]), true)
%!assert(__isequal__(1,{1,2,NaN,4},{1,2,NaN,4}), true)
%!assert(__isequal__(1,[1,2,NaN,4],[1,2,NaN,4]), true)
%!assert(__isequal__(0,['a','b','c','d'],['a','b','c','d']), true)
## Test multi-line strings
%!assert(__isequal__(0,["test";"strings"],["test";"strings"],["test";"strings"]), true)
## test for inequality
%!assert(__isequal__(0,[1,2,3,4],[1;2;3;4]),false)
%!assert(__isequal__(0,{1,2,3,4},[1,2,3,4]),false)
%!assert(__isequal__(0,[1,2,3,4],{1,2,3,4}),false)
%!assert(__isequal__(0,[1,2,NaN,4],[1,2,NaN,4]),false)
%!assert(__isequal__(1,[1,2,NaN,4],[1,NaN,3,4]),false)
%!assert(__isequal__(1,[1,2,NaN,4],[1,2,3,4]),false)
%!assert(__isequal__(0,['a','b','c','d'],['a';'b';'c';'d']),false)
%!assert(__isequal__(0,{'a','b','c','d'},{'a';'b';'c';'d'}),false)
## test for equality (struct)
%!assert(__isequal__(0,struct('a',1,'b',2),struct('a',1,'b',2)),true)
%!assert(__isequal__(0,struct('a',1,'b',2),struct('a',1,'b',2),struct('a',1,'b',2)),true)
%!assert(__isequal__(0,struct('a','abc','b',2),struct('a','abc','b',2)),true)
%!assert(__isequal__(1,struct('a',NaN,'b',2),struct('a',NaN,'b',2),struct('a',NaN,'b',2)),true)
## test for inequality (struct)
%!assert(__isequal__(0,struct('a',NaN,'b',2),struct('a',NaN,'b',2),struct('a',NaN,'b',2)),false)

