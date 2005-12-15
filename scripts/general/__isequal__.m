## Copyright (C) 2000 Paul Kienzle
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

## -*- texinfo -*-
## @deftypefn {Function File} {} __isequal__ (@var{nans_compare_equal}, @var{x1}, @var{x2}, @dots{})
## Return true if @var{x1}, @var{x2}, @dots{} are all equal and
## @var{nans_compare_equal} evaluates to false.
##
## If @var{nans_compare_equal} evaluates to true, then assume NaN == NaN.
## @end deftypefn
##
## @seealso{isequal, isequalwithequalnans}

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
    usage ("__isequal__ (nans_compare_equal, x1, x2, ...)");
  endif

  l_v = nargin - 2;

  if (isstruct (x))

    n_x = length (fieldnames (x));

    t = true;
    for argn = 1:l_v
      y = varargin{argn};
      t = t && isstruct (y) && (n_x == length (fieldnames (y)));
    endfor
    if (!t)
      return;
    endif

    for argn = 1:l_v
      y = varargin{argn};
      for [v, k] = x
	if (iscell (k))
	  fld = y (k{:});
	else
	  fld = y.(k);
	endif
        t = t && struct_contains (y, k) \
              && __isequal__ (nans_compare_equal, v, fld);
      endfor
      if (!t)
        return;
      endif
    endfor

  elseif ((iscell (x)) || (islist (x)))

    x = x(:);
    l_x = length (x);

    t = true;
    for argn = 1:l_v
      y = varargin{argn}(:);
      t = t && (iscell (y) || islist (y)) && (l_x == length (y));
    endfor
    if (!t)
      return;
    endif

    for argn = 1:l_v
      y = varargin{argn}(:);
      for p = 1:l_x
        t = t && __isequal__ (nans_compare_equal, x{p}, y{p});
      endfor
      if (!t)
        return;
      endif
    endfor

  elseif (ischar (x))

    l_x = size (x);

    t = true;
    for argn = 1:l_v
      y = varargin{argn};
      t = t && ischar (y) && (l_x == size (y));
    endfor
    if (!t)
      return;
    endif

    for argn = 1:l_v
      t = t && strcmp (x, varargin{argn});
    endfor

  else

    s_x = size (x);

    t = true;
    for argn = 1:l_v
      y = varargin{argn};
      t = t && (! (isstruct (y) || iscell (y) || islist (y) || ischar (y))) \
            && (s_x == size (y));
    endfor
    if (!t)
      return;
    endif

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

endfunction

# test for equality
%!assert(__isequal__(0,[1,2,3,4],[1,2,3,4]))
%!assert(__isequal__(1,{1,2,NaN,4},{1,2,NaN,4}))
%!assert(__isequal__(1,[1,2,NaN,4],[1,2,NaN,4]))
%!assert(__isequal__(0,['a','b','c','d'],['a','b','c','d']))
# test for inequality
%!assert(__isequal__(0,[1,2,3,4],[1;2;3;4]),false)
%!assert(__isequal__(0,{1,2,3,4},[1,2,3,4]),false)
%!assert(__isequal__(0,[1,2,3,4],{1,2,3,4}),false)
%!assert(__isequal__(0,[1,2,NaN,4],[1,2,NaN,4]),false)
%!assert(__isequal__(1,[1,2,NaN,4],[1,NaN,3,4]),false)
%!assert(__isequal__(1,[1,2,NaN,4],[1,2,3,4]),false)
%!assert(__isequal__(0,['a','b','c','d'],['a';'b';'c';'d']),false)
# test for equality (struct)
%!test
%! A = struct ();
%! A.char1 = "abcd";
%! A.int1 = 123;
%! B = struct ();
%! B.char1 = "abcd";
%! B.int1 = 123;
%! C = struct ();
%! C.char1 = "abcd";
%! C.int1 = 123;
%! assert (__isequal__ (0, A, B, C));
# test for inequality (struct)
%!test
%! A = struct ();
%! A.char1 = "abcd";
%! A.int1 = NaN;
%! B = struct ();
%! B.char1 = "abcd";
%! B.int1 = NaN;
%! C = struct ();
%! C.char1 = "abcd";
%! C.int1 = NaN;
%! assert (__isequal__ (0, A, B, C), false);
