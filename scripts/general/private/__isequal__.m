## Copyright (C) 2000-2012 Paul Kienzle
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

  ## All arguments must either be of the same class or they must be
  ## numeric values.
  t = (all (strcmp (class(x),
                    cellfun ("class", varargin, "uniformoutput", false)))
       || ((isnumeric (x) || islogical (x))
           && all (cellfun ("isnumeric", varargin)
                   | cellfun ("islogical", varargin))));

  if (t)
    ## Test that everything has the same number of dimensions.
    s_x = size (x);
    s_v = cellfun (@size, varargin, "uniformoutput", false);
    t = all (length (s_x) == cellfun ("length", s_v));
  endif

  if (t)
    ## Test that everything is the same size since it has the same
    ## dimensionality.
    l_x = length (s_x);
    s_v = reshape ([s_v{:}], length (s_x), []);
    idx = 0;
    while (t && idx < l_x)
      idx++;
      t = all (s_x(idx) == s_v(idx,:));
    endwhile
  endif

  ## From here on, compare objects as if they were structures.
  if (isobject (x))
    x = builtin ("struct", x);
    varargin = cellfun (@(x) builtin ("struct", x), varargin,
                        "uniformoutput", false);
  endif

  if (t)
    ## Check individual classes.
    if (isstruct (x))
      ## Test the number of fields.
      fn_x = fieldnames (x);
      l_fn_x = length (fn_x);
      fn_v = cellfun ("fieldnames", varargin, "uniformoutput", false);
      t = all (l_fn_x == cellfun ("length", fn_v));

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

    elseif (isa (x, "function_handle"))

      ## The == operator is overloaded for handles.
      t = all (cellfun ("eq", {x}, varargin));

    else
      ## Check the numeric types.

      f_x = find (x);
      l_f_x = length (f_x);
      x = x(f_x);
      for argn = 1:l_v
        y = varargin{argn};
        f_y = find (y);

        t = (l_f_x == length (f_y)) && all (f_x == f_y);
        if (!t)
          return;
        endif

        y = y(f_y);
        m = (x == y);
        t = all (m);

        if (!t && nans_compare_equal)
          t = isnan (x(!m)) && isnan (y(!m));
        endif

        if (!t)
          return;
        endif
      endfor

    endif
  endif

endfunction
