########################################################################
##
## Copyright (C) 2000-2023 The Octave Project Developers
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
## @deftypefn {} {@var{tf} =} isequal (@var{x1}, @var{x2}, @dots{})
## Return true if all of @var{x1}, @var{x2}, @dots{} are equal.
## @seealso{isequaln}
## @end deftypefn

## Algorithm:
##
## 1. Verify the class of x.
##    a. All objects are of the same class
##    b. All objects are of a generic "numeric" class which includes
##       numeric, logical, and character arrays
## 2. Verify size of all objects match.
## 3. Convert objects to struct, and then compare as stated below.
## 4. For each argument after x, compare it for equality with x:
##    a. char       compare each member with strcmp
##    b. numeric    compare each member with '==' with sparsity regarded
##    c. struct     compare number of fieldnames, value of fieldnames,
##                  and then each field with isequal (recursive)
##    d. cellstr    compare each cellstr member with strcmp
##    e. cell       compare each member with isequal (recursive)
##    f. fcn_handle compare using overloaded "eq" operator

function tf = isequal (x, varargin)

  if (nargin < 2)
    print_usage ();
  endif

  nvarargin = nargin - 1;
  two_args = (nvarargin == 1);  # Optimization for base case of just 2 args

  if (two_args)
    y = varargin{1};  # alias y to second input for comparison
  endif

  ############################################################
  ## Generic tests for equality

  ## All arguments must either be of the same class,
  ##  or they must be "numeric" values.
  if (two_args)
    tf = (strcmp (class (x), class (y))
          || ((isreal (x) || iscomplex (x)) && (isreal (y) || iscomplex (y))));
  else
    tf = (all (cellfun ("isclass", varargin, class (x)))
          || ((isreal (x) || iscomplex (x))
              && all (cellfun ("isreal", varargin)
                      | cellfun ("isnumeric", varargin))));
  endif

  ## Test that everything is the same size (which also tests dimensions)
  if (tf)
    tf = size_equal (x, varargin{:});
  endif

  ## From here on, compare any objects as if they were structures.
  if (tf && isobject (x))
    ## Locally suppress class-to-struct warning.  We know what we are doing.
    warning ("off", "Octave:classdef-to-struct", "local");
    x = builtin ("struct", x);
    if (two_args)
      clear y;  # break link to existing variable
      varargin(1) = builtin ("struct", varargin{1});
      y = varargin{1};  # re-alias y to second input
    else
      for i = 1:nvarargin
        varargin(i) = builtin ("struct", varargin{i});
      endfor
    endif
  endif

  ############################################################
  ## Check individual classes.

  if (tf)
    if (two_args)

      if (ischar (x) && ischar (y))
        ## char type.  Optimization, strcmp is ~35% faster than '==' operator.
        tf = strcmp (x, y);

      elseif (isreal (x) || iscomplex (x))
        if (issparse (x))
          ## sparse types.
          [xi, xj, xv] = find (x);
          [yi, yj, yv] = find (y);
          tf = (length (xi) == length (yi)) && all (xi == yi) ...
              && all (xj == yj) && all (xv == yv);
        else
          ## general "numeric" type.  Use '==' operator.
          m = (x == y);
          tf = all (m(:));
        endif

      elseif (isstruct (x))
        ## struct type.  Compare # of fields, fieldnames, then field values.

        ## Test number of fields are equal.
        tf = (numfields (x) == numfields (y));

        ## Test that all the field names are equal.
        if (tf)
          s_fnm_x = sort (fieldnames (x));
          tf = all (strcmp (s_fnm_x, sort (fieldnames (y))));
        endif

        ## Test that all field values are equal.  Slow because of recursion.
        if (tf)
          if (isscalar (x))
            for fldnm = s_fnm_x.'
              tf = isequal (x.(fldnm{1}), y.(fldnm{1}));
              if (! tf)
                break;
              endif
            endfor
          else
            ## struct arrays have to have the contents of each field wrapped
            ## in a cell since it expands to a collection of values.
            for fldnm = s_fnm_x.'
              tf = isequal ({x.(fldnm{1})}, {y.(fldnm{1})});
              if (! tf)
                break;
              endif
            endfor
          endif
        endif

      elseif (iscellstr (x) && iscellstr (y))
        ## cellstr type.  Optimization over cell type by using strcmp.
        ## FIXME: It would be faster to use strcmp on whole cellstr arrays,
        ## but bug #51412 needs to be fixed.  Instead, time/space trade-off.
        ## Convert to char (space) for faster processing with strcmp (time).
        tf = strcmp (char (x), char (y));

      elseif (iscell (x))
        ## cell type.  Check that each element of a cell is equal.  Slow.
        n = numel (x);
        idx = 1;
        while (tf && idx <= n)
          tf = isequal (x{idx}, y{idx});
          idx += 1;
        endwhile

      elseif (is_function_handle (x))
        ## function type.  Use '==' operator which is overloaded.
        tf = (x == y);

      elseif (isjava (x))
        try
          tf = x.equals (y);
        catch
          error ('isequal: Java object does not implement "equals" function');
        end_try_catch

      else
        error ("isequal: Impossible to reach code.  File a bug report.");

      endif

    else  # More than two args.  This is going to be slower in general.

      if (ischar (x) && all (cellfun ("isclass", varargin, "char")))
        ## char type.  Optimization, strcmp is ~35% faster than '==' operator.
        idx = 1;
        while (tf && idx <= nvarargin)
          tf = strcmp (x, varargin{idx});
          idx += 1;
        endwhile

      elseif (isreal (x) || iscomplex (x))

        if (issparse (x))
          ## sparse types.

          idx = 1;
          [xi, xj, xv] = find (x);
          while (tf && idx <= nvarargin)
            y = varargin{idx};
            [yi, yj, yv] = find (y);
            tf = (length (xi) == length (yi)) && all (xi == yi) ...
                 && all (xj == yj) && all (xv == yv);

            idx += 1;
          endwhile

        else
          ## general "numeric" type.  Use '==' operator.

          idx = 1;
          while (tf && idx <= nvarargin)
            y = varargin{idx};
            m = (x == y);
            tf = all (m(:));

            idx += 1;
          endwhile

        endif

      elseif (isstruct (x))
        ## struct type.  Compare # of fields, fieldnames, then field values.

        ## Test number of fields are equal.
        fnm_x = fieldnames (x);
        n = numel (fnm_x);
        fnm_v = cellfun ("fieldnames", varargin, "uniformoutput", false);
        tf = all (n == cellfun ("numel", fnm_v));

        ## Test that all the field names are equal.
        if (tf)
          fnm_x = sort (fnm_x);
          idx = 1;
          while (tf && idx <= nvarargin)
            ## Allow the fieldnames to be in a different order.
            tf = all (strcmp (fnm_x, sort (fnm_v{idx})));
            idx += 1;
          endwhile
        endif

        ## Test that all field values are equal.  Slow because of recursion.
        if (tf)
          args = cell (1, 1 + nvarargin);
          if (isscalar (x))
            for fldnm = fnm_x.'
              args{1} = x.(fldnm{1});
              for argn = 1:nvarargin
                args{argn+1} = varargin{argn}.(fldnm{1});
              endfor

              tf = isequal (args{:});

              if (! tf)
                break;
              endif
            endfor
          else
            ## struct arrays have to have the contents of each field wrapped
            ## in a cell since it expands to a collection of values.
            for fldnm = fnm_x.'
              args{1} = { x.(fldnm{1}) };
              for argn = 1:nvarargin
                args{argn+1} = { varargin{argn}.(fldnm{1}) };
              endfor

              tf = isequal (args{:});

              if (! tf)
                break;
              endif
            endfor
          endif
        endif

      elseif (iscellstr (x) && all (cellfun (@iscellstr, varargin)))
        ## cellstr type.  Optimization over cell type by using strcmp.
        ## FIXME: It would be faster to use strcmp on whole cellstr arrays,
        ## but bug #51412 needs to be fixed.  Instead, time/space trade-off.
        ## Convert to char (space) for faster processing with strcmp (time).
        idx = 1;
        x = char (x);
        while (tf && idx <= nvarargin)
          tf = strcmp (x, char (varargin{idx}));
          idx += 1;
        endwhile

      elseif (iscell (x))
        ## cell type.  Check that each element of a cell is equal.  Slow.
        n = numel (x);
        args = cell (1, 1 + nvarargin);
        idx = 1;
        while (tf && idx <= n)
          args(1) = x{idx};
          args(2:end) = [cellindexmat(varargin, idx){:}];

          tf = isequal (args{:});

          idx += 1;
        endwhile

      elseif (is_function_handle (x))
        ## function type.  Use '==' operator which is overloaded.
        tf = all (cellfun ("eq", {x}, varargin));

      else
        error ("isequal: Impossible to reach code.  File a bug report.");

      endif

    endif
  endif

  tf = full (tf);  # Always return full logical value for Matlab compatibility.

endfunction


## test empty input
%!assert (isequal ([], []), true)
%!assert (isequal ([], 1), false)
%!assert (isequal ([], [], 1), false)
%!assert (isequal ([], 1, []), false)
%!assert (isequal (1, [], []), false)

## test size and shape
%!assert (isequal ([1,2,3,4], [1,2,3,4]), true)
%!assert (isequal ([1;2;3;4], [1;2;3;4]), true)
%!assert (isequal ([1,2,3,4], [1;2;3;4]), false)
%!assert (isequal ([1,2,3,4], [1,2;3,4]), false)
%!assert (isequal ([1,2,3,4], [1,3;2,4]), false)
%!assert (isequal ([1,2,3,4], [1,2,3,4], [1,2,3,4]), true)
%!assert (isequal ([1;2;3;4], [1;2;3;4], [1;2;3;4]), true)
%!assert (isequal ([1,2,3,4], [1,2,3,4], [1;2;3;4]), false)
%!assert (isequal ([1,2,3,4], [1,2,3,4], [1,2;3,4]), false)
%!assert (isequal ([1,2,3,4], [1,2,3,4], [1,3;2,4]), false)

## General tests
%!test
%! A = 1:8;
%! B = reshape (A, 2, 2, 2);
%! assert (isequal (A, B), false);
%! assert (isequal (A, A, B), false);
%!test
%! A = reshape (1:8, 2, 2, 2);
%! B = A;
%! assert (isequal (A, B), true);
%! assert (isequal (A, A, B), true);
%!test
%! A = reshape (1:8, 2, 4);
%! B = reshape (A, 2, 2, 2);
%! assert (isequal (A, B), false);
%! assert (isequal (A, A, B), false);

## test characters and strings
%!assert (isequal ('a', "a"), true)
%!assert (isequal ('a', 'a', "a"), true)
%!assert (isequal ("abab", ["a", "b", "a", "b"]), true)
%!assert (isequal ("abab", "abab", ["a", "b", "a", "b"]), true)
%!assert (isequal (["a","b","c","d"], ["a","b","c","d"]), true)
%!assert (isequal (["a","b","c","d"], ["a","b","c","d"], ["a","b","c","d"]),
%!        true)
%!assert (isequal (["test   ";"strings"], ["test   ";"strings"]), true)
%!assert (isequal (["test   ";"strings"], ["test   ";"strings"],
%!                 ["test   ";"strings"]), true)
%!assert (isequal (["a","b","c","d"], ["a";"b";"c";"d"]), false)
%!assert (isequal (["a","b","c","d"], ["a","b","c","d"], ["a";"b";"c";"d"]),
%!        false)

## test all numeric built-in primitives
%!assert (isequal (false, 0))
%!assert (isequal (char (0), 0))
%!assert (isequal (false, logical (0), char (0),
%!                 int8 (0), int16 (0), int32 (0), int64 (0),
%!                 uint8 (0), uint16 (0), uint32 (0), uint64 (0),
%!                 double (0), single (0),
%!                 double (complex (0,0)), single (complex (0,0)),
%!                 sparse (false), sparse (logical (0)),
%!                 sparse (double (0)), sparse (single (0)),
%!                 sparse (double (complex (0,0))),
%!                 sparse (single (complex (0,0)))),
%!        true)
%!assert (isequal (true, logical (1), char (1),
%!                 int8 (1), int16 (1), int32 (1), int64 (1),
%!                 uint8 (1), uint16 (1), uint32 (1), uint64 (1),
%!                 double (1), single (1),
%!                 double (complex (1,0)), single (complex (1,0)),
%!                 sparse (true), sparse (logical (1)),
%!                 sparse (double (1)), sparse (single (1)),
%!                 sparse (double (complex (1,0))),
%!                 sparse (single (complex (1,0)))),
%!        true)

## test structures
%!assert (isequal (struct ([]), struct ([])), true)
%!assert (isequal (struct ([]), struct ([]), struct ([])), true)
%!assert (isequal (struct ("a",1), struct ("a",1)), true)
%!assert (isequal (struct ("a",1), struct ("a",1), struct ("a",1)), true)
%!assert (isequal (struct ("a",1), struct ("a",2)), false)
%!assert (isequal (struct ("a",1), struct ("a",1), struct ("a",2)), false)
%!assert (isequal (struct ("a",1), struct ("a",1,"b",2)), false)
%!assert (isequal (struct ("a",1), struct ("a",1),struct ("a",1,"b",2)), false)
%!assert (isequal (struct ("a",1), struct ("b",1)), false)
%!assert (isequal (struct ("a",1), struct ("a",1), struct ("b",1)), false)
%!assert (isequal (struct ("a",1,"b",2), struct ("a",1,"b",2)), true)
%!assert (isequal (struct ("a",1,"b",2), struct ("a",1,"b",2),
%!                 struct ("a",1,"b",2)), true)
%!assert (isequal (struct ("a",1,"b",2), struct ("b",2,"a",1)), true)
%!assert (isequal (struct ("a",1,"b",2), struct ("a",1,"b",2),
%!                 struct ("b",2,"a",1)), true)
%!assert (isequal (struct ("a","abc","b",2), struct ("a","abc","b",2)), true)
%!assert (isequal (struct ("a","abc","b",2), struct ("a","abc","b",2),
%!                 struct ("a","abc","b",2)), true)

## recursive structure
%!test
%! x.a = "a1";
%! x.b.a = "ba1";
%! x.b.b = "bb1";
%! assert (isequal (x, x), true);
%! assert (isequal (x, x, x), true);
%! y = x;
%! y.b.b = "bb2";
%! assert (isequal (x, y), false);
%! assert (isequal (x, x, y), false);
%! y = x;
%! y.b = rmfield (y.b, "b");
%! y.b.b.a = "bba1";
%! assert (isequal (x, y), false);
%! assert (isequal (x, x, y), false);

## struct array
%!test
%! x(1).a = 'A';
%! x(2).a = magic (3);
%! assert (isequal (x, x), true);
%! assert (isequal (x, x, x), true);
%! y = x;
%! y(2).a = { magic(3) };
%! assert (isequal (x, y), false);
%! assert (isequal (x, x, y), false);

## test cellstr
%!assert (isequal (cell (1,1), cell (1,1)), true)
%!assert (isequal (cell (1,1), cell (1,2)), false)
%!assert (isequal ({"a","b";"c","d"}, {"a","b";"c","d"}), true)
%!assert (isequal ({"a","b";"c","d"}, {"a","b";"c","d"}, {"a","b";"c","d"}),
%!                 true)
%!assert (isequal ({"a","b","c","d"}, {"a";"b";"c";"d"}), false)
%!assert (isequal ({"a","b","c","d"}, {"a","b","c","d"}, {"a";"b";"c";"d"}),
%!        false)
%!assert (isequal (["a","b","c","d"], {"a","b","c","d"}), false)
%!assert (isequal (["a","b","c","d"], ["a","b","c","d"], {"a","b","c","d"}),
%!        false)
%!test
%! x = { ["ab"; "cd"] ; ["ef"; "gh"] };
%! assert (isequal (x, x), true);
%! assert (isequal (x, x, x), true);
%! y = x;
%! y(2) = ["ef"; "gH"];
%! assert (isequal (x, y), false);
%! assert (isequal (x, x, y), false);

## test cells
%!assert (isequal (cell (1,1), cell (1,1)), true)
%!assert (isequal (cell (1,1), cell (1,1), cell (1,1)), true)
%!assert (isequal (cell (1,1), cell (1,2)), false)
%!assert (isequal (cell (1,1), cell (1,1), cell (1,2)), false)
%!assert (isequal ({"a",1}, {"a",1}), true)
%!assert (isequal ({"a",1}, {"a",1}, {"a",1}), true)
%!assert (isequal ({"a",1}, {"a",2}), false)
%!assert (isequal ({"a",1}, {"a",1}, {"a",2}), false)
%!assert (isequal ({"a",1}, {"b",1}), false)
%!assert (isequal ({"a",1}, {"a",1}, {"b",1}), false)
%!assert (isequal ({"a",1,"b",2}, {"a",1,"b",2}), true)
%!assert (isequal ({"a",1,"b",2}, {"a",1,"b",2}, {"a",1,"b",2}), true)
%!assert (isequal ({"a",1,"b",2}, {"b",2,"a",1}), false)
%!assert (isequal ({"a",1,"b",2}, {"a",1,"b",2}, {"b",2,"a",1}), false)
%!assert (isequal ({"a","abc","b",2}, {"a","abc","b",2}), true)
%!assert (isequal ({"a","abc","b",2}, {"a","abc","b",2}, {"a","abc","b",2}),
%!                 true)

## recursive cell
%!test
%! x = cell (1,3);
%! x{1} = {[1], [1 2]};
%! x{2} = true;
%! x{3} = {{"hello"}, {"world"}};
%! assert (isequal (x, x));
%! y = x;
%! y{3}{1}{1} = "goodbye";
%! assert (isequal (x, y), false);

## test function_handle
%!test
%! fcn = @(x) x.^2;
%! assert (isequal (fcn, fcn), true);
%! assert (isequal (fcn, fcn, fcn), true);
%! assert (isequal (fcn, @(x) x.^2), false);
%! assert (isequal (fcn, fcn, @(x) x.^2), false);
%! assert (isequal (@(x) x.^2, fcn), false);
%! assert (isequal (@(x) x.^2, @(x) x.^2, fcn), false);

## test for sparse matrices
%!shared A, Z
%!  A = sprand (2^31, 1000, 2^(-31));
%!  Z = sparse (2^31, 1000);
%!assert (isequal (sparse ([]), []), true)
%!assert (isequal (sparse ([]), sparse ([]), []), true)
%!assert (isequal ([], sparse ([])), true)
%!assert (isequal ([], [], sparse ([])), true)
%!assert (isequal (sparse (0,1), sparse (0,1)), true)
%!assert (isequal (sparse (0,1), sparse (0,1), sparse (0,1)), true)
%!assert (isequal (sparse (0,1), zeros (0,1)), true)
%!assert (isequal (sparse (0,1), sparse (0,1), zeros (0,1)), true)
%!assert (isequal (sparse (2,2), sparse (2,2)), true)
%!assert (isequal (sparse (2,2), sparse (2,2), sparse (2,2)), true)
%!assert (isequal (zeros (2,2), sparse (2,2)), true)
%!assert (isequal (zeros (2,2), zeros (2,2), sparse (2,2)), true)
%!assert (isequal (speye (1), eye (1)), true)
%!assert (isequal (speye (1), speye (1), eye (1)), true)
%!assert (isequal (eye (300), speye (300)), true)
%!assert (isequal (eye (300), eye (300), speye (300)), true)
%!assert (isequal (sparse (0,1), sparse (1,0)), false)
%!assert (isequal (sparse (0,1), sparse (0,1), sparse (1,0)), false)
%!assert (isequal (Z, Z), true)
%!assert (isequal (A, A), true)
%!assert (isequal (A, Z), false)
%!assert (isequal (Z, Z, Z), true)
%!assert (isequal (A, A, A), true)
%!assert (isequal (A, Z, A), false)

## test NaN
%!assert (isequal (NaN, NaN), false)
%!assert (isequal (NaN, NaN, NaN), false)
%!assert (isequal (NaN, Inf), false)
%!assert (isequal (NaN, Inf, Inf), false)
%!assert (isequal (NaN, 1.0), false)
%!assert (isequal (NaN, 1.0, 1.0), false)
%!assert (isequal ([1,2,NaN,4], [1,2,NaN,4]), false)
%!assert (isequal ([1,2,NaN,4], [1,2,NaN,4], [1,2,NaN,4]), false)
%!assert (isequal (struct ("a",NaN,"b",2), struct ("a",NaN,"b",2)), false)
%!assert (isequal (struct ("a",NaN,"b",2), struct ("a",NaN,"b",2),
%!                 struct ("a",NaN,"b",2)), false)

## Matlab compatibility
%!assert (isequal (sparse (1), sparse (1)), true)
%!assert (isequal (sparse (1), sparse (1)), sparse (1), true)

## Java objects
%!testif HAVE_JAVA; usejava ("jvm")  <*62930>
%! int1 = javaObject ("java.lang.Integer", 1.0);
%! int2 = javaObject ("java.lang.Integer", 2.0);
%! assert (isequal (int1, int1));
%! assert (! isequal (int1, 1.0));
%! assert (! isequal (int1, int2));

## test input validation
%!error <Invalid call> isequal ()
%!error <Invalid call> isequal (1)
