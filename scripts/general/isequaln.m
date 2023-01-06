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
## @deftypefn {} {@var{tf} =} isequaln (@var{x1}, @var{x2}, @dots{})
## Return true if all of @var{x1}, @var{x2}, @dots{} are equal under the
## additional assumption that NaN == NaN (no comparison of NaN placeholders
## in dataset).
## @seealso{isequal}
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
##    b. numeric    compare each member with '==', and assume NaN == NaN
##    c. struct     compare number of fieldnames, value of fieldnames,
##                  and then each field with isequaln (recursive)
##    d. cellstr    compare each cellstr member with strcmp
##    e. cell       compare each member with isequaln (recursive)
##    f. fcn_handle compare using overloaded "eq" operator

function tf = isequaln (x, varargin)

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
        ## general "numeric" type.  Use '==' operator.
        m = (x == y);
        tf = all (m(:));

        if (! tf && isfloat (x) && isfloat (y))
          tf = isnan (x(! m)) && isnan (y(! m));
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
              tf = isequaln (x.(fldnm{1}), y.(fldnm{1}));
              if (! tf)
                break;
              endif
            endfor
          else
            ## struct arrays have to have the contents of each field wrapped
            ## in a cell since it expands to a collection of values.
            for fldnm = s_fnm_x.'
              tf = isequaln ({x.(fldnm{1})}, {y.(fldnm{1})});
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
          tf = isequaln (x{idx}, y{idx});
          idx += 1;
        endwhile

      elseif (is_function_handle (x))
        ## function type.  Use '==' operator which is overloaded.
        tf = (x == y);

      else
        error ("isequaln: Impossible to reach code.  File a bug report.");

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
        ## general "numeric" type.  Use '==' operator.

        idx = 1;
        while (tf && idx <= nvarargin)
          y = varargin{idx};
          m = (x == y);
          tf = all (m(:));

          if (! tf && isfloat (x) && isfloat (y))
            tf = isnan (x(! m)) && isnan (y(! m));
          endif

          idx += 1;
        endwhile

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

              tf = isequaln (args{:});

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

              tf = isequaln (args{:});

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

          tf = isequaln (args{:});

          idx += 1;
        endwhile

      elseif (is_function_handle (x))
        ## function type.  Use '==' operator which is overloaded.
        tf = all (cellfun ("eq", {x}, varargin));

      else
        error ("isequaln: Impossible to reach code.  File a bug report.");

      endif

    endif
  endif

  tf = full (tf);  # Always return full logical value for Matlab compatibility.

endfunction


## test for equality
%!assert (isequaln (1,1), true)
%!assert (isequaln (1,1,1), true)
%!assert (isequaln ({1,2,NaN,4}, {1,2,NaN,4}), true)
%!assert (isequaln ({1,2,NaN,4}, {1,2,NaN,4}, {1,2,NaN,4}), true)
%!assert (isequaln ([1,2,NaN,4], [1,2,NaN,4]), true)
%!assert (isequaln ([1,2,NaN,4], [1,2,NaN,4], [1,2,NaN,4]), true)
## test for inequality
%!assert (isequaln (1,2), false)
%!assert (isequaln (1,1,2), false)
%!assert (isequaln ([1,2,NaN,4], [1,NaN,3,4]), false)
%!assert (isequaln ([1,2,NaN,4], [1,2,NaN,4], [1,NaN,3,4]), false)
%!assert (isequaln ([1,2,NaN,4], [1,2,3,4]), false)
%!assert (isequaln ([1,2,NaN,4], [1,2,NaN,4], [1,2,3,4]), false)
## test for equality (struct)
%!shared st
%! st = struct ("a",NaN,"b",2);
%!assert (isequaln (st, st), true)
%!assert (isequaln (st, st, st), true)

## Input validation
%!error <Invalid call> isequaln ()
%!error <Invalid call> isequaln (1)
