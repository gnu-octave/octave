########################################################################
##
## Copyright (C) 2025 The Octave Project Developers
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
## @deftypefn  {} {} assert_equal (@var{observed}, @var{expected})
## @deftypefnx {} {} assert_equal (@var{observed}, @var{expected}, @var{tol})
##
## Produce an error if observed is not the same as expected.
##
## @code{assert_equal (@var{observed}, @var{expected})} tests for the two input
## arguments being equal with respect to their class, size, and value.  All
## octave values are supported as input for @var{observed} and @var{expected}
## and compared for equality according to the following rules.
##
## @itemize
## @item Numeric inputs that support exceptional values (i.e. Inf, -Inf, NaN,
## NA) are treated under the additional assumption that they are eqaul.  They
## are also tested for sparsity and complexity.
##
## @item Logical inputs are additonally tested for sparsity.
##
## @item Character arrays are compared with the @code{strcmp} function.
##
## @item Structures are additonally checked against having the same unordered
## fieldnames.
##
## @item Cell arrays are recursively tested on an element by element basis.
##
## @item Function handles are tested with the @code{isequal} function.
##
## @item Classdef objects are explicitly tested if and only if they have an
## overloaded @code{eq} method and unless @code{all (eq (@var{observed},
## @var{expected}), "all")} is true, an error is produced.  The handling of
## missing values is strictly defined by the overloaded @code{eq} method.
## @end itemize
##
## @code{assert_equal (@var{observed}, @var{expected}, @var{tol})} further
## specifies a tolerance value, which is explicitly used for numerical inputs.
## Specifying @var{tol} as 0, is equivalent to calling @code{assert_equal} with
## only two input arguments.  Any nonzero tolerance is also ignored when
## @var{observed} and @var{expected} are non-numeric.  Logical and character
## arrays are not considered to be numeric.
##
## If @var{tol} is positive then it is an absolute tolerance which will produce
## an error if @code{abs (@var{observed} - @var{expected}) > abs (@var{tol})}.
##
## If @var{tol} is negative then it is a relative tolerance which will produce
## an error if @code{abs (@var{observed} - @var{expected}) >
## abs (@var{tol} * @var{expected})}.
##
## If @var{expected} is zero @var{tol} will always be interpreted as an
## absolute tolerance.
##
## If @var{tol} is not scalar its dimensions must agree with those of
## @var{observed} and @var{expected} and tests are performed on an
## element-by-element basis.
## @seealso{fail, test, error, isequal, eq, strcmp, assert}
## @end deftypefn

function assert_equal (observed, expected, tol = 0)

  if (nargin < 2 || nargin > 3)
    print_usage ();
  endif

  ## Capture argument names for display at the top level only.
  arg_names = cell (nargin, 1);
  for i = 1:nargin
    arg_names{i} = inputname (i, false);
  endfor
  argin = ["(" strjoin(arg_names, ",") ")"];

  ## Delegate to implementation function which handles recursion.
  __assert_impl__ (observed, expected, tol, argin);

endfunction


## Implementation function for assert comparison.
## This is separated to preserve display names across recursive calls.
function __assert_impl__ (observed, expected, tol, argin)

  persistent call_depth = -1;
  persistent errmsg;

  unwind_protect

    call_depth += 1;

    if (call_depth == 0)
      errmsg = "";
    endif

    ## Add to list as the errors accumulate.  If empty at end then no errors.
    err.index = {};
    err.observed = {};
    err.expected = {};
    err.reason = {};

    if (isobject (expected))
      if (! isobject (observed))
        err.index{end+1} = ".";
        err.observed{end+1} = "O";
        err.expected{end+1} = "E";
        err.reason{end+1} = ["Expected classdef object, but observed " class(observed)];
      elseif (! strcmp (class (observed), class (expected)))
        err.index{end+1} = ".";
        err.observed{end+1} = "O";
        err.expected{end+1} = "E";
        err.reason{end+1} = ["Expected '" class(expected) "' but observed '" class(observed) "'"];
      elseif (ndims (observed) != ndims (expected)
              || any (size (observed) != size (expected)))
        err.index{end+1} = ".";
        err.observed{end+1} = ["O(" sprintf("%dx", size(observed))(1:end-1) ")"];
        err.expected{end+1} = ["E(" sprintf("%dx", size(expected))(1:end-1) ")"];
        err.reason{end+1} = "Dimensions don't match";
      elseif (! ismember ("eq", methods (expected)))
        err.index{end+1} = ".";
        err.observed{end+1} = "O";
        err.expected{end+1} = "E";
        err.reason{end+1} = "Expected classdef object does not have an 'eq' method";
      elseif (! all (eq (observed, expected), "all"))
        err.index{end+1} = "()";
        err.observed{end+1} = "O";
        err.expected{end+1} = "E";
        err.reason{end+1} = "Classdef objects do not match";
      endif

    elseif (ischar (expected))
      if (! ischar (observed))
        err.index{end+1} = ".";
        err.expected{end+1} = ["'" expected "'"];
        if (isnumeric (observed))
          err.observed{end+1} = num2str (observed);
          err.reason{end+1} = "Expected string, but observed number";
        else
          err.observed{end+1} = "O";
          err.reason{end+1} = ["Expected string, but observed " class(observed)];
        endif
      elseif (! strcmp (observed, expected))
        err.index{end+1} = "[]";
        err.observed{end+1} = ["'" observed(:).' "'"];
        err.expected{end+1} = ["'" expected(:).' "'"];
        err.reason{end+1} = "Strings don't match";
      endif

    elseif (iscell (expected))
      if (! iscell (observed))
        err.index{end+1} = ".";
        err.observed{end+1} = "O";
        err.expected{end+1} = "E";
        err.reason{end+1} = ["Expected cell, but observed " class(observed)];
      elseif (ndims (observed) != ndims (expected)
              || any (size (observed) != size (expected)))
        err.index{end+1} = ".";
        err.observed{end+1} = ["O(" sprintf("%dx", size(observed))(1:end-1) ")"];
        err.expected{end+1} = ["E(" sprintf("%dx", size(expected))(1:end-1) ")"];
        err.reason{end+1} = "Dimensions don't match";
      else
        try
          ## Recursively compare cell arrays
          for i = 1:length (expected(:))
            __assert_impl__ (observed{i}, expected{i}, tol, argin);
          endfor
        catch
          err.index{end+1} = "{}";
          err.observed{end+1} = "O";
          err.expected{end+1} = "E";
          err.reason{end+1} = "Cell configuration error";
        end_try_catch
      endif

    elseif (is_function_handle (expected))
      if (! is_function_handle (observed))
        err.index{end+1} = "@";
        err.observed{end+1} = "O";
        err.expected{end+1} = "E";
        err.reason{end+1} = ["Expected function handle, but observed " class(observed)];
      elseif (! isequal (observed, expected))
        err.index{end+1} = "@";
        err.observed{end+1} = "O";
        err.expected{end+1} = "E";
        err.reason{end+1} = "Function handles don't match";
      endif

    elseif (isstruct (expected))
      if (! isstruct (observed))
        err.index{end+1} = ".";
        err.observed{end+1} = "O";
        err.expected{end+1} = "E";
        err.reason{end+1} = ["Expected struct, but observed " class(observed)];
      elseif (ndims (observed) != ndims (expected)
              || any (size (observed) != size (expected))
              || numfields (observed) != numfields (expected))

        err.index{end+1} = ".";
        err.observed{end+1} = ["O(" sprintf("%dx", size(observed))(1:end-1) ")"];
        err.expected{end+1} = ["E(" sprintf("%dx", size(expected))(1:end-1) ")"];
        err.reason{end+1} = "Structure sizes don't match";
      elseif (! strcmp (sort (fieldnames (observed)),
                        sort (fieldnames (expected))))
        err.index{end+1} = ".";
        err.observed{end+1} = "O";
        err.expected{end+1} = "E";
        err.reason{end+1} = "Structure fieldname mismatch";
      else
        try
          ## Test if both structs are empty, i.e. 0x0, Nx0, or 0xN structs.
          ## In this case the values cannot be extracted for comparison as
          ## they are not assignable and not defined.
          __assert_impl__ (isempty (observed), isempty (expected), 0, argin);

          if (! isempty (observed))
            for [v, k] = observed
              if (numel (observed) == 1)
                v = {v};
              else
                v = v(:)';
              endif
              ## Recursively call assert for struct array values
              __assert_impl__ (v, {expected.(k)}, tol, argin);
            endfor
          endif
        catch
          err.index{end+1} = ".";
          err.observed{end+1} = "O";
          err.expected{end+1} = "E";
          err.reason{end+1} = "Structure configuration error";
        end_try_catch
      endif

    elseif (ndims (observed) != ndims (expected)
            || any (size (observed) != size (expected)))
      err.index{end+1} = ".";
      err.observed{end+1} = ["O(" sprintf("%dx", size(observed))(1:end-1) ")"];
      err.expected{end+1} = ["E(" sprintf("%dx", size(expected))(1:end-1) ")"];
      err.reason{end+1} = "Dimensions don't match";

    else  ## Numeric comparison
      ## Always require matching class, sparsity, and complexity,
      ## regardless of whether a tolerance was provided.
      if (! strcmp (class (observed), class (expected)))
        err.index{end+1} = "()";
        err.observed{end+1} = "O";
        err.expected{end+1} = "E";
        err.reason{end+1} = ["Class " class(observed) " != " class(expected)];
      elseif (isnumeric (observed) || islogical (observed))
        if (issparse (observed) != issparse (expected))
          err.index{end+1} = "()";
          err.observed{end+1} = "O";
          err.expected{end+1} = "E";
          if (issparse (observed))
            err.reason{end+1} = "sparse != non-sparse";
          else
            err.reason{end+1} = "non-sparse != sparse";
          endif
        elseif (iscomplex (observed) != iscomplex (expected))
          err.index{end+1} = "()";
          err.observed{end+1} = "O";
          err.expected{end+1} = "E";
          if (iscomplex (observed))
            err.reason{end+1} = "complex != real";
          else
            err.reason{end+1} = "real != complex";
          endif
        endif
      endif

      if (isempty (err.index))

        A = observed;
        B = expected;

        if (isinteger (A) && isinteger (B))
          ## Non-floating point numbers can't have exceptional or complex
          ## values so skip tests.
          A_null = A;
          B_null = B;
        elseif (islogical (A) && islogical (B))
          ## Logical values can't have exceptional or complex
          ## values so skip tests.  Ignore tolerance for logical values.
          A_null = A;
          B_null = B;
          mtol = 0;
        else
          ## Check exceptional values.
          is_real = (isreal (A) && isreal (B));
          if (is_real)
            errvec = (isna (A) != isna (B));
          else
            errvec = (  isna (real (A)) != isna (real (B))
                      | isna (imag (A)) != isna (imag (B)));
          endif
          erridx = find (errvec);
          if (! isempty (erridx))
            err.index(end+1:end+length (erridx)) = ...
              ind2tuple (size (A), erridx);
            err.observed(end+1:end+length (erridx)) = ...
              strtrim (cellstr (num2str (A(erridx) (:))));
            err.expected(end+1:end+length (erridx)) = ...
              strtrim (cellstr (num2str (B(erridx) (:))));
            err.reason(end+1:end+length (erridx)) = ...
              repmat ({"'NA' mismatch"}, length (erridx), 1);
          endif
          errseen = errvec;

          if (is_real)
            errvec = (isnan (A) != isnan (B));
          else
            errvec = (  isnan (real (A)) != isnan (real (B))
                      | isnan (imag (A)) != isnan (imag (B)));
          endif
          erridx = find (errvec & ! errseen);
          if (! isempty (erridx))
            err.index(end+1:end+length (erridx)) = ...
              ind2tuple (size (A), erridx);
            err.observed(end+1:end+length (erridx)) = ...
              strtrim (cellstr (num2str (A(erridx) (:))));
            err.expected(end+1:end+length (erridx)) = ...
              strtrim (cellstr (num2str (B(erridx) (:))));
            err.reason(end+1:end+length (erridx)) = ...
              repmat ({"'NaN' mismatch"}, length (erridx), 1);
          endif
          errseen |= errvec;

          if (is_real)
            errvec = ((isinf (A) | isinf (B)) & (real (A) != real (B)));
          else
            errvec =   ((isinf (real (A)) | isinf (real (B))) ...
                        & (real (A) != real (B)))             ...
                     | ((isinf (imag (A)) | isinf (imag (B))) ...
                        & (imag (A) != imag (B)));
          endif
          erridx = find (errvec & ! errseen);
          if (! isempty (erridx))
            err.index(end+1:end+length (erridx)) = ...
              ind2tuple (size (A), erridx);
            err.observed(end+1:end+length (erridx)) = ...
              strtrim (cellstr (num2str (A(erridx) (:))));
            err.expected(end+1:end+length (erridx)) = ...
              strtrim (cellstr (num2str (B(erridx) (:))));
            err.reason(end+1:end+length (erridx)) = ...
              repmat ({"'Inf' mismatch"}, length (erridx), 1);
          endif
          errseen |= errvec;

          ## Check normal values.
          ## Replace exceptional values already checked above by zero.
          if (is_real)
            A_null_real = A;
            B_null_real = B;
          else
            A_null_real = real (A);
            B_null_real = real (B);
          endif
          if (issparse (errseen))
            exclude = errseen ...
                      | isnan (A_null_real) | isinf (A_null_real) ...
                      | isnan (B_null_real) | isinf (B_null_real);
          else
            exclude = errseen ...
                      | ! isfinite (A_null_real) & ! isfinite (B_null_real);
          endif
          A_null_real(exclude) = 0;
          B_null_real(exclude) = 0;

          if (is_real)
            A_null = A_null_real;
            B_null = B_null_real;
          else
            A_null_imag = imag (A);
            B_null_imag = imag (B);
            if (issparse (errseen))
              exclude = errseen ...
                        | isnan (A_null_imag) | isinf (A_null_imag) ...
                        | isnan (B_null_imag) | isinf (B_null_imag);
            else
              exclude = errseen ...
                        | ! isfinite (A_null_imag) & ! isfinite (B_null_imag);
            endif

            A_null_imag(exclude) = 0;
            B_null_imag(exclude) = 0;
            A_null = complex (A_null_real, A_null_imag);
            B_null = complex (B_null_real, B_null_imag);
          endif

          clear A_null_real B_null_real;
          clear A_null_imag B_null_imag;
        endif

        if (isscalar (tol))
          mtol = tol * ones (size (A));
        else
          mtol = tol;
        endif

        k = (mtol == 0);
        erridx = find ((A_null != B_null) & k);
        if (! isempty (erridx))
          err.index(end+1:end+length (erridx)) = ...
            ind2tuple (size (A), erridx);
          err.observed(end+1:end+length (erridx)) = ...
            strtrim (cellstr (num2str (A(erridx) (:))));
          err.expected(end+1:end+length (erridx)) = ...
            strtrim (cellstr (num2str (B(erridx) (:))));
          err.reason(end+1:end+length (erridx)) = ...
            ostrsplit (deblank (
                       sprintf ("Abs err %.5g exceeds tol %.5g by %.1g\n",
              [abs(A_null(erridx) - B_null(erridx))(:), mtol(erridx)(:), ...
               abs(A_null(erridx) - B_null(erridx))(:)-mtol(erridx)(:)].')),
                       "\n");
        endif

        k = (mtol > 0);
        erridx = find ((abs (A_null - B_null) > mtol) & k);
        if (! isempty (erridx))
          err.index(end+1:end+length (erridx)) = ...
            ind2tuple (size (A), erridx);
          err.observed(end+1:end+length (erridx)) = ...
            strtrim (cellstr (num2str (A(erridx) (:))));
          err.expected(end+1:end+length (erridx)) = ...
            strtrim (cellstr (num2str (B(erridx) (:))));
          err.reason(end+1:end+length (erridx)) = ...
            ostrsplit (deblank (
                       sprintf ("Abs err %.5g exceeds tol %.5g by %.1g\n",
              [abs(A_null(erridx) - B_null(erridx))(:), mtol(erridx)(:), ...
               abs(A_null(erridx) - B_null(erridx))(:)-mtol(erridx)(:)].')),
                       "\n");
        endif

        k = (mtol < 0);
        if (any (k(:)))
          ## Test for absolute error where relative error can't be calculated.
          erridx = find ((B_null == 0) & abs (A_null) > abs (mtol) & k);
          if (! isempty (erridx))
            err.index(end+1:end+length (erridx)) = ...
              ind2tuple (size (A), erridx);
            err.observed(end+1:end+length (erridx)) = ...
              strtrim (cellstr (num2str (A(erridx) (:))));
            err.expected(end+1:end+length (erridx)) = ...
              strtrim (cellstr (num2str (B(erridx) (:))));
            err.reason(end+1:end+length (erridx)) = ...
              ostrsplit (deblank (
                         sprintf ("Abs err %.5g exceeds tol %.5g by %.1g\n",
                    [abs(A_null(erridx) - B_null(erridx)), -mtol(erridx), ...
                     abs(A_null(erridx) - B_null(erridx))+mtol(erridx)].')),
                         "\n");
          endif
          ## Test for relative error
          Bdiv = Inf (size (B_null));
          Bdiv(k & (B_null != 0)) = B_null(k & (B_null != 0));
          relerr = abs ((A_null - B_null) ./ abs (Bdiv));
          erridx = find ((relerr > abs (mtol)) & k);
          if (! isempty (erridx))
            err.index(end+1:end+length (erridx)) = ...
              ind2tuple (size (A), erridx);
            err.observed(end+1:end+length (erridx)) = ...
              strtrim (cellstr (num2str (A(erridx) (:))));
            err.expected(end+1:end+length (erridx)) = ...
              strtrim (cellstr (num2str (B(erridx) (:))));
            err.reason(end+1:end+length (erridx)) = ...
              ostrsplit (deblank (
                         sprintf ("Rel err %.5g exceeds tol %.5g by %.1g\n",
                                  [relerr(erridx)(:), -mtol(erridx)(:), ...
                                   relerr(erridx)(:)+mtol(erridx)(:)].')),
                         "\n");
          endif
        endif
      endif

    endif

    ## Print any errors
    if (! isempty (err.index))
      if (! isempty (errmsg))
        errmsg = [errmsg "\n"];
      endif
      errmsg = [errmsg, pprint(argin, err)];
    endif

  unwind_protect_cleanup
    call_depth -= 1;
  end_unwind_protect

  if (call_depth == -1)
    ## Last time through.  If there were any errors on any pass, raise a flag.
    if (! isempty (errmsg))

      ## We don't want to start the debugger here if debug_on_error is
      ## true so we set it to false and make the change local.  Then
      ## debug_on_error will be reset to true after this function
      ## returns and the debugger will start at the location of the call
      ## to print_usage.
      debug_on_error (false, "local");

      error (errmsg);
    endif
  endif

endfunction


## empty input
%!error assert_equal ([])
%!error assert_equal ("")
%!error assert_equal ({})
%!error assert_equal (struct ([]))
%!assert_equal (zeros (3,0), zeros (3,0))
%!error <O\(3x0\)\s+E\(0x2\)> assert_equal (zeros (3,0), zeros (0,2))
%!error <Dimensions don't match> assert_equal (zeros (3,0), [])
%!error <Dimensions don't match> assert_equal (zeros (2,0,2), zeros (2,0))

## scalars
%!error <Dimensions don't match> assert_equal (3, [3,3])
%!error <Dimensions don't match> assert_equal (3, [3,3; 3,3])
%!error <Dimensions don't match> assert_equal ([3,3; 3,3], 3)
%!assert_equal (3, 3)
%!error <Abs err 1 exceeds tol> assert_equal (3, 4)
%!assert_equal (3+eps, 3, eps)
%!assert_equal (3, 3+eps, eps)
%!error <Abs err 4.4409e-0?16 exceeds tol> assert_equal (3+2*eps, 3, eps)
%!error <Abs err 4.4409e-0?16 exceeds tol> assert_equal (3, 3+2*eps, eps)

## vectors
%!assert_equal ([1,2,3],[1,2,3])
%!assert_equal ([1;2;3],[1;2;3])
%!error <Abs err 1 exceeds tol 0> assert_equal ([2,2,3,3],[1,2,3,4])
%!error <Abs err 1 exceeds tol 0.5> assert_equal ([2,2,3,3],[1,2,3,4],0.5)
%!error <Rel err 1 exceeds tol 0.1> assert_equal ([2,2,3,5],[1,2,3,4],-0.1)
%!error <Abs err 1 exceeds tol 0> assert_equal ([6;6;7;7],[5;6;7;8])
%!error <Abs err 1 exceeds tol 0.5> assert_equal ([6;6;7;7],[5;6;7;8],0.5)
%!error <Rel err .* exceeds tol 0.1> assert_equal ([6;6;7;7],[5;6;7;8],-0.1)
%!error <Dimensions don't match> assert_equal ([1,2,3],[1;2;3])
%!error <Dimensions don't match> assert_equal ([1,2],[1,2,3])
%!error <Dimensions don't match> assert_equal ([1;2;3],[1;2])

## matrices
%!assert_equal ([1,2;3,4],[1,2;3,4])
%!error <\(1,2\)\s+4\s+2> assert_equal ([1,4;3,4],[1,2;3,4])
%!error <Dimensions don't match> assert_equal ([1,3;2,4;3,5],[1,2;3,4])
%!test  # 2-D matrix
%! A = [1 2 3]'*[1,2];
%! assert_equal (A, A);
%! fail ("assert_equal (A.*(A!=2),A)");
%!test  # N-D matrix
%! X = zeros (2,2,3);
%! Y = X;
%! Y(1,2,3) = 1.5;
%! fail ("assert_equal (X,Y)", "\(1,2,3\).*Abs err 1.5 exceeds tol 0");

## must give a small tolerance for floating point errors on relative
%!assert_equal (100+100*eps, 100, -2*eps)
%!assert_equal (100, 100+100*eps, -2*eps)
%!error <Rel err .* exceeds tol> assert_equal (100+300*eps, 100, -2*eps)
%!error <Rel err .* exceeds tol> assert_equal (100, 100+300*eps, -2*eps)

## test relative vs. absolute tolerances
%!test  assert_equal (0.1+eps, 0.1, 2*eps);
%!error <Rel err 2.2204e-0?15 exceeds tol> assert_equal (0.1+eps, 0.1, -2*eps)
%!test  assert_equal (100+100*eps, 100, -2*eps);
%!error <Abs err 2.8422e-0?14 exceeds tol> assert_equal (100+100*eps, 100, 2*eps)

## Corner case of relative tolerance with 0 divider
%!error <Abs err 2 exceeds tol 0.1> assert_equal (2, 0, -0.1)

## Extra checking of inputs when tolerance unspecified.
%!error <Class single != double> assert_equal (single (1), 1)
%!error <Class uint8 != uint16> assert_equal (uint8 (1), uint16 (1))
%!error <sparse != non-sparse> assert_equal (sparse([1]), [1])
%!error <non-sparse != sparse> assert_equal ([1], sparse([1]))
%!error <complex != real> assert_equal (1+i, 1)
%!error <real != complex> assert_equal (1, 1+i)

## exceptional values
%!assert_equal ([NaN, NA, Inf, -Inf, 1+eps, eps], [NaN, NA, Inf, -Inf, 1, 0], eps)

%!error <'NaN' mismatch> assert_equal (NaN, 1)
%!error <'NaN' mismatch> assert_equal ([NaN 1], [1 NaN])
%!test
%! try
%!   assert_equal ([NaN 1], [1 NaN]);
%! catch
%!   errmsg = lasterr ();
%!   if (sum (errmsg () == "\n") != 4)
%!     error ("Too many errors reported for NaN assert_equal");
%!   elseif (strfind (errmsg, "NA"))
%!     error ("NA reported for NaN assert_equal");
%!   elseif (strfind (errmsg, "Abs err NaN exceeds tol 0"))
%!     error ("Abs err reported for NaN assert_equal");
%!   endif
%! end_try_catch

%!error <'NA' mismatch> assert_equal (NA, 1)
%!error assert_equal ([NA 1]', [1 NA]')
%!test
%! try
%!   assert_equal ([NA 1]', [1 NA]');
%! catch
%!   errmsg = lasterr ();
%!   if (sum (errmsg () == "\n") != 4)
%!     error ("Too many errors reported for NA assert_equal");
%!   elseif (strfind (errmsg, "NaN"))
%!     error ("NaN reported for NA assert_equal");
%!   elseif (strfind (errmsg, "Abs err NA exceeds tol 0"))
%!     error ("Abs err reported for NA assert_equal");
%!   endif
%! end_try_catch
%!error assert_equal ([(complex (NA, 1)) (complex (2, NA))], [(complex (NA, 2)) 2])

%!error <'Inf' mismatch> assert_equal (-Inf, Inf)
%!error <'Inf' mismatch> assert_equal ([-Inf Inf], [Inf -Inf])
%!test
%! try
%!   assert_equal (complex (Inf, 0.2), complex (-Inf, 0.2 + 2*eps), eps);
%! catch
%!   errmsg = lasterr ();
%!   if (sum (errmsg () == "\n") != 3)
%!     error ("Too many errors reported for Inf assert_equal");
%!   elseif (strfind (errmsg, "Abs err"))
%!     error ("Abs err reported for Inf assert_equal");
%!   endif
%! end_try_catch
%!error <Abs err> assert_equal (complex (Inf, 0.2), complex (Inf, 0.2 + 2*eps), eps)

## strings
%!assert_equal ("dog", "dog")
%!error <Strings don't match> assert_equal ("dog", "cat")
%!error <Expected string, but observed number> assert_equal (3, "dog")
%!error <Class char != double> assert_equal ("dog", [3 3 3])
%!error <Expected string, but observed cell> assert_equal ({"dog"}, "dog")
%!error <Expected string, but observed struct> assert_equal (struct ("dog", 3), "dog")

## cell arrays
%!error <Expected cell, but observed double> assert_equal (1, {1})
%!error <Dimensions don't match> assert_equal (cell (1,2,3), cell (3,2,1))
%!test
%! x = {{{1}}, 2};  # cell with multiple levels
%! y = x;
%! assert_equal (x,y);
%! y{1}{1}{1} = 3;
%! fail ("assert_equal (x,y)", "Abs err 2 exceeds tol 0");
%!test
%! try
%!   assert_equal ({2}, {single(2)});
%! catch
%!   errmsg = lasterr ();
%!   if (sum (errmsg () == "\n") != 3)
%!     error ("Incorrect number of errors reported");
%!   endif
%!   assert_equal (! isempty (regexp (errmsg, 'Class double != single')), true);
%! end_try_catch

## function handles
%!assert_equal (@sin, @sin)
%!error <Function handles don't match> assert_equal (@sin, @cos)
%!error <Expected function handle, but observed double> assert_equal (pi, @cos)
%!error <Class function_handle != double> assert_equal (@sin, pi)

%!test
%! x = {[3], [1,2,3]; 100+100*eps, "dog"};
%! y = x;
%! assert_equal (x, y);
%! y = x; y(1,1) = [2];
%! fail ("assert_equal (x, y)");
%! y = x; y(1,2) = [0, 2, 3];
%! fail ("assert_equal (x, y)");
%! y = x; y(2,1) = 101;
%! fail ("assert_equal (x, y)");
%! y = x; y(2,2) = "cat";
%! fail ("assert_equal (x, y)");
%! y = x; y(1,1) = [2];  y(1,2) = [0, 2, 3]; y(2,1) = 101; y(2,2) = "cat";
%! fail ("assert_equal (x, y)");

## structures
%!error <Expected struct, but observed double> assert_equal (1, struct ("a", 1))
%!error <Structure sizes don't match>
%! x(1,2,3).a = 1;
%! y(1,2).a = 1;
%! assert_equal (x,y);
%!error <Structure sizes don't match>
%! x(1,2,3).a = 1;
%! y(3,2,2).a = 1;
%! assert_equal (x,y);
%!error <Structure sizes don't match>
%! x.a = 1;
%! x.b = 1;
%! y.a = 1;
%! assert_equal (x,y);
%!error <Structure fieldname mismatch>
%! x.b = 1;
%! y.a = 1;
%! assert_equal (x,y);
%!error <Abs err 1 exceeds tol 0 by 1>
%! x.a = 1;
%! y.a = 2;
%! assert_equal (x,y);

%!test
%! x.a = 1; x.b=[2, 2];
%! y.a = 1; y.b=[2, 2];
%! assert_equal (x, y);
%! y.b=3;
%! fail ("assert_equal (x, y)");
%! fail ("assert_equal (3, x)");
%! fail ("assert_equal (x, 3)");
%! ## Empty structures
%! x = resize (x, 0, 1);
%! y = resize (y, 0, 1);
%! assert_equal (x, y);

## vector of tolerances
%!test
%! x = [-40:0];
%! y1 = (10.^x).*(10.^x);
%! y2 = 10.^(2*x);
%! ## Increase tolerance from eps (y1) to 4*eps (y1) because of an upstream bug
%! ## in mingw-w64: https://sourceforge.net/p/mingw-w64/bugs/466/
%! assert_equal (y1, y2, 4*eps (y1));
%! fail ("assert_equal (y1, y2 + eps*1e-70, eps (y1))");

## Multiple tolerances
%!test
%! x = [1 2; 3 4];
%! y = [0 -1; 1 2];
%! tol = [-0.1 0; -0.2 0.3];
%! try
%!   assert_equal (x, y, tol);
%! catch
%!   errmsg = lasterr ();
%!   if (sum (errmsg () == "\n") != 6)
%!     error ("Incorrect number of errors reported");
%!   endif
%!   assert_equal (! isempty (regexp (errmsg, '\(1,2\).*Abs err 3 exceeds tol 0\>')), true);
%!   assert_equal (! isempty (regexp (errmsg, '\(2,2\).*Abs err 2 exceeds tol 0.3')), true);
%!   assert_equal (! isempty (regexp (errmsg, '\(1,1\).*Abs err 1 exceeds tol 0.1')), true);
%!   assert_equal (! isempty (regexp (errmsg, '\(2,1\).*Rel err 2 exceeds tol 0.2')), true);
%! end_try_catch

%!test <*57615>
%! try
%!   assert_equal (complex (pi*1e-17,2*pi), 0, 1e-1);
%! catch
%!   errmsg = lasterr ();
%!   assert_equal (isempty (strfind (errmsg, "sprintf: invalid field width")), true);
%! end_try_catch

%!test <*63988>
%! A = ["ab"; "cd"];
%! B = ["ad"; "cb"];
%! try
%!   assert_equal (A, B);
%! catch
%!   errmsg = lasterr ();
%!   if (regexp (errmsg, 'horizontal dimensions mismatch'))
%!     error ("assert_equal failed for char arrays with multiple rows");
%!   endif
%! end_try_catch

## classdef
%!error <Expected classdef object, but observed double> assert_equal (5, containers.Map ())
%!error <Expected classdef object does not have an 'eq' method> ...
%!       assert_equal (containers.Map (), containers.Map ())
%!test
%! try
%!   assert_equal (containers.Map ([1, 2], {1, 2}), containers.Map ());
%! catch
%!   errmsg = lasterr ();
%!   assert_equal (! isempty (regexp (errmsg, 'O\(2x1\)')), true);
%!   assert_equal (! isempty (regexp (errmsg, 'E\(0x1\)')), true);
%!   assert_equal (! isempty (regexp (errmsg, "Dimensions don't match")), true);
%! end_try_catch

## test input validation
%!error <Invalid call> assert_equal ()
%!error <assert_equal: function called with too many inputs> assert_equal (1,2,3,4)


## Convert all error indices into tuple format
function cout = ind2tuple (matsize, erridx)

  tmp = cell (1, numel (matsize));
  [tmp{:}] = ind2sub (matsize, erridx(:));
  subs = [tmp{:}];
  if (numel (matsize) == 2)
    subs = subs(:, matsize != 1);  # For vectors, use 1-D index
  endif
  fmt = repmat ('%d,', 1, max (columns (subs), 1));
  fmt(end) = [];   # delete final extra comma
  cout = ostrsplit (sprintf (['(' fmt ')', '$'], subs'), '$');
  cout(end) = [];  # delete extra cell from final '$'
  cout = cout.';   # return column vector

endfunction


## Pretty print the various errors in a condensed tabular format.
function str = pprint (argin, err)

  str = ["ASSERT errors for:  assert_equal " argin "\n"];
  str = [str, "\n  Location  |  Observed  |  Expected  |  Reason\n"];

  pos = numel (str);
  str(pos + 100 * numel (err.index)) = ' ';
  for i = 1:numel (err.index)
    leni = numel (err.index{i});
    leno = numel (err.observed{i});
    lene = numel (err.expected{i});
    tmp = sprintf ("%*s%*s %*s%*s %*s%*s   %s\n",
                   6+fix(leni/2), err.index{i}   , max (6-fix(leni/2), 0), "",
                   6+fix(leno/2), err.observed{i}, max (6-fix(leno/2), 0), "",
                   6+fix(lene/2), err.expected{i}, max (6-fix(lene/2), 0), "",
                   err.reason{i});
    if (pos + numel (tmp) > numel (str))
      str(end + 1e6) = ' ';
    endif
    str((pos + 1):(pos + numel (tmp))) = tmp;
    pos += numel (tmp);
  endfor
  str = str(1:pos);

endfunction
