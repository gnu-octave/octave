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

## -*- texinfo -*-
## @deftypefn  {Function File} {} assert (@var{cond})
## @deftypefnx {Function File} {} assert (@var{cond}, @var{errmsg}, @dots{})
## @deftypefnx {Function File} {} assert (@var{cond}, @var{msg_id}, @var{errmsg}, @dots{})
## @deftypefnx {Function File} {} assert (@var{observed}, @var{expected})
## @deftypefnx {Function File} {} assert (@var{observed}, @var{expected}, @var{tol})
##
## Produce an error if the specified condition is not met.  @code{assert} can
## be called in three different ways.
##
## @table @code
## @item  assert (@var{cond})
## @itemx assert (@var{cond}, @var{errmsg}, @dots{})
## @itemx assert (@var{cond}, @var{msg_id}, @var{errmsg}, @dots{})
## Called with a single argument @var{cond}, @code{assert} produces an
## error if @var{cond} is zero.  When called with more than one argument the
## additional arguments are passed to the @code{error} function.
##
## @item assert (@var{observed}, @var{expected})
## Produce an error if observed is not the same as expected.  Note that
## @var{observed} and @var{expected} can be scalars, vectors, matrices,
## strings, cell arrays, or structures.
##
## @item assert (@var{observed}, @var{expected}, @var{tol})
## Produce an error if observed is not the same as expected but equality
## comparison for numeric data uses a tolerance @var{tol}.
## If @var{tol} is positive then it is an absolute tolerance which will produce
## an error if @code{abs (@var{observed} - @var{expected}) > abs (@var{tol})}.
## If @var{tol} is negative then it is a relative tolerance which will produce
## an error if @code{abs (@var{observed} - @var{expected}) >
## abs (@var{tol} * @var{expected})}.  If @var{expected} is zero @var{tol} will
## always be interpreted as an absolute tolerance.  If @var{tol} is not scalar
## its dimensions must agree with those of @var{observed} and @var{expected}
## and tests are performed on an element-wise basis.
## @end table
## @seealso{test, fail, error}
## @end deftypefn

function assert (cond, varargin)

  if (nargin == 0 || nargin > 3)
    print_usage ();
  endif

  persistent call_depth = -1;
  persistent errmsg;

  call_depth++;

  if (call_depth == 0)
    errmsg = "";
  end

  in = deblank (argn(1,:));
  for i = 2:rows (argn)
    in = [in "," deblank(argn(i,:))];
  endfor
  in = ["(" in ")"];

  if (nargin == 1 || (nargin > 1 && islogical (cond) && ischar (varargin{1})))
    if ((! isnumeric (cond) && ! islogical (cond)) || ! all (cond(:)))
      if (nargin == 1)
        ## Perhaps, say which elements failed?
        error ("assert %s failed", in);
      else
        error (varargin{:});
      endif
    endif
  else
    expected = varargin{1};
    if (nargin < 3)
      tol = 0;
    else
      tol = varargin{2};
    endif

    ## Add to list as the errors accumulate.  If empty at end then no errors.
    err.index = {};
    err.observed = {};
    err.expected = {};
    err.reason = {};

    if (ischar (expected))
      if (! ischar (cond))
        err.index{end+1} = "[]";
        err.expected{end+1} = expected;
        if (isnumeric (cond))
          err.observed{end+1} = num2str (cond);
          err.reason{end+1} = "Expected string, but observed number";
        elseif (iscell (cond))
          err.observed{end+1} = "{}";
          err.reason{end+1} = "Expected string, but observed cell";
        else
          err.observed{end+1} = "[]";
          err.reason{end+1} = "Expected string, but observed struct";
        end
      elseif (! strcmp (cond, expected))
        err.index{end+1} = "[]";
        err.observed{end+1} = cond;
        err.expected{end+1} = expected;
        err.reason{end+1} = "Strings don't match";
      endif

    elseif (iscell (expected))
      if (! iscell (cond) || any (size (cond) != size (expected)))
        err.index{end+1} = "{}";
        err.observed{end+1} = "O";
        err.expected{end+1} = "E";
        err.reason{end+1} = "Cell sizes don't match";
      else
        try
          ## Recursively compare cell arrays
          for i = 1:length (expected(:))
            assert (cond{i}, expected{i}, tol);
          endfor
        catch
          err.index{end+1} = "{}";
          err.observed{end+1} = "O";
          err.expected{end+1} = "E";
          err.reason{end+1} = "Cell configuration error";
        end_try_catch
      endif

    elseif (isstruct (expected))
      if (! isstruct (cond) || any (size (cond) != size (expected))
          || rows (fieldnames (cond)) != rows (fieldnames (expected)))
        err.index{end+1} = "{}";
        err.observed{end+1} = "O";
        err.expected{end+1} = "E";
        err.reason{end+1} = "Structure sizes don't match";
      else
        try
          empty = isempty (cond);
          normal = (numel (cond) == 1);
          for [v, k] = cond
            if (! isfield (expected, k))
              err.index{end+1} = ".";
              err.observed{end+1} = "O";
              err.expected{end+1} = "E";
              err.reason{end+1} = ["'" k "'" " is not an expected field"];
            endif
            if (empty)
              v = {};
            elseif (normal)
              v = {v};
            else
              v = v(:)';
            endif
            ## Recursively call assert for struct array values
            assert (v, {expected.(k)}, tol);
          endfor
        catch
          err.index{end+1} = ".";
          err.observed{end+1} = "O";
          err.expected{end+1} = "E";
          err.reason{end+1} = "Structure configuration error";
        end_try_catch
      endif

    elseif (ndims (cond) != ndims (expected)
            || any (size (cond) != size (expected)))
      err.index{end+1} = ".";
      err.observed{end+1} = ["O(" sprintf("%dx", size(cond))(1:end-1) ")"];
      err.expected{end+1} = ["E(" sprintf("%dx", size(expected))(1:end-1) ")"];
      err.reason{end+1} = "Dimensions don't match";

    else  # Numeric comparison
      if (nargin < 3)
        ## Without explicit tolerance, be more strict.
        if (! strcmp (class (cond), class (expected)))
          err.index{end+1} = "()";
          err.observed{end+1} = "O";
          err.expected{end+1} = "E";
          err.reason{end+1} = ["Class " class(cond) " != " class(expected)];
        elseif (isnumeric (cond))
          if (issparse (cond) != issparse (expected))
            err.index{end+1} = "()";
            err.observed{end+1} = "O";
            err.expected{end+1} = "E";
            if (issparse (cond))
              err.reason{end+1} = "sparse != non-sparse";
            else
              err.reason{end+1} = "non-sparse != sparse";
            endif
          elseif (iscomplex (cond) != iscomplex (expected))
            err.index{end+1} = "()";
            err.observed{end+1} = "O";
            err.expected{end+1} = "E";
           if (iscomplex (cond))
              err.reason{end+1} = "complex != real";
            else
              err.reason{end+1} = "real != complex";
            endif
          endif
        endif
      endif

      if (isempty (err.index))

        A = cond;
        B = expected;

        ## Check exceptional values.
        erridx = find (  isna (real (A)) != isna (real (B))
                       | isna (imag (A)) != isna (imag (B)));
        if (! isempty (erridx))
          err.index(end+1:end + length (erridx)) = ...
              ind2tuple (size (A), erridx);
          err.observed(end+1:end + length (erridx)) = ...
              strtrim (cellstr (num2str (A(erridx) (:))));
          err.expected(end+1:end + length (erridx)) = ...
              strtrim (cellstr (num2str (B(erridx) (:))));
          err.reason(end+1:end + length (erridx)) = ...
              cellstr (repmat ("'NA' mismatch", length (erridx), 1));
        endif

        erridx = find (  isnan (real (A)) != isnan (real (B))
                       | isnan (imag (A)) != isnan (imag (B)));
        if (! isempty (erridx))
          err.index(end+1:end + length (erridx)) = ...
              ind2tuple (size (A), erridx);
          err.observed(end+1:end + length (erridx)) = ...
              strtrim (cellstr (num2str (A(erridx) (:))));
          err.expected(end+1:end + length (erridx)) = ...
              strtrim (cellstr (num2str (B(erridx) (:))));
          err.reason(end+1:end + length (erridx)) = ...
              cellstr (repmat ("'NaN' mismatch", length (erridx), 1));
        endif

        erridx = find (((isinf (real (A)) | isinf (real (B))) ...
                         & real (A) != real (B)) ...
                       | ((isinf (imag (A)) | isinf (imag (B)))
                         & imag (A) != imag (B)));
        if (! isempty (erridx))
          err.index(end+1:end + length (erridx)) = ...
              ind2tuple (size (A), erridx);
          err.observed(end+1:end + length (erridx)) = ...
              strtrim (cellstr (num2str (A(erridx) (:))));
          err.expected(end+1:end + length (erridx)) = ...
              strtrim (cellstr (num2str (B(erridx) (:))));
          err.reason(end+1:end + length (erridx)) = ...
              cellstr (repmat ("'Inf' mismatch", length (erridx), 1));
        endif

        ## Check normal values.
        ## Replace exceptional values already checked above by zero.
        A_null_real = real (A);
        B_null_real = real (B);
        exclude = ! isfinite (A_null_real) & ! isfinite (B_null_real);
        A_null_real(exclude) = 0;
        B_null_real(exclude) = 0;
        A_null_imag = imag (A);
        B_null_imag = imag (B);
        exclude = ! isfinite (A_null_imag) & ! isfinite (B_null_imag);
        A_null_imag(exclude) = 0;
        B_null_imag(exclude) = 0;
        A_null = complex (A_null_real, A_null_imag);
        B_null = complex (B_null_real, B_null_imag);
        if (isscalar (tol))
          mtol = repmat (tol, size (A));
        else
          mtol = tol;
        endif

        k = (mtol == 0);
        erridx = find ((A_null != B_null) & k);
        if (! isempty (erridx))
          err.index(end+1:end + length (erridx)) = ...
              ind2tuple (size (A), erridx);
          err.observed(end+1:end + length (erridx)) = ...
              strtrim (cellstr (num2str (A(erridx) (:))));
          err.expected(end+1:end + length (erridx)) = ...
              strtrim (cellstr (num2str (B(erridx) (:))));
          err.reason(end+1:end + length (erridx)) = ...
              ostrsplit (deblank (sprintf ("Abs err %g exceeds tol %g\n", ...
              [abs(A_null(erridx) - B_null(erridx)) mtol(erridx)]')), "\n");
        endif

        k = (mtol > 0);
        erridx = find ((abs (A_null - B_null) > mtol) & k);
        if (! isempty (erridx))
          err.index(end+1:end + length (erridx)) = ...
              ind2tuple (size (A), erridx);
          err.observed(end+1:end + length (erridx)) = ...
              strtrim (cellstr (num2str (A(erridx) (:))));
          err.expected(end+1:end + length (erridx)) = ...
              strtrim (cellstr (num2str (B(erridx) (:))));
          err.reason(end+1:end + length (erridx)) = ...
              ostrsplit (deblank (sprintf ("Abs err %g exceeds tol %g\n", ...
              [abs(A_null(erridx) - B_null(erridx)) mtol(erridx)]')), "\n");
        endif

        k = (mtol < 0);
        if (any (k))
          ## Test for absolute error where relative error can't be calculated.
          erridx = find ((B_null == 0) & abs (A_null) > abs (mtol) & k);
          if (! isempty (erridx))
            err.index(end+1:end + length (erridx)) = ...
                ind2tuple (size (A), erridx);
            err.observed(end+1:end + length (erridx)) = ...
                strtrim (cellstr (num2str (A(erridx) (:))));
            err.expected(end+1:end + length (erridx)) = ...
                strtrim (cellstr (num2str (B(erridx) (:))));
            err.reason(end+1:end + length (erridx)) = ...
                ostrsplit (deblank (sprintf ("Abs err %g exceeds tol %g\n",
                [abs(A_null(erridx) - B_null(erridx)) -mtol(erridx)]')), "\n");
          endif
          ## Test for relative error
          Bdiv = Inf (size (B_null));
          Bdiv(k & (B_null != 0)) = B_null(k & (B_null != 0));
          relerr = abs ((A_null - B_null) ./ abs (Bdiv));
          erridx = find ((relerr > abs (mtol)) & k);
          if (! isempty (erridx))
            err.index(end+1:end + length (erridx)) = ...
                ind2tuple (size (A), erridx);
            err.observed(end+1:end + length (erridx)) = ...
                strtrim (cellstr (num2str (A(erridx) (:))));
            err.expected(end+1:end + length (erridx)) = ...
                strtrim (cellstr (num2str (B(erridx) (:))));
            err.reason(end+1:end + length (erridx)) = ...
                ostrsplit (deblank (sprintf ("Rel err %g exceeds tol %g\n",
                [relerr(erridx) -mtol(erridx)]')), "\n");
          endif
        endif
      endif

    endif

    ## Print any errors
    if (! isempty (err.index))
      if (! isempty (errmsg))
        errmsg = [errmsg "\n"];
      endif
      errmsg = [errmsg, pprint(in, err)];
    end

  endif

  call_depth--;

  if (call_depth == -1)
    ## Last time through.  If there were any errors on any pass, raise a flag.
    if (! isempty (errmsg))
      error (errmsg);
    endif
  endif

endfunction


## empty input
%!assert ([])
%!assert (zeros (3,0), zeros (3,0))
%!error assert (zeros (3,0), zeros (0,2))
%!error assert (zeros (3,0), [])
%!error <Dimensions don't match> assert (zeros (2,0,2), zeros (2,0))

## conditions
%!assert (isempty ([]))
%!assert (1)
%!error assert (0)
%!assert (ones (3,1))
%!assert (ones (1,3))
%!assert (ones (3,4))
%!error assert ([1,0,1])
%!error assert ([1;1;0])
%!error assert ([1,0;1,1])

## scalars
%!error assert (3, [3,3; 3,3])
%!error assert ([3,3; 3,3], 3)
%!assert (3, 3)
%!assert (3+eps, 3, eps)
%!assert (3, 3+eps, eps)
%!error assert (3+2*eps, 3, eps)
%!error assert (3, 3+2*eps, eps)

## vectors
%!assert ([1,2,3],[1,2,3]);
%!assert ([1;2;3],[1;2;3]);
%!error assert ([2,2,3,3],[1,2,3,4]);
%!error assert ([6;6;7;7],[5;6;7;8]);
%!error assert ([1,2,3],[1;2;3]);
%!error assert ([1,2],[1,2,3]);
%!error assert ([1;2;3],[1;2]);
%!assert ([1,2;3,4],[1,2;3,4]);
%!error assert ([1,4;3,4],[1,2;3,4])
%!error assert ([1,3;2,4;3,5],[1,2;3,4])

## matrices
%!test
%! A = [1 2 3]'*[1,2];
%! assert (A,A);
%! fail ("assert (A.*(A!=2),A)");
%! X = zeros (2,2,3);
%! Y = X;
%! Y (1,2,3) = 1;
%! fail ("assert (X,Y)");

## must give a small tolerance for floating point errors on relative
%!assert (100+100*eps, 100, -2*eps)
%!assert (100, 100+100*eps, -2*eps)
%!error assert (100+300*eps, 100, -2*eps)
%!error assert (100, 100+300*eps, -2*eps)
%!error assert (3, [3,3])
%!error assert (3, 4)

## test relative vs. absolute tolerances
%!test  assert (0.1+eps, 0.1,  2*eps);  # accept absolute
%!error assert (0.1+eps, 0.1, -2*eps);  # fail relative
%!test  assert (100+100*eps, 100, -2*eps);  # accept relative
%!error assert (100+100*eps, 100,  2*eps);  # fail absolute

## exceptional values
%!assert ([NaN, NA, Inf, -Inf, 1+eps, eps], [NaN, NA, Inf, -Inf, 1, 0], eps)
%!error assert (NaN, 1)
%!error assert ([NaN 1], [1 NaN])
%!error assert (NA, 1)
%!error assert ([NA 1]', [1 NA]')
%!error assert ([(complex (NA, 1)) (complex (2, NA))], [(complex (NA, 2)) 2])
%!error assert (-Inf, Inf)
%!error assert ([-Inf Inf], [Inf -Inf])
%!error assert (complex (Inf, 0.2), complex (-Inf, 0.2 + 2*eps), eps)

## strings
%!assert ("dog", "dog")
%!error assert ("dog", "cat")
%!error assert ("dog", 3)
%!error assert (3, "dog")
%!error assert (cellstr ("dog"), "dog")
%!error assert (cell2struct ({"dog"; 3}, {"pet", "age"}, 1), "dog");

## structures
%!shared x,y
%! x.a = 1; x.b=[2, 2];
%! y.a = 1; y.b=[2, 2];
%!assert (x, y)
%!test y.b=3;
%!error assert (x, y)
%!error assert (3, x)
%!error assert (x, 3)
%!test
%! # Empty structures
%! x = resize (x, 0, 1);
%! y = resize (y, 0, 1);
%! assert (x, y);

## cell arrays
%!test
%! x = {[3], [1,2,3]; 100+100*eps, "dog"};
%! y = x;
%! assert (x, y);
%! y = x; y(1,1) = [2];
%! fail ("assert (x, y)");
%! y = x; y(1,2) = [0, 2, 3];
%! fail ("assert (x, y)");
%! y = x; y(2,1) = 101;
%! fail ("assert (x, y)");
%! y = x; y(2,2) = "cat";
%! fail ("assert (x, y)");
%! y = x; y(1,1) = [2];  y(1,2) = [0, 2, 3]; y(2,1) = 101; y(2,2) = "cat";
%! fail ("assert (x, y)");

## variable tolerance
%!test
%! x = [-40:0];
%! y1 = (10.^x).*(10.^x);
%! y2 = 10.^(2*x);
%! assert (y1, y2, eps (y1));
%! fail ("assert (y1, y2 + eps*1e-70, eps (y1))");

## test input validation
%!error assert ()
%!error assert (1,2,3,4)


## Convert all error indices into tuple format
function cout = ind2tuple (matsize, erridx)

  cout = cell (numel (erridx), 1);
  tmp = cell (1, numel (matsize));
  [tmp{:}] = ind2sub (matsize, erridx (:));
  subs = [tmp{:}];
  if (numel (matsize) == 2)
    subs = subs(:, matsize != 1);
  endif
  for i = 1:numel (erridx)
    loc = sprintf ("%d,", subs(i,:));
    cout{i} = ["(" loc(1:end-1) ")"];
  endfor

endfunction


## Pretty print the various errors in a condensed tabular format.
function str = pprint (in, err)

  str = ["ASSERT errors for:  assert " in "\n"];
  str = [str, "\n  Location  |  Observed  |  Expected  |  Reason\n"];
  for i = 1:length (err.index)
    leni = length (err.index{i});
    leno = length (err.observed{i});
    lene = length (err.expected{i});
    str = [str, sprintf("%*s%*s %*s%*s %*s%*s   %s\n",
                        6+fix(leni/2), err.index{i}   , 6-fix(leni/2), "",
                        6+fix(leno/2), err.observed{i}, 6-fix(leno/2), "",
                        6+fix(lene/2), err.expected{i}, 6-fix(lene/2), "",
                        err.reason{i})];
  endfor

endfunction

