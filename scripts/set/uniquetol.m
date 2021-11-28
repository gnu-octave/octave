########################################################################
##
## Copyright (C) 2020-2021 The Octave Project Developers
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
## @deftypefn  {} {@var{c} =} uniquetol (@var{A})
## @deftypefnx {} {@var{c} =} uniquetol (@var{A}, @var{tol})
## @deftypefnx {} {@var{c} =} uniquetol (@dots{}, @var{property}, @var{value})
## @deftypefnx {} {[@var{c}, @var{ia}, @var{ic}] =} uniquetol (@dots{})
## Return the unique elements of @var{A} within tolerance @var{tol}.
##
## Two values, @var{x} and @var{y}, are within relative tolerance if
## @code{abs (@var{x} - @var{y}) <= @var{tol} * max (abs (@var{A}(:)))}.
##
## The input @var{A} must be a floating point type (double or single).
##
## If @var{tol} is unspecified, the default tolerance is 1e-12 for double
## precision input or 1e-6 for single precision input.
##
## The function may also be called with the following optional property/value
## pairs.  Property/value pairs must be passed after other input arguments:
##
## @table @asis
## @item @qcode{"ByRows"} (default: @code{false})
## When true, return the unique rows of @var{A}.  @var{A} must be a 2-D array
## to use this option.  For rows, the criteria for uniqueness is changed to
## @code{all (abs (@var{x} - @var{y}) <= @var{tol}*max (abs (@var{A}),[],1))}
## which compares each column component of a row against a column-specific
## tolerance.
##
## @item @qcode{"DataScale"}
## The tolerance test is changed to
## @code{abs (@var{x} - @var{y}) <= @var{tol}*@var{DS}} where @var{DS} is a
## scalar unless the property @qcode{"ByRows"} is true.  In that case, @var{DS}
## can either be a scalar or a vector with a length equal to the number of
## columns in @var{A}.  Using a value of @code{1.0} for @var{DS} will change
## the tolerance from a relative one to an absolute tolerance.  Using a value
## of @code{Inf} will disable testing.
##
## @item @qcode{"OutputAllIndices"} (default: @code{false})
## When true, @var{ia} is a cell array (not a vector) that contains the indices
## for @emph{all} elements in @var{A} that are within tolerance of a value in
## @var{C}.  That is, each cell in @var{ia} corresponds to a single unique
## value in @var{C}, and the values in each cell correspond to locations in
## @var{A}.
## @end table
##
## The output @var{c} is a row vector if the input @var{A} is a row vector.
## For all other cases, a column vector is returned.
##
## The optional output @var{ia} is a column index vector such that
## @code{@var{c} = @var{A}(@var{ia})}.  If the @qcode{"ByRows"} property is
## true, the condition is @code{@var{c} = @var{A}(@var{ia}, :)}.  If the
## @qcode{"OutputAllIndices"} property is true, then the values
## @code{@var{A}(@var{ia}@{@var{i}@})} are all within tolerance of the unique
## value @code{@var{c}(@var{i})}.
##
## The optional output @var{ic} is a column index vector such that
## @code{@var{A} = @var{c}(@var{ic})} when @var{A} is a vector.  When @var{A}
## is a matrix, @code{@var{A}(:) = @var{c}(@var{ic})}.  If the @qcode{"ByRows"}
## property is true then @code{@var{A} = @var{c}(@var{ic},:)}.
##
## Example: small round-off errors require @code{uniquetol}, not @code{unique}
##
## @example
## @group
## x = [1:5];
## ## Inverse_Function (Function (x)) should return exactly x
## y = exp (log (x));
## D = unique ([x, y])
## @result{} [1.0000   2.0000   3.0000   3.0000   4.0000   5.0000   5.0000]
## C = uniquetol ([x, y])
## @result{} [1   2   3   4   5]
## @end group
## @end example
##
## @seealso{unique, union, intersect, setdiff, setxor, ismember}
## @end deftypefn


function [c, ia, ic] = uniquetol (A, varargin)

  if (nargin < 1)
    print_usage ();
  endif

  if (isempty (A))
    c = A;
    ia = [];
    ic = [];
    return;
  endif

  if (! isfloat (A) || iscomplex (A))
    error ("Octave:uniquetol:unsupported-type",
           "uniquetol: A must be a double or single precision non-complex array");
  endif

  if (nargin == 1 || ischar (varargin{1}))
    tol = ifelse (isa (A, "double"), 1e-12, 1e-6);
  elseif (! (isfloat (varargin{1}) && isscalar (varargin{1}))
          || iscomplex (varargin{1}))
    error ("Octave:uniquetol:unsupported-type",
           "uniquetol: TOL must be a double or single precision non-complex scalar");
  else
    tol = varargin{1};
    varargin(1) = [];
  endif

  if (mod (numel (varargin), 2))
    error ("uniquetol: PROPERTY/VALUE arguments must be passed in pairs");
  endif

  by_rows = false;
  output_all_indices = false;
  data_scale = [];

  for k = 1:2:numel (varargin)
    if (! ischar (varargin{k}))
      error ("uniquetol: PROPERTY must be a string");
    endif

    if (strcmpi (varargin{k}, "ByRows"))
      by_rows = logical (varargin{k+1});
      if (by_rows && ndims (A) > 2)
        error ('uniquetol: A must be a 2-D array when "ByRows" is true');
      endif
    elseif (strcmpi (varargin{k}, "OutputAllIndices"))
      output_all_indices = logical (varargin{k+1});
    elseif (strcmpi (varargin{k}, "DataScale"))
      data_scale = varargin{k+1}(:).';
      if (! isfloat (data_scale) || iscomplex (data_scale)
          || any (data_scale(:) < 0) || any (isnan (data_scale(:))))
        error ("uniquetol: DataScale must be a non-NaN, positive floating point scalar or vector");
      endif
      cols_data_scale = columns (data_scale);
      if (cols_data_scale != 1 && cols_data_scale != columns (A))
        error ("uniquetol: invalid DataScale size");
      endif
    else
      error ("uniquetol: unknown property '%s'", varargin{k});
    endif
  endfor

  if (isempty (data_scale))
    data_scale = max (abs (A(! isinf (A))(:)));
  endif

  tol *= data_scale;

  if (by_rows)

    nr = rows (A);
    nc = columns (A);
    Iall = zeros (nr, 1);
    I = NaN (nc, 1);
    ia = {};
    J = NaN (nc, 1);
    j = 1;
    ii = 0;

    for i = 1:nr
      if (any (Iall == i))
        continue;
      else
        equ = all (abs (A - A(i,:)) <= tol, 2);
        equ(i,1) = equ(i,1) || any (! isfinite (A(i,:)), 2);
        sumeq = sum (equ);
        ia_tmp = find (equ);
        if (output_all_indices)
          ia{end+1} = ia_tmp;
        endif
        Iall(ii+(1:sumeq)) = ia_tmp;
        I(j) = ia_tmp(1);
        J(equ) = j;
        ii += sumeq;
        j += 1;
      endif
    endfor

    I(isnan (I)) = [];
    J(isnan (J)) = [];
    c = A(I,:);

    if (! output_all_indices)
      ia = I(1:j-1);
    endif
    ic = J;

  else
    isrowvec = isrow (A);
    A = A(:);
    nr = rows (A);
    isnanA = isnan (A);
    anyisnanA = any (isnanA);
    [sortA, sAi] = sort (A);
    diffsortA = diff (sortA);
    isinfsortA = isinf (sortA);
    isnansortA = isnan (sortA);
    numnan = sum (isnansortA);
    if (any (isinfsortA))
      sAnin = sortA(! (isinfsortA | isnansortA));
      diffsortA(isinf (diffsortA)) = abs (sAnin(end) - sAnin(1)) + 10;
    endif
    csdx = cumsum (diffsortA);
    ue = [true; diff([0; csdx-mod(csdx,tol)]) > eps(max(csdx))];
    ueold = NaN;
    while (any (ueold != ue))
      ueold = ue;
      belowtol = [false; diff(sortA(ue)) < tol];
      if (any (belowtol))
        needstomove = find (ue)(belowtol);
        ue(needstomove) = false;
        needstomove(needstomove >= nr-numnan) = [];
        ue(needstomove+1) = true;
      endif
    endwhile
    c = sortA(ue);
    [~, sortsAi] = sort (sAi);
    cumsumue = cumsum (ue);
    ic = cumsumue(sortsAi);
    if (anyisnanA)
      findisnanA = find (isnanA);
    else
      findisnanA = [];
    endif
    if (output_all_indices)
      nu = cumsumue(end);
      ia = cell (1, nu);
      for k = 1:nu
        ia{k} = setdiff (sAi(cumsumue==k), findisnanA);
      endfor
    else
      ia = sAi(ue);
    endif

    if (anyisnanA)
      rowsc1 = rows (c) + (1:sum (isnanA));
      c(rowsc1) = NaN;
      ia(rowsc1) = findisnanA;
      ic(isnanA) = rowsc1;
    endif

    ## FIXME: Matlab-compatible orientation of output
    ## Actually, Matlab prefers row vectors (2021/03/24), but this is different
    ## from all the other set functions which prefer column vectors.  Assume
    ## that this is a bug in Matlab's implementation and prefer column vectors.
    if (isrowvec)
      c = c.';
    endif

  endif

endfunction


%!assert (uniquetol ([1 1 2; 1 2 1; 1 1 2+10*eps]), [1;2])
%!assert (uniquetol ([1 1 2; 1 0 1; 1 1 2+10*eps], "byrows", true),
%!        [1 1 2; 1 0 1])
%!assert (uniquetol ([]), [])
%!assert (uniquetol ([1]), [1])
%!assert (uniquetol ([2, 1]), [1, 2]);
%!assert (uniquetol ([1; 2]), [1; 2])
%!assert (uniquetol ([-Inf, 1, NaN, Inf, NaN, Inf]), [-Inf, 1, Inf, NaN, NaN]);
%!assert (uniquetol (zeros (1, 0)), zeros (1, 0));
%!assert (uniquetol (zeros (1, 0), "byrows", true), zeros (1, 0))
%!assert (uniquetol ([1,2,2,3,2,4], "byrows", true), [1,2,2,3,2,4])
%!assert (uniquetol ([1,2,2,3,2,4]), [1,2,3,4])
%!assert (uniquetol ([1,2,2,3,2,4].', "byrows", true), [1;2;3;4])
%!assert (uniquetol (sparse ([2,0;2,0])), sparse ([0;2]))
%!assert (uniquetol (sparse ([1,2;2,3])), sparse ([1;2;3]))
%!assert (uniquetol (single ([1,2,2,3,2,4]), "byrows", true),
%!        single ([1,2,2,3,2,4]))
%!assert (uniquetol (single ([1,2,2,3,2,4])), single ([1,2,3,4]))
%!assert (uniquetol (single ([1,2,2,3,2,4].'), "byrows", true),
%!        single ([1;2;3;4]))

## Matlab compatibility of output
%!test
%! x = 1:0.045:3;
%! y = uniquetol (x, 0.1, "datascale", 1);
%! assert (y(1:4), [1, 1.135, 1.27, 1.405]);

## Test index vector return arguments
%!test
%! [c, ia, ic] = uniquetol ([1,1,2,3,3,3,4]);
%! assert (c, [1,2,3,4]);
%! assert (ia, [1;3;4;7]);
%! assert (ic, [1;1;2;3;3;3;4]);

## Test index vector return arguments with "ByRows"
%!test
%! A = [2, 3, 4; 2, 3, 4];
%! [c, ia, ic] = uniquetol (A, "byrows", true);
%! assert (c, [2, 3, 4]);
%! assert (A(ia,:), c);
%! assert (c(ic,:), A);

%!test
%! x = (2:7)'*pi;
%! y = exp (log (x));
%! C = uniquetol ([x; y]);
%! assert (C, x, 1e-12);

## Test "ByRows" Property
%!test
%! A = [0.06, 0.21, 0.38; 0.38, 0.21, 0.39; 0.54, 0.56, 0.41; 0.46, 0.52, 0.95];
%! B = log (exp (A));
%! C = uniquetol ([A; B], "ByRows", true);
%! assert (C, A);

## Test "DataScale" Property
%!test
%! x = 10^11;
%! C = uniquetol ([x, exp(log(x))], 1e-6, "DataScale", 1);
%! assert (C, [x, exp(log(x))]);

## Test "OutputAllIndices" Property
%!test
%! A = [.1 .2 .3 10];
%! [C, ia, ic] = uniquetol (A, .1, "OutputAllIndices", true);
%! assert (C, [.1, 10]);
%! assert (ia, {(1:3)', 4});
%! assert (ic, [1; 1; 1; 2]);

## Test input validation
%!error <Invalid call> uniquetol ()
%!error <A must be a double or single precision> uniquetol (int8 (1))
%!error <A must be .* non-complex> uniquetol (1i)
%!error <TOL must be a double .* precision> uniquetol (1, int8 (1))
%!error <TOL must be a .* scalar> uniquetol (1, [1, 2])
%!error <TOL must be .* non-complex> uniquetol (1, 1i)
%!error <arguments must be passed in pairs> uniquetol (1, 2, "byrows")
%!error <PROPERTY must be a string> uniquetol (1, 2, 3, "bar")
%!error <A must be a 2-D array> uniquetol (ones (2,2,2), "byrows", true)
%!error <DataScale must be a .* floating point> uniquetol (1, "DataScale", '1')
%!error <DataScale must be .* positive> uniquetol (1, "DataScale", -1)
%!error <DataScale must be .* positive> uniquetol (1, "DataScale", 1i)
%!error <DataScale must be a non-NaN> uniquetol (1, "DataScale", NaN)
%!error <invalid DataScale size> uniquetol (1, "DataScale", [1 2])
%!error <unknown property 'foo'> uniquetol (1, "foo", "bar")
%!error <unknown property 'foo'> uniquetol (1, 2, "foo", "bar")
