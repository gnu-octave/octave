########################################################################
##
## Copyright (C) 2020-2023 The Octave Project Developers
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
## The input @var{A} must be a real (non-complex) floating point type (double
## or single).
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
## @result{} [1   2   3   3   4   5   5]
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

  if (! (isfloat (A) && isreal (A)))
    error ("Octave:uniquetol:unsupported-type",
           "uniquetol: A must be a real floating point array");
  endif

  if (nargin == 1 || ischar (varargin{1}))
    tol = ifelse (isa (A, "double"), 1e-12, 1e-6);
  else
    tol = varargin{1};
    varargin(1) = [];
    if (! (isfloat (tol) && isreal (tol) && isscalar (tol)))
      error ("Octave:uniquetol:unsupported-type",
             "uniquetol: TOL must be a real floating point scalar");
     endif
  endif

  if (mod (numel (varargin), 2))
    error ("uniquetol: PROPERTY/VALUE arguments must occur in pairs");
  endif

  by_rows = false;
  output_all_indices = false;
  data_scale = [];
  calc_indices = nargout > 1;

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
      output_all_indices = logical (varargin{k+1}) & calc_indices;
    elseif (strcmpi (varargin{k}, "DataScale"))
      data_scale = varargin{k+1}(:).';
      if (! isfloat (data_scale) || iscomplex (data_scale)
          || any (data_scale(:) < 0) || any (isnan (data_scale(:))))
        error ("uniquetol: DataScale must be a positive floating point scalar or vector, without NaNs");
      endif
      cols_data_scale = columns (data_scale);
      if (cols_data_scale != 1 && cols_data_scale != columns (A))
        error ("uniquetol: invalid DataScale size");
      endif
    else
      error ("uniquetol: unknown property '%s'", varargin{k});
    endif
  endfor

  if (isempty (A))
    ## hack for Matlab empty input compatibility
    sz_A = size (A);
    if (by_rows)
      c = A;
      sz_A(2) = 1;
      ia = ones (sz_A);
      ic = ones (sz_A);
    else
      c = ones (0, 1, class (A));
      if (sz_A(1) == 1)
        c = c.';
      endif
      ia = ones (0, 1);
      ic = ones (0, 1);
    endif
    return;
  endif

  if (isempty (data_scale))
    data_scale = max (abs (A(! isinf (A))(:)));
  endif

  tol *= data_scale;

  if (by_rows)
    ## Start matrix in sorted order, retain sorting and inverting indices.
    if (calc_indices)
      [A, srtA] = sortrows (A);
      [~, inv_srtA] = sort (srtA);
    else
      A = sortrows (A);
    endif

    [nr, nc] = size (A);
    I = zeros (nr, 1);
    ia = {};
    J = zeros (nr, 1);
    j = 1;

    for i = 1:nr
      if (J(i))
        continue;  # row previously compared equal
      endif

      Arow_i = A(i,:);
      eq_rows = all (abs (A - Arow_i) <= tol, 2);
      eq_rows(i,1) = eq_rows(i,1) || any (! isfinite (Arow_i), 2);
      if (output_all_indices)
        ia_tmp = find (eq_rows);
        ia{end+1,1} = sort (srtA(ia_tmp));
      else
        ia_tmp = find (eq_rows, 1);
      endif
      I(j) = ia_tmp(1);
      J(eq_rows) = j;
      j += 1;
    endfor

    I = I(1:j-1);
    c = A(I,:);

    if (calc_indices)
      if (! output_all_indices)
        ia = srtA(I(1:j-1));
      endif
      ic = J(inv_srtA);
    endif

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
      ia = cell (nu, 1);
      for k = 1:nu
        ia{k} = setdiff (sAi(cumsumue==k), findisnanA);
      endfor
    else
      ia = sAi(ue);
    endif

    if (anyisnanA)
      rowsc1 = [1:sum(isnanA(:))]';
      if (! all (isnanA))
        rowsc1 += rows (c);
      endif
      c(rowsc1) = NaN;
      ic(isnanA) = rowsc1;
      if (output_all_indices)
        ia(rowsc1) = num2cell (findisnanA);
      else
        ia(rowsc1) = findisnanA;
      endif

      ## if numel (c) was 1, appending NaNs creates a row vector instead of
      ## expected column vector.
      if (isrow (c))
        c = c.';
      endif
    endif

    ## Matlab compatibility: Outputs are column vectors unless the input
    ## is a row vector, in which case the output c is also a row vector.
    ## ia and ic are always column vectors. (verified Matlab 2022a)
    if (isrowvec)
      c = c.';
    endif
  endif

endfunction


%!assert (uniquetol ([1 1 2; 1 2 1; 1 1 2+10*eps]), [1;2])
%!assert (uniquetol ([1 1 2; 1 0 1; 1 1 2+10*eps], "byrows", true),
%!        [1 0 1; 1 1 2])
%!assert (uniquetol ([1]), [1])
%!assert (uniquetol ([2, 1]), [1, 2])
%!assert (uniquetol ([1; 2]), [1; 2])
%!assert (uniquetol ([-Inf, 1, NaN, Inf, NaN, Inf]), [-Inf, 1, Inf, NaN, NaN])
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

## Test 2D array sorting
%!test
%! a = [magic(3); 2 * magic(3)];
%! assert (uniquetol (a), [1:10,12,14,16,18]');
%! assert (uniquetol (a, "byrows", true), sortrows (a));

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
%! assert (ia, 1);
%! assert (ic, [1;1]);

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
%! assert (C, sortrows(A), 10*eps);

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
%! assert (ia, {(1:3)'; 4});
%! assert (ic, [1; 1; 1; 2]);

## Test NaN inputs
%!assert (uniquetol (NaN), NaN)
%!assert (uniquetol ([NaN NaN]), [NaN NaN])
%!assert (uniquetol ([NaN NaN]'), [NaN NaN]')
%!assert (uniquetol (NaN(2,2)), NaN(4,1))

%!test
%! a = [magic(3); 2 * magic(3)];
%! a(4:5) = NaN;
%! [c, ia, ic] = uniquetol (a);
%! assert (c, [1:10,12,14,18, NaN, NaN]');
%! assert (ia, [7,10,2,3,8,13,14,1,9,11,16,17,12,4,5]');
%! assert (ic, [8,3,4,14,15,8,1,5,9,2,10,13,6,7,2,11,12,4]');
%! [c, ia, ic] = uniquetol (single (a));
%! assert (class (c), "single");
%! assert (class (ia), "double");
%! assert (class (ic), "double");
%! [c, ia, ic] = uniquetol (a, "ByRows", true);
%! assert (c, sortrows (a));
%! assert (ia, [2,3,1,6,4,5]');
%! assert (ic, [3,1,2,5,6,4]');
%! [c, ia, ic] = uniquetol (single (a), "ByRows", true);
%! assert (class (c), "single");
%! assert (class (ia), "double");
%! assert (class (ic), "double");
%! [c, ia, ic] = uniquetol (a, "OutputAllIndices", true);
%! assert (ia, {7;[10;15];2;[3;18];8;13;14;[1;6];9;11;16;17;12;4;5});
%! [c, ia, ic] = uniquetol (single (a), "OutputAllIndices", true);
%! assert (class (c), "single");
%! assert (class (ia{1}), "double");
%! assert (class (ic), "double");
%! [c, ia, ic] = uniquetol (a, "OutputAllIndices", true, "ByRows", true);
%! assert (ia, {2;3;1;6;4;5});
%! [c, ia, ic] = uniquetol (single (a),
%!                          "OutputAllIndices", true, "ByRows", true);
%! assert (class (c), "single");
%! assert (class (ia{1}), "double");
%! assert (class (ic), "double");

## Test empty input compatibility
%!test
%! [c, ia, ic] = uniquetol ([]);
%! assert (c, ones (0,1));
%! assert (ia, ones (0,1));
%! assert (ic, ones (0,1));
%!test
%! [c, ia, ic] = uniquetol ([], "byrows", true);
%! assert (c, []);
%! assert (ia, ones (0,1));
%! assert (ic, ones (0,1));
%!test
%! [c, ia, ic] = uniquetol (ones (0,1));
%! assert (c, ones (0,1));
%! assert (ia, ones (0,1));
%! assert (ic, ones (0,1));
%!test
%! [c, ia, ic] = uniquetol (ones (0,1), "byrows", true);
%! assert (c, ones (0,1));
%! assert (ia, ones (0,1));
%! assert (ic, ones (0,1));
%!test
%! [c, ia, ic] = uniquetol (ones (1,0));
%! assert (c, ones (1,0));
%! assert (ia, ones (0,1));
%! assert (ic, ones (0,1));
%!test
%! [c, ia, ic] = uniquetol (ones (1,0), "byrows", true);
%! assert (c, ones (1,0));
%! assert (ia, 1);
%! assert (ic, 1);
%!test
%! [c, ia, ic] = uniquetol (ones (1,0,2));
%! assert (c, ones (1,0));
%! assert (ia, ones (0,1));
%! assert (ic, ones (0,1));
%!test
%! [c, ia, ic] = uniquetol (ones (0,1,2));
%! assert (c, ones (0,1));
%! assert (ia, ones (0,1));
%! assert (ic, ones (0,1));
%!test
%! [c, ia, ic] = uniquetol (ones (1,2,0));
%! assert (c, ones (1,0));
%! assert (ia, ones (0,1));
%! assert (ic, ones (0,1));
%!test
%! [c, ia, ic] = uniquetol (single ([]));
%! assert (class (c), "single");
%! assert (class (ia), "double");
%! assert (class (ic), "double");
%!test
%! [c, ia, ic] = uniquetol (single ([]), "byrows", true);
%! assert (class (c), "single");
%! assert (class (ia), "double");
%! assert (class (ic), "double");
%!test
%! [c, ia, ic] = uniquetol (single ([]), "OutputAllIndices", true);
%! assert (class (c), "single");
%! assert (class (ia), "double");
%! assert (class (ic), "double");


## Test input validation
%!error <Invalid call> uniquetol ()
%!error <A must be a real floating point array> uniquetol (int8 (1))
%!error <A must be a real floating point array> uniquetol (1i)
%!error <TOL must be a real floating point scalar> uniquetol (1, int8 (1))
%!error <TOL must be a real floating point scalar> uniquetol (1, [1, 2])
%!error <TOL must be a real floating point scalar> uniquetol (1, 1i)
%!error <arguments must occur in pairs> uniquetol (1, 2, "byrows")
%!error <PROPERTY must be a string> uniquetol (1, 2, 3, "bar")
%!error <A must be a 2-D array> uniquetol (ones (2,2,2), "byrows", true)
%!error <A must be a 2-D array> uniquetol (ones (0,1,2), "byrows", true)
%!error <A must be a 2-D array> uniquetol (ones (1,0,2), "byrows", true)
%!error <A must be a 2-D array> uniquetol (ones (1,2,0), "byrows", true)
%!error <DataScale must be a .* floating point> uniquetol (1, "DataScale", '1')
%!error <DataScale must be .* positive> uniquetol (1, "DataScale", 1i)
%!error <DataScale must be .* positive> uniquetol (1, "DataScale", -1)
%!error <DataScale must be .* without NaNs> uniquetol (1, "DataScale", NaN)
%!error <invalid DataScale size> uniquetol (1, "DataScale", [1 2])
%!error <unknown property 'foo'> uniquetol (1, "foo", "bar")
%!error <unknown property 'foo'> uniquetol (1, 2, "foo", "bar")
