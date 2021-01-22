########################################################################
##
## Copyright (C) 2020 The Octave Project Developers
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
## Two values, @var{x} and @var{y}, are within tolerance if
## @code{abs (@var{x} - @var{y}) <= @var{tol} * max (abs (@var{A}(:)))}.
##
## If it is unspecified, the default tolerance is 1e-12 for double precision
## input or 1e-6 for single precision input, respectively.
##
## The input input @var{A} must be a floating point type (double or single).
##
## The output @var{C} is a row vector if the input @var{A} is a row vector.
## For all other cases, a column vector is returned.
##
## @var{IA} and @var{IC} are column index vectors such that
## @code{@var{C} = @var{A}(@var{IA})} and @code{@var{A} = @var{C}(@var{IC})}.
##
## Additionally, the function can be called with the following optional
## property value pairs. Property value pairs must be passed after the other
## input arguments:
##
## @table @asis
## @item @qcode{"ByRows"}, @code{true}
## The function returns the unique rows of @var{A}.
##
## @item @qcode{"DataScale"}, @var{DS}
## The tolerance test is changed to
## @code{abs (@var{x}-@var{y}) <= @var{tol}*@var{DS}} where @var{DS} is a scalar
## unless the property @qcode{"ByRows"} is set to @code{true}.  In that case,
## @var{DS} can be a scalar or a vector with a length equal to the number of
## rows in @var{A}.  Using a value of 1.0 for @var{DS} will change the
## tolerance from a relative one to an absolute tolerance.
##
## @item @qcode{"OutputAllIndices"}, @code{true}
## @var{IA} is a cell array that contains the indices for all elements in
## @var{A} that are within tolerance of a value in @var{C}. That is, each cell
## in @var{IA} corresponds to a value in @var{C}, and the values in each cell
## correspond to locations in @var{A}.
## @end table
##
## Example:
##
## @example
## @group
## x = (1:6)*pi;
## y = 10.^log10 (x);
## C = uniquetol ([x, y])
## @result{} [3.1416, 6.2832, 9.4248, 12.5664, 15.7080, 18.8496]
## D = unique ([x, y])
## @result{} [3.1416, 6.2832, 9.4248, 12.5664, 12.5664, 15.7080,
## 18.8496, 18.8496]
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

  if (! isfloat (A))
    error ("Octave:uniquetol:unsupported-type",
           "uniquetol: A must be a double or single precision array");
  endif

  if (nargin == 1 || ischar (varargin{1}))
    tol = ifelse (isa (A, "double"), 1e-12, 1e-6);
  elseif (! (isfloat (varargin{1}) && isscalar (varargin{1})))
    error ("Octave:uniquetol:unsupported-type",
           "uniquetol: TOL must be a double or single precision scalar");
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
      if (! isfloat (data_scale) || any (data_scale(:) < 0)
          || any (isnan (data_scale(:))))
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

  if (by_rows)
    isrowvec = false;
  else
    isrowvec = isrow (A);
    A = A(:);
  endif

  points = rows (A);
  d = columns (A);
  Iall = zeros (points, 1);
  I = NaN (d, 1);
  ia = {};
  J = NaN (d, 1);
  j = 1;
  ii = 0;
  tol_data_scale = tol * data_scale;

  for i = 1:points
    if (any (Iall == i))
      continue;
    else
      equ = all (abs (A - A(i,:)) <= tol_data_scale, 2);
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

  ## Matlab-compatible orientation of output
  if (isrowvec)
    c = c.';
    I = I.';
    J = J.';
  endif

  if (! output_all_indices)
    ia = I(1:j-1);
  endif
  ic = J;

endfunction


%!assert (uniquetol ([1 1 2; 1 2 1; 1 1 2]), [1;2])
%!assert (uniquetol ([1 1 2; 1 0 1; 1 1 2], 1e-12, "byrows", true),
%!        [1 1 2; 1 0 1])
%!assert (uniquetol ([]), [])
%!assert (uniquetol ([1]), [1])
%!xtest <59850>
%! ## FIXME: Matlab returns values sorted
%! assert (uniquetol ([2, 1]), [1, 2]);
%!assert (uniquetol ([1; 2]), [1; 2])
%!xtest <59850>
%! ## FIXME: Matlab returns only one unique value for Inf.
%! assert (uniquetol ([1, NaN, Inf, NaN, Inf]), [1, Inf, NaN, NaN]);
%!xtest <59850>
%! ## FIXME: Matlab returns empty column vectors.
%! ##        Do we want to bother with that?
%! assert (uniquetol (zeros (1,0)), zeros (0,1));
%!assert (uniquetol (zeros (1,0), 1e-12, "byrows", true), zeros (1,0))
%!assert (uniquetol ([1,2,2,3,2,4], 1e-12, "byrows", true), [1,2,2,3,2,4])
%!assert (uniquetol ([1,2,2,3,2,4]), [1,2,3,4])
%!assert (uniquetol ([1,2,2,3,2,4].', 1e-12, "byrows", true), [1;2;3;4])
%!assert (uniquetol (sparse ([2,0;2,0])), sparse ([2;0]))
%!assert (uniquetol (sparse ([1,2;2,3])), sparse ([1;2;3]))
%!assert (uniquetol ([1,2,2,3,2,4]', 1e-12, "byrows", true), [1;2;3;4])
%!assert (uniquetol (single ([1,2,2,3,2,4]), 1e-12, "byrows", true),
%!        single ([1,2,2,3,2,4]))
%!assert (uniquetol (single ([1,2,2,3,2,4])), single ([1,2,3,4]))
%!assert (uniquetol (single ([1,2,2,3,2,4].'), 1e-12, "byrows", true),
%!        single ([1;2;3;4]))

%!test
%! [c, ia, ic] = uniquetol ([1,1,2,3,3,3,4]);
%! assert (c, [1,2,3,4]);
%! assert (ia, [1;3;4;7]);
%! assert (ic, [1;1;2;3;3;3;4]);

%!test
%! A = [2, 3, 4; 2, 3, 4];
%! [c, ia, ic] = uniquetol (A, "byrows", true);
%! assert (c, [2, 3, 4]);
%! assert (A(ia,:), c);
%! assert (c(ic,:), A);

%!test
%! x = (2:7)'*pi;
%! y = exp (1).^log (x);
%! C = uniquetol ([x; y]);
%! assert (C, x);

## Test "ByRows" Property
%!test
%! A = [0.06, 0.21, 0.38; 0.38, 0.21, 0.39; 0.54, 0.56, 0.41; 0.46, 0.52, 0.95];
%! B = log (exp (1).^A);
%! C = uniquetol ([A; B], "ByRows", true);
%! assert (C, A);

## Test "OutputAllIndices" Property
%!test
%! A = [.1 .2 .3 10];
%! [C, ia, ic] = uniquetol (A, .1, "OutputAllIndices", true);
%! assert (C, [.1, 10]);
%! assert (ia, {(1:3)', 4});
%! assert (ic, [1; 1; 1; 2]);

## Test "DataScale" Property
%!test
%! x = 10^11;
%! C = uniquetol ([x, exp(log(x))], 1e-6, "DataScale", 1);
%! assert (C, [x, exp(log(x))]);

## Test input validation
%!error <Invalid call> uniquetol ()
%!error <A must be a double or single precision array> uniquetol (int8 (1))
%!error <TOL must be a double .* precision> uniquetol (1, int8 (1))
%!error <TOL must be a .* scalar> uniquetol (1, [1, 2])
%!error <arguments must be passed in pairs> uniquetol (1, 2, "byrows")
%!error <PROPERTY must be a string> uniquetol (1, 2, 3, "bar")
%!error <A must be a 2-D array> uniquetol (ones(2,2,2), "byrows", true)
%!error <DataScale must be a .* floating point> uniquetol (1, "DataScale", '1')
%!error <DataScale must be a .* positive> uniquetol (1, "DataScale", -1)
%!error <DataScale must be a non-NaN> uniquetol (1, "DataScale", NaN)
%!error <invalid DataScale size> uniquetol (1, "DataScale", [1 2])
%!error <unknown property 'foo'> uniquetol (1, "foo", "bar")
%!error <unknown property 'foo'> uniquetol (1, 2, "foo", "bar")
