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
## @deftypefn  {} {@var{C} =} uniquetol (@var{A}, @var{tol})
## @deftypefnx {} {@var{C} =} uniquetol (@var{A})
## @deftypefnx {} {@var{C} =} uniquetol (@dots{}, @var{property}, @var{value}, @dots{})
## @deftypefnx {} {[@var{C}, @var{IA}, @var{IC}] =} uniquetol (@dots{})
## Return the unique elements of @var{A} using tolerance @var{tol}.
##
## Two values, @var{u} and @var{v}, are within tolerance if
## @code {abs (@var{u} - @var{v}) <= @var{tol} * max (abs (@var{A}(:)))}.
##
## If it is unspecified, the default tolerance is 1e-12 for double precision
## input or 1e-6 for single precision input, respectively.
##
## If the input @var{A} is not a floating point type, @code{unique} is called
## without any additional options and a warning is issued.
##
## If the input @var{A} is a column vector, a column vector is returned.
## Otherwise, a row vector is returned.
##
## @var{IA} and @var{IC} are column index vectors such that
## @code{@var{C} = @var{A}(@var{IA})} and @code{@var{A} = @var{C}(@var{IC})}.
##
## Additionally, the function can be called with the following optional property
## value pairs. Property value pairs must be passed after the other input
## arguments:
##
## @table @asis
## @item @qcode{"ByRows"}, @code{true}
## The function returns the unique rows of @var{A}.
##
## @item @qcode{"DataScale"}, @var{DS}
## The tolerance test is changed to
## @code{abs (@var{u}-@var{v}) <= @var{tol}*@var{DS}} where @var{DS} is a scalar
## unless the property @qcode{"ByRows"} is set to @code{true}. In that case,
## @var{DS} can be a scalar or a vector with a length equal to the number of
## rows in @var{A}.
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


function [C, IA, IC] = uniquetol (A, varargin)

  if (nargin < 1)
    print_usage ();
  endif

  if (isempty (A))
    C = A;
    IA = [];
    IC = [];
    return;
  endif

  if (! isfloat (A))
    warning ("Octave:uniquetol:unsupported-type", ...
             ["uniquetol: A is not a numeric, double or single precision " ...
              "array. Calling unique without any additional options."]);
    [C, IA, IC] = unique (A);
    return;
  endif

  by_rows = false;
  output_all_indices = false;
  data_scale = max (abs (A(! isinf (A))(:)));
  size_A = size (A);

  if (nargin == 1 || ! (isnumeric (varargin{1}) && isscalar (varargin{1})))
    if (isa (A, "double"))
      tol = 1e-12;
    elseif (isa (A, "single"))
      tol = 1e-6;
    else
      ## FIXME: Is there a float type that is not "double" or "single"?
      error (["uniquetol: A must be a double or single precision floating ", ...
              "point type"]);
    endif
  else
    tol = varargin{1};
    varargin(1) = [];
  endif

  if mod (numel (varargin), 2)
    error ("uniquetol: property value arguments must be passed in pairs");
  endif

  for k = 1:2:numel (varargin)
    if (! ischar (varargin{k}))
      error ("uniquetol: property must be a string");
    endif

    if (strcmpi (varargin{k}, "ByRows"))
      by_rows = logical (varargin{k+1});
    elseif (strcmpi (varargin{k}, "OutputAllIndices"))
      output_all_indices = logical (varargin{k+1});
    elseif (strcmpi (varargin{k}, "DataScale"))
      data_scale = varargin{k+1}(:).';
      cols_data_scale = columns (data_scale);
      if (! (cols_data_scale == 1 || cols_data_scale == size_A(2)))
        error ("uniquetol: incorrect size of data scale");
      endif
    else
      error ("uniquetol: unknown property '%s'", varargin{k});
    endif
  endfor

  if (! by_rows)
    A = A(:);
  endif

  points = rows (A);
  d = columns (A);
  Iall = zeros (points, 1);
  I = NaN (d, 1);
  IA = {};
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
      ia = find (equ);
      if (output_all_indices)
        IA{end+1} = ia;
      endif
      Iall(ii+(1:sumeq)) = ia;
      I(j) = ia(1);
      J(equ) = j;
      ii += sumeq;
      j += 1;
    endif
  endfor

  I(isnan (I)) = [];
  J(isnan (J)) = [];
  C = A(I,:);

  if (size_A(1) == 1 && size_A(2) > 1 && ! by_rows)
    C = C.';
    I = I.';
    J = J.';
  endif

  if (! output_all_indices)
    IA = I(1:j-1);
  endif
  IC = J;

endfunction


%!assert (uniquetol ([1 1 2; 1 2 1; 1 1 2]), [1;2])
%!assert (uniquetol ([1 1 2; 1 0 1; 1 1 2], 1e-12, "byrows", true), [1 1 2; 1 0 1])
%!assert (uniquetol ([]), [])
%!assert (uniquetol ([1]), [1])
%!assert (uniquetol ([1, 2]), [1, 2])
%!assert (uniquetol ([1; 2]), [1; 2])
%!assert (uniquetol ([1, NaN, Inf, NaN, Inf]), [1, NaN, Inf, NaN, Inf])
%!assert (uniquetol (zeros (1,0)), zeros (1,0))
%!assert (uniquetol (zeros (1,0), 1e-12, "byrows", true), zeros (1,0))
%!assert (uniquetol ([1,2,2,3,2,4], 1e-12, "byrows", true), [1,2,2,3,2,4])
%!assert (uniquetol ([1,2,2,3,2,4]), [1,2,3,4])
%!assert (uniquetol ([1,2,2,3,2,4].', 1e-12, "byrows", true), [1;2;3;4])
%!assert (uniquetol (sparse ([2,0;2,0])), sparse([2;0]))
%!assert (uniquetol (sparse ([1,2;2,3])), sparse([1;2;3]))
%!assert (uniquetol ([1,2,2,3,2,4]', 1e-12, "byrows", true), [1;2;3;4])
%!assert (uniquetol (single ([1,2,2,3,2,4]), 1e-12, "byrows", true), single ([1,2,2,3,2,4]))
%!assert (uniquetol (single ([1,2,2,3,2,4])), single ([1,2,3,4]))
%!assert (uniquetol (single ([1,2,2,3,2,4].'), 1e-12, "byrows", true), single ([1;2;3;4]))
%!assert (unique (uint8 ([1,2,2,3,2,4])), uint8 ([1,2,3,4]))

%!test
%! [y, i, j] = uniquetol ([1,1,2,3,3,3,4]);
%! assert (y, [1,2,3,4]);
%! assert (i, [1;3;4;7]);
%! assert (j, [1;1;2;3;3;3;4]);

%!test
%! A = [2, 3, 4; 2, 3, 4];
%! [y, i, j] = uniquetol (A, "byrows", true);
%! assert (y, [2, 3, 4]);
%! assert (A(i,:), y);
%! assert (y(j,:), A);

%!test
%! x = (2:7)'*pi;
%! y = exp (1).^log (x);
%! C = uniquetol ([x; y]);
%! assert (C, x);

%!test
%! A = [0.06, 0.21, 0.38; 0.38, 0.21, 0.39; 0.54, 0.56, 0.41; 0.46, 0.52, 0.95];
%! B = log (exp (1).^A);
%! C = uniquetol ([A; B], "ByRows", true);
%! assert (C, A);

%!test
%! A = [.1 .2 .3 10];
%! [C, IA, IC] = uniquetol (A, .1, "OutputAllIndices", true);
%! assert (C, [.1, 10]);
%! assert (IA, {(1:3)', 4});
%! assert (IC, [1; 1; 1; 2]);

%!test
%! x = 10^11;
%! C = uniquetol ([x, exp(log(x))], 1e-6, "DataScale", 1);
%! assert (C, [x, exp(log(x))]);

%!error uniquetol ()
%!error <in pairs> uniquetol (1, 2, "byrows")
%!error <unknown property> uniquetol (1, "foo", "bar")
%!error <unknown property> uniquetol (1, 2, "foo", "bar")
%!error <must be a string> uniquetol (1, 2, 3, "foo")
%!error <incorrect size> uniquetol (1, "DataScale", [1 2])
