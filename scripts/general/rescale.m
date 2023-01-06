########################################################################
##
## Copyright (C) 2019-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{B} =} rescale (@var{A})
## @deftypefnx {} {@var{B} =} rescale (@var{A}, @var{l}, @var{u})
## @deftypefnx {} {@var{B} =} rescale (@dots{}, "inputmin", @var{inmin})
## @deftypefnx {} {@var{B} =} rescale (@dots{}, "inputmax", @var{inmax})
## Scale matrix elements to a specified range of values.
##
## When called with a single matrix argument @var{A}, rescale elements to
## occupy the interval [0, 1].
##
## The optional inputs @code{[@var{l}, @var{u}]} will scale @var{A} to the
## interval with lower bound @var{l} and upper bound @var{u}.
##
## The optional input @qcode{"inputmin"} replaces all elements less than
## the specified value @var{inmin} with @var{inmin}.  Similarly, the optional
## input @qcode{"inputmax"} replaces all elements greater than the specified
## value @var{inmax} with @var{inmax}.  If unspecified the minimum and maximum
## are taken from the data itself (@w{@code{@var{inmin} = min (A(:))}} and
## @w{@code{@var{inmax} = max (A(:))}}).
##
## Programming Notes:
## The applied formula is
##
## @tex
## $$B = l + {A - inmin \over inmax - inmin} \cdot (u - l)$$
## @end tex
## @ifnottex
## @var{B} = @var{l} + ((@var{A} - @var{inmin}) ./ (@var{inmax} - @var{inmin}))
## .* (@var{u} - @var{l})
## @end ifnottex
##
## The class of the output matrix @var{B} is single if the input @var{A} is
## single, but otherwise is of class double for inputs which are of double,
## integer, or logical type.
##
## @seealso{bounds, min, max}
## @end deftypefn

function B = rescale (A, varargin)

  if (! any (nargin == [1,3,5,7]))
    print_usage ();
  endif

  ## Verify A input.
  if (! (isnumeric (A) || islogical (A)))
    error ("rescale: A must be a numeric or logical matrix");
  endif
  ## Use class double for integers and logical types
  if (! isa (A, "single"))
    A = double (A);
  endif

  l = 0;  u = 1;

  ## If 2nd and 3rd argument are numeric, set non-default interval [l, u].
  if (nargin > 1 && isnumeric (varargin{1}))
    if (! isnumeric (varargin{2}))
      error ("rescale: upper bound U must be numeric");
    endif
    l = varargin{1};  u = varargin{2};
    varargin(1:2) = [];
    if (any ((l > u)(:)))
      error ("rescale: lower bound L must be smaller than upper bound U");
    endif
    ## FIXME: Need input validation for cases where l or u is not a scalar
  endif

  truncate_range = false;

  ## Check for named argument "inputmin".
  inminidx = find (strcmpi (varargin, "inputmin"), 1, "last");
  if (! isempty (inminidx))
    if (! isnumeric (varargin{inminidx + 1}))
      error ("rescale: INMIN must be numeric");
    endif
    inmin = varargin{inminidx + 1};
    varargin(inminidx:inminidx + 1) = [];
    truncate_range = true;
  else
    inmin = min (A(:));
  endif

  ## Check for named argument "inputmax".
  inmaxidx = find (strcmpi (varargin, "inputmax"), 1, "last");
  if (! isempty (inmaxidx))
    if (! isnumeric (varargin{inmaxidx + 1}))
      error ("rescale: INMAX must be numeric");
    endif
    inmax = varargin{inmaxidx + 1};
    varargin(inmaxidx:inmaxidx + 1) = [];
    truncate_range = true;
  else
    inmax = max (A(:));
  endif

  ## Verify all options were processed
  if (! isempty (varargin))
    error ("rescale: invalid option(s)");
  endif

  ## Rescale A to interval [l,u] in range interval [inmin, inmax].
  range = inmax - inmin;
  range(range == 0) = 1;  # Avoid division by 0 resulting in NaN
  B = l + (A - inmin) ./ range .* (u - l);
  if (truncate_range)
    B(A < inmin) = l;
    B(A > inmax) = u;
  endif

endfunction


%!assert (rescale ([]), [])
%!assert (rescale ([0]), [0])
%!assert (rescale ([1]), [0])
%!assert (rescale (0:5), (0:5)/5)

## Test [l,u] input
%!assert (rescale (1:4, 3, 9), [3,5,7,9])
%!test
%! A = repmat ([1:3]', [1, 3]);
%! B = rescale (A, [1, 2, 3], 5);
%! assert (B, [1, 2, 3; 3, 3.5, 4; 5, 5, 5]);
%!test
%! A = repmat ([1:3]', [1, 3]);
%! B = rescale (A, [1; 2; 3], 5);
%! assert (B, [1, 1, 1; 3.5, 3.5, 3.5; 5, 5, 5]);

## Test property/value options
%!assert (rescale (0:6, "inputmin", 1, "inputmax", 5), [0,0,0.25,0.5,0.75,1,1])

## Test class of returned output
%!assert (class (rescale (single ([0, 5]))), "single")
%!assert (class (rescale (double ([0, 5]))), "double")
%!assert (class (rescale (int8 ([0, 5]))), "double")
%!assert (class (rescale (logical ([0, 1]))), "double")

## Test input validation
%!error <Invalid call> rescale ()
%!error <Invalid call> rescale (1, 2)
%!error <Invalid call> rescale (1, 2, 3, 4)
%!error <Invalid call> rescale (1, 2, 3, 4, 5, 6)
%!error <Invalid call> rescale (1, 2, 3, 4, 5, 6, 7, 8)
%!error <A must be a numeric or logical matrix> rescale ("abc")
%!error <A must be a numeric or logical matrix> rescale ({ [1] })
%!error <U must be numeric> rescale (1, 0, "A")
%!error <L must be smaller than .* U> rescale (1, 2, 0)
## FIXME: Need BIST tests here when input validation has been added for
##        l and u not being scalar.
%!error <INMIN must be numeric> rescale (1, "inputmin", "abc")
%!error <INMAX must be numeric> rescale (1, "inputmax", "abc")
%!error <invalid option> rescale (1, "foobar", 1)
