########################################################################
##
## Copyright (C) 2000-2026 The Octave Project Developers
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
## @deftypefn {} {@var{M} =} blkdiag (@var{A}, @var{B}, @var{C}, @dots{})
## Build a block diagonal matrix from @var{A}, @var{B}, @var{C}, @enddots{}
##
## All arguments must be numeric and either two-dimensional matrices or scalars.
##
## If any argument is of type sparse, the output will also be sparse and will be
## of class @code{double} (Octave's sparse storage does not currently support
## @code{single} or integer classes, so any non-@code{double} inputs are cast on
## assignment).  Otherwise, the class of the output follows the same promotion
## rules as @code{cat}: a mix of @code{single} and @code{double} yields
## @code{single}; any integer input yields an integer output (the first integer
## class encountered, when multiple integer classes are mixed, including across
## @code{single}); otherwise the output is @code{double}.
## @seealso{diag, horzcat, vertcat, cat, sparse}
## @end deftypefn

function M = blkdiag (varargin)

  if (nargin < 1)
    print_usage ();
  endif

  if (! all (cellfun ("isnumeric", varargin)))
    error ("blkdiag: all arguments must be numeric");
  endif

  ## Note: trailing singletons are automatically (correctly) ignored.
  if (! all (cellfun ("ndims", varargin) == 2))
    error ("blkdiag: all arguments must be two-dimensional matrices");
  endif

  ## 'size' is an option for cellfun, but it's a bit different from
  ## calling size directly which is required here.
  tmp = cell2mat (cellfun (@size, varargin', "uniformoutput", false));
  csz = cumsum ([0 0; tmp], 1);

  if (any (cellfun ("issparse", varargin)))
    ## FIXME: If Octave ever supports sparse matrices of class single this
    ##        code will need to be re-written.
    ## Octave only supports double sparse matrices.  Convert all inputs to
    ## double (which is a NOP if variable is already double).
    varargin = cellfun (@double, varargin, 'UniformOutput', false);
    M = sparse (csz(end,1), csz(end,2));
  else
    ## blkdiag follows the same class promotion as cat: same-class inputs keep
    ## that class; mixing double with a non-double class yields the non-double
    ## class; mixing an integer type with a floating point type yields the
    ## integer class; mixing two different integer types takes the first.  Query
    ## cat itself on 0x0 temporaries to recover the promoted class without
    ## actually concatenating any data.
    z = zeros (0,0, class (varargin{1}));
    for p = 2:nargin
      z = horzcat (z, zeros (0, 0, class (varargin{p})));
    endfor
    cls = class (z);
    M = zeros (csz(end,:), cls);
  endif

  for p = 1:nargin
    vp = varargin{p};
    if (! isempty (vp))
      M((csz(p,1)+1):csz(p+1,1),(csz(p,2)+1):csz(p+1,2)) = vp;
    endif
  endfor

endfunction


## regular tests
%!assert (blkdiag (1,ones (2),1), [1,0,0,0;0,1,1,0;0,1,1,0;0,0,0,1])
%!assert (blkdiag ([1,2],[3,4],[5,6]), [1,2,0,0,0,0;0,0,3,4,0,0;0,0,0,0,5,6])
%!assert (blkdiag ([1,2],[3;4],[5,6]),
%!        [1,2,0,0,0;0,0,3,0,0;0,0,4,0,0;0,0,0,5,6])
%!assert (blkdiag ([1,2;3,4],[5,6,7]), [1,2,0,0,0;3,4,0,0,0;0,0,5,6,7])
## tests involving empty matrices
%!assert (blkdiag ([],[],[]), [])
%!assert (blkdiag ([],[1,2;3,4],[],5,[]), [1,2,0;3,4,0;0,0,5])
%!assert (blkdiag (zeros (1,0,1),[1,2,3],1,0,5,zeros (0,1,1)),
%!        [0,0,0,0,0,0,0;1,2,3,0,0,0,0;0,0,0,1,0,0,0;0,0,0,0,0,0,0;0,0,0,0,0,5,0])
## tests involving sparse matrices
%!assert (blkdiag (sparse ([1,2;3,4]),[5,6;7,8]),
%!        sparse ([1,2,0,0;3,4,0,0;0,0,5,6;0,0,7,8]))
%!assert (blkdiag (sparse ([1,2;3,4]),[5,6]),
%!        sparse ([1,2,0,0;3,4,0,0;0,0,5,6]))
## sanity checks
%!test
%! A = rand (round (rand (1, 2) * 10));
%! assert (blkdiag (A), A);

## class preservation: single
%!assert <*68300> (class (blkdiag (single (1))), "single")
%!test <*68300>
%! A = single ([1,2;3,4]);
%! B = blkdiag (A, A);
%! assert (class (B), "single");
%! assert (B, single ([1,2,0,0;3,4,0,0;0,0,1,2;0,0,3,4]));

## class promotion: single + double -> single
%!test
%! B = blkdiag (single (1), 2);
%! assert (class (B), "single");
%! assert (B, single ([1,0;0,2]));
%!test
%! B = blkdiag (1, single ([2,3;4,5]), 6);
%! assert (class (B), "single");
%! assert (B, single ([1,0,0,0;0,2,3,0;0,4,5,0;0,0,0,6]));

## class preservation: signed integer types
%!assert (class (blkdiag (int8  (1), int8  (2))), "int8")
%!assert (class (blkdiag (int16 (1), int16 (2))), "int16")
%!assert (class (blkdiag (int32 (1), int32 (2))), "int32")
%!assert (class (blkdiag (int64 (1), int64 (2))), "int64")

## class preservation: unsigned integer types
%!assert (class (blkdiag (uint8  (1), uint8  (2))), "uint8")
%!assert (class (blkdiag (uint16 (1), uint16 (2))), "uint16")
%!assert (class (blkdiag (uint32 (1), uint32 (2))), "uint32")
%!assert (class (blkdiag (uint64 (1), uint64 (2))), "uint64")

## class promotion: integer + double -> integer (integer wins)
%!test
%! B = blkdiag (int16 (1), 2);
%! assert (class (B), "int16");
%! assert (B, int16 ([1,0;0,2]));
%!test
%! A = int32 ([1,2;3,4]);
%! C = int32 ([5,6,7]);
%! B = blkdiag (A, C);
%! assert (class (B), "int32");
%! assert (B, int32 ([1,2,0,0,0;3,4,0,0,0;0,0,5,6,7]));
%!test
%! B = blkdiag (uint8 ([1,2]), 3, uint8 ([4;5]));
%! assert (class (B), "uint8");
%! assert (B, uint8 ([1,2,0,0;0,0,3,0;0,0,0,4;0,0,0,5]));

## class promotion: integer beats single (cat semantics, not + semantics)
%!test
%! B = blkdiag (single (1), uint8 (2));
%! assert (class (B), "uint8");
%! assert (B, uint8 ([1,0;0,2]));
%!test
%! B = blkdiag (uint8 (1), single (2));
%! assert (class (B), "uint8");
%! assert (B, uint8 ([1,0;0,2]));

## class promotion: first integer class wins when multiple are mixed
%!test
%! B = blkdiag (uint8 (1), int32 (2));
%! assert (class (B), "uint8");
%! assert (B, uint8 ([1,0;0,2]));
%!test
%! B = blkdiag (int32 (1), uint8 (2));
%! assert (class (B), "int32");
%! assert (B, int32 ([1,0;0,2]));
%!test
%! B = blkdiag (1, int16 (2), uint8 (3));
%! assert (class (B), "int16");
%! assert (B, int16 ([1,0,0;0,2,0;0,0,3]));

## empties don't break class preservation
%!assert (class (blkdiag (single ([]), single (1))), "single")
%!assert (class (blkdiag (int8   ([]), int8   (1), int8 ([]))), "int8")
%!assert (class (blkdiag (single ([]), 1)), "single")

## value/shape check with mixed real and complex single
%!test
%! B = blkdiag (single (1+1i), single (2));
%! assert (class (B), "single");
%! assert (iscomplex (B));
%! assert (B, single ([1+1i, 0; 0, 2]));

## sparse + non-double inputs still produce a sparse double output
%!test
%! B = blkdiag (sparse (1), single (2));
%! assert (issparse (B));
%! assert (class (B), "double");
%! assert (full (B), [1,0;0,2]);
%!test
%! B = blkdiag (sparse ([1,2;3,4]), int32 (5));
%! assert (issparse (B));
%! assert (class (B), "double");
%! assert (full (B), [1,2,0;3,4,0;0,0,5]);

## Test input validation
%!error <Invalid call> blkdiag ()
%!error <all arguments must be numeric> blkdiag (1, "1")
%!error <all arguments must be numeric> blkdiag (1, {1})
%!error <arguments must be two-dimensional> blkdiag (1, ones (2,3,4))

