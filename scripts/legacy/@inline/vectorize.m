########################################################################
##
## Copyright (C) 2020-2022 The Octave Project Developers
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
## @deftypefn {} {@var{vfcn} =} vectorize (@var{fcn})
## Create a vectorized version of the inline function @var{fcn} by
## replacing all occurrences of @code{*}, @code{/}, etc., with
## @code{.*}, @code{./}, etc.
##
## This may be useful, for example, when using inline functions with
## numerical integration or optimization where a vector-valued function
## is expected.
##
## @example
## @group
## fcn = vectorize (inline ("x^2 - 1"))
##    @result{} fcn = f(x) = x.^2 - 1
## quadv (fcn, 0, 3)
##    @result{} 6
## @end group
## @end example
## @seealso{inline, formula, argnames}
## @end deftypefn

## The following function was translated directly from the original C++
## version.  Yes, it will be slow, but the use of inline functions is
## strongly discouraged anyway, and most expressions will probably be
## short.  It may also be buggy.  Well, don't use this function!  Use
## function handles instead!

function vfcn = vectorize (fcn)

  if (nargin < 1)
    print_usage ();
  endif

  vfcn = inline (__vectorize__ (fcn.expr));

endfunction
