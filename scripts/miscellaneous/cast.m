########################################################################
##
## Copyright (C) 2007-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{y} =} cast (@var{x}, "@var{type}")
## @deftypefnx {} {@var{y} =} cast (@var{x}, "@var{like}", @var{var})
## Convert @var{x} to data type @var{type}.
##
## The input @var{x} may be a scalar, vector, or matrix of a class that is
## convertible to the target class (see below).
##
## If a variable @var{var} is specified after @qcode{"like"}, @var{x} is
## converted to the same data type and sparsity attribute.  If @var{var} is
## complex, @var{x} will be complex, too.
##
## @var{var} may be and @var{type} may name any of the following built-in
## numeric classes:
##
## @example
## @group
## "double"
## "single"
## "logical"
## "char"
## "int8"
## "int16"
## "int32"
## "int64"
## "uint8"
## "uint16"
## "uint32"
## "uint64"
## @end group
## @end example
##
## The value @var{x} may be modified to fit within the range of the new type.
##
## Examples:
##
## @example
## @group
## cast (-5, "uint8")
##    @result{} 0
## cast (300, "int8")
##    @result{} 127
## @end group
## @end example
##
## Programming Note: This function relies on the object @var{x} having a
## conversion method named @var{type}.  User-defined classes may implement only
## a subset of the full list of types shown above.  In that case, it may be
## necessary to call cast twice in order to reach the desired type.
## For example, the conversion to double is nearly always implemented, but
## the conversion to uint8 might not be.  In that case, the following code will
## work:
##
## @example
## cast (cast (@var{user_defined_val}, "double"), "uint8")
## @end example
##
## @seealso{typecast, int8, uint8, int16, uint16, int32, uint32, int64, uint64,
## double, single, logical, char, class, typeinfo}
## @end deftypefn

function y = cast (x, type, var)

  if (nargin < 2 || nargin > 3)
    print_usage ();
  endif

  if (! ischar (type))
    error ("cast: TYPE must be a string");
  endif

  if (strcmp (type, "like"))
    is_like = true;
    type = class (var);
  else
    is_like = false;
  endif

  if ((! is_like && nargin != 2) || (is_like && nargin != 3))
    print_usage ();
  endif

  if (! isnumeric (x) && ! islogical (x) && ! ischar (x))
    error ("cast: type conversion from '%s' is not supported", class (x));
  endif

  if (! any (strcmp (type, {"int8"; "uint8"; "int16"; "uint16"; "int32";
                            "uint32"; "int64"; "uint64"; "double"; "single";
                            "logical"; "char"})))
    error ("cast: type conversion to '%s' is not supported", type);
  endif

  y = feval (type, x);

  if (is_like)
    if (issparse (var) && ! issparse (y))
      ## y is of the same type as var, so it must be convertible to sparse
      y = sparse (y);
    elseif (! issparse (var) && issparse (y))
      y = full (y);
    endif
    if (iscomplex (var) || iscomplex (x))
      y = complex (y);
    endif
  endif

endfunction


%!assert (cast (single (2.5), "double"), 2.5)
%!assert (cast (2.5, "single"), single (2.5))
%!assert (cast ([5 0 -5], "logical"), [true false true])
%!assert (cast ([65 66 67], "char"), "ABC")
%!assert (cast ([-2.5 1.1 2.5], "int8"), int8 ([-3 1 3]))
%!assert (cast ([-2.5 1.1 2.5], "uint8"), uint8 ([0 1 3]))
%!assert (cast ([-2.5 1.1 2.5], "int16"), int16 ([-3 1 3]))
%!assert (cast ([-2.5 1.1 2.5], "uint16"), uint16 ([0 1 3]))
%!assert (cast ([-2.5 1.1 2.5], "int32"), int32 ([-3 1 3]))
%!assert (cast ([-2.5 1.1 2.5], "uint32"), uint32 ([0 1 3]))
%!assert (cast ([-2.5 1.1 2.5], "int64"), int64 ([-3 1 3]))
%!assert (cast ([-2.5 1.1 2.5], "uint64"), uint64 ([0 1 3]))
%!assert (cast (1, "like", 2), 1)
%!assert (cast (1, "like", 2i), complex (1))
%!assert (cast (1, "like", speye (2)), sparse (1))
%!assert (cast (1, "like", sparse (2i)), complex (sparse (1)))
%!assert (cast (single (1), "like", speye (2)), sparse (1))
%!assert (cast (sparse (1), "like", 2), 1)
%!assert (cast (sparse (1), "like", 2i), complex (1))
%!assert (cast (complex (1), "like", 2), complex (1))
%!assert (cast (complex (1), "like", single (2)), complex (single (1)))
%!assert (cast ("a", "like", "octave"), "a")
%!assert (cast ("a", "like", 1i), complex (97))

## Test input validation
%!error <Invalid call> cast ()
%!error <Invalid call> cast (1)
%!error <Invalid call> cast (1, "double", 2)
%!error <TYPE must be a string> cast (1, {"foobar"})
%!error <type conversion from .* not supported> cast ({}, "double");
%!error <type conversion from .* not supported> cast (struct (), "double")
%!error <type conversion to .* not supported> cast (1, "foobar")
%!error <type conversion to .* not supported> cast (1, "cell")
%!error <type conversion to .* not supported> cast (1, "like", {})
%!error <type conversion to .* not supported> cast (1, "like", struct ())
