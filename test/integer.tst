## Copyright (C) 2018 Rik Wehbring
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

## Test saturation mechanics of lower bound
%!test
%! for cls = {"int8", "uint8", "int16", "uint16", "int32", "uint32", "int64", "uint64"}
%!   clsmin = intmin (cls{1});
%!   xplus = clsmin + (-1);
%!   assert (xplus, clsmin);
%!   xminus = clsmin -1;
%!   assert (xminus, clsmin);
%!   xmult = clsmin * 2;
%!   assert (xmult, clsmin);
%!   xdiv = clsmin / 0.5;
%!   assert (xdiv, clsmin);
%! endfor

## Test saturation mechanics of upper bound
%!test
%! for cls = {"int8", "uint8", "int16", "uint16", "int32", "uint32", "int64", "uint64"}
%!   clsmax = intmax (cls{1});
%!   xplus = clsmax + 1;
%!   assert (xplus, clsmax);
%!   xminus = clsmax - (-1);
%!   assert (xminus, clsmax);
%!   xmult = clsmax * 2;
%!   assert (xmult, clsmax);
%!   xdiv = clsmax / 0.5;
%!   assert (xdiv, clsmax);
%! endfor
