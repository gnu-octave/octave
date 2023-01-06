########################################################################
##
## Copyright (C) 2008-2023 The Octave Project Developers
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

## This piece of test code ensures that all operations which work on
## dimensions alone (squeeze, triu, etc.) work for all objects and
## preserve type. Even if the object is an empty matrix. This code is
## not to check that the function itself returns the correct result,
## just that the results are consistent for all types.

%!function __fntestfunc__ (fn, mn, varargin)
%!  typ = {"double", "complex", "logical", "sparse", "complex sparse", ...
%!         "logical sparse", "int8", "int16", "int32", "int64", "uint8", ...
%!         "uint16", "uint32", "uint64", "char", "cell", "struct", ...
%!         "single", "single complex"};
%!
%!  cmplx = [2, 5, 18];
%!  nlogical = [3, 6];
%!  ninteger = [7, 8, 9, 10, 11, 12, 13, 14];
%!  nsparse = [4, 5, 6];
%!  skip = {};
%!
%!  if (length (varargin) > 0 && iscell (varargin{1}))
%!    skip = varargin{1};
%!    varargin(1) = [];
%!  endif
%!
%!  for i = 1 : length (typ)
%!    if (any (strcmp (skip, typ {i})))
%!      continue;
%!    endif
%!    m = mn;
%!
%!    if (any (nsparse == i))
%!      if (ndims (m) > 2)
%!        sz = size (m);
%!        m = reshape (m, [sz(1), prod(sz(2:end))]);
%!      endif
%!      if (any (cmplx == i))
%!        m = sparse ((1 + 1i) * m);
%!      else
%!        m = sparse (m);
%!      endif
%!    else
%!      if (any (cmplx == i))
%!        m = (1 + 1i) * m;
%!      endif
%!    endif
%!    if (any (nlogical == i))
%!      m = cast (m, "logical");
%!    endif
%!    if (any (ninteger == i))
%!      m = cast (m, typ{i});
%!    endif
%!    if (strcmp (typ{i}, "cell"))
%!      m = num2cell (m);
%!    elseif (strcmp (typ{i}, "struct"))
%!      m = struct ("fld", num2cell (m));
%!    endif
%!
%!    y = feval (fn, m, varargin{:});
%!    y2 = feval (fn, reshape (mn, size (m)), varargin{:});
%!    if (! strcmp (class (y), class (m)) ||
%!         issparse (y) != issparse (m) || ! size_equal (y, y2))
%!      error ("failed for type %s\n", typ{i});
%!    endif
%!    if (!(strcmp (typ{i}, "cell") || strcmp (typ{i}, "struct")) &&
%!        any (vec (cast (real (y), "double")) !=
%!             vec (feval (fn , cast (real (m), "double"), varargin{:}))))
%!      error ("failed for type %s\n", typ{i});
%!    endif
%!  endfor
%!endfunction

%!shared m0, m1, m2, m3
%! m0 = [1:5];
%! m1 = reshape ([1 : 30], [5, 6]);
%! m2 = reshape ([1 : 30], [5, 1, 6]);
%! m3 = [];

%!test
%! __fntestfunc__ ("triu", m1, {"struct"});
%!test
%! __fntestfunc__ ("triu", m1, {"struct"}, -1);
%!test
%! __fntestfunc__ ("triu", m1, {"struct"}, 1);
%!test
%! __fntestfunc__ ("triu", m3, {"struct"});
%!test
%! __fntestfunc__ ("tril", m1, {"struct"});
%!test
%! __fntestfunc__ ("tril", m1, {"struct"}, -1);
%!test
%! __fntestfunc__ ("tril", m1, {"struct"}, 1);
%!test
%! __fntestfunc__ ("tril", m3, {"struct"});
%!test
%! __fntestfunc__ ("squeeze", m2);
%!test
%! __fntestfunc__ ("squeeze", m3);
%!test
%! __fntestfunc__ ("permute", m1, [2, 1]);
%!test
%! __fntestfunc__ ("permute", m2, {"sparse", "logical sparse", "complex sparse"}, [3, 1, 2]);
%!test
%! __fntestfunc__ ("permute", m3, [2, 1]);
%!test
%! __fntestfunc__ ("ipermute", m1, [2, 1]);
%!test
%! __fntestfunc__ ("ipermute", m2, {"sparse", "logical sparse", "complex sparse"}, [3, 1, 2]);
%!test
%! __fntestfunc__ ("ipermute", m3, [2, 1]);
%!test
%! __fntestfunc__ ("shiftdim", m2, 1);
%!test
%! __fntestfunc__ ("shiftdim", m2, {"sparse", "logical sparse", "complex sparse"}, -1);
%!test
%! __fntestfunc__ ("shiftdim", m3, 1);
%!test
%! __fntestfunc__ ("circshift", m2, 1);
%!test
%! __fntestfunc__ ("circshift", m2, [1, -1]);
%!test
%! __fntestfunc__ ("circshift", m3, 1);
%!test
%! __fntestfunc__ ("reshape", m2, [6, 5]);
%!test
%! __fntestfunc__ ("reshape", m3, [1, 0]);
%!test
%! __fntestfunc__ ("diag", m0, {"struct"});
%!test
%! __fntestfunc__ ("diag", m0, {"struct"}, 1);
%!test
%! __fntestfunc__ ("diag", m0, {"struct"}, -1);
%!test
%! __fntestfunc__ ("diag", m1, {"struct"});
%!test
%! __fntestfunc__ ("diag", m1, {"struct"}, 1);
%!test
%! __fntestfunc__ ("diag", m1, {"struct"}, -1);
%!test
%! __fntestfunc__ ("diag", m3, {"struct"});
%!test
%! __fntestfunc__ ("fliplr", m1);
%!test
%! __fntestfunc__ ("fliplr", m3);
%!test
%! __fntestfunc__ ("flipud", m1);
%!test
%! __fntestfunc__ ("flipud", m3);
%!test
%! __fntestfunc__ ("flip", m1, 2);
%!test
%! __fntestfunc__ ("flip", m3, 2);
%!test
%! __fntestfunc__ ("transpose", m1);
%!test
%! __fntestfunc__ ("transpose", m3);
%!test
%! __fntestfunc__ ("ctranspose", m1);
%!test
%! __fntestfunc__ ("ctranspose", m3);
%!test
%! __fntestfunc__ ("rot90", m1);
%!test
%! __fntestfunc__ ("rot90", m1, 2);
%!test
%! __fntestfunc__ ("rot90", m1, -1);
%!test
%! __fntestfunc__ ("rot90", m3);
%!test
%! __fntestfunc__ ("rotdim", m2, 1, [1, 2]);
%!test
%! __fntestfunc__ ("rotdim", m2, 2, [1, 2]);
%!test
%! __fntestfunc__ ("rotdim", m2, -1, [1, 2]);
%!test
%! __fntestfunc__ ("rotdim", m3, 1, [1, 2]);

## Check for error if function parameter is made persistent
%!function retval = __fnpersist1__ (in1)
%!  persistent retval;
%!
%!  retval = 1;
%!endfunction

%!function retval = __fnpersist2__ (in1)
%!  persistent in1;
%!
%!  retval = in1;
%!endfunction

%!error <can't make function parameter retval persistent> __fnpersist1__ (1)
%!error <can't make function parameter in1 persistent> __fnpersist2__ (1)

## Check nargin, nargout validation by interpreter
%!function __fn_nargout0__ (in1)
%!endfunction
%!function [out1] = __fn_nargin2__ (in1, in2)
%!endfunction
%!function [out1] = __fn_nargin0__ ()
%!endfunction
%!function [] = __fn_narginout0__ ()
%!endfunction
%!function __fn_no_arg_list__
%!endfunction

%!error <function called with too many outputs> r = __fn_nargout0__ ()
%!error <function called with too many inputs>  r = __fn_nargin2__ (1,2,3)
%!error <function called with too many inputs>  r = __fn_nargin0__ (1)
%!error <function called with too many inputs>  __fn_narginout0__ (1)
%!error <function called with too many outputs> r = __fn_narginout0__ ()
%!error <function called with too many inputs>  __fn_no_arg_list__ (1)
%!error <function called with too many outputs>  r = __fn_no_arg_list__ ()
