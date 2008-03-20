## Copyright (C) 2008  David Bateman
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## This piece of test code ensures that all operations which work on 
## dimensions alone (squeeze, triu, etc.) work for all objects and 
## preserve type. Even if the object is an empty matrix. This code is
## not to check that the function itself returns teh correct result,
## just that the results are consistent for all types.

%!function __fntestfunc__ (fn, min, varargin)
%!  typ = {'double', 'complex', 'logical', 'sparse', 'complex sparse', ...
%!         'logical sparse', 'int8', 'int16', 'int32', 'int64', 'uint8', ...
%!         'uint16', 'uint32', 'uint64'};
%!
%!  cmplx = [2, 5];
%!  nlogical = [3, 6];
%!  ninteger = [7, 8, 9, 10, 11, 12, 13, 14];
%!  nsparse = [4, 5, 6];
%!  usesparse = true;
%!
%!  if (length (varargin) > 0 && islogical (varargin{1}))
%!    usesparse = varargin{1};
%!    varargin(1) = [];
%!  endif
%!
%!  for i = 1 : length(typ)
%!    m = min;
%!    if (length (varargin) > 0)
%!      args = varargin(1);
%!    else
%!      args = {};
%!    endif
%!
%!    if (any (nsparse == i))
%!      if (! usesparse)
%!        continue;
%!      endif
%!      if (ndims (m) > 2)
%!        sz = size (m);
%!        m = reshape (m, [sz(1), prod(sz (2:end))]);
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
%!      m = cast (m, 'logical');
%!    endif
%!    if (any (ninteger == i))
%!      m = cast (m, typ{i});
%!    endif
%!
%!    y = feval (fn, m, args{:});
%!    if (!strcmp (class (y), class (m)) ||
%!	   issparse (y) != issparse (m) ||
%!         any (cast (real (y), 'double')(:) !=
%!              feval (fn , cast (real (m), 'double'), args{:})(:)))
%!      error ('failed for type %s\n', typ{i});
%!    endif
%!  endfor
%! endfunction

%!shared m0, m1, m2, m3
%! m0 = [1:5];
%! m1 = reshape ([1 : 30], [5, 6]);
%! m2 = reshape ([1 : 30], [5, 1, 6]);
%! m3 = [];

%!test
%! __fntestfunc__('triu', m1);
%!test
%! __fntestfunc__ ('triu', m1, -1);
%!test
%! __fntestfunc__ ('triu', m1, 1);
%!test
%! __fntestfunc__('triu', m3);
%!test
%! __fntestfunc__ ('tril', m1);
%!test
%! __fntestfunc__ ('tril', m1, -1);
%!test
%! __fntestfunc__ ('tril', m1, 1);
%!test
%! __fntestfunc__('tril', m3);
%!test
%! __fntestfunc__ ('squeeze', m2);
%!test
%! __fntestfunc__ ('squeeze', m3);
%!test
%! __fntestfunc__ ('permute', m1, [2, 1]);
%!test
%! __fntestfunc__ ('permute', m2, false, [3, 1, 2]);
%!test
%! __fntestfunc__ ('permute', m3, [2, 1]);
%!test
%! __fntestfunc__ ('ipermute', m1, [2, 1]);
%!test
%! __fntestfunc__ ('ipermute', m2, false, [3, 1, 2]);
%!test
%! __fntestfunc__ ('ipermute', m3, [2, 1]);
%!test
%! __fntestfunc__ ('shiftdim', m2, 1);
%!test
%! __fntestfunc__ ('shiftdim', m2, false, -1);
%!test
%! __fntestfunc__ ('shiftdim', m3, 1);
%!test
%! __fntestfunc__ ('circshift', m2, 1);
%!test
%! __fntestfunc__ ('circshift', m2, [1, -1]);
%!test
%! __fntestfunc__ ('circshift', m3, 1);
%!test
%! __fntestfunc__ ('reshape', m2, [6, 5]);
%!test
%! __fntestfunc__ ('reshape', m3, [1, 0]);
%!test
%! __fntestfunc__ ('diag', m0);
%!test
%! __fntestfunc__ ('diag', m0, 1);
%!test
%! __fntestfunc__ ('diag', m0, -1);
%!test
%! __fntestfunc__ ('diag', m1);
%!test
%! __fntestfunc__ ('diag', m1, 1);
%!test
%! __fntestfunc__ ('diag', m1, -1);
%!test
%! __fntestfunc__ ('diag', m3);
%!test
%! __fntestfunc__ ('fliplr', m1);
%!test
%! __fntestfunc__ ('fliplr', m3);
%!test
%! __fntestfunc__ ('flipud', m1);
%!test
%! __fntestfunc__ ('flipud', m3);
%!test
%! __fntestfunc__ ('flipdim', m1, 2);
%!test
%! __fntestfunc__ ('flipdim', m3, 2);
%!test
%! __fntestfunc__ ('transpose', m1);
%!test
%! __fntestfunc__ ('transpose', m3);
%!test
%! __fntestfunc__ ('ctranspose', m1);
%!test
%! __fntestfunc__ ('ctranspose', m3);
%!test
%! __fntestfunc__ ('rot90', m1);
%!test
%! __fntestfunc__ ('rot90', m1, 2);
%!test
%! __fntestfunc__ ('rot90', m1, -1);
%!test
%! __fntestfunc__ ('rot90', m3);
%!test
%! __fntestfunc__ ('rotdim', m2, 1, [1, 2]);
%!test
%! __fntestfunc__ ('rotdim', m2, 2, [1, 2]);
%!test
%! __fntestfunc__ ('rotdim', m2, -1, [1, 2]);
%!test
%! __fntestfunc__ ('rotdim', m3, 1, [1, 2]);
