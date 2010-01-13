#!/bin/sh

# Copyright (C) 2010 VZLU Prague
#
# This file is part of Octave.
# 
# Octave is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 3 of the License, or (at
# your option) any later version.
# 
# Octave is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
# 
# You should have received a copy of the GNU General Public License
# along with Octave; see the file COPYING.  If not, see
# <http://www.gnu.org/licenses/>.

CLASSES="double single char logical int8 int16 int32 int64 \
uint8 uint16 uint32 uint64 struct cell function_handle"

for class in $CLASSES ; do
	DIR=\@$class
	test -d $DIR || mkdir $DIR || { echo "error: could not create $DIR" ; exit ; }
	cat > $DIR/tbcover.m <<end
% DO NOT EDIT - generated automatically
function s = tbcover (x, y)
  s = '$class';
end
done

cat > tbcover.m <<end
% DO NOT EDIT - generated automatically
function s = tbcover (x, y)
  s = 'none';
end

if test "$1" == "overloads_only" ; then
	exit
fi

cat > test_bc_overloads.m <<end
## THIS IS AN AUTOMATICALLY GENERATED FILE --- DO NOT EDIT ---
## instead modify build_bc_overload_tests.sh to generate the tests that you want.

%!shared ex
%! ex.double = 1;
%! ex.single = single(1);
%! ex.logical = true;
%! ex.char = 'char';
%! ex.int8  = int8 (1);
%! ex.int16 = int16 (1);
%! ex.int32 = int32 (1);
%! ex.int64 = int64 (1);
%! ex.uint8  = uint8 (1);
%! ex.uint16 = uint16 (1);
%! ex.uint32 = uint32 (1);
%! ex.uint64 = uint64 (1);
%! ex.cell = {};
%! ex.struct = struct ();
%! ex.function_handle = @numel;

end

cat bc_overloads_expected |\
while read cl1 cl2 clr ; do
	cat >> test_bc_overloads.m <<end
%% Name call
%!assert (tbcover (ex.$cl1, ex.$cl2), "$clr")
%% Handle call
%!assert ((@tbcover) (ex.$cl1, ex.$cl2), "$clr")

end
done

cat >> test_bc_overloads.m <<end
%%test handles through cellfun
%!test
%! f = fieldnames (ex);
%! n = numel (f);
%! s = c1 = c2 = cell (n);
%! for i = 1:n
%!   for j = 1:n
%!     c1{i,j} = ex.(f{i});
%!     c2{i,j} = ex.(f{j});
%!     s{i,j} = tbcover (ex.(f{i}), ex.(f{j}));
%!   endfor
%! endfor
%! assert (cellfun (@tbcover, c1, c2, "uniformoutput", false), s);
end
