########################################################################
##
## Copyright (C) 2006-2023 The Octave Project Developers
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

%!test
%! a = [];
%! assert (isempty (a));

%!shared a
%! a = 1;
%!assert (a(1), 1)
%!assert (a(:), 1)
%!assert (a(:,:), 1)
%!assert (a(1,:), 1)
%!assert (a(:,1), 1)
%!assert (isempty (a(logical (0))))
%!error a(-1)
%!error a(2)
%!error a(2,:)
%!error a(:,2)
%!error a(-1,:)
%!error a(:,-1)
%!error a([1,2,3])
%!error a([1;2;3])
%!error a([1,2;3,4])
%!error a([0,1])
%!error a([0;1])
%!error a([-1,0])
%!error a([-1;0])

%!shared a, a_prime, mid_a
%! a = [4,3,2,1];
%! a_prime = [4;3;2;1];
%! mid_a = [3,2];

%!assert (a(1),4)
%!assert (a(2),3)
%!assert (all (a(:) == a_prime))
%!assert (all (a(1,:) == a))
%!assert (a(:,3),2)
%!assert (all (a(:,:) == a))
%!assert (all (a(logical ([0,1,1,0])) == mid_a))
%!error a(0)
%!error a(5)
%!error a(0,1)
%!assert (isempty (a(logical (0),:)))
%!error a(:,0)
%!assert (isempty (a([])))
%!assert (isempty (a([],:)))
%!assert (isempty (a(:,[])))

%!shared a, a_fvec, a_col_1, a_col_2, a_row_1, a_row_2
%! a = [1,2;3,4];
%! a_fvec = [1;3;2;4];
%! a_col_1 = [1;3];
%! a_col_2 = [2;4];
%! a_row_1 = [1,2];
%! a_row_2 = [3,4];

%!assert (all (all (a(:,:) == a)))
%!assert (all (a(:) == a_fvec))
%!error a(0)
%!assert (a(2), 3)

## Additional tests

%!shared a, b
%! a = [1,2;3,4];
%! b = a;
%! b(:,:,2) = [5,6;7,8];

%!assert (a(:), [1;3;2;4])
%!assert (a(1:2), [1,3])
%!assert (a(:,:), [1,2;3,4])
%!assert (a(:,1), [1;3])
%!assert (a(1,1), 1)
%!assert (a(1:2,1), [1;3])
%!assert (a(:,:,1), [1,2;3,4])

%!test
%! c(:,:,1) = [1,2;3,4];
%! c(:,:,2) = [1,2;3,4];
%! assert (a(:,:,[1,1]), c);

%!test
%! c(:,:,1,1) = [1,2;3,4];
%! c(:,:,1,2) = [1,2;3,4];
%! assert (a(:,:,1,[1,1]), c);

%!test
%! c(:,:,1,1) = [1,2;3,4];
%! c(:,:,2,1) = [1,2;3,4];
%! c(:,:,1,2) = [1,2;3,4];
%! c(:,:,2,2) = [1,2;3,4];
%! assert (a(:,:,[1,1],[1,1]), c);

%!assert (a(1,[]), zeros (1,0))
%!assert (a(1,[],[1,1]), zeros (1,0,2))
%!assert (a(1,1,[]), zeros (1,1,0))

%!test
%! c (1:10,1) = 1:10;
%! assert (c, [1:10]');

%!assert (b(:), [1; 3; 2; 4; 5; 7; 6; 8])
%!assert (b(:,:), [1, 2, 5, 6; 3, 4, 7, 8])
%!assert (b(:,1), [1;3])
%!assert (b(:,:,:), reshape ([1,3,2,4,5,7,6,8], [2,2,2]))
%!assert (b(:,1,1), [1;3])
%!assert (b(:,1,1,[1,1]),reshape ([1,3,1,3], [2,1,1,2]))
%!assert (b(1,3), 5)
%!assert (b(1,[3,4]), [5,6])
%!assert (b(1,1:4), [1,2,5,6])
%!assert (b(1,[],:), zeros (1,0,2))
%!assert (b(1,[]), zeros (1,0))
%!assert (b(:,3), [5;7])
%!assert (b([1,2],3), [5;7])
%!assert (b(true (2,1), 3), [5;7])
%!assert (b(false (2,1), 3), zeros (0,1))
%!assert (b([],3), zeros (0,1))

%!shared x
%! ## Dummy shared block to clear any previous definitions
%! x = 1;

%!test
%! a(1,:) = [1,3];
%! assert (a, [1,3]);

%!test
%! a(1,:) = [1;3];
%! assert (a, [1,3]);

%!test
%! a(:,1) = [1;3];
%! assert (a, [1;3]);

%!test
%! a = [1,2;3,4];
%! b (1,:,:) = a;
%! assert (b, reshape (a, [1,2,2]));

%!test
%! a(1,1:4,2) = reshape (1:4, [1,1,4]);
%! b(:,:,2) = 1:4;
%! assert (a, b);

%!test
%! a(:,:,:) = 1:4;
%! assert (a, [1:4]);

%!test
%! a(:,:,1) = 1:4;
%! assert (a, [1:4]);

%!test
%! a(:,:,1) = [1:4]';
%! assert (a, [1:4]');

%!test
%! a(:,:,1) = reshape (1:4,[1,1,4]);
%! assert (a, [1:4]');

%!test
%! a(:,1,:) = 1:4;
%! assert (a, reshape (1:4,[1,1,4]));

%!test
%! a(:,1,:) = [1:4]';
%! assert (a, [1:4]');

%!test
%! a(:,1,:) = reshape (1:4,[1,1,4]);
%! assert (a, [1:4]');

%!test
%! a(1,:,:) = 1:4;
%! assert (a, reshape (1:4,[1,1,4]));

%!test
%! a(1,:,:) = [1:4]';
%! assert (a, [1:4]);

%!test
%! a(1,:,:) = reshape (1:4,[1,1,4]);
%! assert (a, [1:4]);

%!test
%! a(1,:,:,:) = reshape (1:4,[1,1,4]);
%! assert (a, reshape (1:4,[1,1,1,4]));

%!error (a(1:2,1:2) = 1:4)

## bug #38357
%!shared d, dd
%! d = diag ([1, 2, 3]);
%! dd = diag ([1, 2, 3], 6, 3);
%!assert (d(1), 1)
%!assert (dd(1), 1)
%!assert (d(3, 3), 3)
%!assert (dd(3, 3), 3)
%!assert (d(2), 0)
%!assert (dd(2), 0)
%!assert (dd(6,1), 0)
%!error d(6,6)
%!error dd(6,6)
%!error d(3,6)
%!error dd(3,6)

## bug 31287
%!test
%! y = ones (2, 2, 2);
%! x = ones (2, 2, 2);
%! x(false) = [];
%! assert (x, y);

%!test
%! y = ones (2, 2, 2);
%! x = ones (2, 2, 2);
%! x(false,[]) = [];
%! assert (x, y);

%!test
%! y = ones (2, 2, 2);
%! x = ones (2, 2, 2);
%! x(false,[],false) = [];
%! assert (x, y);

%!shared x, y
%! y = ones (2, 2, 2);
%! x = ones (2, 2, 2);
%! x(false, 1) = [];
%! assert (x, y);

%!shared x, y
%! y = ones (2, 2, 2);
%! x = ones (2, 2, 2);
%! x(false, false) = [];
%! assert (x, y);

%!test
%! y = ones (2, 2);
%! x = ones (2, 2);
%! x([], []) = [];
%! assert (x, y);

%!test
%! y = sparse (ones (2, 2));
%! x = sparse (ones (2, 2));
%! x([], []) = [];
%! assert (x, y);

%!test
%! y = ones (2, 2);
%! x = ones (2, 2);
%! x(1, []) = [];
%! assert (x, y);

%!test
%! y = ones (2, 2);
%! x = ones (2, 2);
%! x([], 1, []) = [];
%! assert (x, y);

%!test
%! y = ones (2, 2);
%! x = ones (2, 2);
%! x(1, [], 1, 1) = [];
%! assert (x, y);

%!test
%! y = ones (2, 2);
%! x = ones (2, 2);
%! x([], 1, 1) = [];
%! assert (x, y);

%!test
%! y = ones (2, 2);
%! x = ones (2, 2);
%! ea2 = ones (3, 2, 0, 2);
%! x(1, ea2) = [];
%! assert (x, y);

%!test
%! y = sparse (ones (2, 2));
%! x = sparse (ones (2, 2));
%! ea2 = ones (3, 2, 0, 2);
%! x(1, ea2) = [];
%! assert (x, y);

%!test
%! y = ones (2, 2);
%! x = ones (2, 2);
%! ea2 = ones (3, 2, 0, 2);
%! x([], 1, ea2) = [];
%! assert (x, y);

%!test
%! y = ones (2, 2);
%! x = ones (2, 2);
%! ea2 = ones (3, 2, 0, 2);
%! x(1, ea2, ea2) = [];
%! assert (x, y);

%!test
%! y = ones (2, 2);
%! x = ones (2, 2);
%! ea2 = ones (3, 2, 0, 2);
%! x(1, ea2, 1) = [];
%! assert (x, y);

%!test
%! y = ones (2, 2);
%! x = ones (2, 2);
%! x(false, 1) = [];
%! assert (x, y);

%!test
%! y = sparse (ones (2, 2));
%! x = sparse (ones (2, 2));
%! x(false, 1) = [];
%! assert (x, y);

%!test
%! y = ones (2, 2);
%! x = ones (2, 2);
%! x(1, [], false) = [];
%! assert (x, y);

%!test
%! y = ones (2, 2);
%! x = ones (2, 2);
%! x(false, false) = [];
%! assert (x, y);

%!test
%! y = sparse (ones (2, 2));
%! x = sparse (ones (2, 2));
%! x(false, false) = [];
%! assert (x, y);

%!test
%! y = ones (2, 2);
%! x = ones (2, 2);
%! x(false, [], false) = [];
%! assert (x, y);

%!test
%! y = ones (2, 2);
%! x = ones (2, 2);
%! x([], false, false, false) = [];
%! assert (x, y);

%!test
%! y = ones (2, 2);
%! x = ones (2, 2);
%! x(1, [], false, false) = [];
%! assert (x, y);

%!test
%! y = ones (2, 2);
%! x = ones (2, 2);
%! x(:, false) = [];
%! assert (x, y);

%!test
%! y = sparse (ones (2, 2));
%! x = sparse (ones (2, 2));
%! x(:, false) = [];
%! assert (x, y);

%!test
%! y = ones (2, 2);
%! x = ones (2, 2);
%! x(false, :) = [];
%! assert (x, y);

%!test
%! y = sparse (ones (2, 2));
%! x = sparse (ones (2, 2));
%! x(false, :) = [];
%! assert (x, y);

%!test
%! y = ones (2, 2);
%! x = ones (2, 2);
%! x(false, :, [], 1) = [];
%! assert (x, y);

%!test
%! y = ones (2, 2);
%! x = ones (2, 2);
%! x(:, [], false) = [];
%! assert (x, y);

%!shared x, y
%! y = ones (2, 2);
%! x = ones (2, 2);
%!error x(1, 1, []) = []

%!shared x, y
%! y = ones (2, 2);
%! x = ones (2, 2);
%! x(false, false, 1) = [];
%! assert (x, y);

%!shared x, y
%! y = ones (2, 2);
%! x = ones (2, 2);
%! x(false, false, []) = [];
%! assert (x, y);

%!shared x, y
%! y = ones (2, 2);
%! x = ones (2, 2);
%! x(false, false, [], false) = [];
%! assert (x, y);

%!shared x, y
%! y = ones (2, 2);
%! x = ones (2, 2);
%! x(1, false, [], false) = [];
%! assert (x, y);

%!shared x, y
%! y = ones (2, 2);
%! x = ones (2, 2);
%! x(:, false, 1) = [];
%! assert (x, y);

%!test
%! y = ones (2, 2);
%! x = ones (2, 2);
%! x([]) = [];
%! assert (x, y);

%!test
%! y = sparse (ones (2, 2));
%! x = sparse (ones (2, 2));
%! x([]) = [];
%! assert (x, y);

%!test
%! y = [];
%! x = ones (2, 2);
%! x(:) = [];
%! assert (x, y);

%!test
%! y = sparse ([]);
%! x = sparse (ones (2, 2));
%! x(:) = [];
%! assert (x, y);

%!test
%! y = ones (2, 2);
%! x = ones (2, 2);
%! x(false) = [];
%! assert (x, y);

%!test
%! y = sparse (ones (2, 2));
%! x = sparse (ones (2, 2));
%! x(false) = [];
%! assert (x, y);

%!test
%! y = ones (2, 2);
%! x = ones (2, 2);
%! x([], false) = [];
%! assert (x, y);

%!test
%! y = sparse (ones (2, 2));
%! x = sparse (ones (2, 2));
%! x([], false) = [];
%! assert (x, y);

%!test
%! y = ones (2, 2);
%! x = ones (2, 2);
%! x([], false, :) = [];
%! assert (x, y);

##  Test indexing of unnamed constants
%!error <index \(0\): subscripts must be>     1(0)
%!error <index \(-1\): subscripts must be>    1(-1)
%!error <index \(_,0.5\): subscripts>                 {}(1,0.5)
%!error <index \([Nn][aA][Nn],_\): subscripts>                 1(NaN,1)
%!error <index \(_,_,<cell....\[x8\]...\): subscripts> [](1,1,{},1,1,1,1,1,1,1,1)
%!error <index \(...\[x9\]...-1,_\): subscript>      1(1,1,1,1,1,1,1,1,1,-1,1)
%!error <index \(2\): out of bound 1>                1(2)
%!error <index \(1\): out of bound 0>                [](1)
%!error <index \(-1\): subscripts>                   1(1)(-1)(1)
%!error <index \(_,1\): out of bound 0 \(dimensions are 5x0\)> zeros (5,0)(3,1)
%!error <index \(3,_\): out of bound 0 \(dimensions are 0x5\)> zeros (0,5)(3,1)
%!
%!shared abc
%! abc = [1, 2];
%! ##  Test full matrices in variables
%!error <abc\(3\): out of bound 2>      abc([false, true, true])
%!error <abc\(-1\): subscripts>         abc(-1)(1)(1)
%! ## xerror <index \(-1\): subscripts> abc(1)(-1)(1)   ## why no 'xerror' test?

%!shared abc
%! abc = [1 2; 3 4];
%!error <abc\(5\): out of bound 4>         abc(5)
%!error <abc\(_,3\): out of bound 2 \(dimensions are 2x2\)> abc(2,3)
%!error <abc\(_,_,0.5\): subscripts>       exp (abc(2,3,0.5))

%!shared abc
%! abc = [1 2; 3 4]; abc(1,1,2) = 1;
%!error <abc\(_,5\): out of bound 4>                            abc(2,5)
%!error <abc\(_,3,_\): out of bound 2 \(dimensions are 2x2x2\)> abc(2,3,2)
%!error <A\(..,I,..\) = \[\]: .* value 3 out of bound 2>        abc(3,:) = []
%!error <A\(I\) = \[\]: .* value 50 out of bound 8>             abc(3:50) = []
%!error <a null assignment can only have one non-colon index>   abc(3,5) = []
%!error <=: nonconformant arguments \(op1 is 1x1, op2 is 1x5\)> abc(3,5) = 1:5

##  Test diagonal matrices, and access of function results
%!error <index \(_,_,5\): out of bound 1 \(dimensions are 3x3\)> eye (3)(2,3,5)
%!error <index \(-2,_\): subscripts>               eye (4)(-2,3)

##  Test cells
%!shared abc
%! abc = {1, 2; 3, 4};
%!error <abc\(_,0.3,_\): subscripts>  abc(2,0.3,5)
%!error <abc\(_,0.3,_\): subscripts>  abc{2,0.3,5}
%!error <abc\(-2,_,_,_\): subscripts> abc{-2,1,1,1}
%!error <abc\(0,_,_,_\): subscripts>  abc(0,1,1,1) = 1

##  Test permutation matrices
%!shared abc
%! abc = eye(3)([3 1 2],:);
%!error <abc\([Nn][aA][Nn]\): subscripts>         abc(NA)
%!error <abc\(_,_,_,[Ii][nN][Ff],_\): subscripts> abc(1,1,1,Inf,1)

##  Test sparse matrices
%!shared abc
%! abc = sparse (3,3);
%!error <abc\(-1\): subscripts>                abc(-1)
%!error <abc\(-1\): subscripts>                abc(-1) = 1
%!error <abc\(-1,_\): subscripts>              abc(-1,1)
%!error <abc\(-1,_\): subscripts>              abc(-1,1) = 1
%!error <sparse indexing needs 1 or 2 indices> abc(0,0,0,0)
%!error <abc\(4,_\): out of bound 3 \(dimensions are 3x3\)> abc(4,1)

##  Test ranges
%!shared abc
%! abc = 1:10;
%!error <abc\(-1\): subscripts>             abc(-1)
%!error <abc\(-1,_\): subscripts>           abc(-1,1)
%!error <abc\(4,_\): out of bound 1 \(dimensions are 1x10\)> abc(4,1)

##  Test complex
%!shared abc, z
%! abc = [1 2];
%!error <abc\(0\+1i\): subscripts must be real>     abc(i)
%! abc = [1 2; 3 4];
%!error <abc\(1\+0i\): subscripts must be real>     abc(complex (1))
%!error <abc\(1\+0.5i,_\): subscripts must be real> abc(1+0.5*i,3)
%!error <abc\(_,0-2i\): subscripts must be real>    abc(2,0-2*i)

%!test <*35841>
%! a(1,1,1).b(1) = 2;
%! a(1,1,1).b(1) = 3;

%!test <*39789>
%! c = cell (1,1,1);
%! c{1,1,1} = zeros(5, 2);
%! c{1,1,1}(:, 1) = 1;
