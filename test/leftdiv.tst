########################################################################
##
## Copyright (C) 2017-2024 The Octave Project Developers
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

## There are many corner cases for left division operator
%!assert (0 \ 1, Inf)
%!assert (0 \ single (1), single (Inf))
## FIXME: Should return Inf, but not coded correctly yet.
%#!assert (0 \ i, Inf)
%#!assert (0 \ single (i), single (Inf))

################################################################################
## Series of tests for memory leaks in function
## sparse_qr<SPARSE_T>min2norm_solve<RHS_T, RET_T> (A, b)
## in sparse-qr.cc.
################################################################################
%!testif HAVE_SPQR, HAVE_CHOLMOD
%! # <RHS=real full>, <RET=Matrix>, (real sparse, real full)
%! m = 11; n = 10; mn = max (m ,n);
%! A = spdiags ([ones(mn,1), 10*ones(mn,1), -ones(mn,1)],[-1,0,1], m, n);
%! x0 = mldivide (A, ones (m,1));

%!testif HAVE_SPQR, HAVE_CHOLMOD
%! # <RHS=real sparse>, <RET=SparseMatrix>, (real sparse, real sparse)
%! m = 11; n = 10; mn = max (m ,n);
%! A = spdiags ([ones(mn,1), 10*ones(mn,1), -ones(mn,1)],[-1,0,1], m, n);
%! b = sparse (ones (m,1));
%! x0 = mldivide (A, b);

%!testif HAVE_SPQR, HAVE_CHOLMOD
%! # <RHS=complex full>, <RET=ComplexMatrix>, (real sparse, complex full)
%! m = 11; n = 10; mn = max (m ,n);
%! A = spdiags ([ones(mn,1), 10*ones(mn,1), -ones(mn,1)],[-1,0,1], m, n);
%! b = ones (m,1) + i;
%! x0 = mldivide (A, b);

%!testif HAVE_SPQR, HAVE_CHOLMOD
%! # <RHS=complex sparse>, <RET=SparseComplexMatrix>, (real sparse, complex sparse)
%! m = 11; n = 10; mn = max (m ,n);
%! A = spdiags ([ones(mn,1), 10*ones(mn,1), -ones(mn,1)],[-1,0,1], m, n);
%! b = sparse (ones (m, 1) + i);
%! x0 = A \ b;

%!testif HAVE_SPQR, HAVE_CHOLMOD
%! # <RHS=complex sparse>, <RET=SparseComplexMatrix>, (real sparse, complex sparse)
%! m = 11; n = 10; mn = max (m ,n);
%! A = spdiags ([ones(mn,1), 10*ones(mn,1), -ones(mn,1)],[-1,0,1], m, n);
%! b = sparse (ones (m, 1) + i);
%! x0 = A \ b;

%!testif HAVE_SPQR, HAVE_CHOLMOD
%! # <RHS=complex full>, <RET=ComplexMatrix>, (complex sparse, complex full)
%! m = 11; n = 10; mn = max (m ,n);
%! A = spdiags ([ones(mn,1), 10*ones(mn,1) + i, -ones(mn,1)],[-1,0,1], m, n);
%! b = ones (m, 1) + i;
%! x0 = A \ b;

%!testif HAVE_SPQR, HAVE_CHOLMOD
%! # <RHS=real full>, <RET=ComplexMatrix>, (complex sparse, real full)
%! m = 11; n = 10; mn = max (m ,n);
%! A = spdiags ([ones(mn,1), 10*ones(mn,1) + i, -ones(mn,1)],[-1,0,1], m, n);
%! b = ones (m, 1);
%! x0 = A \ b;

%!testif HAVE_SPQR, HAVE_CHOLMOD
%! # <RHS=complex sparse>, <RET=SparseComplexMatrix>, (complex sparse, complex sparse)
%! m = 11; n = 10; mn = max (m ,n);
%! A = spdiags ([ones(mn,1), 10*ones(mn,1) + i, -ones(mn,1)],[-1,0,1], m, n);
%! b = sparse (ones (m, 1) + i);
%! x0 = A \ b;

%!testif HAVE_SPQR, HAVE_CHOLMOD
%! # <RHS=real sparse>, <RET=SparseComplexMatrix>, (complex sparse, real sparse)
%! m = 11; n = 10; mn = max (m ,n);
%! A = spdiags ([ones(mn,1), 10*ones(mn,1) + i, -ones(mn,1)],[-1,0,1], m, n);
%! b = sparse (ones (m, 1));

%!warning <matrix singular to machine precision>
%! warning ('on', 'Octave:singular-matrix', 'local');
%! assert ([Inf, 0; 0, 0] \ [1; 1], zeros (2,1));
%!warning <matrix singular to machine precision>
%! warning ('on', 'Octave:singular-matrix', 'local');
%! assert ([Inf, 0; 0, 0] \ single ([1; 1]), zeros (2,1, "single"));
%!warning <matrix singular to machine precision>
%! warning ('on', 'Octave:singular-matrix', 'local');
%! assert ([Inf, 0; 0, 0] \ [i; 1], zeros (2,1));
%!warning <matrix singular to machine precision>
%! warning ('on', 'Octave:singular-matrix', 'local');
%! assert ([Inf, 0; 0, 0] \ single ([i; 1]), zeros (2,1, "single"));
