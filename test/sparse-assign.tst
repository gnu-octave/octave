########################################################################
##
## Copyright (C) 2021-2023 The Octave Project Developers
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

## FIXME: will also need tests for if assignment of integer values
## once that is implemented.  The pattern can be the same.

%!shared lr, lc, rr, rc, si, sj, mi, mj, sm_lhs, csm_lhs, s, m, sm, cs, cm, csm
%! lr = 6;
%! lc = 8;
%! rr = 2;
%! rc = 3;
%! si = 2;
%! sj = 3;
%! mi = (1:rr)+2;
%! mj = (1:rc)+3;
%! sm_lhs = sprand (lr, lc, 0.1);
%! csm_lhs = sm_lhs + 1i * sprand (lr, lc, 0.1);
%! s = rand ();
%! m = rand (rr, rc);
%! sm = sprand (rr, rc, 0.1);
%! cs = s + 1i * rand ();
%! cm = m + 1i * rand (rr, rc);
%! csm = cm + 1i * sprand (rr, rc, 0.1);

%% Indexed assignment of double scalar, sparse with a single element,
%% complex scalar, and sparse complex with a single element to sparse matrix.

%!test
%! lhs = sm_lhs;
%! rhs = s;
%! flhs = full (lhs);
%! frhs = full (rhs);
%! lhs(si,sj) = rhs;
%! flhs(si,sj) = frhs;
%! assert (issparse (lhs));
%! assert (full (lhs), flhs);
%! assert (iscomplex (lhs), iscomplex (lhs) || iscomplex (rhs));

%!test
%! lhs = sm_lhs;
%! rhs = sparse (s);
%! flhs = full (lhs);
%! frhs = full (rhs);
%! lhs(si,sj) = rhs;
%! flhs(si,sj) = frhs;
%! assert (issparse (lhs));
%! assert (full (lhs), flhs);
%! assert (iscomplex (lhs), iscomplex (lhs) || iscomplex (rhs));

%!test
%! lhs = sm_lhs;
%! rhs = cs;
%! flhs = full (lhs);
%! frhs = full (rhs);
%! lhs(si,sj) = rhs;
%! flhs(si,sj) = frhs;
%! assert (issparse (lhs));
%! assert (full (lhs), flhs);
%! assert (iscomplex (lhs), iscomplex (lhs) || iscomplex (rhs));

%!test
%! lhs = sm_lhs;
%! rhs = sparse (cs);
%! flhs = full (lhs);
%! frhs = full (rhs);
%! lhs(si,sj) = rhs;
%! flhs(si,sj) = frhs;
%! assert (issparse (lhs));
%! assert (full (lhs), flhs);
%! assert (iscomplex (lhs), iscomplex (lhs) || iscomplex (rhs));

%% Indexed assignment of double scalar, sparse with a single element,
%% complex scalar, and sparse complex with a single element to complex
%% sparse matrix.

%!test
%! lhs = csm_lhs;
%! rhs = s;
%! flhs = full (lhs);
%! frhs = full (rhs);
%! lhs(si,sj) = rhs;
%! flhs(si,sj) = frhs;
%! assert (issparse (lhs));
%! assert (full (lhs), flhs);
%! assert (iscomplex (lhs), iscomplex (lhs) || iscomplex (rhs));

%!test
%! lhs = csm_lhs;
%! rhs = sparse (s);
%! flhs = full (lhs);
%! frhs = full (rhs);
%! lhs(si,sj) = rhs;
%! flhs(si,sj) = frhs;
%! assert (issparse (lhs));
%! assert (full (lhs), flhs);
%! assert (iscomplex (lhs), iscomplex (lhs) || iscomplex (rhs));

%!test
%! lhs = csm_lhs;
%! rhs = cs;
%! flhs = full (lhs);
%! frhs = full (rhs);
%! lhs(si,sj) = rhs;
%! flhs(si,sj) = frhs;
%! assert (issparse (lhs));
%! assert (full (lhs), flhs);
%! assert (iscomplex (lhs), iscomplex (lhs) || iscomplex (rhs));

%!test
%! lhs = csm_lhs;
%! rhs = sparse (cs);
%! flhs = full (lhs);
%! frhs = full (rhs);
%! lhs(si,sj) = rhs;
%! flhs(si,sj) = frhs;
%! assert (issparse (lhs));
%! assert (full (lhs), flhs);
%! assert (iscomplex (lhs), iscomplex (lhs) || iscomplex (rhs));

%% Indexed assignment of matrix, sparse matrix, complex matrix,
%% and complex sparse matrix to sparse matrix.

%!test
%! lhs = sm_lhs;
%! rhs = m;
%! flhs = full (lhs);
%! frhs = full (rhs);
%! lhs(mi,mj) = rhs;
%! flhs(mi,mj) = frhs;
%! assert (issparse (lhs));
%! assert (full (lhs), flhs);
%! assert (iscomplex (lhs), iscomplex (lhs) || iscomplex (rhs));

%!test
%! lhs = sm_lhs;
%! rhs = sm;
%! flhs = full (lhs);
%! frhs = full (rhs);
%! lhs(mi,mj) = rhs;
%! flhs(mi,mj) = frhs;
%! assert (issparse (lhs));
%! assert (full (lhs), flhs);
%! assert (iscomplex (lhs), iscomplex (lhs) || iscomplex (rhs));

%!test
%! lhs = sm_lhs;
%! rhs = cm;
%! flhs = full (lhs);
%! frhs = full (rhs);
%! lhs(mi,mj) = rhs;
%! flhs(mi,mj) = frhs;
%! assert (issparse (lhs));
%! assert (full (lhs), flhs);
%! assert (iscomplex (lhs), iscomplex (lhs) || iscomplex (rhs));

%!test
%! lhs = sm_lhs;
%! rhs = csm;
%! flhs = full (lhs);
%! frhs = full (rhs);
%! lhs(mi,mj) = rhs;
%! flhs(mi,mj) = frhs;
%! assert (issparse (lhs));
%! assert (full (lhs), flhs);
%! assert (iscomplex (lhs), iscomplex (lhs) || iscomplex (rhs));

%% Indexed assignment of matrix, sparse matrix, complex matrix,
%% and complex sparse matrix to complex sparse matrix.

%!test
%! lhs = csm_lhs;
%! rhs = m;
%! flhs = full (lhs);
%! frhs = full (rhs);
%! lhs(mi,mj) = rhs;
%! flhs(mi,mj) = frhs;
%! assert (issparse (lhs));
%! assert (full (lhs), flhs);
%! assert (iscomplex (lhs), iscomplex (lhs) || iscomplex (rhs));

%!test
%! lhs = csm_lhs;
%! rhs = sm;
%! flhs = full (lhs);
%! frhs = full (rhs);
%! lhs(mi,mj) = rhs;
%! flhs(mi,mj) = frhs;
%! assert (issparse (lhs));
%! assert (full (lhs), flhs);
%! assert (iscomplex (lhs), iscomplex (lhs) || iscomplex (rhs));

%!test
%! lhs = csm_lhs;
%! rhs = cm;
%! flhs = full (lhs);
%! frhs = full (rhs);
%! lhs(mi,mj) = rhs;
%! flhs(mi,mj) = frhs;
%! assert (issparse (lhs));
%! assert (full (lhs), flhs);
%! assert (iscomplex (lhs), iscomplex (lhs) || iscomplex (rhs));

%!test
%! lhs = csm_lhs;
%! rhs = csm;
%! flhs = full (lhs);
%! frhs = full (rhs);
%! lhs(mi,mj) = rhs;
%! flhs(mi,mj) = frhs;
%! assert (issparse (lhs));
%! assert (full (lhs), flhs);
%! assert (iscomplex (lhs), iscomplex (lhs) || iscomplex (rhs));

%% Indexed assignment of float scalar, float complex scalar, float matrix,
%% and float complex matrix to sparse matrix.

%!test
%! lhs = sm_lhs;
%! rhs = single (s);
%! flhs = full (lhs);
%! frhs = full (rhs);
%! lhs(si,sj) = rhs;
%! flhs(si,sj) = frhs;
%! assert (issparse (lhs));
%! assert (full (lhs), flhs);
%! assert (iscomplex (lhs), iscomplex (lhs) || iscomplex (rhs));

%!test
%! lhs = sm_lhs;
%! rhs = single (cs);
%! flhs = full (lhs);
%! frhs = full (rhs);
%! lhs(si,sj) = rhs;
%! flhs(si,sj) = frhs;
%! assert (issparse (lhs));
%! assert (full (lhs), flhs);
%! assert (iscomplex (lhs), iscomplex (lhs) || iscomplex (rhs));

%!test
%! lhs = sm_lhs;
%! rhs = single (m);
%! flhs = full (lhs);
%! frhs = full (rhs);
%! lhs(mi,mj) = rhs;
%! flhs(mi,mj) = frhs;
%! assert (issparse (lhs));
%! assert (full (lhs), flhs);
%! assert (iscomplex (lhs), iscomplex (lhs) || iscomplex (rhs));

%!test
%! lhs = sm_lhs;
%! rhs = single (cm);
%! flhs = full (lhs);
%! frhs = full (rhs);
%! lhs(mi,mj) = rhs;
%! flhs(mi,mj) = frhs;
%! assert (issparse (lhs));
%! assert (full (lhs), flhs);
%! assert (iscomplex (lhs), iscomplex (lhs) || iscomplex (rhs));

%% Indexed assignment of float scalar, float complex scalar, float matrix,
%% and float complex matrix to complex sparse matrix.

%!test
%! lhs = csm_lhs;
%! rhs = single (s);
%! flhs = full (lhs);
%! frhs = full (rhs);
%! lhs(si,sj) = rhs;
%! flhs(si,sj) = frhs;
%! assert (issparse (lhs));
%! assert (full (lhs), flhs);
%! assert (iscomplex (lhs), iscomplex (lhs) || iscomplex (rhs));

%!test
%! lhs = csm_lhs;
%! rhs = single (cs);
%! flhs = full (lhs);
%! frhs = full (rhs);
%! lhs(si,sj) = rhs;
%! flhs(si,sj) = frhs;
%! assert (issparse (lhs));
%! assert (full (lhs), flhs);
%! assert (iscomplex (lhs), iscomplex (lhs) || iscomplex (rhs));

%!test
%! lhs = csm_lhs;
%! rhs = single (m);
%! flhs = full (lhs);
%! frhs = full (rhs);
%! lhs(mi,mj) = rhs;
%! flhs(mi,mj) = frhs;
%! assert (issparse (lhs));
%! assert (full (lhs), flhs);
%! assert (iscomplex (lhs), iscomplex (lhs) || iscomplex (rhs));

%!test
%! lhs = csm_lhs;
%! rhs = single (cm);
%! flhs = full (lhs);
%! frhs = full (rhs);
%! lhs(mi,mj) = rhs;
%! flhs(mi,mj) = frhs;
%! assert (issparse (lhs));
%! assert (full (lhs), flhs);
%! assert (iscomplex (lhs), iscomplex (lhs) || iscomplex (rhs));
