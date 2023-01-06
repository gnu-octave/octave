////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2016-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if ! defined (octave_lo_blas_proto_h)
#define octave_lo_blas_proto_h 1

#include "octave-config.h"

#include "f77-fcn.h"

extern "C"
{
  // DOT (liboctave/external/blas-xtra)

  F77_RET_T
  F77_FUNC (xddot, XDDOT) (const F77_INT&, const F77_DBLE *,
                           const F77_INT&, const F77_DBLE *,
                           const F77_INT&, F77_DBLE&);

  F77_RET_T
  F77_FUNC (xsdot, XSDOT) (const F77_INT&, const F77_REAL *,
                           const F77_INT&, const F77_REAL *,
                           const F77_INT&, F77_REAL&);

  // DOT3

  F77_RET_T
  F77_FUNC (ddot3, DDOT3) (const F77_INT&, const F77_INT&,
                           const F77_INT&, const F77_DBLE *,
                           const F77_DBLE *, F77_DBLE *);

  F77_RET_T
  F77_FUNC (sdot3, SDOT3) (const F77_INT&, const F77_INT&,
                           const F77_INT&, const F77_REAL *,
                           const F77_REAL *, F77_REAL *);

  // DOTC (liboctave/external/blas-xtra)

  F77_RET_T
  F77_FUNC (xcdotc, XCDOTC) (const F77_INT&, const F77_CMPLX *,
                             const F77_INT&, const F77_CMPLX *,
                             const F77_INT&, F77_CMPLX *);

  F77_RET_T
  F77_FUNC (xzdotc, XZDOTC) (const F77_INT&, const F77_DBLE_CMPLX *,
                             const F77_INT&, const F77_DBLE_CMPLX *,
                             const F77_INT&, F77_DBLE_CMPLX *);

  // DOTC3

  F77_RET_T
  F77_FUNC (cdotc3, CDOTC3) (const F77_INT&, const F77_INT&,
                             const F77_INT&, const F77_CMPLX *,
                             const F77_CMPLX *, F77_CMPLX *);

  F77_RET_T
  F77_FUNC (zdotc3, ZDOTC3) (const F77_INT&, const F77_INT&,
                             const F77_INT&, const F77_DBLE_CMPLX *,
                             const F77_DBLE_CMPLX *, F77_DBLE_CMPLX *);

  // DOTU (liboctave/external/blas-xtra)

  F77_RET_T
  F77_FUNC (xcdotu, XCDOTU) (const F77_INT&, const F77_CMPLX *,
                             const F77_INT&, const F77_CMPLX *,
                             const F77_INT&, F77_CMPLX *);

  F77_RET_T
  F77_FUNC (xzdotu, XZDOTU) (const F77_INT&, const F77_DBLE_CMPLX *,
                             const F77_INT&, const F77_DBLE_CMPLX *,
                             const F77_INT&, F77_DBLE_CMPLX *);

  // GEMM

  F77_RET_T
  F77_FUNC (cgemm, CGEMM) (F77_CONST_CHAR_ARG_DECL,
                           F77_CONST_CHAR_ARG_DECL,
                           const F77_INT&, const F77_INT&,
                           const F77_INT&, const F77_CMPLX&,
                           const F77_CMPLX *, const F77_INT&,
                           const F77_CMPLX *, const F77_INT&,
                           const F77_CMPLX&, F77_CMPLX *,
                           const F77_INT&
                           F77_CHAR_ARG_LEN_DECL
                           F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dgemm, DGEMM) (F77_CONST_CHAR_ARG_DECL,
                           F77_CONST_CHAR_ARG_DECL,
                           const F77_INT&, const F77_INT&,
                           const F77_INT&, const F77_DBLE&,
                           const F77_DBLE *, const F77_INT&,
                           const F77_DBLE *, const F77_INT&,
                           const F77_DBLE&, F77_DBLE *,
                           const F77_INT&
                           F77_CHAR_ARG_LEN_DECL
                           F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (sgemm, SGEMM) (F77_CONST_CHAR_ARG_DECL,
                           F77_CONST_CHAR_ARG_DECL,
                           const F77_INT&, const F77_INT&,
                           const F77_INT&, const F77_REAL&,
                           const F77_REAL *, const F77_INT&,
                           const F77_REAL *, const F77_INT&,
                           const F77_REAL&, F77_REAL *,
                           const F77_INT&
                           F77_CHAR_ARG_LEN_DECL
                           F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (zgemm, ZGEMM) (F77_CONST_CHAR_ARG_DECL,
                           F77_CONST_CHAR_ARG_DECL,
                           const F77_INT&, const F77_INT&,
                           const F77_INT&, const F77_DBLE_CMPLX&,
                           const F77_DBLE_CMPLX *, const F77_INT&,
                           const F77_DBLE_CMPLX *, const F77_INT&,
                           const F77_DBLE_CMPLX&, F77_DBLE_CMPLX *,
                           const F77_INT&
                           F77_CHAR_ARG_LEN_DECL
                           F77_CHAR_ARG_LEN_DECL);

  // GEMV

  F77_RET_T
  F77_FUNC (cgemv, CGEMV) (F77_CONST_CHAR_ARG_DECL,
                           const F77_INT&, const F77_INT&,
                           const F77_CMPLX&, const F77_CMPLX *,
                           const F77_INT&, const F77_CMPLX *,
                           const F77_INT&, const F77_CMPLX&,
                           F77_CMPLX *, const F77_INT&
                           F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dgemv, DGEMV) (F77_CONST_CHAR_ARG_DECL,
                           const F77_INT&, const F77_INT&,
                           const F77_DBLE&, const F77_DBLE *,
                           const F77_INT&, const F77_DBLE *,
                           const F77_INT&, const F77_DBLE&,
                           F77_DBLE *, const F77_INT&
                           F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (sgemv, SGEMV) (F77_CONST_CHAR_ARG_DECL,
                           const F77_INT&, const F77_INT&,
                           const F77_REAL&, const F77_REAL *,
                           const F77_INT&, const F77_REAL *,
                           const F77_INT&, const F77_REAL&,
                           F77_REAL *, const F77_INT&
                           F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (zgemv, ZGEMV) (F77_CONST_CHAR_ARG_DECL,
                           const F77_INT&, const F77_INT&,
                           const F77_DBLE_CMPLX&, const F77_DBLE_CMPLX *,
                           const F77_INT&, const F77_DBLE_CMPLX *,
                           const F77_INT&, const F77_DBLE_CMPLX&,
                           F77_DBLE_CMPLX *, const F77_INT&
                           F77_CHAR_ARG_LEN_DECL);

  // MATM3

  F77_RET_T
  F77_FUNC (cmatm3, CMATM3) (const F77_INT&, const F77_INT&,
                             const F77_INT&, const F77_INT&,
                             const F77_CMPLX *, const F77_CMPLX *,
                             F77_CMPLX *);
  F77_RET_T
  F77_FUNC (dmatm3, DMATM3) (const F77_INT&, const F77_INT&,
                             const F77_INT&, const F77_INT&,
                             const F77_DBLE *, const F77_DBLE *, F77_DBLE *);

  F77_RET_T
  F77_FUNC (smatm3, SMATM3) (const F77_INT&, const F77_INT&,
                             const F77_INT&, const F77_INT&,
                             const F77_REAL *, const F77_REAL *, F77_REAL *);

  F77_RET_T
  F77_FUNC (zmatm3, ZMATM3) (const F77_INT&, const F77_INT&,
                             const F77_INT&, const F77_INT&,
                             const F77_DBLE_CMPLX *, const F77_DBLE_CMPLX *,
                             F77_DBLE_CMPLX *);

  // XERBLA

  OCTAVE_API
  F77_RET_T
  F77_FUNC (xerbla, XERBLA) (F77_CONST_CHAR_ARG_DECL, const F77_INT&
                             F77_CHAR_ARG_LEN_DECL);
}

#endif
