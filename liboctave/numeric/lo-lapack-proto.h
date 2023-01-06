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

#if ! defined (octave_lo_lapack_proto_h)
#define octave_lo_lapack_proto_h 1

#include "octave-config.h"

#include "f77-fcn.h"
#include "oct-cmplx.h"

extern "C"
{
  // GBCON

  F77_RET_T
  F77_FUNC (dgbcon, DGBCON) (F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, const F77_INT&,
                             const F77_INT&, F77_DBLE *,
                             const F77_INT&, const F77_INT *,
                             const F77_DBLE&, F77_DBLE&, F77_DBLE *,
                             F77_INT *, F77_INT&
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (zgbcon, ZGBCON) (F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, const F77_INT&,
                             const F77_INT&, F77_DBLE_CMPLX *,
                             const F77_INT&, const F77_INT *,
                             const F77_DBLE&, F77_DBLE&, F77_DBLE_CMPLX *,
                             F77_DBLE *, F77_INT&
                             F77_CHAR_ARG_LEN_DECL);

  // GBTRF

  F77_RET_T
  F77_FUNC (dgbtrf, DGBTRF) (const F77_INT&, const F77_INT&,
                             const F77_INT&, const F77_INT&,
                             F77_DBLE *, const F77_INT&,
                             F77_INT *, F77_INT&);

  F77_RET_T
  F77_FUNC (zgbtrf, ZGBTRF) (const F77_INT&, const F77_INT&,
                             const F77_INT&, const F77_INT&,
                             F77_DBLE_CMPLX *, const F77_INT&,
                             F77_INT *, F77_INT&);

  // GBTRS

  F77_RET_T
  F77_FUNC (dgbtrs, DGBTRS) (F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, const F77_INT&,
                             const F77_INT&, const F77_INT&,
                             const F77_DBLE *, const F77_INT&,
                             const F77_INT *, F77_DBLE *,
                             const F77_INT&, F77_INT&
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (zgbtrs, ZGBTRS) (F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, const F77_INT&,
                             const F77_INT&, const F77_INT&,
                             const F77_DBLE_CMPLX *, const F77_INT&,
                             const F77_INT *, F77_DBLE_CMPLX *,
                             const F77_INT&, F77_INT&
                             F77_CHAR_ARG_LEN_DECL);

  // GEBAL

  F77_RET_T
  F77_FUNC (cgebal, CGEBAL) (F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, F77_CMPLX *,
                             const F77_INT&, F77_INT&,
                             F77_INT&, F77_REAL *, F77_INT&
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dgebal, DGEBAL) (F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, F77_DBLE *,
                             const F77_INT&, F77_INT&,
                             F77_INT&, F77_DBLE *, F77_INT&
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (sgebal, SGEBAL) (F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, F77_REAL *,
                             const F77_INT&, F77_INT&,
                             F77_INT&, F77_REAL *, F77_INT&
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (zgebal, ZGEBAL) (F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, F77_DBLE_CMPLX *,
                             const F77_INT&, F77_INT&,
                             F77_INT&, F77_DBLE *, F77_INT&
                             F77_CHAR_ARG_LEN_DECL);

  // GEBAK

  F77_RET_T
  F77_FUNC (cgebak, CGEBAK) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, const F77_INT&,
                             const F77_INT&, const F77_REAL *,
                             const F77_INT&, F77_CMPLX *,
                             const F77_INT&, F77_INT&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dgebak, DGEBAK) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, const F77_INT&,
                             const F77_INT&, const F77_DBLE *,
                             const F77_INT&, F77_DBLE *,
                             const F77_INT&, F77_INT&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (sgebak, SGEBAK) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, const F77_INT&,
                             const F77_INT&, const F77_REAL *,
                             const F77_INT&, F77_REAL *,
                             const F77_INT&, F77_INT&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (zgebak, ZGEBAK) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, const F77_INT&,
                             const F77_INT&, const F77_DBLE *,
                             const F77_INT&, F77_DBLE_CMPLX *,
                             const F77_INT&, F77_INT&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  // GECON

  F77_RET_T
  F77_FUNC (cgecon, CGECON) (F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, F77_CMPLX *,
                             const F77_INT&, const F77_REAL&, F77_REAL&,
                             F77_CMPLX *, F77_REAL *, F77_INT&
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dgecon, DGECON) (F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, F77_DBLE *,
                             const F77_INT&, const F77_DBLE&, F77_DBLE&,
                             F77_DBLE *, F77_INT *, F77_INT&
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (sgecon, SGECON) (F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, F77_REAL *,
                             const F77_INT&, const F77_REAL&, F77_REAL&,
                             F77_REAL *, F77_INT *, F77_INT&
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (zgecon, ZGECON) (F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, F77_DBLE_CMPLX *,
                             const F77_INT&, const F77_DBLE&, F77_DBLE&,
                             F77_DBLE_CMPLX *, F77_DBLE *, F77_INT&
                             F77_CHAR_ARG_LEN_DECL);

  // GEHRD

  F77_RET_T
  F77_FUNC (cgehrd, CGEHRD) (const F77_INT&, const F77_INT&,
                             const F77_INT&, F77_CMPLX *,
                             const F77_INT&, F77_CMPLX *, F77_CMPLX *,
                             const F77_INT&, F77_INT&);

  F77_RET_T
  F77_FUNC (dgehrd, DGEHRD) (const F77_INT&, const F77_INT&,
                             const F77_INT&, F77_DBLE *,
                             const F77_INT&, F77_DBLE *, F77_DBLE *,
                             const F77_INT&, F77_INT&);

  F77_RET_T
  F77_FUNC (sgehrd, SGEHRD) (const F77_INT&, const F77_INT&,
                             const F77_INT&, F77_REAL *,
                             const F77_INT&, F77_REAL *, F77_REAL *,
                             const F77_INT&, F77_INT&);

  F77_RET_T
  F77_FUNC (zgehrd, ZGEHRD) (const F77_INT&, const F77_INT&,
                             const F77_INT&, F77_DBLE_CMPLX *,
                             const F77_INT&, F77_DBLE_CMPLX *, F77_DBLE_CMPLX *,
                             const F77_INT&, F77_INT&);

  // GEQP3

  F77_RET_T
  F77_FUNC (cgeqp3, CGEQP3) (const F77_INT&, const F77_INT&,
                             F77_CMPLX *, const F77_INT&,
                             F77_INT *, F77_CMPLX *, F77_CMPLX *,
                             const F77_INT&, F77_REAL *, F77_INT&);

  F77_RET_T
  F77_FUNC (dgeqp3, DGEQP3) (const F77_INT&, const F77_INT&,
                             F77_DBLE *, const F77_INT&,
                             F77_INT *, F77_DBLE *, F77_DBLE *,
                             const F77_INT&, F77_INT&);

  F77_RET_T
  F77_FUNC (sgeqp3, SGEQP3) (const F77_INT&, const F77_INT&,
                             F77_REAL *, const F77_INT&,
                             F77_INT *, F77_REAL *, F77_REAL *,
                             const F77_INT&, F77_INT&);
  F77_RET_T
  F77_FUNC (zgeqp3, ZGEQP3) (const F77_INT&, const F77_INT&,
                             F77_DBLE_CMPLX *, const F77_INT&,
                             F77_INT *, F77_DBLE_CMPLX *, F77_DBLE_CMPLX *,
                             const F77_INT&, F77_DBLE *, F77_INT&);

  // GEQRF

  F77_RET_T
  F77_FUNC (cgeqrf, CGEQRF) (const F77_INT&, const F77_INT&,
                             F77_CMPLX *, const F77_INT&,
                             F77_CMPLX *, F77_CMPLX *,
                             const F77_INT&, F77_INT&);

  F77_RET_T
  F77_FUNC (dgeqrf, DGEQRF) (const F77_INT&, const F77_INT&,
                             F77_DBLE *, const F77_INT&,
                             F77_DBLE *, F77_DBLE *,
                             const F77_INT&, F77_INT&);

  F77_RET_T
  F77_FUNC (sgeqrf, SGEQRF) (const F77_INT&, const F77_INT&,
                             F77_REAL *, const F77_INT&,
                             F77_REAL *, F77_REAL *,
                             const F77_INT&, F77_INT&);

  F77_RET_T
  F77_FUNC (zgeqrf, ZGEQRF) (const F77_INT&, const F77_INT&,
                             F77_DBLE_CMPLX *, const F77_INT&,
                             F77_DBLE_CMPLX *, F77_DBLE_CMPLX *,
                             const F77_INT&, F77_INT&);

  // GELQF

  F77_RET_T
  F77_FUNC (cgelqf, CGELQF) (const F77_INT&, const F77_INT&,
                             F77_CMPLX *, const F77_INT&,
                             F77_CMPLX *, F77_CMPLX *,
                             const F77_INT&, F77_INT&);

  F77_RET_T
  F77_FUNC (dgelqf, DGELQF) (const F77_INT&, const F77_INT&,
                             F77_DBLE *, const F77_INT&,
                             F77_DBLE *, F77_DBLE *,
                             const F77_INT&, F77_INT&);

  F77_RET_T
  F77_FUNC (sgelqf, SGELQF) (const F77_INT&, const F77_INT&,
                             F77_REAL *, const F77_INT&,
                             F77_REAL *, F77_REAL *,
                             const F77_INT&, F77_INT&);

  F77_RET_T
  F77_FUNC (zgelqf, ZGELQF) (const F77_INT&, const F77_INT&,
                             F77_DBLE_CMPLX *, const F77_INT&,
                             F77_DBLE_CMPLX *, F77_DBLE_CMPLX *,
                             const F77_INT&, F77_INT&);

  // ORMLQ

  F77_RET_T
  F77_FUNC (cormlq, CORMLQ) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, const F77_INT&, const F77_INT&,
                             F77_CMPLX *, const F77_INT&,
                             F77_CMPLX *, F77_CMPLX *,
                             const F77_INT&, F77_CMPLX *,
                             const F77_INT&, F77_INT&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dormlq, DORMLQ) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, const F77_INT&, const F77_INT&,
                             F77_DBLE *, const F77_INT&,
                             F77_DBLE *, F77_DBLE *,
                             const F77_INT&, F77_DBLE *,
                             const F77_INT&, F77_INT&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (sormlq, SORMLQ) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, const F77_INT&, const F77_INT&,
                             F77_REAL *, const F77_INT&,
                             F77_REAL *, F77_REAL *,
                             const F77_INT&, F77_REAL *,
                             const F77_INT&, F77_INT&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (zormlq, ZORMLQ) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, const F77_INT&, const F77_INT&,
                             F77_DBLE_CMPLX *, const F77_INT&,
                             F77_DBLE_CMPLX *, F77_DBLE_CMPLX *,
                             const F77_INT&, F77_DBLE_CMPLX *,
                             const F77_INT&, F77_INT&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  // ORMQR

  F77_RET_T
  F77_FUNC (cormqr, CORMQR) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, const F77_INT&, const F77_INT&,
                             F77_CMPLX *, const F77_INT&,
                             F77_CMPLX *, F77_CMPLX *,
                             const F77_INT&, F77_CMPLX *,
                             const F77_INT&, F77_INT&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dormqr, DORMQR) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, const F77_INT&, const F77_INT&,
                             F77_DBLE *, const F77_INT&,
                             F77_DBLE *, F77_DBLE *,
                             const F77_INT&, F77_DBLE *,
                             const F77_INT&, F77_INT&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (sormqr, SORMQR) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, const F77_INT&, const F77_INT&,
                             F77_REAL *, const F77_INT&,
                             F77_REAL *, F77_REAL *,
                             const F77_INT&, F77_REAL *,
                             const F77_INT&, F77_INT&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (zormqr, ZORMQR) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, const F77_INT&, const F77_INT&,
                             F77_DBLE_CMPLX *, const F77_INT&,
                             F77_DBLE_CMPLX *, F77_DBLE_CMPLX *,
                             const F77_INT&, F77_DBLE_CMPLX *,
                             const F77_INT&, F77_INT&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  // GESDD

  F77_RET_T
  F77_FUNC (cgesdd, CGESDD) (F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, const F77_INT&,
                             F77_CMPLX *, const F77_INT&, F77_REAL *,
                             F77_CMPLX *, const F77_INT&,
                             F77_CMPLX *, const F77_INT&,
                             F77_CMPLX *, const F77_INT&, F77_REAL *,
                             F77_INT *, F77_INT&
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dgesdd, DGESDD) (F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, const F77_INT&,
                             F77_DBLE *, const F77_INT&, F77_DBLE *,
                             F77_DBLE *, const F77_INT&,
                             F77_DBLE *, const F77_INT&,
                             F77_DBLE *, const F77_INT&,
                             F77_INT *, F77_INT&
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (sgesdd, SGESDD) (F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, const F77_INT&,
                             F77_REAL *, const F77_INT&, F77_REAL *,
                             F77_REAL *, const F77_INT&,
                             F77_REAL *, const F77_INT&,
                             F77_REAL *, const F77_INT&,
                             F77_INT *, F77_INT&
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (zgesdd, ZGESDD) (F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, const F77_INT&,
                             F77_DBLE_CMPLX *, const F77_INT&, F77_DBLE *,
                             F77_DBLE_CMPLX *, const F77_INT&,
                             F77_DBLE_CMPLX *, const F77_INT&,
                             F77_DBLE_CMPLX *, const F77_INT&, F77_DBLE *,
                             F77_INT *, F77_INT&
                             F77_CHAR_ARG_LEN_DECL);
  // GESVD

  F77_RET_T
  F77_FUNC (cgesvd, CGESVD) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, const F77_INT&,
                             F77_CMPLX *, const F77_INT&, F77_REAL *,
                             F77_CMPLX *, const F77_INT&,
                             F77_CMPLX *, const F77_INT&,
                             F77_CMPLX *, const F77_INT&,
                             F77_REAL *, F77_INT&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dgesvd, DGESVD) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, const F77_INT&,
                             F77_DBLE *, const F77_INT&, F77_DBLE *,
                             F77_DBLE *, const F77_INT&, F77_DBLE *,
                             const F77_INT&, F77_DBLE *,
                             const F77_INT&, F77_INT&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (sgesvd, SGESVD) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, const F77_INT&,
                             F77_REAL *, const F77_INT&, F77_REAL *,
                             F77_REAL *, const F77_INT&, F77_REAL *,
                             const F77_INT&, F77_REAL *,
                             const F77_INT&, F77_INT&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (zgesvd, ZGESVD) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, const F77_INT&,
                             F77_DBLE_CMPLX *, const F77_INT&,
                             F77_DBLE *, F77_DBLE_CMPLX *, const F77_INT&,
                             F77_DBLE_CMPLX *, const F77_INT&, F77_DBLE_CMPLX *,
                             const F77_INT&, F77_DBLE *, F77_INT&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  // GEJSV

  F77_RET_T
  F77_FUNC (cgejsv, CGEJSV) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, const F77_INT&,
                             F77_CMPLX *, const F77_INT&, F77_REAL *,
                             F77_CMPLX *, const F77_INT&,
                             F77_CMPLX *, const F77_INT&,
                             F77_CMPLX *, const F77_INT&,
                             F77_REAL *,  const F77_INT&,
                             F77_INT *, F77_INT&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dgejsv, DGEJSV) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, const F77_INT&,
                             F77_DBLE *, const F77_INT&, F77_DBLE *,
                             F77_DBLE *, const F77_INT&,
                             F77_DBLE *, const F77_INT&,
                             F77_DBLE *, const F77_INT&,
                             F77_INT *, F77_INT&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (sgejsv, SGEJSV) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, const F77_INT&,
                             F77_REAL *, const F77_INT&, F77_REAL *,
                             F77_REAL *, const F77_INT&,
                             F77_REAL *, const F77_INT&,
                             F77_REAL *, const F77_INT&,
                             F77_INT *, F77_INT&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (zgejsv, ZGEJSV) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, const F77_INT&,
                             F77_DBLE_CMPLX *, const F77_INT&, F77_DBLE *,
                             F77_DBLE_CMPLX *, const F77_INT&,
                             F77_DBLE_CMPLX *, const F77_INT&,
                             F77_DBLE_CMPLX *, const F77_INT&,
                             F77_DBLE *,       const F77_INT&,
                             F77_INT *, F77_INT&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  // GEESX

  typedef F77_INT (*double_selector) (const F77_DBLE&, const F77_DBLE&);
  typedef F77_INT (*float_selector) (const F77_REAL&, const F77_REAL&);
  typedef F77_INT (*complex_selector) (const F77_DBLE_CMPLX&);
  typedef F77_INT (*float_complex_selector) (const F77_CMPLX&);

  F77_RET_T
  F77_FUNC (cgeesx, CGEESX) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             float_complex_selector,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, F77_CMPLX *,
                             const F77_INT&, F77_INT&,
                             F77_CMPLX *, F77_CMPLX *,
                             const F77_INT&, F77_REAL&, F77_REAL&,
                             F77_CMPLX *, const F77_INT&,
                             F77_REAL *, F77_INT *, F77_INT&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dgeesx, DGEESX) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             double_selector,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, F77_DBLE *,
                             const F77_INT&, F77_INT&,
                             F77_DBLE *, F77_DBLE *, F77_DBLE *, const F77_INT&,
                             F77_DBLE&, F77_DBLE&, F77_DBLE *, const F77_INT&,
                             F77_INT *, const F77_INT&,
                             F77_INT *, F77_INT&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (sgeesx, SGEESX) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             float_selector,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, F77_REAL *,
                             const F77_INT&, F77_INT&,
                             F77_REAL *, F77_REAL *, F77_REAL *, const F77_INT&,
                             F77_REAL&, F77_REAL&, F77_REAL *, const F77_INT&,
                             F77_INT *, const F77_INT&,
                             F77_INT *, F77_INT&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (zgeesx, ZGEESX) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             complex_selector,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, F77_DBLE_CMPLX *,
                             const F77_INT&, F77_INT&,
                             F77_DBLE_CMPLX *, F77_DBLE_CMPLX *, const F77_INT&,
                             F77_DBLE&, F77_DBLE&, F77_DBLE_CMPLX *,
                             const F77_INT&, F77_DBLE *,
                             F77_INT *, F77_INT&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  // GEEVX

  F77_RET_T
  F77_FUNC (cgeevx, CGEEVX) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, F77_CMPLX *,
                             const F77_INT&, F77_CMPLX *, F77_CMPLX *,
                             const F77_INT&, F77_CMPLX *,
                             const F77_INT&, F77_INT&,
                             F77_INT&, F77_REAL *, F77_REAL&, F77_REAL *,
                             F77_REAL *, F77_CMPLX *, const F77_INT&,
                             F77_REAL *, F77_INT&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dgeevx, DGEEVX) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, F77_DBLE *,
                             const F77_INT&, F77_DBLE *, F77_DBLE *,
                             F77_DBLE *, const F77_INT&, F77_DBLE *,
                             const F77_INT&, F77_INT&,
                             F77_INT&, F77_DBLE *, F77_DBLE&,
                             F77_DBLE *, F77_DBLE *, F77_DBLE *,
                             const F77_INT&, F77_INT *,
                             F77_INT&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (sgeevx, SGEEVX) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, F77_REAL *,
                             const F77_INT&, F77_REAL *, F77_REAL *, F77_REAL *,
                             const F77_INT&, F77_REAL *,
                             const F77_INT&, F77_INT&,
                             F77_INT&, F77_REAL *, F77_REAL&, F77_REAL *,
                             F77_REAL *, F77_REAL *, const F77_INT&,
                             F77_INT *, F77_INT&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (zgeevx, ZGEEVX) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, F77_DBLE_CMPLX *,
                             const F77_INT&, F77_DBLE_CMPLX *,
                             F77_DBLE_CMPLX *, const F77_INT&, F77_DBLE_CMPLX *,
                             const F77_INT&, F77_INT&,
                             F77_INT&, F77_DBLE *, F77_DBLE&,
                             F77_DBLE *, F77_DBLE *, F77_DBLE_CMPLX *,
                             const F77_INT&, F77_DBLE *,
                             F77_INT&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  // GELSD

  F77_RET_T
  F77_FUNC (cgelsd, CGELSD) (const F77_INT&, const F77_INT&,
                             const F77_INT&, F77_CMPLX *,
                             const F77_INT&, F77_CMPLX *,
                             const F77_INT&, F77_REAL *, F77_REAL&,
                             F77_INT&, F77_CMPLX *,
                             const F77_INT&, F77_REAL *,
                             F77_INT *, F77_INT&);

  F77_RET_T
  F77_FUNC (dgelsd, DGELSD) (const F77_INT&, const F77_INT&,
                             const F77_INT&, F77_DBLE *,
                             const F77_INT&, F77_DBLE *,
                             const F77_INT&, F77_DBLE *, F77_DBLE&,
                             F77_INT&, F77_DBLE *,
                             const F77_INT&, F77_INT *,
                             F77_INT&);

  F77_RET_T
  F77_FUNC (sgelsd, SGELSD) (const F77_INT&, const F77_INT&,
                             const F77_INT&, F77_REAL *,
                             const F77_INT&, F77_REAL *,
                             const F77_INT&, F77_REAL *, F77_REAL&,
                             F77_INT&, F77_REAL *,
                             const F77_INT&, F77_INT *,
                             F77_INT&);

  F77_RET_T
  F77_FUNC (zgelsd, ZGELSD) (const F77_INT&, const F77_INT&,
                             const F77_INT&, F77_DBLE_CMPLX *,
                             const F77_INT&, F77_DBLE_CMPLX *,
                             const F77_INT&, F77_DBLE *, F77_DBLE&,
                             F77_INT&, F77_DBLE_CMPLX *,
                             const F77_INT&, F77_DBLE *,
                             F77_INT *, F77_INT&);

  // GELSY

  F77_RET_T
  F77_FUNC (cgelsy, CGELSY) (const F77_INT&, const F77_INT&,
                             const F77_INT&, F77_CMPLX *,
                             const F77_INT&, F77_CMPLX *,
                             const F77_INT&, F77_INT *,
                             F77_REAL&, F77_INT&, F77_CMPLX *,
                             const F77_INT&, F77_REAL *, F77_INT&);

  F77_RET_T
  F77_FUNC (dgelsy, DGELSY) (const F77_INT&, const F77_INT&,
                             const F77_INT&, F77_DBLE *,
                             const F77_INT&, F77_DBLE *,
                             const F77_INT&, F77_INT *,
                             F77_DBLE&, F77_INT&, F77_DBLE *,
                             const F77_INT&, F77_INT&);

  F77_RET_T
  F77_FUNC (sgelsy, SGELSY) (const F77_INT&, const F77_INT&,
                             const F77_INT&, F77_REAL *,
                             const F77_INT&, F77_REAL *,
                             const F77_INT&, F77_INT *,
                             F77_REAL&, F77_INT&, F77_REAL *,
                             const F77_INT&, F77_INT&);

  F77_RET_T
  F77_FUNC (zgelsy, ZGELSY) (const F77_INT&, const F77_INT&,
                             const F77_INT&, F77_DBLE_CMPLX *,
                             const F77_INT&, F77_DBLE_CMPLX *,
                             const F77_INT&, F77_INT *,
                             F77_DBLE&, F77_INT&, F77_DBLE_CMPLX *,
                             const F77_INT&, F77_DBLE *,
                             F77_INT&);

  // GETRF

  F77_RET_T
  F77_FUNC (cgetrf, CGETRF) (const F77_INT&, const F77_INT&,
                             F77_CMPLX *, const F77_INT&,
                             F77_INT *, F77_INT&);

  F77_RET_T
  F77_FUNC (dgetrf, DGETRF) (const F77_INT&, const F77_INT&,
                             F77_DBLE *, const F77_INT&,
                             F77_INT *, F77_INT&);

  F77_RET_T
  F77_FUNC (sgetrf, SGETRF) (const F77_INT&,
                             const F77_INT&, F77_REAL *,
                             const F77_INT&,
                             F77_INT *, F77_INT&);

  F77_RET_T
  F77_FUNC (zgetrf, ZGETRF) (const F77_INT&, const F77_INT&,
                             F77_DBLE_CMPLX *, const F77_INT&,
                             F77_INT *, F77_INT&);

  // GETRI

  F77_RET_T
  F77_FUNC (cgetri, CGETRI) (const F77_INT&, F77_CMPLX *,
                             const F77_INT&, const F77_INT *,
                             F77_CMPLX *, const F77_INT&,
                             F77_INT&);

  F77_RET_T
  F77_FUNC (dgetri, DGETRI) (const F77_INT&, F77_DBLE *,
                             const F77_INT&, const F77_INT *,
                             F77_DBLE *, const F77_INT&,
                             F77_INT&);

  F77_RET_T
  F77_FUNC (sgetri, SGETRI) (const F77_INT&, F77_REAL *,
                             const F77_INT&, const F77_INT *,
                             F77_REAL *, const F77_INT&, F77_INT&);

  F77_RET_T
  F77_FUNC (zgetri, ZGETRI) (const F77_INT&, F77_DBLE_CMPLX *,
                             const F77_INT&, const F77_INT *,
                             F77_DBLE_CMPLX *, const F77_INT&,
                             F77_INT&);

  // GETRS

  F77_RET_T
  F77_FUNC (cgetrs, CGETRS) (F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, const F77_INT&,
                             F77_CMPLX *, const F77_INT&,
                             const F77_INT *, F77_CMPLX *,
                             const F77_INT&, F77_INT&
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dgetrs, DGETRS) (F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, const F77_INT&,
                             const F77_DBLE *, const F77_INT&,
                             const F77_INT *, F77_DBLE *,
                             const F77_INT&, F77_INT&
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (sgetrs, SGETRS) (F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, const F77_INT&,
                             const F77_REAL *, const F77_INT&,
                             const F77_INT *, F77_REAL *,
                             const F77_INT&, F77_INT&
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (zgetrs, ZGETRS) (F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, const F77_INT&,
                             F77_DBLE_CMPLX *, const F77_INT&,
                             const F77_INT *, F77_DBLE_CMPLX *,
                             const F77_INT&, F77_INT&
                             F77_CHAR_ARG_LEN_DECL);

  // GGBAL

  F77_RET_T
  F77_FUNC (cggbal, CGGBAL) (F77_CONST_CHAR_ARG_DECL,
                             const F77_INT& N,
                             F77_CMPLX *A, const F77_INT& LDA,
                             F77_CMPLX *B, const F77_INT& LDB,
                             F77_INT& ILO, F77_INT& IHI,
                             F77_REAL *LSCALE, F77_REAL *RSCALE,
                             F77_REAL *WORK, F77_INT& INFO
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dggbal, DGGBAL) (F77_CONST_CHAR_ARG_DECL,
                             const F77_INT& N, F77_DBLE *A,
                             const F77_INT& LDA, F77_DBLE *B,
                             const F77_INT& LDB, F77_INT& ILO,
                             F77_INT& IHI, F77_DBLE *LSCALE,
                             F77_DBLE *RSCALE, F77_DBLE *WORK,
                             F77_INT& INFO
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (sggbal, SGGBAL) (F77_CONST_CHAR_ARG_DECL,
                             const F77_INT& N, F77_REAL *A,
                             const F77_INT& LDA, F77_REAL *B,
                             const F77_INT& LDB,
                             F77_INT& ILO, F77_INT& IHI,
                             F77_REAL *LSCALE, F77_REAL *RSCALE,
                             F77_REAL *WORK, F77_INT& INFO
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (zggbal, ZGGBAL) (F77_CONST_CHAR_ARG_DECL,
                             const F77_INT& N, F77_DBLE_CMPLX *A,
                             const F77_INT& LDA, F77_DBLE_CMPLX *B,
                             const F77_INT& LDB, F77_INT& ILO,
                             F77_INT& IHI, F77_DBLE *LSCALE,
                             F77_DBLE *RSCALE, F77_DBLE *WORK,
                             F77_INT& INFO
                             F77_CHAR_ARG_LEN_DECL);

  // GGBAK

  F77_RET_T
  F77_FUNC (dggbak, DGGBAK) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT& N,
                             const F77_INT& ILO,
                             const F77_INT& IHI,
                             const F77_DBLE *LSCALE, const F77_DBLE *RSCALE,
                             F77_INT& M, F77_DBLE *V,
                             const F77_INT& LDV, F77_INT& INFO
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (sggbak, SGGBAK) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT& N,
                             const F77_INT& ILO,
                             const F77_INT& IHI,
                             const F77_REAL *LSCALE, const F77_REAL *RSCALE,
                             F77_INT& M, F77_REAL *V,
                             const F77_INT& LDV, F77_INT& INFO
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (zggbak, ZGGBAK) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT& N,
                             const F77_INT& ILO,
                             const F77_INT& IHI,
                             const F77_DBLE *LSCALE, const F77_DBLE *RSCALE,
                             F77_INT& M, F77_DBLE_CMPLX *V,
                             const F77_INT& LDV, F77_INT& INFO
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  // GGEV

  F77_RET_T
  F77_FUNC (cggev, CGGEV) (F77_CONST_CHAR_ARG_DECL,
                           F77_CONST_CHAR_ARG_DECL,
                           const F77_INT&, F77_CMPLX *,
                           const F77_INT&, F77_CMPLX *,
                           const F77_INT&, F77_CMPLX *,
                           F77_CMPLX *, F77_CMPLX *,
                           const F77_INT&, F77_CMPLX *,
                           const F77_INT&, F77_CMPLX *,
                           const F77_INT&, F77_REAL *, F77_INT&
                           F77_CHAR_ARG_LEN_DECL
                           F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dggev, DGGEV) (F77_CONST_CHAR_ARG_DECL,
                           F77_CONST_CHAR_ARG_DECL,
                           const F77_INT&,
                           F77_DBLE *, const F77_INT&,
                           F77_DBLE *, const F77_INT&,
                           F77_DBLE *, F77_DBLE *, F77_DBLE *, F77_DBLE *,
                           const F77_INT&, F77_DBLE *,
                           const F77_INT&, F77_DBLE *,
                           const F77_INT&, F77_INT&
                           F77_CHAR_ARG_LEN_DECL
                           F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (sggev, SGGEV) (F77_CONST_CHAR_ARG_DECL,
                           F77_CONST_CHAR_ARG_DECL,
                           const F77_INT&, F77_REAL *,
                           const F77_INT&, F77_REAL *,
                           const F77_INT&, F77_REAL *, F77_REAL *, F77_REAL *,
                           F77_REAL *, const F77_INT&, F77_REAL *,
                           const F77_INT&, F77_REAL *,
                           const F77_INT&, F77_INT&
                           F77_CHAR_ARG_LEN_DECL
                           F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (zggev, ZGGEV) (F77_CONST_CHAR_ARG_DECL,
                           F77_CONST_CHAR_ARG_DECL,
                           const F77_INT&,
                           F77_DBLE_CMPLX *, const F77_INT&,
                           F77_DBLE_CMPLX *, const F77_INT&,
                           F77_DBLE_CMPLX *, F77_DBLE_CMPLX *, F77_DBLE_CMPLX *,
                           const F77_INT&, F77_DBLE_CMPLX *,
                           const F77_INT&, F77_DBLE_CMPLX *,
                           const F77_INT&, F77_DBLE *, F77_INT&
                           F77_CHAR_ARG_LEN_DECL
                           F77_CHAR_ARG_LEN_DECL);

  // GGHRD

  F77_RET_T
  F77_FUNC (dgghrd, DGGHRD) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT& N,
                             const F77_INT& ILO,
                             const F77_INT& IHI, F77_DBLE *A,
                             const F77_INT& LDA, F77_DBLE *B,
                             const F77_INT& LDB, F77_DBLE *Q,
                             const F77_INT& LDQ, F77_DBLE *Z,
                             const F77_INT& LDZ, F77_INT& INFO
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (zgghrd, ZGGHRD) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT& N,
                             const F77_INT& ILO,
                             const F77_INT& IHI, F77_DBLE_CMPLX *A,
                             const F77_INT& LDA, F77_DBLE_CMPLX *B,
                             const F77_INT& LDB, F77_DBLE_CMPLX *Q,
                             const F77_INT& LDQ, F77_DBLE_CMPLX *Z,
                             const F77_INT& LDZ, F77_INT& INFO
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  // GGSVD

  F77_RET_T
  F77_FUNC (dggsvd, DGGSVD)
  (F77_CONST_CHAR_ARG_DECL,   // JOBU
   F77_CONST_CHAR_ARG_DECL,   // JOBV
   F77_CONST_CHAR_ARG_DECL,   // JOBQ
   const F77_INT&,            // M
   const F77_INT&,            // N
   const F77_INT&,            // P
   F77_INT&,                  // K
   F77_INT&,                  // L
   F77_DBLE *,                // A(LDA,N)
   const F77_INT&,            // LDA
   F77_DBLE *,                // B(LDB,N)
   const F77_INT&,            // LDB
   F77_DBLE *,                // ALPHA(N)
   F77_DBLE *,                // BETA(N)
   F77_DBLE *,                // U(LDU,M)
   const F77_INT&,            // LDU
   F77_DBLE *,                // V(LDV,P)
   const F77_INT&,            // LDV
   F77_DBLE *,                // Q(LDQ,N)
   const F77_INT&,            // LDQ
   F77_DBLE *,                // WORK
   F77_INT *,                 // IWORK(N)
   F77_INT&                   // INFO
   F77_CHAR_ARG_LEN_DECL
   F77_CHAR_ARG_LEN_DECL
   F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (sggsvd, SGGSVD)
  (F77_CONST_CHAR_ARG_DECL,   // JOBU
   F77_CONST_CHAR_ARG_DECL,   // JOBV
   F77_CONST_CHAR_ARG_DECL,   // JOBQ
   const F77_INT&,            // M
   const F77_INT&,            // N
   const F77_INT&,            // P
   F77_INT&,                  // K
   F77_INT&,                  // L
   F77_REAL *,                // A
   const F77_INT&,            // LDA
   F77_REAL *,                // B
   const F77_INT&,            // LDB
   F77_REAL *,                // ALPHA
   F77_REAL *,                // BETA
   F77_REAL *,                // U
   const F77_INT&,            // LDU
   F77_REAL *,                // V
   const F77_INT&,            // LDV
   F77_REAL *,                // Q
   const F77_INT&,            // LDQ
   F77_REAL *,                // WORK
   F77_INT *,                 // IWORK
   F77_INT&                   // INFO
   F77_CHAR_ARG_LEN_DECL
   F77_CHAR_ARG_LEN_DECL
   F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (zggsvd, ZGGSVD)
  (F77_CONST_CHAR_ARG_DECL,   // JOBU
   F77_CONST_CHAR_ARG_DECL,   // JOBV
   F77_CONST_CHAR_ARG_DECL,   // JOBQ
   const F77_INT&,            // M
   const F77_INT&,            // N
   const F77_INT&,            // P
   F77_INT&,                  // K
   F77_INT&,                  // L
   F77_DBLE_CMPLX *,          // A(LDA,N)
   const F77_INT&,            // LDA
   F77_DBLE_CMPLX *,          // B(LDB,N)
   const F77_INT&,            // LDB
   F77_DBLE *,                // ALPHA(N)
   F77_DBLE *,                // BETA(N)
   F77_DBLE_CMPLX *,          // U(LDU,M)
   const F77_INT&,            // LDU
   F77_DBLE_CMPLX *,          // V(LDV,P)
   const F77_INT&,            // LDV
   F77_DBLE_CMPLX *,          // Q(LDQ,N)
   const F77_INT&,            // LDQ
   F77_DBLE_CMPLX *,          // WORK
   F77_DBLE *,                // RWORK
   F77_INT *,                 // IWORK(N)
   F77_INT&                   // INFO
   F77_CHAR_ARG_LEN_DECL
   F77_CHAR_ARG_LEN_DECL
   F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (cggsvd, CGGSVD)
  (F77_CONST_CHAR_ARG_DECL,   // JOBU
   F77_CONST_CHAR_ARG_DECL,   // JOBV
   F77_CONST_CHAR_ARG_DECL,   // JOBQ
   const F77_INT&,            // M
   const F77_INT&,            // N
   const F77_INT&,            // P
   F77_INT&,                  // K
   F77_INT&,                  // L
   F77_CMPLX *,               // A
   const F77_INT&,            // LDA
   F77_CMPLX *,               // B
   const F77_INT&,            // LDB
   F77_REAL *,                // ALPHA
   F77_REAL *,                // BETA
   F77_CMPLX *,               // U
   const F77_INT&,            // LDU
   F77_CMPLX *,               // V
   const F77_INT&,            // LDV
   F77_CMPLX *,               // Q
   const F77_INT&,            // LDQ
   F77_CMPLX *,               // WORK
   F77_REAL *,                // RWORK
   F77_INT *,                 // IWORK
   F77_INT&                   // INFO
   F77_CHAR_ARG_LEN_DECL
   F77_CHAR_ARG_LEN_DECL
   F77_CHAR_ARG_LEN_DECL);

  // GGSVD3

  F77_RET_T
  F77_FUNC (dggsvd3, DGGSVD3)
  (F77_CONST_CHAR_ARG_DECL,   // JOBU
   F77_CONST_CHAR_ARG_DECL,   // JOBV
   F77_CONST_CHAR_ARG_DECL,   // JOBQ
   const F77_INT&,            // M
   const F77_INT&,            // N
   const F77_INT&,            // P
   F77_INT&,                  // K
   F77_INT&,                  // L
   F77_DBLE *,                // A(LDA,N)
   const F77_INT&,            // LDA
   F77_DBLE *,                // B(LDB,N)
   const F77_INT&,            // LDB
   F77_DBLE *,                // ALPHA(N)
   F77_DBLE *,                // BETA(N)
   F77_DBLE *,                // U(LDU,M)
   const F77_INT&,            // LDU
   F77_DBLE *,                // V(LDV,P)
   const F77_INT&,            // LDV
   F77_DBLE *,                // Q(LDQ,N)
   const F77_INT&,            // LDQ
   F77_DBLE *,                // WORK
   const F77_INT&,            // LWORK
   F77_INT *,                 // IWORK(N)
   F77_INT&                   // INFO
   F77_CHAR_ARG_LEN_DECL
   F77_CHAR_ARG_LEN_DECL
   F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (sggsvd3, SGGSVD3)
  (F77_CONST_CHAR_ARG_DECL,   // JOBU
   F77_CONST_CHAR_ARG_DECL,   // JOBV
   F77_CONST_CHAR_ARG_DECL,   // JOBQ
   const F77_INT&,            // M
   const F77_INT&,            // N
   const F77_INT&,            // P
   F77_INT&,                  // K
   F77_INT&,                  // L
   F77_REAL *,                // A
   const F77_INT&,            // LDA
   F77_REAL *,                // B
   const F77_INT&,            // LDB
   F77_REAL *,                // ALPHA
   F77_REAL *,                // BETA
   F77_REAL *,                // U
   const F77_INT&,            // LDU
   F77_REAL *,                // V
   const F77_INT&,            // LDV
   F77_REAL *,                // Q
   const F77_INT&,            // LDQ
   F77_REAL *,                // WORK
   const F77_INT&,            // LWORK
   F77_INT *,                 // IWORK
   F77_INT&                   // INFO
   F77_CHAR_ARG_LEN_DECL
   F77_CHAR_ARG_LEN_DECL
   F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (zggsvd3, ZGGSVD3)
  (F77_CONST_CHAR_ARG_DECL,   // JOBU
   F77_CONST_CHAR_ARG_DECL,   // JOBV
   F77_CONST_CHAR_ARG_DECL,   // JOBQ
   const F77_INT&,            // M
   const F77_INT&,            // N
   const F77_INT&,            // P
   F77_INT&,                  // K
   F77_INT&,                  // L
   F77_DBLE_CMPLX *,          // A(LDA,N)
   const F77_INT&,            // LDA
   F77_DBLE_CMPLX *,          // B(LDB,N)
   const F77_INT&,            // LDB
   F77_DBLE *,                // ALPHA(N)
   F77_DBLE *,                // BETA(N)
   F77_DBLE_CMPLX *,          // U(LDU,M)
   const F77_INT&,            // LDU
   F77_DBLE_CMPLX *,          // V(LDV,P)
   const F77_INT&,            // LDV
   F77_DBLE_CMPLX *,          // Q(LDQ,N)
   const F77_INT&,            // LDQ
   F77_DBLE_CMPLX *,          // WORK
   const F77_INT&,            // LWORK
   F77_DBLE *,                // RWORK
   F77_INT *,                 // IWORK(N)
   F77_INT&                   // INFO
   F77_CHAR_ARG_LEN_DECL
   F77_CHAR_ARG_LEN_DECL
   F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (cggsvd3, CGGSVD3)
  (F77_CONST_CHAR_ARG_DECL,   // JOBU
   F77_CONST_CHAR_ARG_DECL,   // JOBV
   F77_CONST_CHAR_ARG_DECL,   // JOBQ
   const F77_INT&,            // M
   const F77_INT&,            // N
   const F77_INT&,            // P
   F77_INT&,                  // K
   F77_INT&,                  // L
   F77_CMPLX *,               // A
   const F77_INT&,            // LDA
   F77_CMPLX *,               // B
   const F77_INT&,            // LDB
   F77_REAL *,                // ALPHA
   F77_REAL *,                // BETA
   F77_CMPLX *,               // U
   const F77_INT&,            // LDU
   F77_CMPLX *,               // V
   const F77_INT&,            // LDV
   F77_CMPLX *,               // Q
   const F77_INT&,            // LDQ
   F77_CMPLX *,               // WORK
   const F77_INT&,            // LWORK
   F77_REAL *,                // RWORK
   F77_INT *,                 // IWORK
   F77_INT&                   // INFO
   F77_CHAR_ARG_LEN_DECL
   F77_CHAR_ARG_LEN_DECL
   F77_CHAR_ARG_LEN_DECL);

  // GTSV

  F77_RET_T
  F77_FUNC (dgtsv, DGTSV) (const F77_INT&, const F77_INT&,
                           F77_DBLE *, F77_DBLE *, F77_DBLE *, F77_DBLE *,
                           const F77_INT&, F77_INT&);

  F77_RET_T
  F77_FUNC (zgtsv, ZGTSV) (const F77_INT&, const F77_INT&,
                           F77_DBLE_CMPLX *, F77_DBLE_CMPLX *,
                           F77_DBLE_CMPLX *, F77_DBLE_CMPLX *,
                           const F77_INT&, F77_INT&);

  // GTTRF

  F77_RET_T
  F77_FUNC (dgttrf, DGTTRF) (const F77_INT&, F77_DBLE *, F77_DBLE *,
                             F77_DBLE *, F77_DBLE *, F77_INT *,
                             F77_INT&);

  F77_RET_T
  F77_FUNC (zgttrf, ZGTTRF) (const F77_INT&, F77_DBLE_CMPLX *, F77_DBLE_CMPLX *,
                             F77_DBLE_CMPLX *, F77_DBLE_CMPLX *, F77_INT *,
                             F77_INT&);

  // GTTRS

  F77_RET_T
  F77_FUNC (dgttrs, DGTTRS) (F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, const F77_INT&,
                             const F77_DBLE *, const F77_DBLE *,
                             const F77_DBLE *, const F77_DBLE *,
                             const F77_INT *, F77_DBLE *,
                             const F77_INT&, F77_INT&
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (zgttrs, ZGTTRS) (F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, const F77_INT&,
                             const F77_DBLE_CMPLX *, const F77_DBLE_CMPLX *,
                             const F77_DBLE_CMPLX *, const F77_DBLE_CMPLX *,
                             const F77_INT *, F77_DBLE_CMPLX *,
                             const F77_INT&, F77_INT&
                             F77_CHAR_ARG_LEN_DECL);

  // HEEV

  F77_RET_T
  F77_FUNC (cheev, CHEEV) (F77_CONST_CHAR_ARG_DECL,
                           F77_CONST_CHAR_ARG_DECL,
                           const F77_INT&, F77_CMPLX *,
                           const F77_INT&, F77_REAL *, F77_CMPLX *,
                           const F77_INT&, F77_REAL *, F77_INT&
                           F77_CHAR_ARG_LEN_DECL
                           F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (zheev, ZHEEV) (F77_CONST_CHAR_ARG_DECL,
                           F77_CONST_CHAR_ARG_DECL,
                           const F77_INT&, F77_DBLE_CMPLX *,
                           const F77_INT&, F77_DBLE *,
                           F77_DBLE_CMPLX *, const F77_INT&, F77_DBLE *,
                           F77_INT&
                           F77_CHAR_ARG_LEN_DECL
                           F77_CHAR_ARG_LEN_DECL);

  // HEGV

  F77_RET_T
  F77_FUNC (chegv, CHEGV) (const F77_INT&,
                           F77_CONST_CHAR_ARG_DECL,
                           F77_CONST_CHAR_ARG_DECL,
                           const F77_INT&, F77_CMPLX *,
                           const F77_INT&, F77_CMPLX *,
                           const F77_INT&, F77_REAL *, F77_CMPLX *,
                           const F77_INT&, F77_REAL *, F77_INT&
                           F77_CHAR_ARG_LEN_DECL
                           F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (zhegv, ZHEGV) (const F77_INT&,
                           F77_CONST_CHAR_ARG_DECL,
                           F77_CONST_CHAR_ARG_DECL,
                           const F77_INT&, F77_DBLE_CMPLX *,
                           const F77_INT&, F77_DBLE_CMPLX *,
                           const F77_INT&, F77_DBLE *, F77_DBLE_CMPLX *,
                           const F77_INT&, F77_DBLE *, F77_INT&
                           F77_CHAR_ARG_LEN_DECL
                           F77_CHAR_ARG_LEN_DECL);

  // HERK

  F77_RET_T
  F77_FUNC (cherk, CHERK) (F77_CONST_CHAR_ARG_DECL,
                           F77_CONST_CHAR_ARG_DECL,
                           const F77_INT&, const F77_INT&,
                           const F77_REAL&, const F77_CMPLX *,
                           const F77_INT&, const F77_REAL&,
                           F77_CMPLX *, const F77_INT&
                           F77_CHAR_ARG_LEN_DECL
                           F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (zherk, ZHERK) (F77_CONST_CHAR_ARG_DECL,
                           F77_CONST_CHAR_ARG_DECL,
                           const F77_INT&, const F77_INT&,
                           const F77_DBLE&, const F77_DBLE_CMPLX *,
                           const F77_INT&, const F77_DBLE&, F77_DBLE_CMPLX *,
                           const F77_INT&
                           F77_CHAR_ARG_LEN_DECL
                           F77_CHAR_ARG_LEN_DECL);

  // HGEQZ

  F77_RET_T
  F77_FUNC (dhgeqz, DHGEQZ) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT& N,
                             const F77_INT& ILO,
                             const F77_INT& IHI,
                             F77_DBLE *A, const F77_INT& LDA, F77_DBLE *B,
                             const F77_INT& LDB, F77_DBLE *ALPHAR,
                             F77_DBLE *ALPHAI, F77_DBLE *BETA, F77_DBLE *Q,
                             const F77_INT& LDQ, F77_DBLE *Z,
                             const F77_INT& LDZ, F77_DBLE *WORK,
                             const F77_INT& LWORK,
                             F77_INT& INFO
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (zhgeqz, ZHGEQZ) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT& N,
                             const F77_INT& ILO,
                             const F77_INT& IHI,
                             F77_DBLE_CMPLX *A, const F77_INT& LDA,
                             F77_DBLE_CMPLX *B, const F77_INT& LDB,
                             F77_DBLE_CMPLX *ALPHA, F77_DBLE_CMPLX *BETA,
                             F77_DBLE_CMPLX *CQ, const F77_INT& LDQ,
                             F77_DBLE_CMPLX *CZ, const F77_INT& LDZ,
                             F77_DBLE_CMPLX *WORK, const F77_INT& LWORK,
                             F77_DBLE *RWORK, F77_INT& INFO
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  // LAENV (liboctave/external/lapack-xtra)

  F77_RET_T
  F77_FUNC (xilaenv, XILAENV) (const F77_INT&,
                               F77_CONST_CHAR_ARG_DECL,
                               F77_CONST_CHAR_ARG_DECL,
                               const F77_INT&, const F77_INT&,
                               const F77_INT&, const F77_INT&,
                               F77_INT&
                               F77_CHAR_ARG_LEN_DECL
                               F77_CHAR_ARG_LEN_DECL);

  // LAG2

  F77_RET_T
  F77_FUNC (dlag2, DLAG2) (const F77_DBLE *A, const F77_INT& LDA,
                           const F77_DBLE *B, const F77_INT& LDB,
                           const F77_DBLE& SAFMIN, F77_DBLE& SCALE1,
                           F77_DBLE& SCALE2, F77_DBLE& WR1, F77_DBLE& WR2,
                           F77_DBLE& WI);

  // LAMCH (liboctave/external/lapack-xtra)

  F77_RET_T
  F77_FUNC (xdlamch, XDLAMCH) (F77_CONST_CHAR_ARG_DECL,
                               F77_DBLE& retval
                               F77_CHAR_ARG_LEN_DECL);

  // LANGE (liboctave/external/lapack-xtra)

  F77_RET_T
  F77_FUNC (xclange, XCLANGE) (F77_CONST_CHAR_ARG_DECL,
                               const F77_INT&, const F77_INT&,
                               const F77_CMPLX *, const F77_INT&,
                               F77_REAL *, F77_REAL&
                               F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (xdlange, XDLANGE) (F77_CONST_CHAR_ARG_DECL,
                               const F77_INT&, const F77_INT&,
                               const F77_DBLE *, const F77_INT&,
                               F77_DBLE *, F77_DBLE&
                               F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (xslange, XSLANGE) (F77_CONST_CHAR_ARG_DECL,
                               const F77_INT&,
                               const F77_INT&, const F77_REAL *,
                               const F77_INT&, F77_REAL *, F77_REAL&
                               F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (xzlange, XZLANGE) (F77_CONST_CHAR_ARG_DECL,
                               const F77_INT&, const F77_INT&,
                               const F77_DBLE_CMPLX *, const F77_INT&,
                               F77_DBLE *, F77_DBLE&
                               F77_CHAR_ARG_LEN_DECL);
  // LARTG

  F77_RET_T
  F77_FUNC (clartg, CLARTG) (const F77_CMPLX *, const F77_CMPLX *,
                             F77_REAL&, F77_CMPLX *, F77_CMPLX *);

  F77_RET_T
  F77_FUNC (dlartg, DLARTG) (const F77_DBLE&, const F77_DBLE&, F77_DBLE&,
                             F77_DBLE&, F77_DBLE&);

  F77_RET_T
  F77_FUNC (slartg, SLARTG) (const F77_REAL&, const F77_REAL&, F77_REAL&,
                             F77_REAL&, F77_REAL&);

  F77_RET_T
  F77_FUNC (zlartg, ZLARTG) (const F77_DBLE_CMPLX *, const F77_DBLE_CMPLX *,
                             F77_DBLE&,
                             F77_DBLE_CMPLX *, F77_DBLE_CMPLX *);

  // ORGHR

  F77_RET_T
  F77_FUNC (dorghr, DORGHR) (const F77_INT&, const F77_INT&,
                             const F77_INT&, F77_DBLE *,
                             const F77_INT&, F77_DBLE *, F77_DBLE *,
                             const F77_INT&, F77_INT&);

  F77_RET_T
  F77_FUNC (sorghr, SORGHR) (const F77_INT&, const F77_INT&,
                             const F77_INT&, F77_REAL *,
                             const F77_INT&, F77_REAL *, F77_REAL *,
                             const F77_INT&, F77_INT&);

  // ORGQR

  F77_RET_T
  F77_FUNC (dorgqr, DORGQR) (const F77_INT&, const F77_INT&,
                             const F77_INT&, F77_DBLE *,
                             const F77_INT&, F77_DBLE *, F77_DBLE *,
                             const F77_INT&, F77_INT&);

  F77_RET_T
  F77_FUNC (sorgqr, SORGQR) (const F77_INT&, const F77_INT&,
                             const F77_INT&, F77_REAL *,
                             const F77_INT&, F77_REAL *, F77_REAL *,
                             const F77_INT&, F77_INT&);

  // PBCON

  F77_RET_T
  F77_FUNC (dpbcon, DPBCON) (F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, const F77_INT&,
                             F77_DBLE *, const F77_INT&,
                             const F77_DBLE&, F77_DBLE&, F77_DBLE *,
                             F77_INT *, F77_INT&
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (zpbcon, ZPBCON) (F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, const F77_INT&,
                             F77_DBLE_CMPLX *, const F77_INT&, const F77_DBLE&,
                             F77_DBLE&, F77_DBLE_CMPLX *, F77_DBLE *, F77_INT&
                             F77_CHAR_ARG_LEN_DECL);

  // PBTRF

  F77_RET_T
  F77_FUNC (dpbtrf, DPBTRF) (F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, const F77_INT&,
                             F77_DBLE *, const F77_INT&, F77_INT&
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (zpbtrf, ZPBTRF) (F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, const F77_INT&,
                             F77_DBLE_CMPLX *, const F77_INT&, F77_INT&
                             F77_CHAR_ARG_LEN_DECL);

  // PBTRS

  F77_RET_T
  F77_FUNC (dpbtrs, DPBTRS) (F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, const F77_INT&,
                             const F77_INT&, F77_DBLE *,
                             const F77_INT&, F77_DBLE *,
                             const F77_INT&, F77_INT&
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (zpbtrs, ZPBTRS) (F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, const F77_INT&,
                             const F77_INT&, F77_DBLE_CMPLX *,
                             const F77_INT&, F77_DBLE_CMPLX *,
                             const F77_INT&, F77_INT&
                             F77_CHAR_ARG_LEN_DECL);

  // POCON

  F77_RET_T
  F77_FUNC (cpocon, CPOCON) (F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, F77_CMPLX *,
                             const F77_INT&, const F77_REAL&, F77_REAL&,
                             F77_CMPLX *, F77_REAL *, F77_INT&
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dpocon, DPOCON) (F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, F77_DBLE *,
                             const F77_INT&, const F77_DBLE&,
                             F77_DBLE&, F77_DBLE *, F77_INT *,
                             F77_INT&
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (spocon, SPOCON) (F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, F77_REAL *,
                             const F77_INT&, const F77_REAL&,
                             F77_REAL&, F77_REAL *, F77_INT *,
                             F77_INT&
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (zpocon, ZPOCON) (F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, F77_DBLE_CMPLX *,
                             const F77_INT&, const F77_DBLE&,
                             F77_DBLE&, F77_DBLE_CMPLX *, F77_DBLE *, F77_INT&
                             F77_CHAR_ARG_LEN_DECL);

  // POTRF

  F77_RET_T
  F77_FUNC (cpotrf, CPOTRF) (F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, F77_CMPLX *,
                             const F77_INT&, F77_INT&
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dpotrf, DPOTRF) (F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, F77_DBLE *,
                             const F77_INT&, F77_INT&
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (spotrf, SPOTRF) (F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, F77_REAL *,
                             const F77_INT&, F77_INT&
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (zpotrf, ZPOTRF) (F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, F77_DBLE_CMPLX *,
                             const F77_INT&, F77_INT&
                             F77_CHAR_ARG_LEN_DECL);

  // POTRI

  F77_RET_T
  F77_FUNC (cpotri, CPOTRI) (F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, F77_CMPLX *,
                             const F77_INT&, F77_INT&
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dpotri, DPOTRI) (F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, F77_DBLE *,
                             const F77_INT&, F77_INT&
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (spotri, SPOTRI) (F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, F77_REAL *,
                             const F77_INT&, F77_INT&
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (zpotri, ZPOTRI) (F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, F77_DBLE_CMPLX *,
                             const F77_INT&, F77_INT&
                             F77_CHAR_ARG_LEN_DECL);

  // POTRS

  F77_RET_T
  F77_FUNC (spotrs, SPOTRS) (F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, const F77_INT&,
                             const F77_REAL *, const F77_INT&, F77_REAL *,
                             const F77_INT&, F77_INT&
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (cpotrs, CPOTRS) (F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&,
                             const F77_INT&, const F77_CMPLX *,
                             const F77_INT&, F77_CMPLX *,
                             const F77_INT&, F77_INT&
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dpotrs, DPOTRS) (F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, const F77_INT&,
                             const F77_DBLE *, const F77_INT&, F77_DBLE *,
                             const F77_INT&, F77_INT&
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (zpotrs, ZPOTRS) (F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, const F77_INT&,
                             const F77_DBLE_CMPLX *, const F77_INT&,
                             F77_DBLE_CMPLX *, const F77_INT&, F77_INT&
                             F77_CHAR_ARG_LEN_DECL);

  // PTSV

  F77_RET_T
  F77_FUNC (dptsv, DPTSV) (const F77_INT&, const F77_INT&,
                           F77_DBLE *, F77_DBLE *, F77_DBLE *, const F77_INT&,
                           F77_INT&);

  F77_RET_T
  F77_FUNC (zptsv, ZPTSV) (const F77_INT&, const F77_INT&,
                           F77_DBLE *, F77_DBLE_CMPLX *, F77_DBLE_CMPLX *,
                           const F77_INT&, F77_INT&);

  // RSF2CSF (liboctave/external/lapack-xtra)

  F77_RET_T
  F77_FUNC (zrsf2csf, ZRSF2CSF) (const F77_INT&, F77_DBLE_CMPLX *,
                                 F77_DBLE_CMPLX *, F77_DBLE *, F77_DBLE *);

  F77_RET_T
  F77_FUNC (crsf2csf, CRSF2CSF) (const F77_INT&, F77_CMPLX *,
                                 F77_CMPLX *, F77_REAL *, F77_REAL *);

  // SYEV

  F77_RET_T
  F77_FUNC (dsyev, DSYEV) (F77_CONST_CHAR_ARG_DECL,
                           F77_CONST_CHAR_ARG_DECL,
                           const F77_INT&, F77_DBLE *,
                           const F77_INT&, F77_DBLE *, F77_DBLE *,
                           const F77_INT&, F77_INT&
                           F77_CHAR_ARG_LEN_DECL
                           F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (ssyev, SSYEV) (F77_CONST_CHAR_ARG_DECL,
                           F77_CONST_CHAR_ARG_DECL,
                           const F77_INT&, F77_REAL *,
                           const F77_INT&, F77_REAL *, F77_REAL *,
                           const F77_INT&, F77_INT&
                           F77_CHAR_ARG_LEN_DECL
                           F77_CHAR_ARG_LEN_DECL);

  // SYGV

  F77_RET_T
  F77_FUNC (dsygv, DSYGV) (const F77_INT&,
                           F77_CONST_CHAR_ARG_DECL,
                           F77_CONST_CHAR_ARG_DECL,
                           const F77_INT&, F77_DBLE *,
                           const F77_INT&, F77_DBLE *,
                           const F77_INT&, F77_DBLE *, F77_DBLE *,
                           const F77_INT&, F77_INT&
                           F77_CHAR_ARG_LEN_DECL
                           F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (ssygv, SSYGV) (const F77_INT&,
                           F77_CONST_CHAR_ARG_DECL,
                           F77_CONST_CHAR_ARG_DECL,
                           const F77_INT&, F77_REAL *,
                           const F77_INT&, F77_REAL *,
                           const F77_INT&, F77_REAL *, F77_REAL *,
                           const F77_INT&, F77_INT&
                           F77_CHAR_ARG_LEN_DECL
                           F77_CHAR_ARG_LEN_DECL);

  // SYRK

  F77_RET_T
  F77_FUNC (csyrk, CSYRK) (F77_CONST_CHAR_ARG_DECL,
                           F77_CONST_CHAR_ARG_DECL,
                           const F77_INT&, const F77_INT&,
                           const F77_CMPLX&, const F77_CMPLX *,
                           const F77_INT&, const F77_CMPLX&,
                           F77_CMPLX *, const F77_INT&
                           F77_CHAR_ARG_LEN_DECL
                           F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dsyrk, DSYRK) (F77_CONST_CHAR_ARG_DECL,
                           F77_CONST_CHAR_ARG_DECL,
                           const F77_INT&, const F77_INT&,
                           const F77_DBLE&, const F77_DBLE *, const F77_INT&,
                           const F77_DBLE&, F77_DBLE *, const F77_INT&
                           F77_CHAR_ARG_LEN_DECL
                           F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (ssyrk, SSYRK) (F77_CONST_CHAR_ARG_DECL,
                           F77_CONST_CHAR_ARG_DECL,
                           const F77_INT&, const F77_INT&,
                           const F77_REAL&, const F77_REAL *, const F77_INT&,
                           const F77_REAL&, F77_REAL *, const F77_INT&
                           F77_CHAR_ARG_LEN_DECL
                           F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (zsyrk, ZSYRK) (F77_CONST_CHAR_ARG_DECL,
                           F77_CONST_CHAR_ARG_DECL,
                           const F77_INT&, const F77_INT&,
                           const F77_DBLE_CMPLX&, const F77_DBLE_CMPLX *,
                           const F77_INT&, const F77_DBLE_CMPLX&,
                           F77_DBLE_CMPLX *, const F77_INT&
                           F77_CHAR_ARG_LEN_DECL
                           F77_CHAR_ARG_LEN_DECL);

  // TGEVC

  // Documentation for DTGEVC incorrectly states that VR, VL are
  // complex*16; they are declared in DTGEVC as double precision
  // (probably a cut and paste problem fro ZTGEVC).
  F77_RET_T
  F77_FUNC (dtgevc, DTGEVC) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             F77_INT *SELECT,
                             const F77_INT& N, F77_DBLE *A,
                             const F77_INT& LDA, F77_DBLE *B,
                             const F77_INT& LDB, F77_DBLE *VL,
                             const F77_INT& LDVL, F77_DBLE *VR,
                             const F77_INT& LDVR,
                             const F77_INT& MM, F77_INT& M,
                             F77_DBLE *WORK, F77_INT& INFO
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (ztgevc, ZTGEVC) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             F77_INT *SELECT,
                             const F77_INT& N, const F77_DBLE_CMPLX *A,
                             const F77_INT& LDA, const F77_DBLE_CMPLX *B,
                             const F77_INT& LDB, F77_DBLE_CMPLX *xVL,
                             const F77_INT& LDVL, F77_DBLE_CMPLX *xVR,
                             const F77_INT& LDVR,
                             const F77_INT& MM, F77_INT& M,
                             F77_DBLE_CMPLX *CWORK, F77_DBLE *RWORK,
                             F77_INT& INFO
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  // TGSEN

  F77_RET_T
  F77_FUNC (dtgsen, DTGSEN) (const F77_INT& IJOB,
                             const F77_LOGICAL& WANTQ,
                             const F77_LOGICAL& WANTZ,
                             const F77_LOGICAL *SELECT,
                             const F77_INT& N,
                             F77_DBLE *A,
                             const F77_INT& LDA,
                             F77_DBLE *B,
                             const F77_INT& LDB,
                             F77_DBLE *ALPHAR,
                             F77_DBLE *ALPHAI,
                             F77_DBLE *BETA,
                             F77_DBLE *Q,
                             const F77_INT& LDQ,
                             F77_DBLE *Z,
                             const F77_INT& LDZ,
                             F77_INT& M,
                             F77_DBLE& PL,
                             F77_DBLE& PR,
                             F77_DBLE *DIF,
                             F77_DBLE *WORK,
                             const F77_INT& LWORK,
                             F77_INT *IWORK,
                             const F77_INT& LIWORK,
                             F77_INT& INFO);

  F77_RET_T
  F77_FUNC (ztgsen, ZTGSEN) (const F77_INT& IJOB,
                             const F77_LOGICAL& WANTQ,
                             const F77_LOGICAL& WANTZ,
                             const F77_LOGICAL *SELECT,
                             const F77_INT& N,
                             F77_DBLE_CMPLX *A,
                             const F77_INT& LDA,
                             F77_DBLE_CMPLX *B,
                             const F77_INT& LDB,
                             F77_DBLE_CMPLX *ALPHA,
                             F77_DBLE_CMPLX *BETA,
                             F77_DBLE_CMPLX *Q,
                             const F77_INT& LDQ,
                             F77_DBLE_CMPLX *Z,
                             const F77_INT& LDZ,
                             F77_INT& M,
                             F77_DBLE& PL,
                             F77_DBLE& PR,
                             F77_DBLE *DIF,
                             F77_DBLE_CMPLX *WORK,
                             const F77_INT& LWORK,
                             F77_INT *IWORK,
                             const F77_INT& LIWORK,
                             F77_INT& INFO);

  // TRCON

  F77_RET_T
  F77_FUNC (ctrcon, CTRCON) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, const F77_CMPLX *,
                             const F77_INT&, F77_REAL&, F77_CMPLX *,
                             F77_REAL *, F77_INT&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dtrcon, DTRCON) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, const F77_DBLE *,
                             const F77_INT&, F77_DBLE&,
                             F77_DBLE *, F77_INT *, F77_INT&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);
  F77_RET_T
  F77_FUNC (strcon, STRCON) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, const F77_REAL *,
                             const F77_INT&, F77_REAL&,
                             F77_REAL *, F77_INT *, F77_INT&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (ztrcon, ZTRCON) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, const F77_DBLE_CMPLX *,
                             const F77_INT&, F77_DBLE&,
                             F77_DBLE_CMPLX *, F77_DBLE *, F77_INT&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  // TRSEN

  F77_RET_T
  F77_FUNC (ctrsen, CTRSEN) (F77_CONST_CHAR_ARG_DECL, F77_CONST_CHAR_ARG_DECL,
                             const F77_INT *, const F77_INT&,
                             F77_CMPLX *, const F77_INT&, F77_CMPLX *,
                             const F77_INT&, F77_CMPLX *, F77_INT&,
                             F77_REAL&, F77_REAL&, F77_CMPLX *,
                             const F77_INT&, F77_INT&);

  F77_RET_T
  F77_FUNC (dtrsen, DTRSEN) (F77_CONST_CHAR_ARG_DECL, F77_CONST_CHAR_ARG_DECL,
                             const F77_INT *, const F77_INT&,
                             F77_DBLE *, const F77_INT&, F77_DBLE *,
                             const F77_INT&, F77_DBLE *, F77_DBLE *,
                             F77_INT&, F77_DBLE&, F77_DBLE&, F77_DBLE *,
                             const F77_INT&, F77_INT *,
                             const F77_INT&, F77_INT&);

  F77_RET_T
  F77_FUNC (strsen, STRSEN) (F77_CONST_CHAR_ARG_DECL, F77_CONST_CHAR_ARG_DECL,
                             const F77_INT *, const F77_INT&,
                             F77_REAL *, const F77_INT&, F77_REAL *,
                             const F77_INT&, F77_REAL *, F77_REAL *,
                             F77_INT&, F77_REAL&, F77_REAL&, F77_REAL *,
                             const F77_INT&, F77_INT *,
                             const F77_INT&, F77_INT&);

  F77_RET_T
  F77_FUNC (ztrsen, ZTRSEN) (F77_CONST_CHAR_ARG_DECL, F77_CONST_CHAR_ARG_DECL,
                             const F77_INT *, const F77_INT&,
                             F77_DBLE_CMPLX *, const F77_INT&,
                             F77_DBLE_CMPLX *, const F77_INT&,
                             F77_DBLE_CMPLX *, F77_INT&, F77_DBLE&,
                             F77_DBLE&, F77_DBLE_CMPLX *,
                             const F77_INT&, F77_INT&);

  // TRSYL

  F77_RET_T
  F77_FUNC (ctrsyl, CTRSYL) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, const F77_INT&,
                             const F77_INT&, const F77_CMPLX *,
                             const F77_INT&, const F77_CMPLX *,
                             const F77_INT&, const F77_CMPLX *,
                             const F77_INT&, F77_REAL&, F77_INT&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dtrsyl, DTRSYL) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, const F77_INT&,
                             const F77_INT&, const F77_DBLE *,
                             const F77_INT&, const F77_DBLE *,
                             const F77_INT&, const F77_DBLE *,
                             const F77_INT&, F77_DBLE&, F77_INT&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (strsyl, STRSYL) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, const F77_INT&,
                             const F77_INT&, const F77_REAL *,
                             const F77_INT&, const F77_REAL *,
                             const F77_INT&, const F77_REAL *,
                             const F77_INT&, F77_REAL&, F77_INT&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (ztrsyl, ZTRSYL) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, const F77_INT&,
                             const F77_INT&, const F77_DBLE_CMPLX *,
                             const F77_INT&, const F77_DBLE_CMPLX *,
                             const F77_INT&, const F77_DBLE_CMPLX *,
                             const F77_INT&, F77_DBLE&, F77_INT&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  // TRTRI

  F77_RET_T
  F77_FUNC (ctrtri, CTRTRI) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, const F77_CMPLX *,
                             const F77_INT&, F77_INT&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dtrtri, DTRTRI) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, const F77_DBLE *,
                             const F77_INT&, F77_INT&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (strtri, STRTRI) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, const F77_REAL *,
                             const F77_INT&, F77_INT&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (ztrtri, ZTRTRI) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, const F77_DBLE_CMPLX *,
                             const F77_INT&, F77_INT&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  // TRTRS

  F77_RET_T
  F77_FUNC (ctrtrs, CTRTRS) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, const F77_INT&,
                             const F77_CMPLX *, const F77_INT&,
                             F77_CMPLX *, const F77_INT&,
                             F77_INT&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dtrtrs, DTRTRS) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, const F77_INT&,
                             const F77_DBLE *, const F77_INT&, F77_DBLE *,
                             const F77_INT&, F77_INT&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (strtrs, STRTRS) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&,
                             const F77_INT&, const F77_REAL *,
                             const F77_INT&, F77_REAL *,
                             const F77_INT&, F77_INT&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (ztrtrs, ZTRTRS) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, const F77_INT&,
                             const F77_DBLE_CMPLX *, const F77_INT&, F77_DBLE_CMPLX *,
                             const F77_INT&, F77_INT&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  // UNGHR

  F77_RET_T
  F77_FUNC (cunghr, CUNGHR) (const F77_INT&, const F77_INT&,
                             const F77_INT&, F77_CMPLX *,
                             const F77_INT&, F77_CMPLX *,
                             F77_CMPLX *, const F77_INT&,
                             F77_INT&);

  F77_RET_T
  F77_FUNC (zunghr, ZUNGHR) (const F77_INT&, const F77_INT&,
                             const F77_INT&, F77_DBLE_CMPLX *,
                             const F77_INT&, F77_DBLE_CMPLX *, F77_DBLE_CMPLX *,
                             const F77_INT&, F77_INT&);
  // UNGQR

  F77_RET_T
  F77_FUNC (cungqr, CUNGQR) (const F77_INT&, const F77_INT&,
                             const F77_INT&, F77_CMPLX *,
                             const F77_INT&, F77_CMPLX *,
                             F77_CMPLX *, const F77_INT&,
                             F77_INT&);

  F77_RET_T
  F77_FUNC (zungqr, ZUNGQR) (const F77_INT&, const F77_INT&,
                             const F77_INT&, F77_DBLE_CMPLX *,
                             const F77_INT&, F77_DBLE_CMPLX *, F77_DBLE_CMPLX *,
                             const F77_INT&, F77_INT&);
}

#endif
