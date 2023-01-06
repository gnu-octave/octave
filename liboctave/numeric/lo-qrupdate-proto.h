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

#if ! defined (octave_lo_qrupdate_proto_h)
#define octave_lo_qrupdate_proto_h 1

#include "octave-config.h"

#include "f77-fcn.h"

extern "C"
{

#if defined (HAVE_QRUPDATE)

  // CH1DN

  F77_RET_T
  F77_FUNC (cch1dn, CCH1DN) (const F77_INT&, F77_CMPLX *,
                             const F77_INT&, F77_CMPLX *,
                             F77_REAL *, F77_INT&);

  F77_RET_T
  F77_FUNC (dch1dn, DCH1DN) (const F77_INT&, F77_DBLE *,
                             const F77_INT&, F77_DBLE *,
                             F77_DBLE *, F77_INT&);

  F77_RET_T
  F77_FUNC (sch1dn, SCH1DN) (const F77_INT&, F77_REAL *,
                             const F77_INT&, F77_REAL *,
                             F77_REAL *, F77_INT&);

  F77_RET_T
  F77_FUNC (zch1dn, ZCH1DN) (const F77_INT&, F77_DBLE_CMPLX *,
                             const F77_INT&, F77_DBLE_CMPLX *,
                             F77_DBLE *, F77_INT&);

  // CH1UP

  F77_RET_T
  F77_FUNC (cch1up, CCH1UP) (const F77_INT&, F77_CMPLX *,
                             const F77_INT&, F77_CMPLX *, F77_REAL *);

  F77_RET_T
  F77_FUNC (dch1up, DCH1UP) (const F77_INT&, F77_DBLE *,
                             const F77_INT&, F77_DBLE *, F77_DBLE *);

  F77_RET_T
  F77_FUNC (sch1up, SCH1UP) (const F77_INT&, F77_REAL *,
                             const F77_INT&, F77_REAL *, F77_REAL *);

  F77_RET_T
  F77_FUNC (zch1up, ZCH1UP) (const F77_INT&, F77_DBLE_CMPLX *,
                             const F77_INT&, F77_DBLE_CMPLX *, F77_DBLE *);

  // CHDEX

  F77_RET_T
  F77_FUNC (dchdex, DCHDEX) (const F77_INT&, F77_DBLE *,
                             const F77_INT&, const F77_INT&,
                             F77_DBLE *);

  F77_RET_T
  F77_FUNC (schdex, SCHDEX) (const F77_INT&, F77_REAL *,
                             const F77_INT&, const F77_INT&,
                             F77_REAL *);

  F77_RET_T
  F77_FUNC (cchdex, CCHDEX) (const F77_INT&, F77_CMPLX *,
                             const F77_INT&, const F77_INT&,
                             F77_REAL *);

  F77_RET_T
  F77_FUNC (zchdex, ZCHDEX) (const F77_INT&, F77_DBLE_CMPLX *,
                             const F77_INT&, const F77_INT&,
                             F77_DBLE *);

  // CHINX

  F77_RET_T
  F77_FUNC (cchinx, CCHINX) (const F77_INT&, F77_CMPLX *,
                             const F77_INT&, const F77_INT&,
                             F77_CMPLX *, F77_REAL *, F77_INT&);

  F77_RET_T
  F77_FUNC (dchinx, DCHINX) (const F77_INT&, F77_DBLE *,
                             const F77_INT&, const F77_INT&,
                             F77_DBLE *, F77_DBLE *, F77_INT&);

  F77_RET_T
  F77_FUNC (schinx, SCHINX) (const F77_INT&, F77_REAL *,
                             const F77_INT&, const F77_INT&,
                             F77_REAL *, F77_REAL *, F77_INT&);

  F77_RET_T
  F77_FUNC (zchinx, ZCHINX) (const F77_INT&, F77_DBLE_CMPLX *,
                             const F77_INT&, const F77_INT&,
                             F77_DBLE_CMPLX *, F77_DBLE *, F77_INT&);

  // CHSHX

  F77_RET_T
  F77_FUNC (cchshx, CCHSHX) (const F77_INT&, F77_CMPLX *,
                             const F77_INT&, const F77_INT&,
                             const F77_INT&, F77_CMPLX *, F77_REAL *);

  F77_RET_T
  F77_FUNC (dchshx, DCHSHX) (const F77_INT&, F77_DBLE *,
                             const F77_INT&, const F77_INT&,
                             const F77_INT&, F77_DBLE *);

  F77_RET_T
  F77_FUNC (schshx, SCHSHX) (const F77_INT&, F77_REAL *,
                             const F77_INT&, const F77_INT&,
                             const F77_INT&, F77_REAL *);

  F77_RET_T
  F77_FUNC (zchshx, ZCHSHX) (const F77_INT&, F77_DBLE_CMPLX *,
                             const F77_INT&, const F77_INT&,
                             const F77_INT&, F77_DBLE_CMPLX *, F77_DBLE *);

  // QR1UP

  F77_RET_T
  F77_FUNC (cqr1up, CQR1UP) (const F77_INT&, const F77_INT&,
                             const F77_INT&, F77_CMPLX *,
                             const F77_INT&, F77_CMPLX *,
                             const F77_INT&, F77_CMPLX *,
                             F77_CMPLX *, F77_CMPLX *, F77_REAL *);

  F77_RET_T
  F77_FUNC (dqr1up, DQR1UP) (const F77_INT&, const F77_INT&,
                             const F77_INT&, F77_DBLE *,
                             const F77_INT&, F77_DBLE *,
                             const F77_INT&, F77_DBLE *, F77_DBLE *, F77_DBLE *);

  F77_RET_T
  F77_FUNC (sqr1up, SQR1UP) (const F77_INT&, const F77_INT&,
                             const F77_INT&, F77_REAL *,
                             const F77_INT&, F77_REAL *,
                             const F77_INT&, F77_REAL *, F77_REAL *, F77_REAL *);

  F77_RET_T
  F77_FUNC (zqr1up, ZQR1UP) (const F77_INT&, const F77_INT&,
                             const F77_INT&, F77_DBLE_CMPLX *,
                             const F77_INT&, F77_DBLE_CMPLX *,
                             const F77_INT&, F77_DBLE_CMPLX *,
                             F77_DBLE_CMPLX *, F77_DBLE_CMPLX *, F77_DBLE *);

  // QRDEC

  F77_RET_T
  F77_FUNC (cqrdec, CQRDEC) (const F77_INT&, const F77_INT&,
                             const F77_INT&, F77_CMPLX *,
                             const F77_INT&, F77_CMPLX *,
                             const F77_INT&, const F77_INT&,
                             F77_REAL *);

  F77_RET_T
  F77_FUNC (dqrdec, DQRDEC) (const F77_INT&, const F77_INT&,
                             const F77_INT&, F77_DBLE *,
                             const F77_INT&, F77_DBLE *,
                             const F77_INT&, const F77_INT&,
                             F77_DBLE *);

  F77_RET_T
  F77_FUNC (sqrdec, SQRDEC) (const F77_INT&, const F77_INT&,
                             const F77_INT&, F77_REAL *,
                             const F77_INT&, F77_REAL *,
                             const F77_INT&, const F77_INT&,
                             F77_REAL *);

  F77_RET_T
  F77_FUNC (zqrdec, ZQRDEC) (const F77_INT&, const F77_INT&,
                             const F77_INT&, F77_DBLE_CMPLX *,
                             const F77_INT&, F77_DBLE_CMPLX *,
                             const F77_INT&, const F77_INT&,
                             F77_DBLE *);

  // QRDER

  F77_RET_T
  F77_FUNC (cqrder, CQRDER) (const F77_INT&, const F77_INT&,
                             F77_CMPLX *, const F77_INT&,
                             F77_CMPLX *, const F77_INT&,
                             const F77_INT&, F77_CMPLX *, F77_REAL *);

  F77_RET_T
  F77_FUNC (dqrder, DQRDER) (const F77_INT&, const F77_INT&,
                             F77_DBLE *, const F77_INT&,
                             F77_DBLE *, const F77_INT&,
                             const F77_INT&, F77_DBLE *);

  F77_RET_T
  F77_FUNC (sqrder, SQRDER) (const F77_INT&, const F77_INT&,
                             F77_REAL *, const F77_INT&,
                             F77_REAL *, const F77_INT&,
                             const F77_INT&, F77_REAL *);

  F77_RET_T
  F77_FUNC (zqrder, ZQRDER) (const F77_INT&, const F77_INT&,
                             F77_DBLE_CMPLX *, const F77_INT&,
                             F77_DBLE_CMPLX *, const F77_INT&,
                             const F77_INT&, F77_DBLE_CMPLX *, F77_DBLE *);

  // QRINC

  F77_RET_T
  F77_FUNC (cqrinc, CQRINC) (const F77_INT&, const F77_INT&,
                             const F77_INT&, F77_CMPLX *,
                             const F77_INT&, F77_CMPLX *,
                             const F77_INT&, const F77_INT&,
                             const F77_CMPLX *, F77_REAL *);

  F77_RET_T
  F77_FUNC (dqrinc, DQRINC) (const F77_INT&, const F77_INT&,
                             const F77_INT&, F77_DBLE *,
                             const F77_INT&, F77_DBLE *,
                             const F77_INT&, const F77_INT&,
                             const F77_DBLE *, F77_DBLE *);

  F77_RET_T
  F77_FUNC (sqrinc, SQRINC) (const F77_INT&, const F77_INT&,
                             const F77_INT&, F77_REAL *,
                             const F77_INT&, F77_REAL *,
                             const F77_INT&, const F77_INT&,
                             const F77_REAL *, F77_REAL *);

  F77_RET_T
  F77_FUNC (zqrinc, ZQRINC) (const F77_INT&, const F77_INT&,
                             const F77_INT&, F77_DBLE_CMPLX *,
                             const F77_INT&, F77_DBLE_CMPLX *,
                             const F77_INT&, const F77_INT&,
                             const F77_DBLE_CMPLX *, F77_DBLE *);

  // QRINR

  F77_RET_T
  F77_FUNC (cqrinr, CQRINR) (const F77_INT&, const F77_INT&,
                             F77_CMPLX *, const F77_INT&,
                             F77_CMPLX *, const F77_INT&,
                             const F77_INT&, const F77_CMPLX *,
                             F77_REAL *);

  F77_RET_T
  F77_FUNC (dqrinr, DQRINR) (const F77_INT&, const F77_INT&,
                             F77_DBLE *, const F77_INT&,
                             F77_DBLE *, const F77_INT&,
                             const F77_INT&, const F77_DBLE *,
                             F77_DBLE *);

  F77_RET_T
  F77_FUNC (sqrinr, SQRINR) (const F77_INT&, const F77_INT&,
                             F77_REAL *, const F77_INT&,
                             F77_REAL *, const F77_INT&,
                             const F77_INT&, const F77_REAL *,
                             F77_REAL *);

  F77_RET_T
  F77_FUNC (zqrinr, ZQRINR) (const F77_INT&, const F77_INT&,
                             F77_DBLE_CMPLX *, const F77_INT&,
                             F77_DBLE_CMPLX *, const F77_INT&,
                             const F77_INT&, const F77_DBLE_CMPLX *,
                             F77_DBLE *);

  // QRSHC

  F77_RET_T
  F77_FUNC (cqrshc, CQRSHC) (const F77_INT&, const F77_INT&,
                             const F77_INT&, F77_CMPLX *,
                             const F77_INT&, F77_CMPLX *,
                             const F77_INT&, const F77_INT&,
                             const F77_INT&,
                             F77_CMPLX *, F77_REAL *);
  F77_RET_T
  F77_FUNC (dqrshc, DQRSHC) (const F77_INT&, const F77_INT&,
                             const F77_INT&, F77_DBLE *,
                             const F77_INT&, F77_DBLE *,
                             const F77_INT&, const F77_INT&,
                             const F77_INT&,
                             F77_DBLE *);

  F77_RET_T
  F77_FUNC (sqrshc, SQRSHC) (const F77_INT&, const F77_INT&,
                             const F77_INT&, F77_REAL *,
                             const F77_INT&, F77_REAL *,
                             const F77_INT&, const F77_INT&,
                             const F77_INT&,
                             F77_REAL *);

  F77_RET_T
  F77_FUNC (zqrshc, ZQRSHC) (const F77_INT&, const F77_INT&,
                             const F77_INT&, F77_DBLE_CMPLX *,
                             const F77_INT&, F77_DBLE_CMPLX *,
                             const F77_INT&, const F77_INT&,
                             const F77_INT&,
                             F77_DBLE_CMPLX *, F77_DBLE *);

#endif

#if defined (HAVE_QRUPDATE_LUU)

  // LU1UP

  F77_RET_T
  F77_FUNC (clu1up, CLU1UP) (const F77_INT&, const F77_INT&,
                             F77_CMPLX *, const F77_INT&,
                             F77_CMPLX *, const F77_INT&,
                             F77_CMPLX *, F77_CMPLX *);

  F77_RET_T
  F77_FUNC (dlu1up, DLU1UP) (const F77_INT&, const F77_INT&,
                             F77_DBLE *, const F77_INT&,
                             F77_DBLE *, const F77_INT&,
                             F77_DBLE *, F77_DBLE *);

  F77_RET_T
  F77_FUNC (slu1up, SLU1UP) (const F77_INT&, const F77_INT&,
                             F77_REAL *, const F77_INT&,
                             F77_REAL *, const F77_INT&,
                             F77_REAL *, F77_REAL *);

  F77_RET_T
  F77_FUNC (zlu1up, ZLU1UP) (const F77_INT&, const F77_INT&,
                             F77_DBLE_CMPLX *, const F77_INT&,
                             F77_DBLE_CMPLX *, const F77_INT&,
                             F77_DBLE_CMPLX *, F77_DBLE_CMPLX *);

  // LUP1UP

  F77_RET_T
  F77_FUNC (clup1up, CLUP1UP) (const F77_INT&, const F77_INT&,
                               F77_CMPLX *, const F77_INT&,
                               F77_CMPLX *, const F77_INT&,
                               F77_INT *, const F77_CMPLX *,
                               const F77_CMPLX *, F77_CMPLX *);
  F77_RET_T
  F77_FUNC (dlup1up, DLUP1UP) (const F77_INT&, const F77_INT&,
                               F77_DBLE *, const F77_INT&,
                               F77_DBLE *, const F77_INT&,
                               F77_INT *, const F77_DBLE *,
                               const F77_DBLE *, F77_DBLE *);

  F77_RET_T
  F77_FUNC (slup1up, SLUP1UP) (const F77_INT&, const F77_INT&,
                               F77_REAL *, const F77_INT&,
                               F77_REAL *, const F77_INT&,
                               F77_INT *, const F77_REAL *,
                               const F77_REAL *, F77_REAL *);

  F77_RET_T
  F77_FUNC (zlup1up, ZLUP1UP) (const F77_INT&, const F77_INT&,
                               F77_DBLE_CMPLX *, const F77_INT&,
                               F77_DBLE_CMPLX *, const F77_INT&,
                               F77_INT *, const F77_DBLE_CMPLX *,
                               const F77_DBLE_CMPLX *, F77_DBLE_CMPLX *);

#endif

}

#endif
