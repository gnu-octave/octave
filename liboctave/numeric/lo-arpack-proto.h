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

#if ! defined (octave_lo_arpack_proto_h)
#define octave_lo_arpack_proto_h 1

#include "octave-config.h"

#include "f77-fcn.h"

extern "C"
{

#if defined (HAVE_ARPACK)

  // NAUPD

  F77_RET_T
  F77_FUNC (dnaupd, DNAUPD) (F77_INT&,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&,
                             F77_CONST_CHAR_ARG_DECL,
                             F77_INT&, const F77_DBLE&,
                             F77_DBLE *, const F77_INT&, F77_DBLE *,
                             const F77_INT&, F77_INT *,
                             F77_INT *, F77_DBLE *, F77_DBLE *,
                             const F77_INT&, F77_INT&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (znaupd, ZNAUPD) (F77_INT&,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, const F77_DBLE&,
                             F77_DBLE_CMPLX *, const F77_INT&, F77_DBLE_CMPLX *,
                             const F77_INT&, F77_INT *,
                             F77_INT *, F77_DBLE_CMPLX *, F77_DBLE_CMPLX *,
                             const F77_INT&, F77_DBLE *, F77_INT&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  // NEUPD

  F77_RET_T
  F77_FUNC (dneupd, DNEUPD) (const F77_INT&,
                             F77_CONST_CHAR_ARG_DECL,
                             F77_INT *, F77_DBLE *, F77_DBLE *,
                             F77_DBLE *, const F77_INT&, const F77_DBLE&,
                             const F77_DBLE&, F77_DBLE *,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&,
                             F77_CONST_CHAR_ARG_DECL,
                             F77_INT&, const F77_DBLE&, F77_DBLE *,
                             const F77_INT&, F77_DBLE *,
                             const F77_INT&, F77_INT *,
                             F77_INT *, F77_DBLE *, F77_DBLE *,
                             const F77_INT&, F77_INT&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (zneupd, ZNEUPD) (const F77_INT&,
                             F77_CONST_CHAR_ARG_DECL,
                             F77_INT *, F77_DBLE_CMPLX *, F77_DBLE_CMPLX *,
                             const F77_INT&, const F77_DBLE_CMPLX *,
                             F77_DBLE_CMPLX *,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, const F77_DBLE&,
                             F77_DBLE_CMPLX *, const F77_INT&, F77_DBLE_CMPLX *,
                             const F77_INT&, F77_INT *,
                             F77_INT *, F77_DBLE_CMPLX *, F77_DBLE_CMPLX *,
                             const F77_INT&, F77_DBLE *, F77_INT&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  // SAUPD

  F77_RET_T
  F77_FUNC (dsaupd, DSAUPD) (F77_INT&,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, const F77_DBLE&,
                             F77_DBLE *, const F77_INT&, F77_DBLE *,
                             const F77_INT&, F77_INT *,
                             F77_INT *, F77_DBLE *, F77_DBLE *,
                             const F77_INT&, F77_INT&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  // SEUPD

  F77_RET_T
  F77_FUNC (dseupd, DSEUPD) (const F77_INT&,
                             F77_CONST_CHAR_ARG_DECL,
                             F77_INT *, F77_DBLE *, F77_DBLE *,
                             const F77_INT&, const F77_DBLE&,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, const F77_DBLE&, F77_DBLE *,
                             const F77_INT&, F77_DBLE *,
                             const F77_INT&, F77_INT *,
                             F77_INT *, F77_DBLE *, F77_DBLE *,
                             const F77_INT&, F77_INT&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

#endif

}

#endif
