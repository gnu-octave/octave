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

#if ! defined (octave_lo_amos_proto_h)
#define octave_lo_amos_proto_h 1

#include "octave-config.h"

#include "f77-fcn.h"

extern "C"
{
  // AIRY

  F77_RET_T
  F77_FUNC (cairy, CAIRY) (const F77_CMPLX *, const F77_INT&,
                           const F77_INT&, F77_CMPLX *,
                           F77_INT&, F77_INT&);

  F77_RET_T
  F77_FUNC (zairy, ZAIRY) (const F77_DBLE&, const F77_DBLE&,
                           const F77_INT&, const F77_INT&,
                           F77_DBLE&, F77_DBLE&, F77_INT&,
                           F77_INT&);

  // BESH

  F77_RET_T
  F77_FUNC (cbesh, CBESH) (const F77_CMPLX *, const F77_REAL&,
                           const F77_INT&, const F77_INT&,
                           const F77_INT&, F77_CMPLX *,
                           F77_INT&, F77_INT&);

  F77_RET_T
  F77_FUNC (zbesh, ZBESH) (const F77_DBLE&, const F77_DBLE&, const F77_DBLE&,
                           const F77_INT&, const F77_INT&,
                           const F77_INT&, F77_DBLE *, F77_DBLE *,
                           F77_INT&, F77_INT&);

  // BESI

  F77_RET_T
  F77_FUNC (cbesi, CBESI) (const F77_CMPLX *, const F77_REAL&,
                           const F77_INT&, const F77_INT&,
                           F77_CMPLX *, F77_INT&, F77_INT&);

  F77_RET_T
  F77_FUNC (zbesi, ZBESI) (const F77_DBLE&, const F77_DBLE&, const F77_DBLE&,
                           const F77_INT&, const F77_INT&,
                           F77_DBLE *, F77_DBLE *, F77_INT&,
                           F77_INT&);

  // BESJ

  F77_RET_T
  F77_FUNC (cbesj, CBESJ) (const F77_CMPLX *, const F77_REAL&,
                           const F77_INT&, const F77_INT&,
                           F77_CMPLX *, F77_INT&, F77_INT&);

  F77_RET_T
  F77_FUNC (zbesj, ZBESJ) (const F77_DBLE&, const F77_DBLE&, const F77_DBLE&,
                           const F77_INT&, const F77_INT&,
                           F77_DBLE *, F77_DBLE *, F77_INT&,
                           F77_INT&);

  // BESK

  F77_RET_T
  F77_FUNC (cbesk, CBESK) (const F77_CMPLX *, const F77_REAL&,
                           const F77_INT&, const F77_INT&,
                           F77_CMPLX *, F77_INT&, F77_INT&);

  F77_RET_T
  F77_FUNC (zbesk, ZBESK) (const F77_DBLE&, const F77_DBLE&, const F77_DBLE&,
                           const F77_INT&, const F77_INT&,
                           F77_DBLE *, F77_DBLE *, F77_INT&,
                           F77_INT&);

  // BESY

  F77_RET_T
  F77_FUNC (cbesy, CBESY) (const F77_CMPLX *, const F77_REAL&,
                           const F77_INT&, const F77_INT&,
                           F77_CMPLX *, F77_INT&,
                           F77_CMPLX *, F77_INT&);

  F77_RET_T
  F77_FUNC (zbesy, ZBESY) (const F77_DBLE&, const F77_DBLE&, const F77_DBLE&,
                           const F77_INT&, const F77_INT&,
                           F77_DBLE *, F77_DBLE *, F77_INT&, F77_DBLE *,
                           F77_DBLE *, F77_INT&);

  // BIRY

  F77_RET_T
  F77_FUNC (cbiry, CBIRY) (const F77_CMPLX *, const F77_INT&, const F77_INT&,
                           const F77_CMPLX *, F77_INT&);

  F77_RET_T
  F77_FUNC (zbiry, ZBIRY) (const F77_DBLE&, const F77_DBLE&,
                           const F77_INT&, const F77_INT&,
                           F77_DBLE&, F77_DBLE&, F77_INT&);
}

#endif
