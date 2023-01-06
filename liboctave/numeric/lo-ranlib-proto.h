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

#if ! defined (octave_lo_ranlib_proto_h)
#define octave_lo_ranlib_proto_h 1

#include "octave-config.h"

#include "f77-fcn.h"

extern "C"
{
  F77_RET_T
  F77_FUNC (dgenexp, DGENEXP) (const F77_DBLE&, F77_DBLE&);

  F77_RET_T
  F77_FUNC (dgengam, DGENGAM) (const F77_DBLE&, const F77_DBLE&, F77_DBLE&);

  F77_RET_T
  F77_FUNC (dgennor, DGENNOR) (const F77_DBLE&, const F77_DBLE&, F77_DBLE&);

  F77_RET_T
  F77_FUNC (dgenunf, DGENUNF) (const F77_DBLE&, const F77_DBLE&, F77_DBLE&);

  F77_RET_T
  F77_FUNC (dignpoi, DIGNPOI) (const F77_DBLE&, F77_DBLE&);

  F77_RET_T
  F77_FUNC (fgenexp, FGENEXP) (const F77_REAL&, F77_REAL&);

  F77_RET_T
  F77_FUNC (fgengam, FGENGAM) (const F77_REAL&, const F77_REAL&, F77_REAL&);

  F77_RET_T
  F77_FUNC (fgennor, FGENNOR) (const F77_REAL&, const F77_REAL&, F77_REAL&);

  F77_RET_T
  F77_FUNC (fgenunf, FGENUNF) (const F77_REAL&, const F77_REAL&, F77_REAL&);

  F77_RET_T
  F77_FUNC (fignpoi, FIGNPOI) (const F77_REAL&, F77_REAL&);

  F77_RET_T
  F77_FUNC (getsd, GETSD) (F77_INT4&, F77_INT4&);

  F77_RET_T
  F77_FUNC (setall, SETALL) (const F77_INT4&, const F77_INT4&);

  F77_RET_T
  F77_FUNC (setcgn, SETCGN) (const F77_INT4&);

  F77_RET_T
  F77_FUNC (setsd, SETSD) (const F77_INT4&, const F77_INT4&);
}

#endif
