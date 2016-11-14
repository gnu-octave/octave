/*

Copyright (C) 2016 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if ! defined (octave_lo_slatec_proto_h)
#define octave_lo_slatec_proto_h 1

#include "octave-config.h"

#include "f77-fcn.h"

extern "C"
{
  // ACOSH

  F77_RET_T
  F77_FUNC (xacosh, XACOSH) (const F77_REAL&, F77_REAL&);

  F77_RET_T
  F77_FUNC (xdacosh, XDACOSH) (const F77_DBLE&, F77_DBLE&);

  // ASINH

  F77_RET_T
  F77_FUNC (xasinh, XASINH) (const F77_REAL&, F77_REAL&);

  F77_RET_T
  F77_FUNC (xdasinh, XDASINH) (const F77_DBLE&, F77_DBLE&);

  // ATANH

  F77_RET_T
  F77_FUNC (xatanh, XATANH) (const F77_REAL&, F77_REAL&);

  F77_RET_T
  F77_FUNC (xdatanh, XDATANH) (const F77_DBLE&, F77_DBLE&);

  // BETAI

  F77_RET_T
  F77_FUNC (xbetai, XBETAI) (const F77_REAL&, const F77_REAL&,
                             const F77_REAL&, F77_REAL&);

  F77_RET_T
  F77_FUNC (xdbetai, XDBETAI) (const F77_DBLE&, const F77_DBLE&,
                               const F77_DBLE&, F77_DBLE&);

  // ERF

  F77_RET_T
  F77_FUNC (xerf, XERF) (const F77_REAL&, F77_REAL&);

  F77_RET_T
  F77_FUNC (xderf, XDERF) (const F77_DBLE&, F77_DBLE&);

  // ERFC

  F77_RET_T
  F77_FUNC (xderfc, XDERFC) (const F77_DBLE&, F77_DBLE&);

  F77_RET_T
  F77_FUNC (xerfc, XERFC) (const F77_REAL&, F77_REAL&);

  // GAMMA

  F77_RET_T
  F77_FUNC (xgamma, XGAMMA) (const F77_REAL&, F77_REAL&);

  F77_RET_T
  F77_FUNC (xdgamma, XDGAMMA) (const F77_DBLE&, F77_DBLE&);

  // GAMMAINC

  F77_RET_T
  F77_FUNC (xgammainc, XGAMMAINC) (const F77_DBLE&, const F77_DBLE&, F77_DBLE&);

  F77_RET_T
  F77_FUNC (xsgammainc, XSGAMMAINC) (const F77_REAL&, const F77_REAL&, F77_REAL&);

  // LGAMS

  F77_RET_T
  F77_FUNC (algams, ALGAMS) (const F77_REAL&, F77_REAL&, F77_REAL&);

  F77_RET_T
  F77_FUNC (dlgams, DLGAMS) (const F77_DBLE&, F77_DBLE&, F77_DBLE&);

  // PCHIM

  F77_RET_T
  F77_FUNC (dpchim, DPCHIM) (const F77_INT& n, const F77_DBLE *x,
                             const F77_DBLE *f, F77_DBLE *d,
                             const F77_INT &incfd,
                             F77_INT *ierr);

  F77_RET_T
  F77_FUNC (pchim, PCHIM) (const F77_INT& n, const F77_REAL *x,
                           const F77_REAL *f, F77_REAL *d,
                           const F77_INT& incfd,
                           F77_INT *ierr);

  // PSIFN

  F77_RET_T
  F77_FUNC (psifn, PSIFN) (const F77_REAL*, const F77_INT&,
                           const F77_INT&, const F77_INT&,
                           F77_REAL*, F77_INT*, F77_INT*);

  F77_RET_T
  F77_FUNC (dpsifn, DPSIFN) (const F77_DBLE*, const F77_INT&,
                             const F77_INT&, const F77_INT&,
                             F77_DBLE*, F77_INT*, F77_INT*);
}

#endif

