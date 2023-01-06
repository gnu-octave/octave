////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1994-2023 The Octave Project Developers
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

#if ! defined (octave_mx_ext_h)
#define octave_mx_ext_h 1

#include "octave-config.h"

// Result of a AEP Balance operation.

#include "aepbalance.h"

// Result of a GEP Balance operation.

#include "gepbalance.h"

// Result of a Determinant calculation.

#include "DET.h"

// Result of a Cholesky Factorization

#include "chol.h"

// Result of a Hessenberg Decomposition

#include "hess.h"

// Result of a Schur Decomposition

#include "schur.h"

// Result of a Singular Value Decomposition.

#include "svd.h"

// Result of an Eigenvalue computation.

#include "EIG.h"

// Result of a Generalized Singular Value Decomposition.

#include "gsvd.h"


// Result of an LU decomposition.

#include "lu.h"

// Result of a QR decomposition.

#include "qr.h"

#endif
