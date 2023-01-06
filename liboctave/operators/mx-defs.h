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

#if ! defined (octave_mx_defs_h)
#define octave_mx_defs_h 1

#include "octave-config.h"

// Classes we declare.

#include "mx-fwd.h"

template <typename T> class aepbalance;

template <typename T> class gepbalance;

template <typename T> class chol;

class EIG;

template <typename T> class gsvd;

template <typename T> class hess;

template <typename T> class schur;

template <typename T> class svd;

template <typename T> class lu;

template <typename T> class qr;

template <typename T> class qrp;

// Other data types we use but that don't always need to have full
// declarations.

#include "oct-cmplx.h"

#if ! defined (MAPPER_FCN_TYPEDEFS)
#  define MAPPER_FCN_TYPEDEFS 1

typedef bool (*b_d_Mapper)(double);
typedef bool (*b_c_Mapper)(const Complex&);

typedef double (*d_d_Mapper)(double);
typedef double (*d_c_Mapper)(const Complex&);
typedef Complex (*c_c_Mapper)(const Complex&);

typedef bool (*b_f_Mapper)(float);
typedef bool (*b_fc_Mapper)(const FloatComplex&);

typedef float (*f_f_Mapper)(float);
typedef float (*f_fc_Mapper)(const FloatComplex&);
typedef FloatComplex (*fc_fc_Mapper)(const FloatComplex&);

enum blas_trans_type
{
  blas_no_trans = 'N',
  blas_trans = 'T',
  blas_conj_trans = 'C'
};

inline char
get_blas_char (blas_trans_type transt)
{
  return static_cast<char> (transt);
}

#  endif

#endif
