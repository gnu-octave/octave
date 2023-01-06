////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1995-2023 The Octave Project Developers
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

#if ! defined (octave_oct_cmplx_h)
#define octave_oct_cmplx_h 1

#include "octave-config.h"

#include <complex>

typedef std::complex<double> Complex;
typedef std::complex<float> FloatComplex;

// template declaration
# define DECL_COMPLEXR_COMP(OP)                                               \
    template <typename T> bool operator OP (const std::complex<T>& a,         \
                                            const std::complex<T>& b);        \
    template <typename T> bool operator OP (const std::complex<T>& a, T b);   \
    template <typename T> bool operator OP (T a, const std::complex<T>& b);

DECL_COMPLEXR_COMP(>)
DECL_COMPLEXR_COMP(<)
DECL_COMPLEXR_COMP(>=)
DECL_COMPLEXR_COMP(<=)

// extern template instantiations
# define EXT_INST_COMPLEXR_COMP(OP, T)                                       \
    extern template OCTAVE_EXTERN_TEMPLATE_API                               \
    bool operator OP (const std::complex<T>& a, const std::complex<T>& b);   \
    extern template OCTAVE_EXTERN_TEMPLATE_API                               \
    bool operator OP (const std::complex<T>& a, T b);                        \
    extern template OCTAVE_EXTERN_TEMPLATE_API                               \
    bool operator OP (T a, const std::complex<T>& b);

EXT_INST_COMPLEXR_COMP(>, double)
EXT_INST_COMPLEXR_COMP(>, float)
EXT_INST_COMPLEXR_COMP(<, double)
EXT_INST_COMPLEXR_COMP(<, float)
EXT_INST_COMPLEXR_COMP(>=, double)
EXT_INST_COMPLEXR_COMP(>=, float)
EXT_INST_COMPLEXR_COMP(<=, double)
EXT_INST_COMPLEXR_COMP(<=, float)

#endif
