////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2005-2023 The Octave Project Developers
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

#if ! defined (octave_oct_sparse_h)
#define octave_oct_sparse_h 1

#include "octave-config.h"

#include <limits>

#if defined (HAVE_CHOLMOD)
#  include "dSparse.h"
#  include "CSparse.h"
#endif

#if defined (HAVE_SUITESPARSE_AMD_H)
#  include <suitesparse/amd.h>
#elif defined (HAVE_AMD_AMD_H)
#  include <amd/amd.h>
#elif defined (HAVE_AMD_H)
#  include <amd.h>
#endif

#if defined (HAVE_SUITESPARSE_CCOLAMD_H)
#  include <suitesparse/ccolamd.h>
#elif defined (HAVE_CCOLAMD_CCOLAMD_H)
#  include <ccolamd/ccolamd.h>
#elif defined (HAVE_CCOLAMD_H)
#  include <ccolamd.h>
#endif

#if defined (HAVE_SUITESPARSE_CHOLMOD_H)
#  include <suitesparse/cholmod.h>
#elif defined (HAVE_CHOLMOD_CHOLMOD_H)
#  include <cholmod/cholmod.h>
#elif defined (HAVE_CHOLMOD_H)
#  include <cholmod.h>
#endif

#if defined (HAVE_SUITESPARSE_COLAMD_H)
#  include <suitesparse/colamd.h>
#elif defined (HAVE_COLAMD_COLAMD_H)
#  include <colamd/colamd.h>
#elif defined (HAVE_COLAMD_H)
#  include <colamd.h>
#endif

#if defined (HAVE_SUITESPARSE_CS_H)
#  include <suitesparse/cs.h>
#elif defined (HAVE_CXSPARSE_CS_H)
#  include <cxsparse/cs.h>
#elif defined (HAVE_CS_H)
#  include <cs.h>
#endif

#if defined (HAVE_SUITESPARSE_UMFPACK_H)
#  include <suitesparse/umfpack.h>
#elif defined (HAVE_UMFPACK_UMFPACK_H)
#  include <umfpack/umfpack.h>
#elif defined (HAVE_UMFPACK_H)
#  include <umfpack.h>
#endif

#if defined (HAVE_SUITESPARSE_SUITESPARSEQR_HPP)
#  include <suitesparse/SuiteSparseQR.hpp>
#elif defined (HAVE_SUITESPARSEQR_HPP)
#  include <SuiteSparseQR.hpp>
#endif

// Cope with API differences between SuiteSparse versions

#if defined (SUITESPARSE_VERSION)
#  if (SUITESPARSE_VERSION >= SUITESPARSE_VER_CODE (7, 0))
#    define SUITESPARSE_NAME(name) SuiteSparse_ ## name
#    define SUITESPARSE_SET_FCN(name) SuiteSparse_config_ ## name ## _set
#    define SUITESPARSE_ASSIGN_FPTR(f_name, f_var, f_assign) \
       SUITESPARSE_SET_FCN(f_name) (f_assign)
#    define SUITESPARSE_ASSIGN_FPTR2(f_name, f_var, f_assign) \
       SUITESPARSE_SET_FCN(f_name) (SUITESPARSE_NAME (f_assign))
#  elif (SUITESPARSE_VERSION >= SUITESPARSE_VER_CODE (4, 3))
#    define SUITESPARSE_NAME(name) SuiteSparse_ ## name
#    define SUITESPARSE_ASSIGN_FPTR(f_name, f_var, f_assign) \
       (SuiteSparse_config.f_name = f_assign)
#    define SUITESPARSE_ASSIGN_FPTR2(f_name, f_var, f_assign) \
       (SuiteSparse_config.f_name = SUITESPARSE_NAME (f_assign))
#  else
#    define SUITESPARSE_ASSIGN_FPTR(f_name, f_var, f_assign) \
       (f_var = f_assign)
#    define SUITESPARSE_ASSIGN_FPTR2(f_name, f_var, f_assign) \
       (f_var = CHOLMOD_NAME (f_assign))
#  endif
#endif

// Function names depend on integer type.

#if defined (HAVE_AMD)
#  if defined (OCTAVE_ENABLE_64)
#    define AMD_NAME(name) amd_l ## name
#  else
#    define AMD_NAME(name) amd ## name
#  endif
#endif

#if defined (HAVE_CCOLAMD)
#  if defined (OCTAVE_ENABLE_64)
#    define CCOLAMD_NAME(name) ccolamd_l ## name
#    define CSYMAMD_NAME(name) csymamd_l ## name
#  else
#    define CCOLAMD_NAME(name) ccolamd ## name
#    define CSYMAMD_NAME(name) csymamd ## name
#  endif
#endif

#if defined (HAVE_CHOLMOD)
#  if defined (OCTAVE_ENABLE_64)
#    define CHOLMOD_NAME(name) cholmod_l_ ## name
#  else
#    define CHOLMOD_NAME(name) cholmod_ ## name
#  endif
#endif

#if defined (HAVE_COLAMD)
#  if defined (OCTAVE_ENABLE_64)
#    define COLAMD_NAME(name) colamd_l ## name
#    define SYMAMD_NAME(name) symamd_l ## name
#  else
#    define COLAMD_NAME(name) colamd ## name
#    define SYMAMD_NAME(name) symamd ## name
#  endif
#endif

#if defined (HAVE_CXSPARSE)
#  if defined (OCTAVE_ENABLE_64)
#    define CXSPARSE_DNAME(name) cs_dl ## name
#    define CXSPARSE_ZNAME(name) cs_cl ## name
#  else
#    define CXSPARSE_DNAME(name) cs_di ## name
#    define CXSPARSE_ZNAME(name) cs_ci ## name
#  endif
#endif

#if defined (HAVE_UMFPACK)
#  if defined (OCTAVE_ENABLE_64)
#    define UMFPACK_DNAME(name) umfpack_dl_ ## name
#    define UMFPACK_ZNAME(name) umfpack_zl_ ## name
#  else
#    define UMFPACK_DNAME(name) umfpack_di_ ## name
#    define UMFPACK_ZNAME(name) umfpack_zi_ ## name
#  endif
#endif

#if (defined (HAVE_AMD) || defined (HAVE_CCOLAMD)               \
     || defined (HAVE_CHOLMOD) || defined (HAVE_COLAMD)         \
     || defined (HAVE_CXSPARSE) || defined (HAVE_SPQR)          \
     || defined (HAVE_UMFPACK))

OCTAVE_BEGIN_NAMESPACE(octave)

#  if defined (OCTAVE_ENABLE_64)
typedef SuiteSparse_long suitesparse_integer;
#  else
typedef int suitesparse_integer;
#  endif

extern OCTAVE_API suitesparse_integer *
to_suitesparse_intptr (octave_idx_type *i);

extern const OCTAVE_API suitesparse_integer *
to_suitesparse_intptr (const octave_idx_type *i);

extern OCTAVE_API octave_idx_type *
to_octave_idx_type_ptr (suitesparse_integer *i);

extern const OCTAVE_API octave_idx_type *
to_octave_idx_type_ptr (const suitesparse_integer *i);

inline octave_idx_type
from_suitesparse_long (SuiteSparse_long x)
{
  if (x < std::numeric_limits<octave_idx_type>::min ()
      || x > std::numeric_limits<octave_idx_type>::max ())
    (*current_liboctave_error_handler)
      ("integer dimension or index out of range for Octave's indexing type");

  return static_cast<octave_idx_type> (x);
}

inline octave_idx_type
from_size_t (std::size_t x)
{
  // std::size_t is guaranteed to be unsigned
  if (x > std::numeric_limits<octave_idx_type>::max ())
    (*current_liboctave_error_handler)
      ("integer dimension or index out of range for Octave's index type");

  return static_cast<octave_idx_type> (x);
}

OCTAVE_END_NAMESPACE(octave)

#endif
#endif
