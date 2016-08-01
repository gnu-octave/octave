/*

Copyright (C) 2005-2015 David Bateman

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if ! defined (octave_oct_sparse_h)
#define octave_oct_sparse_h 1

#include "octave-config.h"

#if defined (HAVE_SUITESPARSE_AMD_H)
#  include <suitesparse/amd.h>
#elif defined (HAVE_UFSPARSE_AMD_H)
#  include <ufsparse/amd.h>
#elif defined (HAVE_AMD_AMD_H)
#  include <amd/amd.h>
#elif defined (HAVE_AMD_H)
#  include <amd.h>
#endif

#if defined (HAVE_SUITESPARSE_UMFPACK_H)
#  include <suitesparse/umfpack.h>
#elif defined (HAVE_UFSPARSE_UMFPACK_H)
#  include <ufsparse/umfpack.h>
#elif defined (HAVE_UMFPACK_UMFPACK_H)
#  include <umfpack/umfpack.h>
#elif defined (HAVE_UMFPACK_H)
#  include <umfpack.h>
#endif

#if defined (HAVE_SUITESPARSE_COLAMD_H)
#  include <suitesparse/colamd.h>
#elif defined (HAVE_UFSPARSE_COLAMD_H)
#  include <ufsparse/colamd.h>
#elif defined (HAVE_COLAMD_COLAMD_H)
#  include <colamd/colamd.h>
#elif defined (HAVE_COLAMD_H)
#  include <colamd.h>
#endif

#if defined (HAVE_SUITESPARSE_CCOLAMD_H)
#  include <suitesparse/ccolamd.h>
#elif defined (HAVE_UFSPARSE_CCOLAMD_H)
#  include <ufsparse/ccolamd.h>
#elif defined (HAVE_CCOLAMD_CCOLAMD_H)
#  include <ccolamd/ccolamd.h>
#elif defined (HAVE_CCOLAMD_H)
#  include <ccolamd.h>
#endif

#if defined (HAVE_SUITESPARSE_CHOLMOD_H)
#  include <suitesparse/cholmod.h>
#elif defined (HAVE_UFSPARSE_CHOLMOD_H)
#  include <ufsparse/cholmod.h>
#elif defined (HAVE_CHOLMOD_CHOLMOD_H)
#  include <cholmod/cholmod.h>
#elif defined (HAVE_CHOLMOD_H)
#  include <cholmod.h>
#endif

#if defined (HAVE_SUITESPARSE_CS_H)
#  include <suitesparse/cs.h>
#elif defined (HAVE_UFSPARSE_CS_H)
#  include <ufsparse/cs.h>
#elif defined (HAVE_CXSPARSE_CS_H)
#  include <cxsparse/cs.h>
#elif defined (HAVE_CS_H)
#  include <cs.h>
#endif

#if (defined (HAVE_SUITESPARSE_CHOLMOD_H)       \
     || defined (HAVE_UFSPARSE_CHOLMOD_H)       \
     || defined (HAVE_CHOLMOD_CHOLMOD_H)        \
     || defined (HAVE_CHOLMOD_H))
#  if defined (OCTAVE_ENABLE_64)
#    define CHOLMOD_NAME(name) cholmod_l_ ## name
#  else
#    define CHOLMOD_NAME(name) cholmod_ ## name
#  endif
#endif

// Cope with new SuiteSparse versions

#if defined (SUITESPARSE_VERSION)
#  if (SUITESPARSE_VERSION >= SUITESPARSE_VER_CODE (4, 3))
#    define SUITESPARSE_NAME(name) SuiteSparse_ ## name
#    define SUITESPARSE_ASSIGN_FPTR(f_name, f_var, f_assign) (SuiteSparse_config.f_name = f_assign)
#    define SUITESPARSE_ASSIGN_FPTR2(f_name, f_var, f_assign) (SuiteSparse_config.f_name = SUITESPARSE_NAME (f_assign))
#  else
#    define SUITESPARSE_ASSIGN_FPTR(f_name, f_var, f_assign) (f_var = f_assign)
#    define SUITESPARSE_ASSIGN_FPTR2(f_name, f_var, f_assign) (f_var = CHOLMOD_NAME (f_assign))
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

#endif
