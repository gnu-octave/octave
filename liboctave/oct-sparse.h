/*

Copyright (C) 2005 David Bateman

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

#if !defined (oct_sparse_h)
#define oct_sparse_h 1

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

#if defined (HAVE_SPARSESUITE_UMFPACK_H)
#include <sparsesuite/umfpack.h>
#elif defined (HAVE_UFSPARSE_UMFPACK_H)
#include <ufsparse/umfpack.h>
#elif defined (HAVE_UMFPACK_UMFPACK_H)
#include <umfpack/umfpack.h>
#elif defined (HAVE_UMFPACK_H)
#include <umfpack.h>
#endif

#if defined (HAVE_SPARSESUITE_COLAMD_H)
#include <sparsesuite/colamd.h>
#elif defined (HAVE_UFSPARSE_COLAMD_H)
#include <ufsparse/colamd.h>
#elif defined (HAVE_COLAMD_COLAMD_H)
#include <colamd/colamd.h>
#elif defined (HAVE_COLAMD_H)
#include <colamd.h>
#endif

#if defined (HAVE_SPARSESUITE_CCOLAMD_H)
#include <sparsesuite/ccolamd.h>
#elif defined (HAVE_UFSPARSE_CCOLAMD_H)
#include <ufsparse/ccolamd.h>
#elif defined (HAVE_CCOLAMD_CCOLAMD_H)
#include <ccolamd/ccolamd.h>
#elif defined (HAVE_CCOLAMD_H)
#include <ccolamd.h>
#endif

#if defined (HAVE_SPARSESUITE_METIS_H)
#include <sparsesuite/metis.h>
#elif defined (HAVE_UFSPARSE_METIS_H)
#include <ufsparse/metis.h>
#elif defined (HAVE_METIS_METIS_H)
#include <metis/metis.h>
#elif defined (HAVE_METIS_H)
#include <metis.h>
#endif

#if defined (HAVE_SPARSESUITE_CHOLMOD_H)
#include <sparsesuite/cholmod.h>
#elif defined (HAVE_UFSPARSE_CHOLMOD_H)
#include <ufsparse/cholmod.h>
#elif defined (HAVE_UMFPACK_CHOLMOD_H)
#include <cholmod/cholmod.h>
#elif defined (HAVE_CHOLMOD_H)
#include <cholmod.h>
#endif

#if defined (HAVE_SPARSESUITE_CS_H)
#include <sparsesuite/cs.h>
#elif defined (HAVE_UFSPARSE_CS_H)
#include <ufsparse/cs.h>
#elif defined (HAVE_CXSPARSE_CS_H)
#include <cxsparse/cs.h>
#elif defined (HAVE_CS_H)
#include <cs.h>
#endif

#if (defined (HAVE_SPARSESUITE_CHOLMOD_H) \
     || defined (HAVE_UFSPARSE_CHOLMOD_H) \
     || defined (HAVE_UMFPACK_CHOLMOD_H) \
     || defined (HAVE_CHOLMOD_H))
#ifdef IDX_TYPE_LONG
#define CHOLMOD_NAME(name) cholmod_l_ ## name
#else
#define CHOLMOD_NAME(name) cholmod_ ## name
#endif
#endif

#ifdef __cplusplus
}
#endif

#endif
