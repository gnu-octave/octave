////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2023 The Octave Project Developers
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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <iostream>

#include "f77-fcn.h"
#include "lo-blas-proto.h"

typedef void (*xerbla_handler_fptr) (void);

/* Pointer to function to call to handle error.  In the Octave
   interpreter we set this to a function that throws an exception and
   transfers control to the enclosing try/catch block.  That is
   typically at the top-level REPL.

   We must use a function pointer instead of simply calling error
   directly here because this function is called by LAPACK and BLAS
   subroutines.  To build shared libraries of those packages on Windows
   requires that all symbols be known when the shared library is
   constructed.  If we call error directly, that would mean that the
   BLAS and LAPACK libraries would have to depend on Octave...  */

static xerbla_handler_fptr xerbla_handler = nullptr;

extern "C" void
octave_set_xerbla_handler (xerbla_handler_fptr fcn)
{
  xerbla_handler = fcn;
}

/* Replacement for BLAS and LAPACK XERBLA subroutine that calls an
   optionally installed handler function.  */

F77_RET_T
F77_FUNC (xerbla, XERBLA) (F77_CONST_CHAR_ARG_DEF (s_arg, len),
                           const F77_INT& info
                           F77_CHAR_ARG_LEN_DEF (len))
{
  const char *s = F77_CHAR_ARG_USE (s_arg);
  int slen = F77_CHAR_ARG_LEN_USE (s_arg, len);

  std::cerr << std::string (s, slen) << ": parameter number " << info
            << " is invalid" << std::endl;

  if (xerbla_handler)
    (*xerbla_handler) ();

  F77_RETURN (0)
}
