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

#include <stdlib.h>
#include <string.h>

#include "f77-fcn.h"
#include "quit.h"
#include "lo-error.h"

/* All the STOP statements in the Fortran routines have been replaced
   with a call to XSTOPX.

   XSTOPX calls the liboctave error handler.  In the Octave interpreter
   we set this to a function that throws an exception and transfers
   control to the enclosing try/catch block.  That is typically at the
   top-level REPL.  */

F77_RET_T
F77_FUNC (xstopx, XSTOPX) (F77_CONST_CHAR_ARG_DEF (s_arg, len)
                           F77_CHAR_ARG_LEN_DEF (len))
{
  const char *s = F77_CHAR_ARG_USE (s_arg);
  size_t slen = F77_CHAR_ARG_LEN_USE (s_arg, len);

  /* Skip printing message if it is just a single blank character.  */
  if (! (s && slen > 0 && ! (slen == 1 && *s == ' ')))
    {
      s = "unknown error in fortran subroutine";
      slen = strlen (s);
    }

  (*current_liboctave_error_handler) ("%.*s", (int) slen, s);

  F77_NORETURN (0)
}
