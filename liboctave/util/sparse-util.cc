////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1998-2023 The Octave Project Developers
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

#include <cinttypes>
#include <cstdarg>
#include <cstdio>

#include "lo-error.h"
#include "oct-sparse.h"
#include "sparse-util.h"

static inline void
sparse_chol_error_internal (int status, const char *file,
                            int line, const char *message)
{
#if defined (HAVE_CHOLMOD)

  // Ignore CHOLMOD_NOT_POSDEF, since we handle that in Fchol as an
  // error or exit status.
  if (status != CHOLMOD_NOT_POSDEF)
    (*current_liboctave_warning_with_id_handler)
      ("Octave:cholmod-message", "warning %i, at line %i in file %s: %s",
       status, line, file, message);

#else

  octave_unused_parameter (status);
  octave_unused_parameter (file);
  octave_unused_parameter (line);
  octave_unused_parameter (message);

#endif
}

// FIXME: this overload is here due to API change in SuiteSparse (3.1 -> 3.2)

void
SparseCholError (int status, char *file, int line, char *message)
{
  sparse_chol_error_internal (status, file, line, message);
}

void
SparseCholError (int status, const char *file, int line, const char *message)
{
  sparse_chol_error_internal (status, file, line, message);
}

int
SparseCholPrint (const char *fmt, ...)
{
  va_list args;
  va_start (args, fmt);
  int ret = std::vfprintf (stderr, fmt, args);
  std::fflush (stderr);
  va_end (args);
  return ret;
}

bool
sparse_indices_ok (octave_idx_type *r, octave_idx_type *c,
                   octave_idx_type nrows, octave_idx_type ncols,
                   octave_idx_type nnz)
{
  if (nnz > 0)
    {
      if (c[0] != 0)
        (*current_liboctave_error_handler)
          ("invalid sparse matrix: cidx[0] must be zero");

      octave_idx_type jold = 0;

      for (octave_idx_type j = 1; j < ncols+1; j++)
        {
          if (c[j] < c[j-1])
            (*current_liboctave_error_handler)
              ("invalid sparse matrix: cidx elements must appear in ascending order");

          if (c[j] > nnz)
            (*current_liboctave_error_handler)
              ("invalid sparse matrix: cidx[%" OCTAVE_IDX_TYPE_FORMAT "] = "
               "%" OCTAVE_IDX_TYPE_FORMAT " exceeds number of nonzero elements",
               j, c[j]+1);

          if (c[j] != jold)
            {
              for (octave_idx_type i = jold+1; i < c[j]; i++)
                {
                  if (r[i] < r[i-1])
                    (*current_liboctave_error_handler)
                      ("invalid sparse matrix: ridx elements must appear "
                       "in ascending order for each column");

                  if (r[i] >= nrows)
                    (*current_liboctave_error_handler)
                      ("invalid sparse matrix: ridx[%" OCTAVE_IDX_TYPE_FORMAT
                       "] = %" OCTAVE_IDX_TYPE_FORMAT " out of range",
                       i, r[i]+1);
                }

              jold = c[j];
            }
        }
    }

  return true;
}
