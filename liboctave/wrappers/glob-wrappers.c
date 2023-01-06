////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2016-2023 The Octave Project Developers
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

// These functions may be provided by gnulib.  We don't include gnulib
// headers directly in Octave's C++ source files to avoid problems that
// may be caused by the way that gnulib overrides standard library
// functions.

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <stdlib.h>

#include <fnmatch.h>
#include <glob.h>

#include "glob-wrappers.h"

void *
octave_create_glob_info_struct (void)
{
  return malloc (sizeof (glob_t));
}

// Does not call globfree.
void
octave_destroy_glob_info_struct (void *glob_info)
{
  free (glob_info);
}

int
octave_glob_wrapper (const char *pattern, int flags, void *glob_info)
{
  return glob (pattern, flags, 0, glob_info);
}

int
octave_glob_num_matches (void *glob_info)
{
  return glob_info ? ((glob_t *) glob_info)->gl_pathc : 0;
}

char **
octave_glob_match_list (void *glob_info)
{
  return glob_info ? ((glob_t *) glob_info)->gl_pathv : 0;
}

void
octave_globfree_wrapper (void *glob_info)
{
  globfree ((glob_t *) glob_info);
}

int
octave_glob_nosort_wrapper (void)
{
  return GLOB_NOSORT;
}

int
octave_fnmatch_wrapper (const char *pattern, const char *name, int flags)
{
  return fnmatch (pattern, name, flags);
}

int
octave_fnm_nomatch_wrapper (void)
{
  return FNM_NOMATCH;
}

int
octave_fnm_pathname_wrapper (void)
{
  return FNM_PATHNAME;
}

int
octave_fnm_noescape_wrapper (void)
{
  return FNM_NOESCAPE;
}

int
octave_fnm_period_wrapper (void)
{
  return FNM_PERIOD;
}
