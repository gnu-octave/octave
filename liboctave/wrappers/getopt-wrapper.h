////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2016-2026 The Octave Project Developers
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

#if ! defined (octave_getopt_wrapper_h)
#define octave_getopt_wrapper_h 1

#if defined (__cplusplus)
extern "C" {
#endif

struct octave_getopt_options
{
  const char *name;
  int has_arg;
  int *flag;
  int val;
};

#define octave_no_arg 0
#define octave_required_arg 1
#define octave_optional_arg 2

OCTAVE_API int
octave_getopt_long_wrapper (int argc, char **argv,
                            const char *shortopts,
                            const struct octave_getopt_options *longopts,
                            int *longind);

OCTAVE_API char * octave_optarg_wrapper (void);

OCTAVE_API int octave_optind_wrapper (void);

// FIXME: 2026-07-31 : wrapper is unused.  consider deprecating and deleting.
OCTAVE_API int octave_get_opterr_wrapper (void);

OCTAVE_API int octave_set_opterr_wrapper (int val);

#if defined (__cplusplus)
}
#endif

#endif
