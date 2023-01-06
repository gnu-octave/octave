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

// getopt_long may be provided by gnulib.  We don't include gnulib
// headers directly in Octave's C++ source files to avoid problems that
// may be caused by the way that gnulib overrides standard library
// functions.

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <stdlib.h>

#include <getopt.h>

#include "getopt-wrapper.h"

static struct option *
make_option_struct (const struct octave_getopt_options *opts)
{
  const struct octave_getopt_options *p = opts;
  struct option *retval = 0, *q = 0;

  int n = 0;
  while (p->name)
    {
      n++;
      p++;
    }

  retval = (struct option *) malloc ((n+1) * sizeof (struct option));

  // If we don't have enough memory even to start Octave
  // then we might as well quit now.
  if (! retval)
    abort ();

  p = opts;
  q = retval;
  while (p->name)
    {
      q->name = p->name;

      switch (p->has_arg)
        {
        case octave_required_arg:
          q->has_arg = required_argument;
          break;

        case octave_optional_arg:
          q->has_arg = optional_argument;
          break;

        default:
          q->has_arg = no_argument;
          break;
        }

      q->flag = p->flag;

      q->val = p->val;

      q++;
      p++;
    }

  q->name = 0;
  q->has_arg = 0;
  q->flag = 0;
  q->val = 0;

  return retval;
}

int
octave_getopt_long_wrapper (int argc, char **argv,
                            const char *shortopts,
                            const struct octave_getopt_options *longopts,
                            int *longind)
{
  struct option *lopts = make_option_struct (longopts);

  int retval = getopt_long (argc, argv, shortopts, lopts, longind);

  free (lopts);

  return retval;
}

char *
octave_optarg_wrapper (void)
{
  return optarg;
}

int
octave_optind_wrapper (void)
{
  return optind;
}

int
octave_get_opterr_wrapper (void)
{
  return opterr;
}

int
octave_set_opterr_wrapper (int val)
{
  int retval = opterr;

  opterr = val;

  return retval;
}

