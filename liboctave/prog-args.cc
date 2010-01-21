/*

Copyright (C) 1996, 1997, 1998, 2000, 2005, 2006, 2007 John W. Eaton

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "oct-getopt.h"

#include "prog-args.h"

int
prog_args::getopt (void)
{
  if (long_opts)
    return ::octave_getopt_long
      (xargc, xargv, short_opts,
       reinterpret_cast<const struct option *> (long_opts), 0);
  else
    return ::octave_getopt (xargc, xargv, short_opts);
}

const char *
prog_args::optarg (void)
{
  return ::optarg;
}

int
prog_args::optind (void)
{
  return ::optind;
}

// This is intended to communicate to getopt that it is supposed to
// start over on the next call, but it may not be portable.  See the
// comments in getopt.c for more information.

void
prog_args::init (void)
{
  ::optind = 0;
}
