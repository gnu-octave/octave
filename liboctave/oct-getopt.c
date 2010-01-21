/*

Copyright (C) 2000, 2003, 2005, 2007 John W. Eaton

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

#include "getopt.h"

int
octave_getopt (int argc, char *const *argv, const char *optstring)
{
  return getopt (argc, argv, optstring);
}

int
octave_getopt_long (int argc, char *const *argv, const char *options,
		    const struct option *long_options, int *opt_index)
{
  return getopt_long (argc, argv, options, long_options, opt_index);
}
