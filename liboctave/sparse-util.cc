/*

Copyright (C) 2005 David Bateman
Copyright (C) 1998-2005 Andy Adler

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdarg.h>
#include "lo-error.h"
#include "sparse-util.h"

void
SparseCholError (int status, char *file, int line, char *message)
{
  (*current_liboctave_warning_handler)("warning %i, at line %i in file %s",
				     status, line, file);

  (*current_liboctave_warning_handler)(message);
}

int
SparseCholPrint (const char *fmt, ...)
{
  va_list args;
  va_start (args, fmt);
  int ret = vfprintf (stderr, fmt, args);
  fflush (stderr);
  va_end (args);
  return ret;
}


/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
