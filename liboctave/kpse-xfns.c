/*

Copyright (C) 1992, 93, 94, 95, 96 Free Software Foundation, Inc.
Copyright (C) 1993, 94, 95, 96, 97, 98 Karl Berry.
Copyright (C) 1994, 95, 96, 97 Karl Berry & Olaf Weber.

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version. 

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.  */

#if defined (HAVE_CONFIG_H)
#include <config.h>
#endif

#include <string.h>

#include "kpse-xfns.h"

/* Return the last element in a path.  */

#ifndef HAVE_BASENAME

/* Return NAME with any leading path stripped off.  This returns a
   pointer into NAME.  For example, `basename ("/foo/bar.baz")'
   returns "bar.baz".  */

static const char *
basename (const char *name)
{
  const char *base = NULL;
  unsigned len = strlen (name);
  
  for (len = strlen (name); len > 0; len--) {
    if (IS_DIR_SEP (name[len - 1]) || IS_DEVICE_SEP (name[len - 1])) {
      base = name + len;
      break;
    }
  }

  if (!base)
    base = name;
  
  return base;
}

#endif

const char *
octave_basename (const char *name)
{
  return (const char *) basename (name);
}
