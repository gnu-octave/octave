/*

Copyright (C) 2000-2012 John W. Eaton

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

/* This gives us a better chance of finding a prototype for strptime
   on some systems.  */

#if ! defined (_XOPEN_SOURCE)
#define _XOPEN_SOURCE
#endif

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "lo-cutils.h"
#include "syswait.h"

#include "savewd.h"
#include "mkdir-p.h"

OCTAVE_API void
octave_qsort (void *base, size_t n, size_t size,
              int (*cmp) (const void *, const void *))
{
  qsort (base, n, size, cmp);
}

OCTAVE_API int
octave_strcasecmp (const char *s1, const char *s2)
{
  return strcasecmp (s1, s2);
}

OCTAVE_API int
octave_strncasecmp (const char *s1, const char *s2, size_t n)
{
  return strncasecmp (s1, s2, n);
}

#ifdef HAVE_LOADLIBRARY_API
#include <windows.h>

/* Need this since in C++ can't cast from int(*)() to void* */
OCTAVE_API void *
octave_w32_library_search (HINSTANCE handle, const char * name)
{
  return (GetProcAddress (handle, name));
}
#endif

OCTAVE_API pid_t
octave_waitpid (pid_t pid, int *status, int options)
{
  return WAITPID (pid, status, options);
}

struct mkdir_options
{
  int (*make_ancestor_function) (const char *, const char *, void *);
  mode_t ancestor_mode;
  mode_t mode;
  mode_t mode_bits;
};

static void
announce_mkdir (const char *dir, void *options)
{
  /* Do nothing */
}

static int
process_dir (char *dir, struct savewd *wd, void *options)
{
  struct mkdir_options const *o = options;
  mode_t ancestor_mode = o->ancestor_mode;
  return (make_dir_parents (dir, wd, o->make_ancestor_function, &ancestor_mode,
                            o->mode, announce_mkdir, o->mode_bits, 
                            (uid_t) -1, (gid_t) -1, false) ? 0 : -1);
}

OCTAVE_API int
octave_mkdir_parents (const char *dir, mode_t mode,
                      int (*make_ancestor) (const char *, const char *, void *))
{
  char *argv[1];
  int retval;
  char *dir2 = malloc (strlen (dir) + 1);
  strcpy (dir2, dir); /* Make a copy to avoid passing a const char* as char* */
  argv[0] = dir2;
  struct mkdir_options o;
  o.make_ancestor_function = make_ancestor;
  o.ancestor_mode = mode | S_IWUSR | S_IXUSR; 
  o.mode = mode | umask(0);
  o.mode_bits = ~(mode & umask(0));
  retval = (savewd_process_files (1, argv, process_dir, &o));
  free (dir2);
  return retval;
}
