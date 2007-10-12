/*

Copyright (C) 1996, 1997, 2000, 2005, 2006, 2007 John W. Eaton

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

#if !defined (octave_liboctave_sysdep_h)
#define octave_liboctave_sysdep_h 1

#include <string>

#include "lo-ieee.h"
class string_vector;

extern std::string octave_getcwd (void);

extern int octave_chdir (const std::string&);

#if ! defined (HAVE_GETHOSTNAME) && defined (HAVE_SYS_UTSNAME_H)
extern int gethostname (char *, int);
#endif

#if defined (__WIN32__) && ! defined (__CYGWIN__)
extern pid_t octave_popen2 (const std::string&, const string_vector&,
    bool, int *, std::string&);
#endif

#if defined (_MSC_VER) && ! defined (HAVE_DIRENT_H)

// FIXME -- it would probably be better to adapt the versions of
// opendir, readdir, and closedir from Emacs as they appear to be more
// complete implementations.  We can probably get along without
// rewinddir.

struct direct
{
  char *d_name;
};

typedef struct __DIR DIR;

extern DIR* opendir (const char *name);
extern void rewinddir (DIR *d);
extern void closedir (DIR *d);
extern struct direct *readdir (DIR *d);

#endif

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
