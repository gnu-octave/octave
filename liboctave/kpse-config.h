/* config.h: master configuration file, included first by all compilable
   source files (not headers).

Copyright (C) 1993, 95, 96, 97 Free Software Foundation, Inc.

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

#ifndef KPATHSEA_CONFIG_H
#define KPATHSEA_CONFIG_H

#if defined (__cplusplus)
extern "C" {
#endif

/* System defines are for non-Unix systems only.  (Testing for all Unix
   variations should be done in configure.)  Presently the defines used
   are: DOS OS2 WIN32.  I do not use any of these systems
   myself; if you do, I'd be grateful for any changes. --kb@mail.tug.org */

/* If we have either DOS or OS2, we are DOSISH.  */
#if defined (DOS) || defined (OS2) || defined (WIN32) || defined(__MSDOS__)
#define DOSISH
#endif

#if defined (DOSISH)
#define MONOCASE_FILENAMES	/* case-insensitive filename comparisons */
#endif

#if defined(__MINGW32__)
#include <windows.h>
#include <fcntl.h>
#include <dirent.h>
#elif defined(WIN32)
#define __STDC__ 1
#include "win32lib.h"
#endif /* not WIN32 */

#ifdef __DJGPP__
#include <fcntl.h>	/* for long filenames' stuff */
#include <dir.h>	/* for `getdisk' */
#include <io.h>		/* for `setmode' */
#endif

/* Some drivers have partially integrated kpathsea changes.  */
#ifndef KPATHSEA
#define KPATHSEA 32
#endif
 
/* System dependencies that are figured out by `configure'.  If we are
   compiling standalone, we get our c-auto.h.  Otherwise, the package
   containing us must provide this (unless it can somehow generate ours
   from c-auto.in).  We use <...> instead of "..." so that the current
   cpp directory (i.e., kpathsea/) won't be searched. */

#include "kpse-lib.h"      /* STREQ, etc. */

extern DIR *xopendir (char *dirname);

extern void xclosedir (DIR *d);
   
/* If you want to find subdirectories in a directory with non-Unix
   semantics (specifically, if a directory with no subdirectories does
   not have exactly two links), define this.  */
#if !defined (DOSISH) || defined(__DJGPP__)
/* Surprise!  DJGPP returns st_nlink exactly like on Unix.  */
#define ST_NLINK_TRICK
#endif /* either not DOSISH or __DJGPP__ */

#ifdef OS2
#define access ln_access
#define chmod ln_chmod
#define creat ln_creat
#define fopen ln_fopen
#define freopen ln_freopen
#define lstat ln_lstat
#define open ln_open
#define remove ln_remove
#define rename ln_rename
#define sopen ln_sopen
#define stat ln_stat
#define unlink ln_unlink
#endif /* OS2 */

#if defined (__cplusplus)
}
#endif

#endif /* not KPATHSEA_CONFIG_H */
