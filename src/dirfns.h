// dirfns.h                                              -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994, 1995 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/

#if !defined (octave_dirfns_h)
#define octave_dirfns_h 1

extern char *polite_directory_format (char *);
extern int absolute_pathname (const char *);
extern int absolute_program (const char *);
extern char *base_pathname (char *);
extern void pathname_backup (char *, int);
extern char *make_absolute (const char *, const char *);
extern int is_newer (const char *, time_t);
extern char *get_working_directory (const char *);
extern char *octave_tilde_expand (const char *);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
