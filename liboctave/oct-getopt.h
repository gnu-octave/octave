/*

Copyright (C) 2000 John W. Eaton

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
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#if !defined (octave_getopt_h)
#define octave_getopt_h 1

extern int
octave_getopt (int, char *const *, const char *);

extern int
octave_getopt_long (int, char *const *, const char *,
		    const struct option *, int *);

extern char *optarg;

extern int optind;

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
