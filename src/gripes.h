// gripes.h                                              -*- C++ -*-
/*

Copyright (C) 1992, 1993 John W. Eaton

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

#if !defined (_gripes_h)
#define _gripes_h 1

extern void gripe_string_invalid (void);
extern void gripe_range_invalid (void);
extern void gripe_nonconformant (void);
extern void gripe_nonconformant (int r1, int c1, int r2, int c2);
extern void gripe_empty_arg (const char *name, int is_error);
extern void gripe_square_matrix_required (const char *name);
extern void gripe_user_supplied_eval (const char *name);
extern void gripe_user_returned_invalid (const char *name);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
