// dynamic-ld.h                                        -*- C++ -*-
/*

Copyright (C) 1993 John W. Eaton

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

#if !defined (_dynamic_ld_h)
#define _dynamic_ld_h 1

class tree_constant;

typedef tree_constant* (*builtin_fcn_ptr) (tree_constant*, int, int);

extern void octave_dld_tc2_unlink_by_symbol (const char *name, int hard = 1);

extern void octave_dld_tc2_unlink_by_file (const char *name, int hard = 1);

extern builtin_fcn_ptr octave_dld_tc2 (const char *name,
				       const char *fcn,
				       const char *object);

extern tree_constant *octave_dld_tc2_and_go (tree_constant *args,
					     int nargin, int nargout,
					     const char *name,
					     const char *fcn,
					     const char *object);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
