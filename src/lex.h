// lex.h                                                 -*- C++ -*-
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

#if !defined (_lex_h)
#define _lex_h 1

typedef struct yy_buffer_state *YY_BUFFER_STATE;

// Associate a buffer with a new file to read.
extern YY_BUFFER_STATE create_buffer (FILE *f);

// Report the current buffer.
extern YY_BUFFER_STATE current_buffer (void);

// Connect to new buffer buffer.
extern void switch_to_buffer (YY_BUFFER_STATE buf);

// Delete a buffer.
extern void delete_buffer (YY_BUFFER_STATE buf);

// Restore a buffer (for unwind-prot).
extern void restore_input_buffer (void *buf);

// Delete a buffer (for unwind-prot).
extern void delete_input_buffer (void *buf);

// See if a function file has extra garbage after the end statement.
extern void check_for_garbage_after_fcn_def (void);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
