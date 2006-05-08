/*

Copyright (C) 1996, 1997 John W. Eaton

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
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

#if !defined (octave_error_h)
#define octave_error_h 1

#include <string>

#define panic_impossible() \
  panic ("impossible state reached in file `%s' at line %d", \
	 __FILE__, __LINE__)

extern void reset_error_handler (void);

extern int warning_enabled (const std::string& id);

extern void message (const char *name, const char *fmt, ...);
extern void usage (const char *fmt, ...);
extern void warning (const char *fmt, ...);
extern void error (const char *fmt, ...);
extern void parse_error (const char *fmt, ...);

extern void
message_with_id (const char *id, const char *name, const char *fmt, ...);

extern void
usage_with_id (const char *id, const char *fmt, ...);

extern void
warning_with_id (const char *id, const char *fmt, ...);

extern void
error_with_id (const char *id, const char *fmt, ...);

extern void
parse_error_with_id (const char *id, const char *fmt, ...);

extern void panic (const char *fmt, ...) GCC_ATTR_NORETURN;

// Helper function for print_usage defined in defun.cc.
extern void defun_usage_message (const std::string& msg);

extern void initialize_default_warning_state (void);

// Current error state.
extern int error_state;

// Current warning state.
extern int warning_state;

// Tell the error handler whether to print messages, or just store
// them for later.  Used for handling errors in eval() and
// the `unwind_protect' statement.
extern int buffer_error_messages;

// TRUE means error messages are turned off.
extern bool discard_error_messages;

// TRUE means warning messages are turned off.
extern bool discard_warning_messages;

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
