// file-io.h                                              -*- C++ -*-
/*

Copyright (C) 1993, 1994 John W. Eaton

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

// Written by John C. Campbell <jcc@che.utexas.edu>.

#if !defined (octave_files_h)
#define octave_files_h 1

#include <Pix.h>

#include "oct-obj.h"

extern Pix return_valid_file (const tree_constant& arg);

extern Octave_object fclose_internal (const Octave_object& args);
extern Octave_object feof_internal (const Octave_object& args, int nargout);
extern Octave_object ferror_internal (const Octave_object& args, int nargout);
extern Octave_object fflush_internal (const Octave_object& args);
extern Octave_object fgets_internal (const Octave_object& args, int nargout);
extern Octave_object fopen_internal (const Octave_object& args);
extern Octave_object fread_internal (const Octave_object& args, int nargout);
extern Octave_object freport_internal (void);
extern Octave_object frewind_internal (const Octave_object& args);
extern Octave_object fseek_internal (const Octave_object& args);
extern Octave_object ftell_internal (const Octave_object& args);
extern Octave_object fwrite_internal (const Octave_object& args, int nargout);

extern void initialize_file_io (void);

extern void close_files (void);

extern Octave_object do_printf (const char *type, const Octave_object& args,
				int nargout);

extern Octave_object do_scanf (const char *type, const Octave_object& args,
			       int nargout);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
