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

class tree_constant;

extern Pix return_valid_file (const tree_constant& arg);

extern tree_constant *fclose_internal (const tree_constant *args);
extern tree_constant *fflush_internal (const tree_constant *args);
extern tree_constant *fgets_internal (const tree_constant *args, int nargout);
extern tree_constant *fopen_internal (const tree_constant *args);
extern tree_constant *freport_internal (void);
extern tree_constant *frewind_internal (const tree_constant *args);
extern tree_constant *fseek_internal (const tree_constant *args, int nargin);
extern tree_constant *ftell_internal (const tree_constant *args);

extern void initialize_file_io (void);

extern void close_files (void);

extern tree_constant *do_printf (const char *type, const tree_constant *args,
				 int nargin, int nargout);

extern tree_constant *do_scanf (const char *type, const tree_constant *args,
				int nargin, int nargout);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
