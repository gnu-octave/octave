// file-io.h                                              -*- C++ -*-
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

// Written by John C. Campbell <jcc@che.utexas.edu>.

#if !defined (_files_h)
#define _files_h 1

#ifdef __GNUG__
#pragma interface
#endif

#include "tree-const.h"
#include "utils.h"
#include <Pix.h>
#include <stdio.h>

extern Pix return_valid_file (tree_constant& arg);

extern tree_constant *fclose_internal (tree_constant *args);

extern tree_constant *fflush_internal (tree_constant *args);

extern tree_constant *fgets_internal (tree_constant *args, int nargout);

extern tree_constant *fopen_internal (tree_constant *args);

extern tree_constant *freport_internal ();

extern tree_constant *frewind_internal (tree_constant *args);

extern tree_constant *fseek_internal (tree_constant *args, int nargin);

extern tree_constant *ftell_internal (tree_constant *args);

extern void initialize_file_io ();

extern void close_files ();

extern tree_constant *do_printf (char *type, tree_constant *args,
				 int nargin, int nargout);

extern tree_constant *do_scanf (char *type, tree_constant *args,
				int nargin, int nargout);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
