// file-io.h                                              -*- C++ -*-
/*

Copyright (C) 1993, 1994, 1995 John W. Eaton

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

// Written by John C. Campbell <jcc@che.utexas.edu>.

#if !defined (octave_files_h)
#define octave_files_h 1

#include <Pix.h>

#include "oct-obj.h"

extern void initialize_file_io (void);

extern void close_files (void);

class
file_info
{
public:
  file_info (void);
  file_info (int num, const char *nm, FILE *t, const char *md);
  file_info (const file_info& f);

  file_info& operator = (const file_info& f);

  ~file_info (void);

  int number (void) const;
  const char *name (void) const;
  FILE *fptr (void) const;
  const char *mode (void) const;

  int eof (void) const;
  int error (void) const;

private:
  int file_number;
  char *file_name;
  FILE *file_fptr;
  char *file_mode;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
